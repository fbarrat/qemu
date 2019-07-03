/*
 * QEMU PowerPC PowerNV Processor Nvlink Processing Unit (NPU) model
 *
 * Copyright (c) 2019, IBM Corporation.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "qemu/module.h"
#include "qemu/log.h"

#include "hw/ppc/pnv.h"
#include "hw/ppc/pnv_npu.h"
#include "hw/ppc/pnv_xscom.h"
#include "hw/ppc/fdt.h"

#include <libfdt.h>

#define NPU2_STACK1_CS_SM0_GENID_BAR            0x207
#define NPU2_STACK1_CS_SM1_GENID_BAR            0x237
#define NPU2_STACK1_CS_SM2_GENID_BAR            0x267
#define NPU2_STACK1_CS_SM3_GENID_BAR            0x297
#define NPU2_STACK2_CS_SM0_GENID_BAR            0x407
#define NPU2_STACK2_CS_SM1_GENID_BAR            0x437
#define NPU2_STACK2_CS_SM2_GENID_BAR            0x467
#define NPU2_STACK2_CS_SM3_GENID_BAR            0x497
#define  NPU2_GENID_BAR_EN                      PPC_BIT(0)
#define NPU2_MISC_SCOM_IND_SCOM_ADDR		0x68e
#define NPU2_MISC_SCOM_IND_SCOM_DATA		0x68f
#define NPU2_STACK1_CQ_CTL_FENCE_CONTROL0	0x2e8
#define NPU2_STACK1_CQ_CTL_FENCE_CONTROL1	0x2e9
#define NPU2_STACK2_CQ_CTL_FENCE_CONTROL0	0x4e8
#define NPU2_STACK2_CQ_CTL_FENCE_CONTROL1	0x4e9
#define NPU2_STACK1_CQ_CTL_STATUS		0x2d2
#define NPU2_STACK2_CQ_CTL_STATUS		0x4d2

#define OBUS_ODL0_STATUS 0x2c
#define OBUS_ODL1_STATUS 0x2d

#define INVALID_SCOM_OFFSET (~0ULL)
#define INVALID_SCOM_DATA (~0ULL)
#define INVALID_MMIO_DATA (~0ULL)

/* TODO: fix GETFIELD/SETFIELD macro definitions */
#if HOST_LONG_BITS == 32
# define MASK_TO_LSH(m)          (__builtin_ffsll(m) - 1)
#elif HOST_LONG_BITS == 64
# define MASK_TO_LSH(m)          (__builtin_ffsl(m) - 1)
#else
# error Unknown sizeof long
#endif

#define GETFIELD(m, v)          (((v) & (m)) >> MASK_TO_LSH(m))
#define SETFIELD(m, v, val)                             \
        (((v) & ~(m)) | ((((typeof(v))(val)) << MASK_TO_LSH(m)) & (m)))

static void dt_add_npu_link(void *fdt, int offset, int group, int link_index)
{
    char *name;
    int link_offset;
    uint32_t lane_mask;
    const char compat[] = "ibm,npu-link";

    switch (link_index) {
    case 2:
        lane_mask = 0xf1e000; /* 0-3, 7-10 */
        break;
    case 3:
        lane_mask = 0x00078f; /* 13-16, 20-23 */
        break;
    default:
        assert(0);
    }

    name = g_strdup_printf("link@%x", link_index);
    link_offset = fdt_add_subnode(fdt, offset, name);
    _FDT(link_offset);
    g_free(name);

    _FDT(fdt_setprop(fdt, link_offset, "compatible", compat, sizeof(compat)));
    _FDT(fdt_setprop_cell(fdt, link_offset, "ibm,npu-link-index", link_index));
    _FDT(fdt_setprop_u64(fdt, link_offset, "ibm,npu-phy", PNV9_XSCOM_NPU_INDIRECT0));
    _FDT(fdt_setprop_cell(fdt, link_offset, "ibm,npu-lane-mask", lane_mask));
    _FDT(fdt_setprop_cell(fdt, link_offset, "ibm,npu-group-id", group));
    _FDT(fdt_setprop_u64(fdt, link_offset, "ibm,link-speed", 25000000000ul));
}

static int pnv_npu2_dt_xscom(PnvXScomInterface *dev, void *fdt,
                             int offset)
{
    char *name;
    int npu_offset;
    static int npu_index = 0;
    static int phb_index = 7; /* to match what skiboot does from HDAT */
    const char compat[] = "ibm,power9-npu";
    uint32_t reg[2] = {
        cpu_to_be32(PNV9_XSCOM_NPU_BASE1),
        cpu_to_be32(PNV9_XSCOM_NPU_SIZE1)
    };

    name = g_strdup_printf("npu@%x", PNV9_XSCOM_NPU_BASE1);
    npu_offset = fdt_add_subnode(fdt, offset, name);
    _FDT(npu_offset);
    g_free(name);

    _FDT(fdt_setprop(fdt, npu_offset, "reg", reg, sizeof(reg)));
    _FDT(fdt_setprop(fdt, npu_offset, "compatible", compat, sizeof(compat)));
    _FDT(fdt_setprop_cell(fdt, npu_offset, "ibm,npu-index", npu_index++));
    _FDT(fdt_setprop_cell(fdt, npu_offset, "ibm,phb-index", phb_index++));
    _FDT(fdt_setprop_cell(fdt, npu_offset, "ibm,npu-links", 2));
    dt_add_npu_link(fdt, npu_offset, 1, 2);
    dt_add_npu_link(fdt, npu_offset, 2, 3);
    return 0;
}

static hwaddr ring_to_scom(hwaddr ring_addr)
{
    int stack, block;
    hwaddr scom_offset;

    stack = GETFIELD(PPC_BITMASK(40, 43), ring_addr);
    block = GETFIELD(PPC_BITMASK(44, 47), ring_addr);

    switch (stack) {
    case 1 ... 2:
        scom_offset = 0x200 * stack;
        if (block == 0xC) /* User OTL0 */
            scom_offset += 0x178;
        else if (block == 0xD) /* User OTL1 */
            scom_offset += 0x1A8;
        else
            goto unimplemented;
        break;
    case 3:
        if (block == 0x3) /* MISC */
            scom_offset = 0x680;
        else
            goto unimplemented;
        break;
    case 4 ... 6:
        scom_offset = 0x200 * (stack - 4);
        switch (block) {
        case 0 ... 3: /* CQ SM */
            scom_offset += 0x30 * block;
            break;
        case 4: /* CQ CTL */
            scom_offset += 0xC0;
            break;
        case 5: /* CQ DAT */
            scom_offset += 0xF0;
            break;
        case 0xC: /* OTL0 */
            if (stack == 4)
                goto unimplemented;
            scom_offset += 0x150;
            break;
        case 0xD: /* OTL1 */
            if (stack == 4)
                goto unimplemented;
            scom_offset += 0x180;
            break;
        case 0xE: /* XSL */
            if (stack == 4)
                goto unimplemented;
            scom_offset += 0x1B0;
            break;
        default:
            goto unimplemented;
        }
        break;
    case 7:
        switch (block) {
        case 0: /* ATS */
            scom_offset = 0x600;
            break;
        case 1: /* XTS */
            scom_offset = 0x640;
            break;
        case 2 ... 3: /* MISC */
            scom_offset = 0x680;
            break;
        default:
            goto unimplemented;
        }
        break;
    default:
        goto unimplemented;
    }

    scom_offset += GETFIELD(PPC_BITMASK(48, 63), ring_addr) >> 3;
    printf("ring addr=%lx => scom offset=%lx\n", ring_addr, scom_offset);
    assert(scom_offset <= PNV9_XSCOM_NPU_SIZE1);
    return scom_offset;

unimplemented:
    qemu_log_mask(LOG_UNIMP, "NPU: untranslated ring addr %lx\n", ring_addr);
    return INVALID_SCOM_OFFSET;
}

static hwaddr indirect_to_scom(hwaddr indirect_addr)
{
    int indirect_len;
    hwaddr ring_addr;

    indirect_len = GETFIELD(PPC_BITMASK(24, 25), indirect_addr);
    if (indirect_len != 0b11) {
        qemu_log_mask(LOG_UNIMP, "NPU: only 8 byte mmio access supported\n");
        return INVALID_SCOM_OFFSET;
    }
    ring_addr = GETFIELD(PPC_BITMASK(0, 23), indirect_addr);
    return ring_to_scom(ring_addr);
}

static void update_fence_state(PnvNpu2 *npu, int brick, int val)
{
    hwaddr offset;
    uint64_t fence_mask;

    printf("Setting fence state of brick %d to %d\n", brick, val);
    switch (brick) {
    case 2:
        offset = NPU2_STACK1_CQ_CTL_STATUS;
        fence_mask = PPC_BITMASK(48, 49);
        break;
    case 3:
        offset = NPU2_STACK1_CQ_CTL_STATUS;
        fence_mask = PPC_BITMASK(50, 51);
        break;
    case 4:
        offset = NPU2_STACK2_CQ_CTL_STATUS;
        fence_mask = PPC_BITMASK(48, 49);
        break;
    case 5:
        offset = NPU2_STACK2_CQ_CTL_STATUS;
        fence_mask = PPC_BITMASK(50, 51);
        break;
    default:
        assert(false);
    }
    npu->scom[offset] = SETFIELD(fence_mask, npu->scom[offset], val);
    npu->fence_state[brick] = !!val;
}

static void set_genid_bar(PnvNpu2 *npu, int stack, uint64_t val)
{
    MemoryRegion *sysmem = get_system_memory();
    uint64_t addr;

    assert(stack == 1 || stack == 2);

    /*
     * The genid bar is replicated in each SMx block, so we'll get 4
     * identical operations each time the bar is set. So don't do
     * anything is we're writing the exact same value.
     */
    if (val == npu->genid_bar[stack])
        return;

    if (npu->genid_bar[stack])
        memory_region_del_subregion(sysmem, &npu->genid_mr[stack]);

    npu->genid_bar[stack] = val;
    addr = (GETFIELD(PPC_BITMASK(3, 35), val) << 16 ) | 0x6000000000000;
    if (val & NPU2_GENID_BAR_EN) {
        printf("mapping genid bar at %lx\n", addr);
        memory_region_add_subregion(sysmem, addr, &npu->genid_mr[stack]);
    }
}

static uint64_t pnv_npu2_xscom1_read(void *opaque, hwaddr addr, unsigned size)
{
    PnvNpu2 *npu = PNV_NPU2(opaque);
    hwaddr scom_offset;
    int reg = addr >> 3;
    uint64_t val;

    if (reg >= PNV9_XSCOM_NPU_SIZE1)
        return INVALID_SCOM_DATA;

    val = npu->scom[reg];
    switch(reg) {
        case NPU2_MISC_SCOM_IND_SCOM_DATA:
            scom_offset = indirect_to_scom(npu->scom[NPU2_MISC_SCOM_IND_SCOM_ADDR]);
            return pnv_npu2_xscom1_read(opaque, scom_offset << 3, size);
    }

    return val;
}

static void pnv_npu2_xscom1_write(void *opaque, hwaddr addr,
                                  uint64_t val, unsigned size)
{
    PnvNpu2 *npu = PNV_NPU2(opaque);
    hwaddr scom_offset;
    int brick, reg = addr >> 3;
    uint64_t subval;

    if (reg >= PNV9_XSCOM_NPU_SIZE1)
        return;

    npu->scom[reg] = val;
    switch(reg) {
    case NPU2_MISC_SCOM_IND_SCOM_DATA:
        scom_offset = indirect_to_scom(npu->scom[NPU2_MISC_SCOM_IND_SCOM_ADDR]);
        pnv_npu2_xscom1_write(opaque, scom_offset << 3, val, size);
        return;
    case NPU2_STACK1_CQ_CTL_FENCE_CONTROL0:
    case NPU2_STACK1_CQ_CTL_FENCE_CONTROL1:
    case NPU2_STACK2_CQ_CTL_FENCE_CONTROL0:
    case NPU2_STACK2_CQ_CTL_FENCE_CONTROL1:
        brick = (reg / 0x200) * 2 + reg % 2;
        subval = GETFIELD(PPC_BITMASK(0, 1), val);
        update_fence_state(npu, brick, subval);
        break;
    case NPU2_STACK1_CS_SM0_GENID_BAR:
    case NPU2_STACK1_CS_SM1_GENID_BAR:
    case NPU2_STACK1_CS_SM2_GENID_BAR:
    case NPU2_STACK1_CS_SM3_GENID_BAR:
        set_genid_bar(npu, 1, val);
        break;
    case NPU2_STACK2_CS_SM0_GENID_BAR:
    case NPU2_STACK2_CS_SM1_GENID_BAR:
    case NPU2_STACK2_CS_SM2_GENID_BAR:
    case NPU2_STACK2_CS_SM3_GENID_BAR:
        set_genid_bar(npu, 2, val);
        break;
    }
}

static const MemoryRegionOps pnv_npu2_xscom1_ops = {
    .read = pnv_npu2_xscom1_read,
    .write = pnv_npu2_xscom1_write,
    .valid.min_access_size = 8,
    .valid.max_access_size = 8,
    .impl.min_access_size = 8,
    .impl.max_access_size = 8,
    .endianness = DEVICE_BIG_ENDIAN,
};

static uint64_t pnv_npu2_xscom2_read(void *opaque, hwaddr addr, unsigned size)
{
    int reg = addr >> 3;

    if (reg >= PNV9_XSCOM_NPU_SIZE2)
        return INVALID_SCOM_DATA;

    return 0;
}

static void pnv_npu2_xscom2_write(void *opaque, hwaddr addr,
                                 uint64_t val, unsigned size)
{
}

static const MemoryRegionOps pnv_npu2_xscom2_ops = {
    .read = pnv_npu2_xscom2_read,
    .write = pnv_npu2_xscom2_write,
    .valid.min_access_size = 8,
    .valid.max_access_size = 8,
    .impl.min_access_size = 8,
    .impl.max_access_size = 8,
    .endianness = DEVICE_BIG_ENDIAN,
};

static uint64_t pnv_obus0_xscom_read(void *opaque, hwaddr addr, unsigned size)
{
    int reg = addr >> 3;

    printf("reading obus0 offset %x\n", reg);
    switch (reg) {
    case OBUS_ODL0_STATUS:
    case OBUS_ODL1_STATUS:
        return 0x2900ffff00007100; /* link is trained */
    default:
        return INVALID_SCOM_DATA;
    }
}

static void pnv_obus0_xscom_write(void *opaque, hwaddr addr,
                                  uint64_t val, unsigned size)
{
    int reg = addr >> 3;

    printf("writing obus0 offset %x\n", reg);
}

static const MemoryRegionOps pnv_obus0_xscom_ops = {
    .read = pnv_obus0_xscom_read,
    .write = pnv_obus0_xscom_write,
    .valid.min_access_size = 8,
    .valid.max_access_size = 8,
    .impl.min_access_size = 8,
    .impl.max_access_size = 8,
    .endianness = DEVICE_BIG_ENDIAN,
};

/* fxb high-level logic should be moved to pnv_xscom, since it's
 * independent from chiplet */
static uint64_t pnv_obus0_xscom_indirect_read(void *opaque, hwaddr addr,
                                              unsigned size)
{
    PnvNpu2 *npu = PNV_NPU2(opaque);
    bool read;
    uint64_t res, reg, data;

    if (!npu->xscom_obus0_indirect_addr) {
        qemu_log_mask(LOG_GUEST_ERROR,
                      "Indirect xscom read with no operation in progress\n");
        return INVALID_SCOM_DATA;
    }
    addr = GETFIELD(PPC_BITMASK(12, 31), npu->xscom_obus0_indirect_addr);
    reg = GETFIELD(PPC_BITMASK(10, 21), npu->xscom_obus0_indirect_addr);
    read = npu->xscom_obus0_indirect_addr & PPC_BIT(0);

    switch (reg) {
    case 0x3c1: /* ZCAL DONE, ZCAL ERROR */
        data = PPC_BIT(50);
        break;
    case 0x0ca: /* INIT DONE, DCCAL DONE, LANE BUSY */
        data = PPC_BIT(48) | PPC_BIT(49);
        break;
    default:
        data = 0;
    }
    printf("fxb obus indirect op reg=%lx data=%lx\n", reg, data);
    res = PPC_BIT(32); /* data ready */
    res = SETFIELD(PPC_BITMASK(12, 31), res, addr);
    res = SETFIELD(PPC_BITMASK(48, 63), res, data);
    if (read) {
        res |= PPC_BIT(0);
    }
    npu->xscom_obus0_indirect_addr = 0;
    printf("fxb obus indirect op at %lx, returning %lx\n", addr, res);
    return res;
}

static void pnv_obus0_xscom_indirect_write(void *opaque, hwaddr addr,
                                           uint64_t val, unsigned size)
{
    PnvNpu2 *npu = PNV_NPU2(opaque);

    if (npu->xscom_obus0_indirect_addr) {
        qemu_log_mask(LOG_GUEST_ERROR,
                      "Indirect xscom write while previous operation still in progress\n");
        /* keep going */
    }
    npu->xscom_obus0_indirect_addr = val;
}

static const MemoryRegionOps pnv_obus0_xscom_indirect_ops = {
    .read = pnv_obus0_xscom_indirect_read,
    .write = pnv_obus0_xscom_indirect_write,
    .valid.min_access_size = 8,
    .valid.max_access_size = 8,
    .impl.min_access_size = 8,
    .impl.max_access_size = 8,
    .endianness = DEVICE_BIG_ENDIAN,
};

static uint64_t pnv_npu2_genid_mmio_read(void *opaque, hwaddr addr,
                                         unsigned size)
{
    printf("reading genid mmio addr=%lx, size %d\n", addr, size);

    return INVALID_MMIO_DATA;
}

static void pnv_npu2_genid_mmio_write(void *opaque, hwaddr addr,
                                      uint64_t val, unsigned size)
{
    printf("writing genid mmio addr=%lx, size %d\n", addr, size);
}

static const MemoryRegionOps pnv_npu2_genid_mmio_ops = {
    .read = pnv_npu2_genid_mmio_read,
    .write = pnv_npu2_genid_mmio_write,
    .endianness = DEVICE_BIG_ENDIAN,
    .valid = {
        .min_access_size = 1,
        .max_access_size = 8,
    },
    .impl = {
        .min_access_size = 1,
        .max_access_size = 8,
    },
};

static void pnv_npu2_realize(DeviceState *dev, Error **errp)
{
    PnvNpu2 *npu = PNV_NPU2(dev);

    /* NPU xscom region */
    pnv_xscom_region_init(&npu->xscom_regs1, OBJECT(npu),
                          &pnv_npu2_xscom1_ops, npu,
                          "xscom1-npu2",
                          PNV9_XSCOM_NPU_SIZE1);

    pnv_xscom_region_init(&npu->xscom_regs2, OBJECT(npu),
                          &pnv_npu2_xscom2_ops, npu,
                          "xscom2-npu2",
                          PNV9_XSCOM_NPU_SIZE2);

    /* PHY xscom region */
    pnv_xscom_region_init(&npu->xscom_obus0_regs, OBJECT(npu),
                          &pnv_obus0_xscom_ops, npu,
                          "xscom-obus0",
                          PNV9_XSCOM_OBUS_SIZE);

    pnv_xscom_region_init(&npu->xscom_obus0_indirect_regs, OBJECT(npu),
                          &pnv_obus0_xscom_indirect_ops, npu,
                          "xscom-obus0-indirect",
                          PNV9_XSCOM_OBUS_INDIRECT_SIZE);

    /* generation ID bar, used for config space access on opencapi */
    memory_region_init_io(&npu->genid_mr[1], OBJECT(npu), &pnv_npu2_genid_mmio_ops,
                          npu, "genid-stack1", PNV9_NPU_GENID_SIZE);
}

static void pnv_npu2_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    PnvXScomInterfaceClass *xscomc = PNV_XSCOM_INTERFACE_CLASS(klass);
//    PnvNpu2Class *npu2c = PNV_NPU2_CLASS(klass);

    xscomc->dt_xscom = pnv_npu2_dt_xscom;

    dc->desc = "PowerNV NPU2";
    dc->realize = pnv_npu2_realize;
}

static const TypeInfo pnv_npu2_info = {
    .name          = TYPE_PNV_NPU2,
    .parent        = TYPE_DEVICE,
    .instance_size = sizeof(PnvNpu2),
    .class_size    = sizeof(PnvNpu2Class),
    .class_init    = pnv_npu2_class_init,
    .interfaces    = (InterfaceInfo[]) {
        { TYPE_PNV_XSCOM_INTERFACE },
        { }
    }
};

static void pnv_npu_register_types(void)
{
    type_register_static(&pnv_npu2_info);
}

type_init(pnv_npu_register_types);
