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


static uint64_t pnv_npu2_xscom_read(void *opaque, hwaddr addr, unsigned size)
{
//    PnvNpu2 *npu = PNV_NPU2(opaque);

    printf("fxb 0 xscom read %lx\n", addr);
    return 0;
}

static void pnv_npu2_xscom_write(void *opaque, hwaddr addr,
                                 uint64_t val, unsigned size)
{
//    PnvNpu2 *npu = PNV_NPU2(opaque);
    printf("fxb 0 xscom write %lx\n", addr);
}

static const MemoryRegionOps pnv_npu2_xscom_ops = {
    .read = pnv_npu2_xscom_read,
    .write = pnv_npu2_xscom_write,
    .valid.min_access_size = 8,
    .valid.max_access_size = 8,
    .impl.min_access_size = 8,
    .impl.max_access_size = 8,
    .endianness = DEVICE_BIG_ENDIAN,
};

static void pnv_npu2_realize(DeviceState *dev, Error **errp)
{
    PnvNpu2 *npu = PNV_NPU2(dev);

    pnv_xscom_region_init(&npu->xscom_regs1, OBJECT(npu),
                          &pnv_npu2_xscom_ops, npu, "xscom1-npu2",
                          PNV9_XSCOM_NPU_SIZE1);
    pnv_xscom_region_init(&npu->xscom_regs2, OBJECT(npu),
                          &pnv_npu2_xscom_ops, npu, "xscom2-npu2",
                          PNV9_XSCOM_NPU_SIZE2);
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
