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

#ifndef PPC_PNV_NPU_H
#define PPC_PNV_NPU_H

#include "hw/ppc/ocapi_afu.h"

#define TYPE_PNV_NPU2 "pnv-npu2"

#define PNV_NPU2(obj)                           \
    OBJECT_CHECK(PnvNpu2, (obj), TYPE_PNV_NPU2)

#define NPU2_STACK_COUNT 3
#define NPU2_BRICK_COUNT 6

typedef struct PnvNpu2 {
    DeviceState parent;

    MemoryRegion xscom_obus0_regs;
    MemoryRegion xscom_obus0_indirect_regs;
    uint64_t xscom_obus0_indirect_addr;

    MemoryRegion xscom_regs1;
    uint64_t scom[0x702 /* fxb PNV9_XSCOM_NPU_SIZE1 */];
    MemoryRegion xscom_regs2;

    MemoryRegion genid_mr[NPU2_STACK_COUNT];
    uint64_t genid_bar[NPU2_STACK_COUNT];

    bool fence_state[NPU2_BRICK_COUNT];

    ocapi_afu_t *afu[NPU2_BRICK_COUNT];
    uint64_t config_space_addr[NPU2_BRICK_COUNT];
} PnvNpu2;

#define PNV_NPU2_CLASS(klass)                                   \
    OBJECT_CLASS_CHECK(PnvNpu2Class, (klass), TYPE_PNV_NPU2)
#define PNV_NPU2_GET_CLASS(obj)                                 \
    OBJECT_GET_CLASS(PnvNpu2Class, (obj), TYPE_PNV_NPU2)

typedef struct PnvNpu2Class {
    DeviceClass parent_class;
} PnvNpu2Class;

#endif /* PPC_PNV_NPU_H */
