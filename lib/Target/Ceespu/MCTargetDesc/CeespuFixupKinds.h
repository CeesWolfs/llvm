//===-- CeespuFixupKinds.h - Ceespu Specific Fixup Entries --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuFIXUPKINDS_H
#define LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

#undef Ceespu

namespace llvm {
namespace Ceespu {
enum Fixups {
  // fixup_ceespu_hi20 - 20-bit fixup corresponding to hi(foo) for
  // instructions like lui
  fixup_ceespu_hi20 = FirstTargetFixupKind,
  // fixup_ceespu_lo12_i - 12-bit fixup corresponding to lo(foo) for
  // instructions like addi
  fixup_ceespu_lo12_i,
  // fixup_ceespu_lo12_s - 12-bit fixup corresponding to lo(foo) for
  // the S-type store instructions
  fixup_ceespu_lo12_s,
  // fixup_ceespu_pcrel_hi20 - 20-bit fixup corresponding to pcrel_hi(foo) for
  // instructions like auipc
  fixup_ceespu_pcrel_hi20,
  // fixup_ceespu_pcrel_lo12_i - 12-bit fixup corresponding to pcrel_lo(foo) for
  // instructions like addi
  fixup_ceespu_pcrel_lo12_i,
  // fixup_ceespu_pcrel_lo12_s - 12-bit fixup corresponding to pcrel_lo(foo) for
  // the S-type store instructions
  fixup_ceespu_pcrel_lo12_s,
  // fixup_ceespu_jal - 20-bit fixup for symbol references in the jal
  // instruction
  fixup_ceespu_jal,
  // fixup_ceespu_branch - 12-bit fixup for symbol references in the branch
  // instructions
  fixup_ceespu_branch,
  // fixup_ceespu_rvc_jump - 11-bit fixup for symbol references in the
  // compressed jump instruction
  fixup_ceespu_rvc_jump,
  // fixup_ceespu_rvc_branch - 8-bit fixup for symbol references in the
  // compressed branch instruction
  fixup_ceespu_rvc_branch,
  // fixup_ceespu_call - A fixup representing a call attached to the auipc
  // instruction in a pair composed of adjacent auipc+jalr instructions.
  fixup_ceespu_call,
  // fixup_ceespu_relax - Used to generate an R_Ceespu_RELAX relocation type,
  // which indicates the linker may relax the instruction pair.
  fixup_ceespu_relax,

  // fixup_ceespu_invalid - used as a sentinel and a marker, must be last fixup
  fixup_ceespu_invalid,
  NumTargetFixupKinds = fixup_ceespu_invalid - FirstTargetFixupKind
};
} // end namespace Ceespu
} // end namespace llvm

#endif
