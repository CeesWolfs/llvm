//===-- CeespuFixupKinds.h - Ceespu Specific Fixup Entries --------*- C++
//-*-===//
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
  // fixup_ceespu_hi16 - 16-bit fixup corresponding to hi(foo) for
  // instructions like sethi
  fixup_ceespu_hi16 = FirstTargetFixupKind,
  // fixup_ceespu_lo12_i - 12-bit fixup corresponding to lo(foo) for
  // instructions like addi
  fixup_ceespu_lo16,
  // fixup_ceespu_lo12_s - 12-bit fixup corresponding to lo(foo) for
  // the store instructions
  fixup_ceespu_lo12,
  // fixup_ceespu_cbranch - 16 bit relative symbol
  fixup_ceespu_cbranch,
  // fixup_ceespu_lo_22 - 22 bit symbol fixup for unconditianal branches and
  // calls
  fixup_ceespu_lo22,

  // fixup_ceespu_invalid - used as a sentinel and a marker, must be last fixup
  fixup_ceespu_invalid,
  NumTargetFixupKinds = fixup_ceespu_invalid - FirstTargetFixupKind
};
}  // end namespace Ceespu
}  // end namespace llvm

#endif
