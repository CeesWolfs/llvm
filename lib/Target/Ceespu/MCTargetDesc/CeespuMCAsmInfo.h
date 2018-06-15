//===-- CeespuMCAsmInfo.h - Ceespu Asm Info ----------------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the CeespuMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuMCASMINFO_H
#define LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {
class Triple;

class CeespuMCAsmInfo : public MCAsmInfoELF {
  void anchor() override;

public:
  explicit CeespuMCAsmInfo(const Triple &TargetTriple);
};

} // namespace llvm

#endif
