//===-- CeespuMCAsmInfo.h - Ceespu asm properties -------------------*- C++ -*--====//
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

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/ADT/Triple.h"

namespace llvm {
class Target;
class Triple;

class CeespuMCAsmInfo : public MCAsmInfo {
public:
  explicit CeespuMCAsmInfo(const Triple &TT) {
    if (TT.getArch() == Triple::Ceespueb)
      IsLittleEndian = false;

    WeakRefDirective = "\t.weak\t";
    Data16bitsDirective = ".hword ";
    Data32bitsDirective = ".word ";
    ZeroDirective = ".space ";
    CommentString = ";";
    AscizDirective = ".ascii ";
	PrivateLabelPrefix = "LP";
    UsesELFSectionDirectiveForBSS = true;
    HasSingleParameterDotFile = false;
    HasDotTypeDotSizeDirective = false;

    SupportsDebugInformation = true;
  }
};
}

#endif
