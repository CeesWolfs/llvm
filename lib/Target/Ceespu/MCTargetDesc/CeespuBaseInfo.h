//===-- CeespuBaseInfo.h - Top level definitions for Ceespu MC ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone enum definitions for the Ceespu target
// useful for the compiler back-end and the MC libraries.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuBASEINFO_H
#define LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuBASEINFO_H

#include "CeespuMCTargetDesc.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"

namespace llvm {

// CeespuII - This namespace holds all of the target specific flags that
// instruction info tracks. All definitions must match CeespuInstrFormats.td.
namespace CeespuII {
enum {
  InstFormatPseudo = 0,
  InstFormatR = 1,
  InstFormatR4 = 2,
  InstFormatI = 3,
  InstFormatS = 4,
  InstFormatB = 5,
  InstFormatU = 6,
  InstFormatJ = 7,
  InstFormatCR = 8,
  InstFormatCI = 9,
  InstFormatCSS = 10,
  InstFormatCIW = 11,
  InstFormatCL = 12,
  InstFormatCS = 13,
  InstFormatCB = 14,
  InstFormatCJ = 15,
  InstFormatOther = 16,

  InstFormatMask = 31
};

enum {
  MO_None,
  MO_LO,
  MO_HI,
  MO_PCREL_HI,
};
} // namespace CeespuII

// Describes the predecessor/successor bits used in the FENCE instruction.
namespace CeespuFenceField {
enum FenceField {
  I = 8,
  O = 4,
  R = 2,
  W = 1
};
}

// Describes the supported floating point rounding mode encodings.
namespace CeespuFPRndMode {
enum RoundingMode {
  RNE = 0,
  RTZ = 1,
  RDN = 2,
  RUP = 3,
  RMM = 4,
  DYN = 7,
  Invalid
};

inline static StringRef roundingModeToString(RoundingMode RndMode) {
  switch (RndMode) {
  default:
    llvm_unreachable("Unknown floating point rounding mode");
  case CeespuFPRndMode::RNE:
    return "rne";
  case CeespuFPRndMode::RTZ:
    return "rtz";
  case CeespuFPRndMode::RDN:
    return "rdn";
  case CeespuFPRndMode::RUP:
    return "rup";
  case CeespuFPRndMode::RMM:
    return "rmm";
  case CeespuFPRndMode::DYN:
    return "dyn";
  }
}

inline static RoundingMode stringToRoundingMode(StringRef Str) {
  return StringSwitch<RoundingMode>(Str)
      .Case("rne", CeespuFPRndMode::RNE)
      .Case("rtz", CeespuFPRndMode::RTZ)
      .Case("rdn", CeespuFPRndMode::RDN)
      .Case("rup", CeespuFPRndMode::RUP)
      .Case("rmm", CeespuFPRndMode::RMM)
      .Case("dyn", CeespuFPRndMode::DYN)
      .Default(CeespuFPRndMode::Invalid);
}
} // namespace CeespuFPRndMode
} // namespace llvm

#endif
