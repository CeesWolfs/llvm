//===-- CeespuSubtarget.cpp - Ceespu Subtarget Information ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Ceespu specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "CeespuSubtarget.h"
#include "Ceespu.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "Ceespu-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "CeespuGenSubtargetInfo.inc"

void CeespuSubtarget::anchor() {}

CeespuSubtarget::CeespuSubtarget(const Triple &TT, const std::string &CPU,
                           const std::string &FS, const TargetMachine &TM)
    : CeespuGenSubtargetInfo(TT, CPU, FS), InstrInfo(), FrameLowering(*this),
      TLInfo(TM, *this) {}
bool CeespuSubtarget::abiUsesSoftFloat() const {
//  return TM->Options.UseSoftFloat;
  return true;
}