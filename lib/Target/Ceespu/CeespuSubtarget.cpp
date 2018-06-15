//===-- CeespuSubtarget.cpp - Ceespu Subtarget Information
//------------------===//
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
#include "CeespuFrameLowering.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "ceespu-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "CeespuGenSubtargetInfo.inc"

void CeespuSubtarget::anchor() {}

CeespuSubtarget &CeespuSubtarget::initializeSubtargetDependencies(
    StringRef CPU, StringRef FS, bool Is64Bit) {
  // Determine default and user-specified characteristics
  std::string CPUName = CPU;
  if (CPUName.empty()) CPUName = Is64Bit ? "generic-ceespu" : "generic-ceespu";
  ParseSubtargetFeatures(CPUName, FS);
  return *this;
}

CeespuSubtarget::CeespuSubtarget(const Triple &TT, const std::string &CPU,
                                 const std::string &FS, const TargetMachine &TM)
    : CeespuGenSubtargetInfo(TT, CPU, FS),
      FrameLowering(initializeSubtargetDependencies(CPU, FS, TT.isArch64Bit())),
      InstrInfo(),
      RegInfo(getHwMode()),
      TLInfo(TM, *this) {}
