//===-- CeespuMCTargetDesc.cpp - Ceespu Target Descriptions
//-----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// This file provides Ceespu-specific target descriptions.
///
//===----------------------------------------------------------------------===//

#include "CeespuMCTargetDesc.h"
#include "CeespuELFStreamer.h"
#include "CeespuMCAsmInfo.h"
#include "CeespuTargetStreamer.h"
#include "InstPrinter/CeespuInstPrinter.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "CeespuGenInstrInfo.inc"

#define GET_REGINFO_MC_DESC
#include "CeespuGenRegisterInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "CeespuGenSubtargetInfo.inc"

using namespace llvm;

static MCInstrInfo *createCeespuMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitCeespuMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createCeespuMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitCeespuMCRegisterInfo(X, Ceespu::R1);
  return X;
}

static MCAsmInfo *createCeespuMCAsmInfo(const MCRegisterInfo &MRI,
                                        const Triple &TT) {
  return new CeespuMCAsmInfo(TT);
}

static MCSubtargetInfo *createCeespuMCSubtargetInfo(const Triple &TT,
                                                    StringRef CPU,
                                                    StringRef FS) {
  std::string CPUName = CPU;
  if (CPUName.empty()) CPUName = "generic-ceespu";
  return createCeespuMCSubtargetInfoImpl(TT, CPUName, FS);
}

static MCInstPrinter *createCeespuMCInstPrinter(const Triple &T,
                                                unsigned SyntaxVariant,
                                                const MCAsmInfo &MAI,
                                                const MCInstrInfo &MII,
                                                const MCRegisterInfo &MRI) {
  return new CeespuInstPrinter(MAI, MII, MRI);
}

static MCTargetStreamer *createCeespuObjectTargetStreamer(
    MCStreamer &S, const MCSubtargetInfo &STI) {
  const Triple &TT = STI.getTargetTriple();
  if (TT.isOSBinFormatELF()) return new CeespuTargetELFStreamer(S, STI);
  return nullptr;
}

static MCTargetStreamer *createCeespuAsmTargetStreamer(
    MCStreamer &S, formatted_raw_ostream &OS, MCInstPrinter *InstPrint,
    bool isVerboseAsm) {
  return new CeespuTargetAsmStreamer(S, OS);
}

extern "C" void LLVMInitializeCeespuTargetMC() {
  for (Target *T : {&getTheCeespuebTarget(), &getTheCeespuTarget()}) {
    TargetRegistry::RegisterMCAsmInfo(*T, createCeespuMCAsmInfo);
    TargetRegistry::RegisterMCInstrInfo(*T, createCeespuMCInstrInfo);
    TargetRegistry::RegisterMCRegInfo(*T, createCeespuMCRegisterInfo);
    TargetRegistry::RegisterMCAsmBackend(*T, createCeespuAsmBackend);
    TargetRegistry::RegisterMCCodeEmitter(*T, createCeespuMCCodeEmitter);
    TargetRegistry::RegisterMCInstPrinter(*T, createCeespuMCInstPrinter);
    TargetRegistry::RegisterMCSubtargetInfo(*T, createCeespuMCSubtargetInfo);
    TargetRegistry::RegisterObjectTargetStreamer(
        *T, createCeespuObjectTargetStreamer);

    // Register the asm target streamer.
    TargetRegistry::RegisterAsmTargetStreamer(*T,
                                              createCeespuAsmTargetStreamer);
  }
}
