//===-- CeespuMCTargetDesc.cpp - Ceespu Target Descriptions
//---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Ceespu specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "CeespuMCTargetDesc.h"
#include "Ceespu.h"
#include "CeespuMCAsmInfo.h"
#include "InstPrinter/CeespuInstPrinter.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "CeespuGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "CeespuGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "CeespuGenRegisterInfo.inc"

using namespace llvm;

static MCInstrInfo *createCeespuMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitCeespuMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createCeespuMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitCeespuMCRegisterInfo(X, Ceespu::R11 /* RAReg doesn't exist */);
  return X;
}

static MCSubtargetInfo *createCeespuMCSubtargetInfo(const Triple &TT,
                                                    StringRef CPU,
                                                    StringRef FS) {
  return createCeespuMCSubtargetInfoImpl(TT, CPU, FS);
}

static MCCodeGenInfo *createCeespuMCCodeGenInfo(const Triple &TT,
                                                Reloc::Model RM,
                                                CodeModel::Model CM,
                                                CodeGenOpt::Level OL) {
  MCCodeGenInfo *X = new MCCodeGenInfo();
  X->initMCCodeGenInfo(RM, CM, OL);
  return X;
}

static MCStreamer *createCeespuMCStreamer(const Triple &T, MCContext &Ctx,
                                          MCAsmBackend &MAB,
                                          raw_pwrite_stream &OS,
                                          MCCodeEmitter *Emitter,
                                          bool RelaxAll) {
  return createELFStreamer(Ctx, MAB, OS, Emitter, RelaxAll);
}

static MCInstPrinter *createCeespuMCInstPrinter(const Triple &T,
                                                unsigned SyntaxVariant,
                                                const MCAsmInfo &MAI,
                                                const MCInstrInfo &MII,
                                                const MCRegisterInfo &MRI) {
  if (SyntaxVariant == 0) return new CeespuInstPrinter(MAI, MII, MRI);
  return 0;
}

extern "C" void LLVMInitializeCeespuTargetMC() {
  for (Target *T : {&TheCeespuleTarget, &TheCeespubeTarget, &TheCeespuTarget}) {
    // Register the MC asm info.
    RegisterMCAsmInfo<CeespuMCAsmInfo> X(*T);

    // Register the MC codegen info.
    TargetRegistry::RegisterMCCodeGenInfo(*T, createCeespuMCCodeGenInfo);

    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createCeespuMCInstrInfo);

    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createCeespuMCRegisterInfo);

    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T, createCeespuMCSubtargetInfo);

    // Register the object streamer
    TargetRegistry::RegisterELFStreamer(*T, createCeespuMCStreamer);

    // Register the MCInstPrinter.
    TargetRegistry::RegisterMCInstPrinter(*T, createCeespuMCInstPrinter);
  }

  // Register the MC code emitter
  TargetRegistry::RegisterMCCodeEmitter(TheCeespuleTarget,
                                        createCeespuMCCodeEmitter);
  TargetRegistry::RegisterMCCodeEmitter(TheCeespubeTarget,
                                        createCeespubeMCCodeEmitter);

  // Register the ASM Backend
  TargetRegistry::RegisterMCAsmBackend(TheCeespuleTarget,
                                       createCeespuAsmBackend);
  TargetRegistry::RegisterMCAsmBackend(TheCeespubeTarget,
                                       createCeespubeAsmBackend);

  if (sys::IsLittleEndianHost) {
    TargetRegistry::RegisterMCCodeEmitter(TheCeespuTarget,
                                          createCeespuMCCodeEmitter);
    TargetRegistry::RegisterMCAsmBackend(TheCeespuTarget,
                                         createCeespuAsmBackend);
  } else {
    TargetRegistry::RegisterMCCodeEmitter(TheCeespuTarget,
                                          createCeespubeMCCodeEmitter);
    TargetRegistry::RegisterMCAsmBackend(TheCeespuTarget,
                                         createCeespubeAsmBackend);
  }
}
