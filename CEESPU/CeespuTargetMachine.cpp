//===-- CeespuTargetMachine.cpp - Define TargetMachine for Ceespu ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements the info about Ceespu target spec.
//
//===----------------------------------------------------------------------===//

#include "Ceespu.h"
#include "CeespuTargetMachine.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
using namespace llvm;

extern "C" void LLVMInitializeCeespuTarget() {
  // Register the target.
  RegisterTargetMachine<CeespuTargetMachine> X(TheCeespuleTarget);
  RegisterTargetMachine<CeespuTargetMachine> Y(TheCeespubeTarget);
  RegisterTargetMachine<CeespuTargetMachine> Z(TheCeespuTarget);
}

// DataLayout: little or big endian
static std::string computeDataLayout(const Triple &TT) {
  if (TT.getArch() == Triple::Ceespueb)
    return "E-m:e-p:32:32-i32:32-n32:32";
  else
    return "e-m:e-p:32:32-i32:32-n32:32";
}

CeespuTargetMachine::CeespuTargetMachine(const Target &T, const Triple &TT,
                                   StringRef CPU, StringRef FS,
                                   const TargetOptions &Options,
                                   Reloc::Model RM, CodeModel::Model CM,
                                   CodeGenOpt::Level OL)
    : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options, RM, CM,
                        OL),
      TLOF(make_unique<TargetLoweringObjectFileELF>()),
      Subtarget(TT, CPU, FS, *this) {
  initAsmInfo();
}
namespace {
// Ceespu Code Generator Pass Configuration Options.
class CeespuPassConfig : public TargetPassConfig {
public:
  CeespuPassConfig(CeespuTargetMachine *TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  CeespuTargetMachine &getCeespuTargetMachine() const {
    return getTM<CeespuTargetMachine>();
  }

  bool addInstSelector() override;
};
}

TargetPassConfig *CeespuTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new CeespuPassConfig(this, PM);
}

// Install an instruction selector pass using
// the ISelDag to gen Ceespu code.
bool CeespuPassConfig::addInstSelector() {
  addPass(createCeespuISelDag(getCeespuTargetMachine()));

  return false;
}
