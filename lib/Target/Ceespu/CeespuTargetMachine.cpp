//===-- CeespuTargetMachine.cpp - Define TargetMachine for Ceespu
//-----------===//
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

#include "CeespuTargetMachine.h"
#include "Ceespu.h"
#include "CeespuTargetObjectFile.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
using namespace llvm;

extern "C" void LLVMInitializeCeespuTarget() {
  RegisterTargetMachine<CeespuTargetMachine> X(getTheCeespuTarget());
  RegisterTargetMachine<CeespuTargetMachine> Y(getTheCeespuebTarget());
}

static std::string computeDataLayout(const Triple &TT) {
  if (TT.getArch() == Triple::ceespueb) {
    return "E-m:E-p:32:32:32-i32:32-n32-S32";
  }
  return "e-m:e-p:32:32:32-i32:32-n32-S32";
}

static Reloc::Model getEffectiveRelocModel(const Triple &TT,
                                           Optional<Reloc::Model> RM) {
  if (!RM.hasValue()) return Reloc::Static;
  return *RM;
}

static CodeModel::Model getEffectiveCodeModel(Optional<CodeModel::Model> CM) {
  if (CM) return *CM;
  return CodeModel::Small;
}

CeespuTargetMachine::CeespuTargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         Optional<Reloc::Model> RM,
                                         Optional<CodeModel::Model> CM,
                                         CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options,
                        getEffectiveRelocModel(TT, RM),
                        getEffectiveCodeModel(CM), OL),
      TLOF(make_unique<CeespuELFTargetObjectFile>()),
      Subtarget(TT, CPU, FS, *this) {
  initAsmInfo();
}

namespace {
class CeespuPassConfig : public TargetPassConfig {
 public:
  CeespuPassConfig(CeespuTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  CeespuTargetMachine &getCeespuTargetMachine() const {
    return getTM<CeespuTargetMachine>();
  }

  bool addInstSelector() override;
  void addPreEmitPass() override;
};
}  // namespace

TargetPassConfig *CeespuTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new CeespuPassConfig(*this, PM);
}

bool CeespuPassConfig::addInstSelector() {
  addPass(createCeespuISelDag(getCeespuTargetMachine()));

  return false;
}

void CeespuPassConfig::addPreEmitPass() { addPass(&BranchRelaxationPassID); }
