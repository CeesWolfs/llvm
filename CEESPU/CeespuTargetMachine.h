//===-- CeespuTargetMachine.h - Define TargetMachine for Ceespu --- C++ ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the Ceespu specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_CeespuTARGETMACHINE_H
#define LLVM_LIB_TARGET_Ceespu_CeespuTARGETMACHINE_H

#include "CeespuSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class CeespuTargetMachine : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  CeespuSubtarget Subtarget;

public:
  CeespuTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                   StringRef FS, const TargetOptions &Options, Reloc::Model RM,
                   CodeModel::Model CM, CodeGenOpt::Level OL);

  const CeespuSubtarget *getSubtargetImpl() const { return &Subtarget; }
  const CeespuSubtarget *getSubtargetImpl(const Function &) const override {
    return &Subtarget;
  }

  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;

  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }
};
}

#endif
