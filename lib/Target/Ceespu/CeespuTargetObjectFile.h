//===-- CeespuTargetObjectFile.h - Ceespu Object Info -*- C++ ---------*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_CeespuTARGETOBJECTFILE_H
#define LLVM_LIB_TARGET_Ceespu_CeespuTARGETOBJECTFILE_H

#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"

namespace llvm {
class CeespuTargetMachine;

/// This implementation is used for Ceespu ELF targets.
class CeespuELFTargetObjectFile : public TargetLoweringObjectFileELF {
  void Initialize(MCContext &Ctx, const TargetMachine &TM) override;
};

} // end namespace llvm

#endif
