//===-- CeespuTargetObjectFile.cpp - Ceespu Object Info -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "CeespuTargetObjectFile.h"
#include "CeespuTargetMachine.h"

using namespace llvm;

void CeespuELFTargetObjectFile::Initialize(MCContext &Ctx,
                                          const TargetMachine &TM) {
  TargetLoweringObjectFileELF::Initialize(Ctx, TM);
  InitializeELF(TM.Options.UseInitArray);
}
