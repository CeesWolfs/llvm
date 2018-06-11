//===-- Ceespu.h - Top-level interface for Ceespu representation ------*- C++
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_Ceespu_H
#define LLVM_LIB_TARGET_Ceespu_Ceespu_H

#include "MCTargetDesc/CeespuMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class CeespuTargetMachine;

FunctionPass *createCeespuISelDag(CeespuTargetMachine &TM);
}  // namespace llvm

#endif
