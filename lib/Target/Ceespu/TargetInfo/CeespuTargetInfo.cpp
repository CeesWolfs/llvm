//===-- CeespuTargetInfo.cpp - Ceespu Target Implementation
//-----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace llvm {
Target &getTheCeespuTarget() {
  static Target TheCeespuTarget;
  return TheCeespuTarget;
}

Target &getTheCeespuebTarget() {
  static Target TheCeespuebTarget;
  return TheCeespuebTarget;
}
}  // namespace llvm

extern "C" void LLVMInitializeCeespuTargetInfo() {
  RegisterTarget<Triple::ceespu> X(getTheCeespuTarget(), "ceespu",
                                   "Ceespu (host endian)", "Ceespu");
  RegisterTarget<Triple::ceespueb> Y(getTheCeespuebTarget(), "ceespu",
                                     "Ceespu (big endian)", "Ceespu");
}
