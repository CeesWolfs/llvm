//===-- CeespuTargetInfo.cpp - Ceespu Target Implementation
//---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Ceespu.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace llvm {
Target TheCeespuleTarget;
Target TheCeespubeTarget;
Target TheCeespuTarget;
}  // namespace llvm

extern "C" void LLVMInitializeCeespuTargetInfo() {
  RegisterTarget<Triple::Ceespu, /*HasJIT=*/false> X(
      TheCeespuleTarget, "Ceespu", "Ceespu (host endian)", "Ceespu");
  RegisterTarget<Triple::Ceespuel, /*HasJIT=*/false> Y(
      TheCeespuleTarget, "Ceespuel", "Ceespu (little endian)", "Ceespu");
  RegisterTarget<Triple::Ceespueb, /*HasJIT=*/false> Z(
      TheCeespubeTarget, "Ceespueb", "Ceespu (big endian)", "Ceespu");
}
