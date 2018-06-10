//===-- CeespuTargetInfo.cpp - Ceespu Target Implementation ---------------------===//
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
}

extern "C" void LLVMInitializeCeespuTargetInfo() {
  TargetRegistry::RegisterTarget(TheCeespuTarget, "Ceespu",
                                 "Ceespu (host endian)",
                                 [](Triple::ArchType) { return false; }, false);
  RegisterTarget<Triple::Ceespuel, /*HasJIT=*/false> X(
      TheCeespuleTarget, "Ceespuel", "Ceespu (little endian)");
  RegisterTarget<Triple::Ceespueb, /*HasJIT=*/false> Y(
      TheCeespubeTarget, "Ceespueb", "Ceespu (big endian)");
}
