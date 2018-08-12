//===-- CeespuMachineFuctionInfo.cpp - Ceespu machine function info ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "CeespuMachineFunctionInfo.h"

using namespace llvm;

void CeespuMachineFunctionInfo::anchor() {}

unsigned CeespuMachineFunctionInfo::getGlobalBaseReg() {
  // Return if it has already been initialized.
  if (GlobalBaseReg) return GlobalBaseReg;

  return GlobalBaseReg =
             MF.getRegInfo().createVirtualRegister(&Ceespu::GPRRegClass);
}
