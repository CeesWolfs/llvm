//===-- CeespuMCAsmInfo.cpp - Ceespu Asm properties
//-------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the CeespuMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "CeespuMCAsmInfo.h"
#include "llvm/ADT/Triple.h"
using namespace llvm;

void CeespuMCAsmInfo::anchor() {}

CeespuMCAsmInfo::CeespuMCAsmInfo(const Triple &TT) {
  IsLittleEndian = false;
  CodePointerSize = CalleeSaveStackSlotSize = TT.isArch64Bit() ? 8 : 4;
  CommentString = ";";
  AlignmentIsInBytes = false;
  SupportsDebugInformation = false;  // true;
  Data16bitsDirective = "\t.hword\t";
  Data32bitsDirective = "\t.word\t";
  ZeroDirective = "\t.space\t";
}
