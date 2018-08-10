//=- CeespuMachineFunctionInfo.h - Ceespu machine function info -----*- C++ -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares Ceespu-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_CeespuMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_Ceespu_CeespuMACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

/// CeespuMachineFunctionInfo - This class is derived from MachineFunctionInfo
/// and contains private Ceespu-specific information for each MachineFunction.
class CeespuMachineFunctionInfo : public MachineFunctionInfo {
private:
  MachineFunction &MF;
  /// FrameIndex for start of varargs area
  int VarArgsFrameIndex = 0;
  /// Size of the save area used for varargs
  int VarArgsSaveSize = 0;
  /// FrameIndex used for transferring values between 64-bit FPRs and a pair
  /// of 32-bit GPRs via the stack.
  int MoveF64FrameIndex = -1;

public:
  //  CeespuMachineFunctionInfo() = default;

  CeespuMachineFunctionInfo(MachineFunction &MF) : MF(MF) {}

  int getVarArgsFrameIndex() const { return VarArgsFrameIndex; }
  void setVarArgsFrameIndex(int Index) { VarArgsFrameIndex = Index; }

  unsigned getVarArgsSaveSize() const { return VarArgsSaveSize; }
  void setVarArgsSaveSize(int Size) { VarArgsSaveSize = Size; }

  int getMoveF64FrameIndex() {
    if (MoveF64FrameIndex == -1)
      MoveF64FrameIndex = MF.getFrameInfo().CreateStackObject(8, 8, false);
    return MoveF64FrameIndex;
  }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_Ceespu_CeespuMACHINEFUNCTIONINFO_H
