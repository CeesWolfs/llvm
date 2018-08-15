//===-- CeespuInstrInfo.h - Ceespu Instruction Information --------*- C++
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Ceespu implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_CeespuINSTRINFO_H
#define LLVM_LIB_TARGET_Ceespu_CeespuINSTRINFO_H

#include "CeespuRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "CeespuGenInstrInfo.inc"

namespace llvm {

class CeespuInstrInfo : public CeespuGenInstrInfo {
 public:
  CeespuInstrInfo();

  unsigned isLoadFromStackSlot(const MachineInstr &MI,
                               int &FrameIndex) const override;
  unsigned isStoreToStackSlot(const MachineInstr &MI,
                              int &FrameIndex) const override;

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                   const DebugLoc &DL, unsigned DstReg, unsigned SrcReg,
                   bool KillSrc) const override;

  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MBBI, unsigned SrcReg,
                           bool IsKill, int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI) const override;

  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MBBI, unsigned DstReg,
                            int FrameIndex, const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI) const override;

  // Materializes the given int32 Val into DstReg.
  void movImm32(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                const DebugLoc &DL, unsigned DstReg, uint64_t Val,
                MachineInstr::MIFlag Flag = MachineInstr::NoFlags) const;

  unsigned getInstSizeInBytes(const MachineInstr &MI) const override;

  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify) const override;

  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                        const DebugLoc &dl,
                        int *BytesAdded = nullptr) const override;

  unsigned insertIndirectBranch(MachineBasicBlock &MBB,
                                MachineBasicBlock &NewDestBB,
                                const DebugLoc &DL, int64_t BrOffset,
                                RegScavenger *RS = nullptr) const override{};

  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;

  // bool reverseBranchCondition(
  //    SmallVectorImpl<MachineOperand> &Cond) const override;

  MachineBasicBlock *getBranchDestBlock(const MachineInstr &MI) const override;

  bool isBranchOffsetInRange(unsigned BranchOpc,
                             int64_t BrOffset) const override;
  bool expandPostRAPseudo(MachineInstr &MI) const override;
};
}  // namespace llvm
#endif
