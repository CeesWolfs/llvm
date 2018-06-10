//===-- CeespuFrameLowering.h - Define frame lowering for Ceespu -----*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class implements Ceespu-specific bits of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_CeespuFRAMELOWERING_H
#define LLVM_LIB_TARGET_Ceespu_CeespuFRAMELOWERING_H

#include "llvm/Target/TargetFrameLowering.h"

namespace llvm {
class CeespuSubtarget;

class CeespuFrameLowering : public TargetFrameLowering {
public:
  explicit CeespuFrameLowering(const CeespuSubtarget &sti)
      : TargetFrameLowering(TargetFrameLowering::StackGrowsUp, 4, 0) {}

  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

  bool hasFP(const MachineFunction &MF) const override;
  //! Stack slot size (4 bytes)
  static int stackSlotSize() { return 4; }
  void
	  eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
		  MachineBasicBlock::iterator MI) const override;
  virtual void processFunctionBeforeCalleeSavedScan(MachineFunction &MF,
	  RegScavenger *RS) const;
private:
  uint64_t computeStackSize(MachineFunction &MF) const;
};
}
#endif
