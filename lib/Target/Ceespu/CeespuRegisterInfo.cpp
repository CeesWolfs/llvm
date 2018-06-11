//===-- CeespuRegisterInfo.cpp - Ceespu Register Information ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Ceespu implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "Ceespu.h"
#include "CeespuRegisterInfo.h"
#include "CeespuSubtarget.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetFrameLowering.h"
#include "llvm/Target/TargetInstrInfo.h"

#define GET_REGINFO_TARGET_DESC
#include "CeespuGenRegisterInfo.inc"
using namespace llvm;

CeespuRegisterInfo::CeespuRegisterInfo()
    : CeespuGenRegisterInfo(Ceespu::R0) {}

const MCPhysReg *
CeespuRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  static const uint16_t CalleeSavedRegs[] = { Ceespu::LR, Ceespu::R1, Ceespu::R2, Ceespu::R3,
                                              Ceespu::R4, Ceespu::R5, Ceespu::R6,
                                              Ceespu::R7, Ceespu::R8, Ceespu::R9,
                                              Ceespu::R10, Ceespu::R11, Ceespu::R12,
                                              0 };
  return CalleeSavedRegs;
}

BitVector CeespuRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  Reserved.set(Ceespu::R0); // R0 is always zero
  Reserved.set(Ceespu::SP); // R18 is stack pointer
  Reserved.set(Ceespu::LR); // R19 is link register
  Reserved.set(Ceespu::R17);// R17 is interrupt register
  return Reserved;
}

void CeespuRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                          int SPAdj, unsigned FIOperandNum,
                                          RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  unsigned i = 0;
  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  DebugLoc DL = MI.getDebugLoc();

  while (!MI.getOperand(i).isFI()) {
    ++i;
    assert(i < MI.getNumOperands() && "Instr doesn't have FrameIndex operand!");
  }

  unsigned FrameReg = getFrameRegister(MF);
  int FrameIndex = MI.getOperand(i).getIndex();
  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
  MachineBasicBlock &MBB = *MI.getParent();

  if (MI.getOpcode() == Ceespu::MOV_rr) {
    int Offset = MF.getFrameInfo()->getObjectOffset(FrameIndex);

    MI.getOperand(i).ChangeToRegister(FrameReg, false);
    unsigned reg = MI.getOperand(i - 1).getReg();
    BuildMI(MBB, ++II, DL, TII.get(Ceespu::ADD_ri), reg)
        .addReg(reg)
        .addImm(Offset);
    return;
  }

  int Offset = MF.getFrameInfo()->getObjectOffset(FrameIndex) +
               MI.getOperand(i + 1).getImm();

  if (!isInt<32>(Offset))
    llvm_unreachable("bug in frame offset");

  if (MI.getOpcode() == Ceespu::FI_ri) {
    // architecture does not really support FI_ri, replace it with
    //    MOV_rr <target_reg>, frame_reg
    //    ADD_ri <target_reg>, imm
    unsigned reg = MI.getOperand(i - 1).getReg();

    BuildMI(MBB, ++II, DL, TII.get(Ceespu::MOV_rr), reg)
        .addReg(FrameReg);
    BuildMI(MBB, II, DL, TII.get(Ceespu::ADD_ri), reg)
        .addReg(reg)
        .addImm(Offset);

    // Remove FI_ri instruction
    MI.eraseFromParent();
  } else {
    MI.getOperand(i).ChangeToRegister(FrameReg, false);
    MI.getOperand(i + 1).ChangeToImmediate(Offset);
  }
}

unsigned CeespuRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return Ceespu::SP;
}
