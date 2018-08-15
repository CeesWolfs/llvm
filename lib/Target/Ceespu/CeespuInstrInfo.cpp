//===-- CeespuInstrInfo.cpp - Ceespu Instruction Information ------*- C++
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

#include "CeespuInstrInfo.h"
#include "Ceespu.h"
#include "CeespuSubtarget.h"
#include "CeespuTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_CTOR_DTOR
#include "CeespuGenInstrInfo.inc"

using namespace llvm;

CeespuInstrInfo::CeespuInstrInfo()
    : CeespuGenInstrInfo(Ceespu::ADJCALLSTACKDOWN, Ceespu::ADJCALLSTACKUP) {}

unsigned CeespuInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                              int &FrameIndex) const {
  switch (MI.getOpcode()) {
    default:
      return 0;
    case Ceespu::LB:
    case Ceespu::LBU:
    case Ceespu::LH:
    case Ceespu::LHU:
    case Ceespu::LW:
      break;
  }

  if (MI.getOperand(1).isFI() && MI.getOperand(2).isImm() &&
      MI.getOperand(2).getImm() == 0) {
    FrameIndex = MI.getOperand(1).getIndex();
    return MI.getOperand(0).getReg();
  }

  return 0;
}

unsigned CeespuInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                             int &FrameIndex) const {
  switch (MI.getOpcode()) {
    default:
      return 0;
    case Ceespu::SB:
    case Ceespu::SH:
    case Ceespu::SW:
      break;
  }

  if (MI.getOperand(0).isFI() && MI.getOperand(1).isImm() &&
      MI.getOperand(1).getImm() == 0) {
    FrameIndex = MI.getOperand(0).getIndex();
    return MI.getOperand(2).getReg();
  }

  return 0;
}

void CeespuInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  const DebugLoc &DL, unsigned DstReg,
                                  unsigned SrcReg, bool KillSrc) const {
  if (Ceespu::GPRRegClass.contains(DstReg, SrcReg)) {
    BuildMI(MBB, MBBI, DL, get(Ceespu::ADDI), DstReg)
        .addReg(SrcReg, getKillRegState(KillSrc))
        .addImm(0);
    return;
  } else
    llvm_unreachable("Impossible reg-to-reg copy");
  unsigned Opc;
  BuildMI(MBB, MBBI, DL, get(Opc), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc))
      .addReg(SrcReg, getKillRegState(KillSrc));
}

void CeespuInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator I,
                                          unsigned SrcReg, bool IsKill, int FI,
                                          const TargetRegisterClass *RC,
                                          const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  unsigned Opcode;

  if (Ceespu::GPRRegClass.hasSubClassEq(RC))
    Opcode = Ceespu::SW;
  else
    llvm_unreachable("Can't store this register to stack slot");

  BuildMI(MBB, I, DL, get(Opcode))
      .addReg(SrcReg, getKillRegState(IsKill))
      .addFrameIndex(FI)
      .addImm(0);
}

void CeespuInstrInfo::loadRegFromStackSlot(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator I, unsigned DstReg,
    int FI, const TargetRegisterClass *RC,
    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  unsigned Opcode;

  if (Ceespu::GPRRegClass.hasSubClassEq(RC))
    Opcode = Ceespu::LW;
  else
    llvm_unreachable("Can't load this register from stack slot");

  BuildMI(MBB, I, DL, get(Opcode), DstReg).addFrameIndex(FI).addImm(0);
}

void CeespuInstrInfo::movImm32(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator MBBI,
                               const DebugLoc &DL, unsigned DstReg,
                               uint64_t Val, MachineInstr::MIFlag Flag) const {
  assert(isInt<32>(Val) && "Can only materialize 32-bit constants");

  // TODO: If the value can be materialized using only one instruction, only
  // insert a single instruction.

  uint64_t Hi16 = Val >> 16;
  uint64_t Lo16 = Val & 0xffff;
  BuildMI(MBB, MBBI, DL, get(Ceespu::SETHI)).addImm(Hi16);
  BuildMI(MBB, MBBI, DL, get(Ceespu::ADDI), DstReg)
      .addReg(Ceespu::R0)
      .addImm(Lo16)
      .setMIFlag(Flag);
}

// The contents of values added to Cond are not examined outside of
// CeespuInstrInfo, giving us flexibility in what to push to it. For Ceespu, we
// push BranchOpcode, Reg1, Reg2.
static void parseCondBranch(MachineInstr &LastInst, MachineBasicBlock *&Target,
                            SmallVectorImpl<MachineOperand> &Cond) {
  // Block ends with fall-through condbranch.
  assert(LastInst.getDesc().isConditionalBranch() &&
         "Unknown conditional branch");
  Target = LastInst.getOperand(2).getMBB();
  Cond.push_back(MachineOperand::CreateImm(LastInst.getOpcode()));
  Cond.push_back(LastInst.getOperand(0));
  Cond.push_back(LastInst.getOperand(1));
}

static unsigned getOppositeBranchOpcode(int Opc) {
  switch (Opc) {
    default:
      llvm_unreachable("Unrecognized conditional branch");
    case Ceespu::BEQ:
      return Ceespu::BNE;
    case Ceespu::BNE:
      return Ceespu::BEQ;
    case Ceespu::BGT:
      return Ceespu::BGT;
    case Ceespu::BGE:
      return Ceespu::BGE;
    case Ceespu::BGEU:
      return Ceespu::BGEU;
    case Ceespu::BGU:
      return Ceespu::BGU;
  }
}

bool CeespuInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify) const {
  TBB = FBB = nullptr;
  Cond.clear();

  // If the block has no terminators, it just falls into the block after it.
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end() || !isUnpredicatedTerminator(*I)) return false;

  // Count the number of terminators and find the first unconditional or
  // indirect branch.
  MachineBasicBlock::iterator FirstUncondOrIndirectBr = MBB.end();
  int NumTerminators = 0;
  for (auto J = I.getReverse(); J != MBB.rend() && isUnpredicatedTerminator(*J);
       J++) {
    NumTerminators++;
    if (J->getDesc().isUnconditionalBranch() ||
        J->getDesc().isIndirectBranch()) {
      FirstUncondOrIndirectBr = J.getReverse();
    }
  }

  // If AllowModify is true, we can erase any terminators after
  // FirstUncondOrIndirectBR.
  if (AllowModify && FirstUncondOrIndirectBr != MBB.end()) {
    while (std::next(FirstUncondOrIndirectBr) != MBB.end()) {
      std::next(FirstUncondOrIndirectBr)->eraseFromParent();
      NumTerminators--;
    }
    I = FirstUncondOrIndirectBr;
  }

  // We can't handle blocks that end in an indirect branch.
  if (I->getDesc().isIndirectBranch()) return true;

  // We can't handle blocks with more than 2 terminators.
  if (NumTerminators > 2) return true;

  // Handle a single unconditional branch.
  if (NumTerminators == 1 && I->getDesc().isUnconditionalBranch()) {
    TBB = I->getOperand(0).getMBB();
    return false;
  }

  // Handle a single conditional branch.
  if (NumTerminators == 1 && I->getDesc().isConditionalBranch()) {
    parseCondBranch(*I, TBB, Cond);
    return false;
  }

  // Handle a conditional branch followed by an unconditional branch.
  if (NumTerminators == 2 && std::prev(I)->getDesc().isConditionalBranch() &&
      I->getDesc().isUnconditionalBranch()) {
    parseCondBranch(*std::prev(I), TBB, Cond);
    FBB = I->getOperand(0).getMBB();
    return false;
  }

  // Otherwise, we can't handle this.
  return true;
}

unsigned CeespuInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                       int *BytesRemoved) const {
  if (BytesRemoved) *BytesRemoved = 0;
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end()) return 0;

  if (!I->getDesc().isUnconditionalBranch() &&
      !I->getDesc().isConditionalBranch())
    return 0;

  // Remove the branch.
  I->eraseFromParent();
  if (BytesRemoved) *BytesRemoved += getInstSizeInBytes(*I);

  I = MBB.end();

  if (I == MBB.begin()) return 1;
  --I;
  if (!I->getDesc().isConditionalBranch()) return 1;

  // Remove the branch.
  I->eraseFromParent();
  if (BytesRemoved) *BytesRemoved += getInstSizeInBytes(*I);
  return 2;
}

// Inserts a branch into the end of the specific MachineBasicBlock, returning
// the number of instructions inserted.
unsigned CeespuInstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB, MachineBasicBlock *FBB,
    ArrayRef<MachineOperand> Cond, const DebugLoc &DL, int *BytesAdded) const {
  if (BytesAdded) *BytesAdded = 0;

  // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 3 || Cond.size() == 0) &&
         "Ceespu branch conditions have two components!");

  // Unconditional branch.
  if (Cond.empty()) {
    MachineInstr &MI = *BuildMI(&MBB, DL, get(Ceespu::JMP)).addMBB(TBB);
    if (BytesAdded) *BytesAdded += getInstSizeInBytes(MI);
    return 1;
  }

  // Either a one or two-way conditional branch.
  unsigned Opc = Cond[0].getImm();
  MachineInstr &CondMI =
      *BuildMI(&MBB, DL, get(Opc)).add(Cond[1]).add(Cond[2]).addMBB(TBB);
  if (BytesAdded) *BytesAdded += getInstSizeInBytes(CondMI);

  // One-way conditional branch.
  if (!FBB) return 1;

  // llvm_unreachable("Ceespu two-way conditional branches not supported");

  // Two-way conditional branch.
  MachineInstr &MI = *BuildMI(&MBB, DL, get(Ceespu::JMP)).addMBB(FBB);
  if (BytesAdded) *BytesAdded += getInstSizeInBytes(MI);
  return 2;
}

/*unsigned CeespuInstrInfo::insertIndirectBranch(MachineBasicBlock &MBB,
                                               MachineBasicBlock &DestBB,
                                               const DebugLoc &DL,
                                               int64_t BrOffset,
                                               RegScavenger *RS) const {
  assert(RS && "RegScavenger required for long branching");
  assert(MBB.empty() &&
         "new block should be inserted for expanding unconditional branch");
  assert(MBB.pred_size() == 1);

  MachineFunction *MF = MBB.getParent();
  MachineRegisterInfo &MRI = MF->getRegInfo();
  const auto &TM = static_cast<const CeespuTargetMachine &>(MF->getTarget());
  const auto &STI = MF->getSubtarget<CeespuSubtarget>();

  if (TM.isPositionIndependent() || STI.is64Bit())
    report_fatal_error("Unable to insert indirect branch");

  if (!isInt<32>(BrOffset))
    report_fatal_error(
        "Branch offsets outside of the signed 32-bit range not supported");

  // FIXME: A virtual register must be used initially, as the register
  // scavenger won't work with empty blocks (SIInstrInfo::insertIndirectBranch
  // uses the same workaround).
  unsigned ScratchReg = MRI.createVirtualRegister(&Ceespu::GPRRegClass);
  auto II = MBB.end();

  MachineInstr &LuiMI = *BuildMI(MBB, II, DL, get(Ceespu::LUI), ScratchReg)
                             .addMBB(&DestBB, CeespuII::MO_HI);
  BuildMI(MBB, II, DL, get(Ceespu::PseudoBRIND))
      .addReg(ScratchReg, RegState::Kill)
      .addMBB(&DestBB, CeespuII::MO_LO);

  RS->enterBasicBlockEnd(MBB);
  unsigned Scav = RS->scavengeRegisterBackwards(
      Ceespu::GPRRegClass, MachineBasicBlock::iterator(LuiMI), false, 0);
  MRI.replaceRegWith(ScratchReg, Scav);
  MRI.clearVirtRegs();
  RS->setRegUsed(Scav);
  return 8;
}
*/

// bool CeespuInstrInfo::reverseBranchCondition(
//    SmallVectorImpl<MachineOperand> &Cond) const {
//  assert((Cond.size() == 3) && "Invalid branch condition!");
//  Cond[0].setImm(getOppositeBranchOpcode(Cond[0].getImm()));
//  return false;
//}

MachineBasicBlock *CeespuInstrInfo::getBranchDestBlock(
    const MachineInstr &MI) const {
  assert(MI.getDesc().isBranch() && "Unexpected opcode!");
  // The branch target is always the last operand.
  int NumOp = MI.getNumExplicitOperands();
  return MI.getOperand(NumOp - 1).getMBB();
}

bool CeespuInstrInfo::isBranchOffsetInRange(unsigned BranchOp,
                                            int64_t BrOffset) const {
  // Ideally we could determine the supported branch offset from the
  // CeespuII::FormMask, but this can't be used for Pseudo instructions like
  // PseudoBR.
  return BrOffset < 32767 && BrOffset > -32768;
}

bool CeespuInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  unsigned ReplaceOpc;
  switch (MI.getOpcode()) {
    case Ceespu::ADDX:
      ReplaceOpc = Ceespu::ADDI;
      break;
    case Ceespu::ADEX:
      ReplaceOpc = Ceespu::ADEI;
      break;
    case Ceespu::ADCX:
      ReplaceOpc = Ceespu::ADCI;
      break;
    case Ceespu::SUBX:
      ReplaceOpc = Ceespu::SUBI;
      break;
    case Ceespu::SBEX:
      ReplaceOpc = Ceespu::SBEI;
      break;
    case Ceespu::SBBX:
      ReplaceOpc = Ceespu::SBBI;
      break;
    case Ceespu::ORX:
      ReplaceOpc = Ceespu::ORI;
      break;
    case Ceespu::ANDX:
      ReplaceOpc = Ceespu::ANDI;
      break;
    case Ceespu::XORX:
      ReplaceOpc = Ceespu::XORI;
      break;
    case Ceespu::MULX:
      ReplaceOpc = Ceespu::MULI;
      break;
    default:
      return false;
  }
  DebugLoc DL = MI.getDebugLoc();
  MachineBasicBlock &MBB = *MI.getParent();
  const unsigned DstReg = MI.getOperand(0).getReg();
  const unsigned SrcReg = MI.getOperand(1).getReg();
  const MachineOperand &MO = MI.getOperand(2);
  if (MO.isImm()) {
    uint64_t imm = MO.getImm();
    uint16_t lo = imm & 0xffff;
    uint16_t hi = imm >> 16;
    if (!isInt<16>(imm)) {
      BuildMI(MBB, MI, DL, get(Ceespu::SETHI)).addImm(hi);
    }
    BuildMI(MBB, MI, DL, get(ReplaceOpc), DstReg).addReg(SrcReg).addImm(lo);
    MBB.erase(MI);
    return true;
  } else {
    BuildMI(MBB, MI, DL, get(Ceespu::SETHI)).add(MO);
    BuildMI(MBB, MI, DL, get(ReplaceOpc), DstReg).addReg(SrcReg).add(MO);
    MBB.erase(MI);
    return true;
  }
}

unsigned CeespuInstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  unsigned Opcode = MI.getOpcode();

  switch (Opcode) {
    default: { return get(Opcode).getSize(); }
    case TargetOpcode::EH_LABEL:
    case TargetOpcode::IMPLICIT_DEF:
    case TargetOpcode::KILL:
    case TargetOpcode::DBG_VALUE:
      return 0;
    case TargetOpcode::INLINEASM: {
      const MachineFunction &MF = *MI.getParent()->getParent();
      const auto &TM = static_cast<const CeespuTargetMachine &>(MF.getTarget());
      return getInlineAsmLength(MI.getOperand(0).getSymbolName(),
                                *TM.getMCAsmInfo());
    }
  }
}
