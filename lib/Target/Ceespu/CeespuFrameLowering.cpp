//===-- CeespuFrameLowering.cpp - Ceespu Frame Information ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Ceespu implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "CeespuFrameLowering.h"
#include "CeespuMachineFunction.h"
#include "CeespuInstrInfo.h"
#include "CeespuSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"

using namespace llvm;

bool CeespuFrameLowering::hasFP(const MachineFunction &MF) const {
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         MF.getFrameInfo()->hasVarSizedObjects();
}

uint64_t CeespuFrameLowering::computeStackSize(MachineFunction &MF) const {
  MachineFrameInfo *MFI = MF.getFrameInfo();
  uint64_t StackSize = MFI->getStackSize();
  unsigned StackAlign = getStackAlignment();
  if (StackAlign > 0) {
    StackSize = RoundUpToAlignment(StackSize, StackAlign);
  }
  return StackSize;
}

static void replaceFrameIndexes(MachineFunction &MF,
	SmallVector<std::pair<int, int64_t>, 16> &FR) {
	MachineFrameInfo *MFI = MF.getFrameInfo();
	CeespuFunctionInfo *CeespuFI = MF.getInfo<CeespuFunctionInfo>();
	const SmallVector<std::pair<int, int64_t>, 16>::iterator FRB = FR.begin();
	const SmallVector<std::pair<int, int64_t>, 16>::iterator FRE = FR.end();
	printf("strange faces");
	SmallVector<std::pair<int, int64_t>, 16>::iterator FRI = FRB;
	for (; FRI != FRE; ++FRI) {
		MFI->RemoveStackObject(FRI->first);
		int NFI = MFI->CreateFixedObject(4, FRI->second, true);
		CeespuFI->recordReplacement(FRI->first, NFI);

		for (MachineFunction::iterator MB = MF.begin(), ME = MF.end(); MB != ME; ++MB) {
			MachineBasicBlock::iterator MBB = MB->begin();
			const MachineBasicBlock::iterator MBE = MB->end();

			for (; MBB != MBE; ++MBB) {
				MachineInstr::mop_iterator MIB = MBB->operands_begin();
				const MachineInstr::mop_iterator MIE = MBB->operands_end();

				for (MachineInstr::mop_iterator MII = MIB; MII != MIE; ++MII) {
					if (!MII->isFI() || MII->getIndex() != FRI->first) continue;
					//DEBUG(dbgs() << "FOUND FI#" << MII->getIndex() << "\n");
					MII->setIndex(NFI);
				}
			}
		}
	}
}

//===----------------------------------------------------------------------===//
//
// Stack Frame Processing methods
// +----------------------------+
//
// The stack is allocated decrementing the stack pointer on
// the first instruction of a function prologue. Once decremented,
// all stack references are are done through a positive offset
// from the stack/frame pointer, so the stack is considered
// to grow up.
//
//===----------------------------------------------------------------------===//

static void analyzeFrameIndexes(MachineFunction &MF) {

	MachineFrameInfo *MFI = MF.getFrameInfo();
	CeespuFunctionInfo *CeespuFI = MF.getInfo<CeespuFunctionInfo>();
	const MachineRegisterInfo &MRI = MF.getRegInfo();

	MachineRegisterInfo::livein_iterator LII = MRI.livein_begin();
	MachineRegisterInfo::livein_iterator LIE = MRI.livein_end();
	const SmallVector<int, 16> &LiveInFI = CeespuFI->getLiveIn();
	SmallVector<MachineInstr*, 16> EraseInstr;
	SmallVector<std::pair<int, int64_t>, 16> FrameRelocate;

	MachineBasicBlock *MBB = MF.getBlockNumbered(0);
	MachineBasicBlock::iterator MIB = MBB->begin();
	MachineBasicBlock::iterator MIE = MBB->end();

	int StackAdjust = 4;
	int StackOffset = -28;

	// In this loop we are searching frame indexes that corrospond to incoming
	// arguments that are already in the stack. We look for instruction sequences
	// like the following:
	//    
	//    LWI REG, FI1, 0
	//    ...
	//    SWI REG, FI2, 0
	//
	// As long as there are no defs of REG in the ... part, we can eliminate
	// the SWI instruction because the value has already been stored to the
	// stack by the caller. All we need to do is locate FI at the correct
	// stack location according to the calling convensions.
	//
	// Additionally, if the SWI operation kills the def of REG then we don't
	// need the LWI operation so we can erase it as well.
	for (unsigned i = 0, e = LiveInFI.size(); i < e; ++i) {
		for (MachineBasicBlock::iterator I = MIB; I != MIE; ++I) {
			if (I->getOpcode() != Ceespu::LDW || I->getNumOperands() != 3 ||
				!I->getOperand(1).isFI() || !I->getOperand(0).isReg() ||
				I->getOperand(1).getIndex() != LiveInFI[i]) continue;

			unsigned FIReg = I->getOperand(0).getReg();
			MachineBasicBlock::iterator SI = I;
			for (SI++; SI != MIE; ++SI) {
				if (!SI->getOperand(0).isReg() ||
					!SI->getOperand(1).isFI() ||
					SI->getOpcode() != Ceespu::STW) continue;

				int FI = SI->getOperand(1).getIndex();
				if (SI->getOperand(0).getReg() != FIReg ||
					MFI->isFixedObjectIndex(FI) ||
					MFI->getObjectSize(FI) != 4) continue;

				if (SI->getOperand(0).isDef()) break;

				if (SI->getOperand(0).isKill()) {
					//DEBUG(dbgs() << "LWI for FI#" << I->getOperand(1).getIndex()
					//	<< " removed\n");
					EraseInstr.push_back(I);
				}

				EraseInstr.push_back(SI);
				//DEBUG(dbgs() << "SWI for FI#" << FI << " removed\n");

				FrameRelocate.push_back(std::make_pair(FI, StackOffset));
				//DEBUG(dbgs() << "FI#" << FI << " relocated to " << StackOffset << "\n");

				StackOffset -= 4;
				StackAdjust += 4;
				break;
			}
		}
	}

	// In this loop we are searching for frame indexes that corrospond to
	// incoming arguments that are in registers. We look for instruction
	// sequences like the following:
	//    
	//    ...  SWI REG, FI, 0
	// 
	// As long as the ... part does not define REG and if REG is an incoming
	// parameter register then we know that, according to ABI convensions, the
	// caller has allocated stack space for it already.  Instead of allocating
	// stack space on our frame, we record the correct location in the callers
	// frame.
	/*for (MachineRegisterInfo::livein_iterator LI = LII; LI != LIE; ++LI) {
		for (MachineBasicBlock::iterator I = MIB; I != MIE; ++I) {
			if (I->definesRegister(LI->first))
				break;

			if (I->getOpcode() != Ceespu::SWI || I->getNumOperands() != 3 ||
				!I->getOperand(1).isFI() || !I->getOperand(0).isReg() ||
				I->getOperand(1).getIndex() < 0) continue;

			if (I->getOperand(0).getReg() == LI->first) {
				int FI = I->getOperand(1).getIndex();
				CeespuFI->recordLiveIn(FI);

				int FILoc = 0;
				switch (LI->first) {
				default: llvm_unreachable("invalid incoming parameter!");
				case Ceespu::R5:  FILoc = -4; break;
				case Ceespu::R6:  FILoc = -8; break;
				case Ceespu::R7:  FILoc = -12; break;
				case Ceespu::R8:  FILoc = -16; break;
				case Ceespu::R9:  FILoc = -20; break;
				case Ceespu::R10: FILoc = -24; break;
				}

				StackAdjust += 4;
				FrameRelocate.push_back(std::make_pair(FI, FILoc));
				DEBUG(dbgs() << "FI#" << FI << " relocated to " << FILoc << "\n");
				break;
			}
		}
	}*/

	// Go ahead and erase all of the instructions that we determined were
	// no longer needed.
	for (int i = 0, e = EraseInstr.size(); i < e; ++i)
		MBB->erase(EraseInstr[i]);

	// Replace all of the frame indexes that we have relocated with new
	// fixed object frame indexes.
	replaceFrameIndexes(MF, FrameRelocate);
}

static void determineFrameLayout(MachineFunction &MF) {
	MachineFrameInfo *MFI = MF.getFrameInfo();
	CeespuFunctionInfo *CeespuFI = MF.getInfo<CeespuFunctionInfo>();

	// Replace the dummy '0' SPOffset by the negative offsets, as explained on
	// LowerFORMAL_ARGUMENTS. Leaving '0' for while is necessary to avoid
	// the approach done by calculateFrameObjectOffsets to the stack frame.
	CeespuFI->adjustStoreVarArgsFI(MFI);
	// Get the number of bytes to allocate from the FrameInfo
	unsigned FrameSize = MFI->getStackSize();
	//DEBUG(dbgs() << "Original Frame Size: " << FrameSize << "\n");

	// Get the alignments provided by the target, and the maximum alignment
	// (if any) of the fixed frame objects.
	// unsigned MaxAlign = MFI->getMaxAlignment();
	unsigned TargetAlign = 4;
	unsigned AlignMask = TargetAlign - 1;
	// Make sure the frame is aligned.
	FrameSize = (FrameSize + AlignMask) & ~AlignMask;
	MFI->setStackSize(FrameSize);
	CeespuFI->adjustLoadArgsFI(MFI, FrameSize);
	//DEBUG(dbgs() << "Aligned Frame Size: " << FrameSize << "\n");
}

// Materialize an offset for a ADD/SUB stack operation.
// Return zero if the offset fits into the instruction as an immediate,
// or the number of the register where the offset is materialized.
static unsigned materializeOffset(MachineFunction &MF, MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  unsigned Offset) {
  return 0;
}

void CeespuFrameLowering::emitPrologue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  // Compute the stack size, to determine if we need a prologue at all.
  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
  determineFrameLayout(MF);
  uint64_t StackSize = computeStackSize(MF);
  if (!StackSize) {
    return;
  }
  
  // Adjust the stack pointer.
  unsigned StackReg = Ceespu::SP;
  unsigned OffsetReg = materializeOffset(MF, MBB, MBBI, (unsigned)StackSize);
  if (OffsetReg) {
    BuildMI(MBB, MBBI, dl, TII.get(Ceespu::SUB_rr), StackReg)
        .addReg(StackReg)
        .addReg(OffsetReg)
        .setMIFlag(MachineInstr::FrameSetup);
  } else {
    BuildMI(MBB, MBBI, dl, TII.get(Ceespu::ADD_ri), StackReg)
        .addReg(StackReg)
        .addImm(-StackSize)
        .setMIFlag(MachineInstr::FrameSetup);
  }
}

void CeespuFrameLowering::emitEpilogue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  // Compute the stack size, to determine if we need an epilogue at all.
  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  DebugLoc dl = MBBI->getDebugLoc();
  uint64_t StackSize = computeStackSize(MF);
  if (!StackSize) {
    return;
  }

  // Restore the stack pointer to what it was at the beginning of the function.
  unsigned StackReg = Ceespu::SP;
  unsigned OffsetReg = materializeOffset(MF, MBB, MBBI, (unsigned)StackSize);
  if (OffsetReg) {
    BuildMI(MBB, MBBI, dl, TII.get(Ceespu::ADD_rr), StackReg)
        .addReg(StackReg)
        .addReg(OffsetReg)
        .setMIFlag(MachineInstr::FrameSetup);
  } else {
    BuildMI(MBB, MBBI, dl, TII.get(Ceespu::ADD_ri), StackReg)
        .addReg(StackReg)
        .addImm(StackSize)
        .setMIFlag(MachineInstr::FrameSetup);
  }
}

// This function eliminates ADJCALLSTACKDOWN, ADJCALLSTACKUP pseudo
// instructions
void CeespuFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const {
  if (I->getOpcode() == Ceespu::ADJCALLSTACKUP ||
      I->getOpcode() == Ceespu::ADJCALLSTACKDOWN) {
    MBB.erase(I);
  }
  return;
}

void CeespuFrameLowering::
processFunctionBeforeCalleeSavedScan(MachineFunction &MF,
	RegScavenger *RS) const {
	MachineFrameInfo *MFI = MF.getFrameInfo();
	CeespuFunctionInfo *CeespuFI = MF.getInfo<CeespuFunctionInfo>();
	CallingConv::ID CallConv = MF.getFunction()->getCallingConv();

	if (hasFP(MF)) {
		CeespuFI->setFPStackOffset(4);
		MFI->CreateFixedObject(4, 4, true);
	}
	printf("poptard");
	analyzeFrameIndexes(MF);
}

