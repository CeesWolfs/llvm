//===-- CeespuISelLowering.cpp - Ceespu DAG Lowering Implementation
//--------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that Ceespu uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#include "CeespuISelLowering.h"
#include "Ceespu.h"
#include "CeespuMachineFunctionInfo.h"
#include "CeespuRegisterInfo.h"
#include "CeespuSubtarget.h"
#include "CeespuTargetMachine.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "ceespu-lower"

STATISTIC(NumTailCalls, "Number of tail calls");

CeespuTargetLowering::CeespuTargetLowering(const TargetMachine &TM,
                                           const CeespuSubtarget &STI)
    : TargetLowering(TM), Subtarget(STI) {
  MVT XLenVT = MVT::i32;

  // Set up the register classes.
  addRegisterClass(XLenVT, &Ceespu::GPRRegClass);

  // Compute derived properties from the register classes.
  computeRegisterProperties(STI.getRegisterInfo());

  setStackPointerRegisterToSaveRestore(Ceespu::SP);

  for (auto N : {ISD::EXTLOAD, ISD::SEXTLOAD, ISD::ZEXTLOAD})
    setLoadExtAction(N, XLenVT, MVT::i1, Promote);

  // TODO: add all necessary setOperationAction calls.
  setOperationAction(ISD::DYNAMIC_STACKALLOC, XLenVT, Expand);

  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC, XLenVT, Expand);
  setOperationAction(ISD::SELECT, XLenVT, Custom);
  setOperationAction(ISD::SELECT_CC, XLenVT, Expand);

  setOperationAction(ISD::STACKSAVE, MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE, MVT::Other, Expand);

  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  setOperationAction(ISD::VAARG, MVT::Other, Expand);
  setOperationAction(ISD::VACOPY, MVT::Other, Expand);
  setOperationAction(ISD::VAEND, MVT::Other, Expand);

  setOperationAction(ISD::MULHS, XLenVT, Expand);
  setOperationAction(ISD::MULHU, XLenVT, Expand);
  setOperationAction(ISD::SDIV, XLenVT, Expand);
  setOperationAction(ISD::UDIV, XLenVT, Expand);
  setOperationAction(ISD::SREM, XLenVT, Expand);
  setOperationAction(ISD::UREM, XLenVT, Expand);
  setOperationAction(ISD::SDIVREM, XLenVT, Expand);
  setOperationAction(ISD::UDIVREM, XLenVT, Expand);
  setOperationAction(ISD::SMUL_LOHI, XLenVT, Expand);
  setOperationAction(ISD::UMUL_LOHI, XLenVT, Expand);

  setOperationAction(ISD::SHL_PARTS, XLenVT, Expand);
  setOperationAction(ISD::SRL_PARTS, XLenVT, Expand);
  setOperationAction(ISD::SRA_PARTS, XLenVT, Expand);

  setOperationAction(ISD::ROTL, XLenVT, Expand);
  setOperationAction(ISD::ROTR, XLenVT, Expand);
  setOperationAction(ISD::BSWAP, XLenVT, Expand);
  setOperationAction(ISD::CTTZ, XLenVT, Expand);
  setOperationAction(ISD::CTLZ, XLenVT, Expand);
  setOperationAction(ISD::CTPOP, XLenVT, Expand);

  ISD::CondCode FPCCToExtend[] = {
      ISD::SETOGT, ISD::SETOGE, ISD::SETONE, ISD::SETO,   ISD::SETUEQ,
      ISD::SETUGT, ISD::SETUGE, ISD::SETULT, ISD::SETULE, ISD::SETUNE,
      ISD::SETGT,  ISD::SETGE,  ISD::SETNE};

  setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
  // setOperationAction(ISD::BlockAddress, XLenVT, Custom);
  // setOperationAction(ISD::ConstantPool, XLenVT, Custom);

  setBooleanContents(ZeroOrOneBooleanContent);

  // Function alignments (log2).
  unsigned FunctionAlignment = 2;
  setMinFunctionAlignment(FunctionAlignment);
  setPrefFunctionAlignment(FunctionAlignment);

  // Effectively disable jump table generation.
  // setMinimumJumpTableEntries(INT_MAX);
}

EVT CeespuTargetLowering::getSetCCResultType(const DataLayout &DL,
                                             LLVMContext &, EVT VT) const {
  if (!VT.isVector()) return getPointerTy(DL);
  return VT.changeVectorElementTypeToInteger();
}

bool CeespuTargetLowering::isLegalAddressingMode(const DataLayout &DL,
                                                 const AddrMode &AM, Type *Ty,
                                                 unsigned AS,
                                                 Instruction *I) const {
  // No global is ever allowed as a base.
  // if (AM.BaseGV) return false;

  // Require a 16-bit signed offset.
  if (!isInt<16>(AM.BaseOffs)) return false;

  switch (AM.Scale) {
    case 0:  // "r+i" or just "i", depending on HasBaseReg.
      break;
    case 1:
      if (!AM.HasBaseReg)  // allow "r+i".
        break;
      return false;  // disallow "r+r" or "r+r+i".
    default:
      return false;
  }

  return true;
}

bool CeespuTargetLowering::isLegalICmpImmediate(int64_t Imm) const {
  return isInt<12>(Imm);
}

bool CeespuTargetLowering::isLegalAddImmediate(int64_t Imm) const {
  return isInt<16>(Imm);
}

// On RV32, 64-bit integers are split into their high and low parts and held
// in two different registers, so the trunc is free since the low register can
// just be used.
bool CeespuTargetLowering::isTruncateFree(Type *SrcTy, Type *DstTy) const {
  if (Subtarget.is64Bit() || !SrcTy->isIntegerTy() || !DstTy->isIntegerTy())
    return false;
  unsigned SrcBits = SrcTy->getPrimitiveSizeInBits();
  unsigned DestBits = DstTy->getPrimitiveSizeInBits();
  return (SrcBits == 64 && DestBits == 32);
}

bool CeespuTargetLowering::isTruncateFree(EVT SrcVT, EVT DstVT) const {
  if (Subtarget.is64Bit() || SrcVT.isVector() || DstVT.isVector() ||
      !SrcVT.isInteger() || !DstVT.isInteger())
    return false;
  unsigned SrcBits = SrcVT.getSizeInBits();
  unsigned DestBits = DstVT.getSizeInBits();
  return (SrcBits == 64 && DestBits == 32);
}

bool CeespuTargetLowering::isZExtFree(SDValue Val, EVT VT2) const {
  // Zexts are free if they can be combined with a load.
  if (auto *LD = dyn_cast<LoadSDNode>(Val)) {
    EVT MemVT = LD->getMemoryVT();
    if ((MemVT == MVT::i8 || MemVT == MVT::i16 ||
         (Subtarget.is64Bit() && MemVT == MVT::i32)) &&
        (LD->getExtensionType() == ISD::NON_EXTLOAD ||
         LD->getExtensionType() == ISD::ZEXTLOAD))
      return true;
  }

  return TargetLowering::isZExtFree(Val, VT2);
}

// Changes the condition code and swaps operands if necessary, so the SetCC
// operation matches one of the comparisons supported directly in the RISC-V
// ISA.
static void normaliseSetCC(SDValue &LHS, SDValue &RHS, ISD::CondCode &CC) {
  switch (CC) {
    default:
      break;
    case ISD::SETLT:
    case ISD::SETLE:
    case ISD::SETULT:
    case ISD::SETULE:
      CC = ISD::getSetCCSwappedOperands(CC);
      std::swap(LHS, RHS);
      break;
  }
}

// Return the RISC-V branch opcode that matches the given DAG integer
// condition code. The CondCode must be one of those supported by the RISC-V
// ISA (see normaliseSetCC).
static unsigned getBranchOpcodeForIntCondCode(ISD::CondCode CC) {
  switch (CC) {
    default:
      llvm_unreachable("Unsupported CondCode");
    case ISD::SETEQ:
      return Ceespu::BEQ;
    case ISD::SETNE:
      return Ceespu::BNE;
    case ISD::SETGT:
      return Ceespu::BGT;
    case ISD::SETGE:
      return Ceespu::BGE;
    case ISD::SETUGT:
      return Ceespu::BGU;
    case ISD::SETUGE:
      return Ceespu::BGEU;
  }
}

SDValue CeespuTargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
    default:
      report_fatal_error("unimplemented operand");
    case ISD::GlobalAddress:
      return lowerGlobalAddress(Op, DAG);
    case ISD::SELECT:
      return lowerSELECT(Op, DAG);
    case ISD::VASTART:
      return lowerVASTART(Op, DAG);
    case ISD::FRAMEADDR:
      return LowerFRAMEADDR(Op, DAG);
    case ISD::RETURNADDR:
      return LowerRETURNADDR(Op, DAG);
  }
}

SDValue CeespuTargetLowering::lowerGlobalAddress(SDValue Op,
                                                 SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT Ty = Op.getValueType();
  GlobalAddressSDNode *N = cast<GlobalAddressSDNode>(Op);
  const GlobalValue *GV = N->getGlobal();
  int64_t Offset = N->getOffset();

  // if (isPositionIndependent() || Subtarget.is64Bit())
  // report_fatal_error("Unable to lowerGlobalAddress");
  // In order to maximise the opportunity for common subexpression elimination,
  // emit a separate ADD node for the global address offset instead of folding
  // it in the global address node. Later peephole optimisations may choose to
  // fold it back in when profitable.
  // SDValue GAHi = DAG.getTargetGlobalAddress(GV, DL, Ty, 0, CeespuII::MO_HI);
  // SDValue GALo = DAG.getTargetGlobalAddress(GV, DL, Ty, 0, CeespuII::MO_LO);
  // SDValue MNHi = SDValue(DAG.getMachineNode(Ceespu::LUI, DL, Ty, GAHi), 0);
  // SDValue MNLo =
  //    SDValue(DAG.getMachineNode(Ceespu::ADDI, DL, Ty, MNHi, GALo), 0);
  // if (Offset != 0)
  // return DAG.getNode(ISD::ADD, DL, Ty, MNLo,
  // DAG.getConstant(Offset, DL, XLenVT));
  // return MNLo;
  SDValue GA = DAG.getTargetGlobalAddress(GV, DL, MVT::i32);

  return DAG.getNode(CeespuISD::Wrapper, DL, MVT::i32, GA);
}

/*SDValue CeespuTargetLowering::lowerBlockAddress(SDValue Op,
                                                SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT Ty = Op.getValueType();
  BlockAddressSDNode *N = cast<BlockAddressSDNode>(Op);
  const BlockAddress *BA = N->getBlockAddress();
  int64_t Offset = N->getOffset();

  if (isPositionIndependent() || Subtarget.is64Bit())
    report_fatal_error("Unable to lowerBlockAddress");

  SDValue BAHi = DAG.getTargetBlockAddress(BA, Ty, Offset, CeespuII::MO_HI);
  SDValue BALo = DAG.getTargetBlockAddress(BA, Ty, Offset, CeespuII::MO_LO);
  SDValue MNHi = SDValue(DAG.getMachineNode(Ceespu::LUI, DL, Ty, BAHi), 0);
  SDValue MNLo =
      SDValue(DAG.getMachineNode(Ceespu::ADDI, DL, Ty, MNHi, BALo), 0);
  return MNLo;
}

SDValue CeespuTargetLowering::lowerConstantPool(SDValue Op,
                                                SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT Ty = Op.getValueType();
  ConstantPoolSDNode *N = cast<ConstantPoolSDNode>(Op);
  const Constant *CPA = N->getConstVal();
  int64_t Offset = N->getOffset();
  unsigned Alignment = N->getAlignment();

  if (!isPositionIndependent()) {
    SDValue CPAHi =
        DAG.getTargetConstantPool(CPA, Ty, Alignment, Offset, CeespuII::MO_HI);
    SDValue CPALo =
        DAG.getTargetConstantPool(CPA, Ty, Alignment, Offset, CeespuII::MO_LO);
    SDValue MNHi = SDValue(DAG.getMachineNode(Ceespu::LUI, DL, Ty, CPAHi), 0);
    SDValue MNLo =
        SDValue(DAG.getMachineNode(Ceespu::ADDI, DL, Ty, MNHi, CPALo), 0);
    return MNLo;
  } else {
    report_fatal_error("Unable to lowerConstantPool");
  }
}

SDValue CeespuTargetLowering::lowerExternalSymbol(SDValue Op,
                                                  SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT Ty = Op.getValueType();
  ExternalSymbolSDNode *N = cast<ExternalSymbolSDNode>(Op);
  const char *Sym = N->getSymbol();

  // TODO: should also handle gp-relative loads.

  if (isPositionIndependent() || Subtarget.is64Bit())
    report_fatal_error("Unable to lowerExternalSymbol");

  SDValue GAHi = DAG.getTargetExternalSymbol(Sym, Ty, CeespuII::MO_HI);
  SDValue GALo = DAG.getTargetExternalSymbol(Sym, Ty, CeespuII::MO_LO);
  SDValue MNHi = SDValue(DAG.getMachineNode(Ceespu::LUI, DL, Ty, GAHi), 0);
  SDValue MNLo =
      SDValue(DAG.getMachineNode(Ceespu::ADDI, DL, Ty, MNHi, GALo), 0);
  return MNLo;
}*/

SDValue CeespuTargetLowering::lowerSELECT(SDValue Op, SelectionDAG &DAG) const {
  SDValue CondV = Op.getOperand(0);
  SDValue TrueV = Op.getOperand(1);
  SDValue FalseV = Op.getOperand(2);
  SDLoc DL(Op);
  MVT XLenVT = MVT::i32;

  // If the result type is XLenVT and CondV is the output of a SETCC node
  // which also operated on XLenVT inputs, then merge the SETCC node into the
  // lowered CeespuISD::SELECT_CC to take advantage of the integer
  // compare+branch instructions. i.e.:
  // (select (setcc lhs, rhs, cc), truev, falsev)
  // -> (ceespuisd::select_cc lhs, rhs, cc, truev, falsev)
  if (Op.getSimpleValueType() == XLenVT && CondV.getOpcode() == ISD::SETCC &&
      CondV.getOperand(0).getSimpleValueType() == XLenVT) {
    SDValue LHS = CondV.getOperand(0);
    SDValue RHS = CondV.getOperand(1);
    auto CC = cast<CondCodeSDNode>(CondV.getOperand(2));
    ISD::CondCode CCVal = CC->get();

    normaliseSetCC(LHS, RHS, CCVal);

    SDValue TargetCC = DAG.getConstant(CCVal, DL, XLenVT);
    SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::Glue);
    SDValue Ops[] = {LHS, RHS, TargetCC, TrueV, FalseV};
    return DAG.getNode(CeespuISD::SELECT_CC, DL, VTs, Ops);
  }

  // Otherwise:
  // (select condv, truev, falsev)
  // -> (ceespuisd::select_cc condv, zero, setne, truev, falsev)
  SDValue Zero = DAG.getConstant(0, DL, XLenVT);
  SDValue SetNE = DAG.getConstant(ISD::SETNE, DL, XLenVT);

  SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::Glue);
  SDValue Ops[] = {CondV, Zero, SetNE, TrueV, FalseV};

  return DAG.getNode(CeespuISD::SELECT_CC, DL, VTs, Ops);
}

SDValue CeespuTargetLowering::lowerVASTART(SDValue Op,
                                           SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  CeespuMachineFunctionInfo *FuncInfo = MF.getInfo<CeespuMachineFunctionInfo>();

  SDLoc DL(Op);
  SDValue FI = DAG.getFrameIndex(FuncInfo->getVarArgsFrameIndex(),
                                 getPointerTy(MF.getDataLayout()));

  // vastart just stores the address of the VarArgsFrameIndex slot into the
  // memory location argument.
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  return DAG.getStore(Op.getOperand(0), DL, FI, Op.getOperand(1),
                      MachinePointerInfo(SV));
}

SDValue CeespuTargetLowering::LowerFRAMEADDR(SDValue Op,
                                             SelectionDAG &DAG) const {
  const CeespuRegisterInfo &RI = *Subtarget.getRegisterInfo();
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MFI.setFrameAddressIsTaken(true);
  unsigned FrameReg = RI.getFrameRegister(MF);
  int XLenInBytes = Subtarget.getXLen() / 8;

  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  SDValue FrameAddr = DAG.getCopyFromReg(DAG.getEntryNode(), DL, FrameReg, VT);
  unsigned Depth = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();
  while (Depth--) {
    int Offset = -(XLenInBytes * 2);
    SDValue Ptr = DAG.getNode(ISD::ADD, DL, VT, FrameAddr,
                              DAG.getIntPtrConstant(Offset, DL));
    FrameAddr =
        DAG.getLoad(VT, DL, DAG.getEntryNode(), Ptr, MachinePointerInfo());
  }
  return FrameAddr;
}

SDValue CeespuTargetLowering::LowerRETURNADDR(SDValue Op,
                                              SelectionDAG &DAG) const {
  const CeespuRegisterInfo &RI = *Subtarget.getRegisterInfo();
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MFI.setReturnAddressIsTaken(true);
  MVT XLenVT = Subtarget.getXLenVT();
  int XLenInBytes = Subtarget.getXLen() / 8;

  if (verifyReturnAddressArgumentIsConstant(Op, DAG)) return SDValue();

  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  unsigned Depth = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();
  if (Depth) {
    int Off = -XLenInBytes;
    SDValue FrameAddr = LowerFRAMEADDR(Op, DAG);
    SDValue Offset = DAG.getConstant(Off, DL, VT);
    return DAG.getLoad(VT, DL, DAG.getEntryNode(),
                       DAG.getNode(ISD::ADD, DL, VT, FrameAddr, Offset),
                       MachinePointerInfo());
  }

  // Return the value of the return address register, marking it an implicit
  // live-in.
  unsigned Reg = MF.addLiveIn(RI.getRARegister(), getRegClassFor(XLenVT));
  return DAG.getCopyFromReg(DAG.getEntryNode(), DL, Reg, XLenVT);
}

MachineBasicBlock *CeespuTargetLowering::EmitInstrWithCustomInserter(
    MachineInstr &MI, MachineBasicBlock *BB) const {
  const TargetInstrInfo &TII = *BB->getParent()->getSubtarget().getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();
  // DEBUG(dbgs() << "EmitInstrWithCustomInserter \n");
  assert(MI.getOpcode() == Ceespu::Select && "Unexpected instr type to insert");

  // To "insert" a SELECT instruction, we actually have to insert the diamond
  // control-flow pattern.  The incoming instruction knows the destination
  // vreg to set, the condition code register to branch on, the true/false
  // values to select between, and a branch opcode to use.
  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineFunction::iterator I = ++BB->getIterator();

  // ThisMBB:
  // ...
  //  TrueVal = ...
  //  jmp_XX r1, r2 goto Copy1MBB
  //  fallthrough --> Copy0MBB
  MachineBasicBlock *ThisMBB = BB;
  MachineFunction *F = BB->getParent();
  MachineBasicBlock *Copy0MBB = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *Copy1MBB = F->CreateMachineBasicBlock(LLVM_BB);

  F->insert(I, Copy0MBB);
  F->insert(I, Copy1MBB);
  // Update machine-CFG edges by transferring all successors of the current
  // block to the new block which will contain the Phi node for the select.
  Copy1MBB->splice(Copy1MBB->begin(), BB,
                   std::next(MachineBasicBlock::iterator(MI)), BB->end());
  Copy1MBB->transferSuccessorsAndUpdatePHIs(BB);
  // Next, add the true and fallthrough blocks as its successors.
  BB->addSuccessor(Copy0MBB);
  BB->addSuccessor(Copy1MBB);

  // Insert Branch if Flag
  unsigned LHS = MI.getOperand(1).getReg();
  unsigned RHS = MI.getOperand(2).getReg();
  int CC = MI.getOperand(3).getImm();
  switch (CC) {
    case ISD::SETGT:
      BuildMI(BB, DL, TII.get(Ceespu::BGT))
          .addReg(LHS)
          .addReg(RHS)
          .addMBB(Copy1MBB);
      break;
    case ISD::SETUGT:
      BuildMI(BB, DL, TII.get(Ceespu::BGU))
          .addReg(LHS)
          .addReg(RHS)
          .addMBB(Copy1MBB);
      break;
    case ISD::SETGE:
      BuildMI(BB, DL, TII.get(Ceespu::BGE))
          .addReg(LHS)
          .addReg(RHS)
          .addMBB(Copy1MBB);
      break;
    case ISD::SETUGE:
      BuildMI(BB, DL, TII.get(Ceespu::BGEU))
          .addReg(LHS)
          .addReg(RHS)
          .addMBB(Copy1MBB);
      break;
    case ISD::SETEQ:
      BuildMI(BB, DL, TII.get(Ceespu::BEQ))
          .addReg(LHS)
          .addReg(RHS)
          .addMBB(Copy1MBB);
      break;
    case ISD::SETNE:
      BuildMI(BB, DL, TII.get(Ceespu::BNE))
          .addReg(LHS)
          .addReg(RHS)
          .addMBB(Copy1MBB);
      break;
    default:
      report_fatal_error("unimplemented select CondCode " + Twine(CC));
  }

  // Copy0MBB:
  //  %FalseValue = ...
  //  # fallthrough to Copy1MBB
  BB = Copy0MBB;

  // Update machine-CFG edges
  BB->addSuccessor(Copy1MBB);

  // Copy1MBB:
  //  %Result = phi [ %FalseValue, Copy0MBB ], [ %TrueValue, ThisMBB ]
  // ...
  BB = Copy1MBB;
  if (MI.getOperand(0).getReg() == Ceespu::R0) {
    unsigned VReg = F->getRegInfo().createVirtualRegister(&Ceespu::GPRRegClass);
    BuildMI(*Copy0MBB, Copy0MBB->begin(), DL, TII.get(TargetOpcode::COPY), VReg)
        .addReg(MI.getOperand(0).getReg());
    BuildMI(*BB, BB->begin(), DL, TII.get(Ceespu::PHI), VReg)
        .addReg(MI.getOperand(5).getReg())
        .addMBB(Copy0MBB)
        .addReg(MI.getOperand(4).getReg())
        .addMBB(ThisMBB);
  } else if (MI.getOperand(4).getReg() == Ceespu::R0) {
    unsigned VReg = F->getRegInfo().createVirtualRegister(&Ceespu::GPRRegClass);
    BuildMI(*Copy0MBB, Copy0MBB->begin(), DL, TII.get(TargetOpcode::COPY), VReg)
        .addReg(MI.getOperand(4).getReg());
    BuildMI(*BB, BB->begin(), DL, TII.get(Ceespu::PHI),
            MI.getOperand(0).getReg())
        .addReg(MI.getOperand(5).getReg())
        .addMBB(Copy0MBB)
        .addReg(VReg)
        .addMBB(ThisMBB);
  } else if (MI.getOperand(5).getReg() == Ceespu::R0) {
    unsigned VReg = F->getRegInfo().createVirtualRegister(&Ceespu::GPRRegClass);
    BuildMI(*Copy0MBB, Copy0MBB->begin(), DL, TII.get(TargetOpcode::COPY), VReg)
        .addReg(MI.getOperand(5).getReg());
    BuildMI(*BB, BB->begin(), DL, TII.get(Ceespu::PHI),
            MI.getOperand(0).getReg())
        .addReg(VReg)
        .addMBB(Copy0MBB)
        .addReg(MI.getOperand(4).getReg())
        .addMBB(ThisMBB);
  } else {
    BuildMI(*BB, BB->begin(), DL, TII.get(Ceespu::PHI),
            MI.getOperand(0).getReg())
        .addReg(MI.getOperand(5).getReg())
        .addMBB(Copy0MBB)
        .addReg(MI.getOperand(4).getReg())
        .addMBB(ThisMBB);
  }
  MI.eraseFromParent();  // The pseudo instruction is gone now.
  return BB;
}
/* switch (MI.getOpcode()) {
   default:
     llvm_unreachable("Unexpected instr type to insert");
 }

 // To "insert" a SELECT instruction, we actually have to insert the triangle
 // control-flow pattern.  The incoming instruction knows the destination vreg
 // to set, the condition code register to branch on, the true/false values to
 // select between, and the condcode to use to select the appropriate branch.
 //
 // We produce the following control flow:
 //     HeadMBB
 //     |  \
 //     |  IfFalseMBB
 //     | /
 //    TailMBB
 const TargetInstrInfo &TII = *BB->getParent()->getSubtarget().getInstrInfo();
 const BasicBlock *LLVM_BB = BB->getBasicBlock();
 DebugLoc DL = MI.getDebugLoc();
 MachineFunction::iterator I = ++BB->getIterator();

 MachineBasicBlock *HeadMBB = BB;
 MachineFunction *F = BB->getParent();
 MachineBasicBlock *TailMBB = F->CreateMachineBasicBlock(LLVM_BB);
 MachineBasicBlock *IfFalseMBB = F->CreateMachineBasicBlock(LLVM_BB);

 F->insert(I, IfFalseMBB);
 F->insert(I, TailMBB);
 // Move all remaining instructions to TailMBB.
 TailMBB->splice(TailMBB->begin(), HeadMBB,
                 std::next(MachineBasicBlock::iterator(MI)), HeadMBB->end());
 // Update machine-CFG edges by transferring all successors of the current
 // block to the new block which will contain the Phi node for the select.
 TailMBB->transferSuccessorsAndUpdatePHIs(HeadMBB);
 // Set the successors for HeadMBB.
 HeadMBB->addSuccessor(IfFalseMBB);
 HeadMBB->addSuccessor(TailMBB);

 // Insert appropriate branch.
 unsigned LHS = MI.getOperand(1).getReg();
 unsigned RHS = MI.getOperand(2).getReg();
 auto CC = static_cast<ISD::CondCode>(MI.getOperand(3).getImm());
 unsigned Opcode = getBranchOpcodeForIntCondCode(CC);

 BuildMI(HeadMBB, DL, TII.get(Opcode)).addReg(LHS).addReg(RHS).addMBB(TailMBB);

 // IfFalseMBB just falls through to TailMBB.
 IfFalseMBB->addSuccessor(TailMBB);

 // %Result = phi [ %TrueValue, HeadMBB ], [ %FalseValue, IfFalseMBB ]
 BuildMI(*TailMBB, TailMBB->begin(), DL, TII.get(Ceespu::PHI),
         MI.getOperand(0).getReg())
     .addReg(MI.getOperand(4).getReg())
     .addMBB(HeadMBB)
     .addReg(MI.getOperand(5).getReg())
     .addMBB(IfFalseMBB);

 MI.eraseFromParent();  // The pseudo instruction is gone now.
 return TailMBB;
}
* /
   // Calling Convention Implementation.
   // The expectations for frontend ABI lowering vary from target to target.
   // Ideally, an LLVM frontend would be able to avoid worrying about many ABI
   // details, but this is a longer term goal. For now, we simply try to keep
   // the role of the frontend as simple and well-defined as possible. The
   // rules can be summarised as:
   // * Never split up large scalar arguments. We handle them here.
   // * If a hardfloat calling convention is being used, and the struct may be
   // passed in a pair of registers (fp+fp, int+fp), and both registers are
   // available, then pass as two separate arguments. If either the GPRs or
   // FPRs are exhausted, then pass according to the rule below.
   // * If a struct could never be passed in registers or directly in a stack
   // slot (as it is larger than 2*XLEN and the floating point rules don't
   // apply), then pass it using a pointer with the byval attribute.
   // * If a struct is less than 2*XLEN, then coerce to either a two-element
   // word-sized array or a 2*XLEN scalar (depending on alignment).
   // * The frontend can determine whether a struct is returned by reference or
   // not based on its size and fields. If it will be returned by reference,
   // the frontend must modify the prototype so a pointer with the sret
   // annotation is passed as the first argument. This is not necessary for
   // large scalar returns.
   // * Struct return values and varargs should be coerced to structs
   // containing register-size fields in the same situations they would be for
   // fixed arguments.

   static const MCPhysReg ArgGPRs[] = {Ceespu::X10, Ceespu::X11, Ceespu::X12,
                                       Ceespu::X13, Ceespu::X14, Ceespu::X15,
                                       Ceespu::X16, Ceespu::X17};

// Pass a 2*XLEN argument that has been split into two XLEN values through
// registers or the stack as necessary.
static bool CC_CeespuAssign2XLen(unsigned XLen, CCState &State, CCValAssign VA1,
                                ISD::ArgFlagsTy ArgFlags1, unsigned ValNo2,
                                MVT ValVT2, MVT LocVT2,
                                ISD::ArgFlagsTy ArgFlags2) {
 unsigned XLenInBytes = XLen / 8;
 if (unsigned Reg = State.AllocateReg(ArgGPRs)) {
   // At least one half can be passed via register.
   State.addLoc(CCValAssign::getReg(VA1.getValNo(), VA1.getValVT(), Reg,
                                    VA1.getLocVT(), CCValAssign::Full));
 } else {
   // Both halves must be passed on the stack, with proper alignment.
   unsigned StackAlign = std::max(XLenInBytes, ArgFlags1.getOrigAlign());
   State.addLoc(
       CCValAssign::getMem(VA1.getValNo(), VA1.getValVT(),
                           State.AllocateStack(XLenInBytes, StackAlign),
                           VA1.getLocVT(), CCValAssign::Full));
   State.addLoc(CCValAssign::getMem(
       ValNo2, ValVT2, State.AllocateStack(XLenInBytes, XLenInBytes), LocVT2,
       CCValAssign::Full));
   return false;
 }

 if (unsigned Reg = State.AllocateReg(ArgGPRs)) {
   // The second half can also be passed via register.
   State.addLoc(
       CCValAssign::getReg(ValNo2, ValVT2, Reg, LocVT2, CCValAssign::Full));
 } else {
   // The second half is passed via the stack, without additional alignment.
   State.addLoc(CCValAssign::getMem(
       ValNo2, ValVT2, State.AllocateStack(XLenInBytes, XLenInBytes), LocVT2,
       CCValAssign::Full));
 }

 return false;
}
*/
/*
// Implements the RISC-V calling convention. Returns true upon failure.
static bool CC_Ceespu(const DataLayout &DL, unsigned ValNo, MVT ValVT,
                      MVT LocVT, CCValAssign::LocInfo LocInfo,
                      ISD::ArgFlagsTy ArgFlags, CCState &State, bool IsFixed,
                      bool IsRet, Type *OrigTy) {
  MVT XlenVT = MVT::i32;
  if (ValVT == MVT::f32) {
    LocVT = MVT::i32;
    LocInfo = CCValAssign::BCvt;
  }

  // Any return value split in to more than two values can't be returned
  // directly.
  if (IsRet && ValNo > 1) return true;

  // If this is a variadic argument, the RISC-V calling convention requires
  // that it is assigned an 'even' or 'aligned' register if it has 8-byte
  // alignment (RV32) or 16-byte alignment (RV64). An aligned register should
  // be used regardless of whether the original argument was split during
  // legalisation or not. The argument will not be passed by registers if the
  // original type is larger than 2*XLEN, so the register alignment rule does
  // not apply.
  unsigned TwoXLenInBytes = 8;
  if (!IsFixed && ArgFlags.getOrigAlign() == TwoXLenInBytes &&
      DL.getTypeAllocSize(OrigTy) == TwoXLenInBytes) {
    unsigned RegIdx = State.getFirstUnallocated(ArgGPRs);
    // Skip 'odd' register if necessary.
    if (RegIdx != array_lengthof(ArgGPRs) && RegIdx % 2 == 1)
      State.AllocateReg(ArgGPRs);
  }

  SmallVectorImpl<CCValAssign> &PendingLocs = State.getPendingLocs();
  SmallVectorImpl<ISD::ArgFlagsTy> &PendingArgFlags =
      State.getPendingArgFlags();

  assert(PendingLocs.size() == PendingArgFlags.size() &&
         "PendingLocs and PendingArgFlags out of sync");

  // Handle passing f64 on RV32D with a soft float ABI.
  if (XLen == 32 && ValVT == MVT::f64) {
    assert(!ArgFlags.isSplit() && PendingLocs.empty() &&
           "Can't lower f64 if it is split");
    // Depending on available argument GPRS, f64 may be passed in a pair of
    // GPRs, split between a GPR and the stack, or passed completely on the
    // stack. LowerCall/LowerFormalArguments/LowerReturn must recognise these
    // cases.
    unsigned Reg = State.AllocateReg(ArgGPRs);
    LocVT = MVT::i32;
    if (!Reg) {
      unsigned StackOffset = State.AllocateStack(8, 8);
      State.addLoc(
          CCValAssign::getMem(ValNo, ValVT, StackOffset, LocVT, LocInfo));
      return false;
    }
    if (!State.AllocateReg(ArgGPRs)) State.AllocateStack(4, 4);
    State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
    return false;
  }

  // Split arguments might be passed indirectly, so keep track of the pending
  // values.
  if (ArgFlags.isSplit() || !PendingLocs.empty()) {
    LocVT = XLenVT;
    LocInfo = CCValAssign::Indirect;
    PendingLocs.push_back(
        CCValAssign::getPending(ValNo, ValVT, LocVT, LocInfo));
    PendingArgFlags.push_back(ArgFlags);
    if (!ArgFlags.isSplitEnd()) {
      return false;
    }
  }

  // If the split argument only had two elements, it should be passed directly
  // in registers or on the stack.
  if (ArgFlags.isSplitEnd() && PendingLocs.size() <= 2) {
    assert(PendingLocs.size() == 2 && "Unexpected PendingLocs.size()");
    // Apply the normal calling convention rules to the first half of the
    // split argument.
    CCValAssign VA = PendingLocs[0];
    ISD::ArgFlagsTy AF = PendingArgFlags[0];
    PendingLocs.clear();
    PendingArgFlags.clear();
    return CC_CeespuAssign2XLen(XLen, State, VA, AF, ValNo, ValVT, LocVT,
                                ArgFlags);
  }

  // Allocate to a register if possible, or else a stack slot.
  unsigned Reg = State.AllocateReg(ArgGPRs);
  unsigned StackOffset = Reg ? 0 : State.AllocateStack(XLen / 8, XLen / 8);

  // If we reach this point and PendingLocs is non-empty, we must be at the
  // end of a split argument that must be passed indirectly.
  if (!PendingLocs.empty()) {
    assert(ArgFlags.isSplitEnd() && "Expected ArgFlags.isSplitEnd()");
    assert(PendingLocs.size() > 2 && "Unexpected PendingLocs.size()");

    for (auto &It : PendingLocs) {
      if (Reg)
        It.convertToReg(Reg);
      else
        It.convertToMem(StackOffset);
      State.addLoc(It);
    }
    PendingLocs.clear();
    PendingArgFlags.clear();
    return false;
  }

  assert(LocVT == XLenVT && "Expected an XLenVT at this stage");

  if (Reg) {
    State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
  } else {
    State.addLoc(
        CCValAssign::getMem(ValNo, ValVT, StackOffset, LocVT, LocInfo));
  }
  return false;
}
*/

//===----------------------------------------------------------------------===//
//                      Calling Convention Implementation
//===----------------------------------------------------------------------===//

#include "CeespuGenCallingConv.inc"

static unsigned NumFixedArgs;
static bool CC_Ceespu_VarArg(unsigned ValNo, MVT ValVT, MVT LocVT,
                             CCValAssign::LocInfo LocInfo,
                             ISD::ArgFlagsTy ArgFlags, CCState &State) {
  // Handle fixed arguments with default CC.
  // Note: Both the default and fast CC handle VarArg the same and hence the
  // calling convention of the function is not considered here.
  if (ValNo < NumFixedArgs) {
    return CC_Ceespu(ValNo, ValVT, LocVT, LocInfo, ArgFlags, State);
  }

  // Promote i8/i16 args to i32
  if (LocVT == MVT::i8 || LocVT == MVT::i16) {
    LocVT = MVT::i32;
    if (ArgFlags.isSExt())
      LocInfo = CCValAssign::SExt;
    else if (ArgFlags.isZExt())
      LocInfo = CCValAssign::ZExt;
    else
      LocInfo = CCValAssign::AExt;
  }

  // VarArgs get passed on stack
  unsigned Offset = State.AllocateStack(4, 4);
  State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset, LocVT, LocInfo));
  return false;
}

SDValue CeespuTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  switch (CallConv) {
    case CallingConv::C:
    case CallingConv::Fast:
      return LowerCCCArguments(Chain, CallConv, IsVarArg, Ins, DL, DAG, InVals);
    default:
      report_fatal_error("Unsupported calling convention");
  }
}

SDValue CeespuTargetLowering::LowerCall(
    TargetLowering::CallLoweringInfo &CLI,
    SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &DL = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  bool &IsTailCall = CLI.IsTailCall;
  CallingConv::ID CallConv = CLI.CallConv;
  bool IsVarArg = CLI.IsVarArg;

  // Ceespu target does not yet support tail call optimization.
  IsTailCall = false;

  switch (CallConv) {
    case CallingConv::Fast:
    case CallingConv::C:
      return LowerCCCCallTo(Chain, Callee, CallConv, IsVarArg, IsTailCall, Outs,
                            OutVals, Ins, DL, DAG, InVals);
    default:
      report_fatal_error("Unsupported calling convention");
  }
}

// LowerCCCArguments - transform physical registers into virtual registers and
// generate load operations for arguments places on the stack.
SDValue CeespuTargetLowering::LowerCCCArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  CeespuMachineFunctionInfo *CeespuMFI =
      MF.getInfo<CeespuMachineFunctionInfo>();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());

  CCInfo.AnalyzeFormalArguments(Ins, CC_Ceespu);

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    if (VA.isRegLoc()) {
      // Arguments passed in registers
      EVT RegVT = VA.getLocVT();
      switch (RegVT.getSimpleVT().SimpleTy) {
        case MVT::i32: {
          unsigned VReg = RegInfo.createVirtualRegister(&Ceespu::GPRRegClass);
          RegInfo.addLiveIn(VA.getLocReg(), VReg);
          SDValue ArgValue = DAG.getCopyFromReg(Chain, DL, VReg, RegVT);

          // If this is an 8/16-bit value, it is really passed promoted to 32
          // bits. Insert an assert[sz]ext to capture this, then truncate to the
          // right size.
          if (VA.getLocInfo() == CCValAssign::SExt)
            ArgValue = DAG.getNode(ISD::AssertSext, DL, RegVT, ArgValue,
                                   DAG.getValueType(VA.getValVT()));
          else if (VA.getLocInfo() == CCValAssign::ZExt)
            ArgValue = DAG.getNode(ISD::AssertZext, DL, RegVT, ArgValue,
                                   DAG.getValueType(VA.getValVT()));

          if (VA.getLocInfo() != CCValAssign::Full)
            ArgValue = DAG.getNode(ISD::TRUNCATE, DL, VA.getValVT(), ArgValue);

          InVals.push_back(ArgValue);
          break;
        }
        default:
          LLVM_DEBUG(dbgs() << "LowerFormalArguments Unhandled argument type: "
                            << RegVT.getEVTString() << "\n");
          llvm_unreachable("unhandled argument type");
      }
    } else {
      // Sanity check
      assert(VA.isMemLoc());
      // Load the argument to a virtual register
      unsigned ObjSize = VA.getLocVT().getSizeInBits() / 8;
      // Check that the argument fits in stack slot
      if (ObjSize > 4) {
        errs() << "LowerFormalArguments Unhandled argument type: "
               << EVT(VA.getLocVT()).getEVTString() << "\n";
      }
      // Create the frame index object for this incoming parameter...
      int FI = MFI.CreateFixedObject(ObjSize, VA.getLocMemOffset(), true);

      // Create the SelectionDAG nodes corresponding to a load
      // from this parameter
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i32);
      InVals.push_back(DAG.getLoad(
          VA.getLocVT(), DL, Chain, FIN,
          MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI)));
    }
  }

  // The Ceespu ABI for returning structs by value requires that we copy
  // the sret argument into rv for the return. Save the argument into
  // a virtual register so that we can access it from the return points.
  if (MF.getFunction().hasStructRetAttr()) {
    unsigned Reg = CeespuMFI->getSRetReturnReg();
    if (!Reg) {
      Reg = MF.getRegInfo().createVirtualRegister(getRegClassFor(MVT::i32));
      CeespuMFI->setSRetReturnReg(Reg);
    }
    SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), DL, Reg, InVals[0]);
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, Copy, Chain);
  }

  if (IsVarArg) {
    // Record the frame index of the first variable argument
    // which is a value necessary to VASTART.
    int FI = MFI.CreateFixedObject(4, CCInfo.getNextStackOffset(), true);
    CeespuMFI->setVarArgsFrameIndex(FI);
  }

  return Chain;
}

SDValue CeespuTargetLowering::LowerReturn(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::OutputArg> &Outs,
    const SmallVectorImpl<SDValue> &OutVals, const SDLoc &DL,
    SelectionDAG &DAG) const {
  // CCValAssign - represent the assignment of the return value to a location
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  // Analize return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_Ceespu);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutVals[i], Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  // The Ceespu ABI for returning structs by value requires that we copy
  // the sret argument into rv for the return. We saved the argument into
  // a virtual register in the entry block, so now we copy the value out
  // and into rv.
  if (DAG.getMachineFunction().getFunction().hasStructRetAttr()) {
    MachineFunction &MF = DAG.getMachineFunction();
    CeespuMachineFunctionInfo *CeespuMFI =
        MF.getInfo<CeespuMachineFunctionInfo>();
    unsigned Reg = CeespuMFI->getSRetReturnReg();
    assert(Reg &&
           "SRetReturnReg should have been set in LowerFormalArguments().");
    SDValue Val =
        DAG.getCopyFromReg(Chain, DL, Reg, getPointerTy(DAG.getDataLayout()));

    Chain = DAG.getCopyToReg(Chain, DL, Ceespu::R20, Val, Flag);
    Flag = Chain.getValue(1);
    RetOps.push_back(
        DAG.getRegister(Ceespu::R20, getPointerTy(DAG.getDataLayout())));
  }

  RetOps[0] = Chain;  // Update chain

  unsigned Opc = CeespuISD::RET_FLAG;
  if (Flag.getNode()) RetOps.push_back(Flag);

  // Return Void
  return DAG.getNode(Opc, DL, MVT::Other,
                     ArrayRef<SDValue>(&RetOps[0], RetOps.size()));
}

// LowerCCCCallTo - functions arguments are copied from virtual regs to
// (physical regs)/(stack frame), CALLSEQ_START and CALLSEQ_END are emitted.
SDValue CeespuTargetLowering::LowerCCCCallTo(
    SDValue Chain, SDValue Callee, CallingConv::ID CallConv, bool IsVarArg,
    bool /*IsTailCall*/, const SmallVectorImpl<ISD::OutputArg> &Outs,
    const SmallVectorImpl<SDValue> &OutVals,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee);
  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();

  NumFixedArgs = 0;
  if (IsVarArg && G) {
    const Function *CalleeFn = dyn_cast<Function>(G->getGlobal());
    if (CalleeFn) NumFixedArgs = CalleeFn->getFunctionType()->getNumParams();
  }
  if (NumFixedArgs)
    CCInfo.AnalyzeCallOperands(Outs, CC_Ceespu_VarArg);
  else {
    CCInfo.AnalyzeCallOperands(Outs, CC_Ceespu);
  }

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = CCInfo.getNextStackOffset();

  // Create local copies for byval args.
  SmallVector<SDValue, 8> ByValArgs;
  for (unsigned I = 0, E = Outs.size(); I != E; ++I) {
    ISD::ArgFlagsTy Flags = Outs[I].Flags;
    if (!Flags.isByVal()) continue;

    SDValue Arg = OutVals[I];
    unsigned Size = Flags.getByValSize();
    unsigned Align = Flags.getByValAlign();

    int FI = MFI.CreateStackObject(Size, Align, false);
    SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
    SDValue SizeNode = DAG.getConstant(Size, DL, MVT::i32);

    Chain = DAG.getMemcpy(Chain, DL, FIPtr, Arg, SizeNode, Align,
                          /*IsVolatile=*/false,
                          /*AlwaysInline=*/false,
                          /*isTailCall=*/false, MachinePointerInfo(),
                          MachinePointerInfo());
    ByValArgs.push_back(FIPtr);
  }

  Chain = DAG.getCALLSEQ_START(Chain, NumBytes, 0, DL);

  SmallVector<std::pair<unsigned, SDValue>, 4> RegsToPass;
  SmallVector<SDValue, 12> MemOpChains;
  SDValue StackPtr;

  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned I = 0, J = 0, E = ArgLocs.size(); I != E; ++I) {
    CCValAssign &VA = ArgLocs[I];
    SDValue Arg = OutVals[I];
    ISD::ArgFlagsTy Flags = Outs[I].Flags;

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
      case CCValAssign::Full:
        break;
      case CCValAssign::SExt:
        Arg = DAG.getNode(ISD::SIGN_EXTEND, DL, VA.getLocVT(), Arg);
        break;
      case CCValAssign::ZExt:
        Arg = DAG.getNode(ISD::ZERO_EXTEND, DL, VA.getLocVT(), Arg);
        break;
      case CCValAssign::AExt:
        Arg = DAG.getNode(ISD::ANY_EXTEND, DL, VA.getLocVT(), Arg);
        break;
      default:
        llvm_unreachable("Unknown loc info!");
    }

    // Use local copy if it is a byval arg.
    if (Flags.isByVal()) Arg = ByValArgs[J++];

    // Arguments that can be passed on register must be kept at RegsToPass
    // vector
    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
    } else {
      assert(VA.isMemLoc());

      if (StackPtr.getNode() == nullptr)
        StackPtr = DAG.getCopyFromReg(Chain, DL, Ceespu::SP,
                                      getPointerTy(DAG.getDataLayout()));

      SDValue PtrOff =
          DAG.getNode(ISD::ADD, DL, getPointerTy(DAG.getDataLayout()), StackPtr,
                      DAG.getIntPtrConstant(VA.getLocMemOffset(), DL));

      MemOpChains.push_back(
          DAG.getStore(Chain, DL, Arg, PtrOff, MachinePointerInfo()));
    }
  }

  // Transform all store nodes into one single node because all store nodes are
  // independent of each other.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                        ArrayRef<SDValue>(&MemOpChains[0], MemOpChains.size()));

  SDValue InFlag;

  // Build a sequence of copy-to-reg nodes chained together with token chain and
  // flag operands which copy the outgoing args into registers.  The InFlag in
  // necessary since all emitted instructions must be stuck together.
  for (unsigned I = 0, E = RegsToPass.size(); I != E; ++I) {
    Chain = DAG.getCopyToReg(Chain, DL, RegsToPass[I].first,
                             RegsToPass[I].second, InFlag);
    InFlag = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  uint8_t OpFlag = 0;  // CeespuII::MO_NO_FLAG;
  if (G) {
    Callee = DAG.getTargetGlobalAddress(
        G->getGlobal(), DL, getPointerTy(DAG.getDataLayout()), 0, OpFlag);
  } else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    Callee = DAG.getTargetExternalSymbol(
        E->getSymbol(), getPointerTy(DAG.getDataLayout()), OpFlag);
  }

  // Returns a chain & a flag for retval copy to use.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);
  const CeespuRegisterInfo *TRI = Subtarget.getRegisterInfo();
  // Add a register mask operand representing the call-preserved registers.
  // TODO: Should return-twice functions be handled?
  const uint32_t *Mask =
      TRI->getCallPreservedMask(DAG.getMachineFunction(), CallConv);
  assert(Mask && "Missing call preserved mask for calling convention");
  Ops.push_back(DAG.getRegisterMask(Mask));

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (unsigned I = 0, E = RegsToPass.size(); I != E; ++I)
    Ops.push_back(DAG.getRegister(RegsToPass[I].first,
                                  RegsToPass[I].second.getValueType()));

  if (InFlag.getNode()) Ops.push_back(InFlag);

  Chain = DAG.getNode(CeespuISD::CALL, DL, NodeTys,
                      ArrayRef<SDValue>(&Ops[0], Ops.size()));
  InFlag = Chain.getValue(1);

  // Create the CALLSEQ_END node.
  Chain = DAG.getCALLSEQ_END(
      Chain,
      DAG.getConstant(NumBytes, DL, getPointerTy(DAG.getDataLayout()), true),
      DAG.getConstant(0, DL, getPointerTy(DAG.getDataLayout()), true), InFlag,
      DL);
  InFlag = Chain.getValue(1);

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(Chain, InFlag, CallConv, IsVarArg, Ins, DL, DAG,
                         InVals);
}

// LowerCallResult - Lower the result values of a call into the
// appropriate copies out of appropriate physical registers.
SDValue CeespuTargetLowering::LowerCallResult(
    SDValue Chain, SDValue InFlag, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  CCInfo.AnalyzeCallResult(Ins, RetCC_Ceespu);

  // Copy all of the result registers out of their specified physreg.
  for (unsigned I = 0; I != RVLocs.size(); ++I) {
    Chain = DAG.getCopyFromReg(Chain, DL, RVLocs[I].getLocReg(),
                               RVLocs[I].getValVT(), InFlag)
                .getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

/// IsEligibleForTailCallOptimization - Check whether the call is eligible
/// for tail call optimization.
/// Note: This is modelled after ARM's IsEligibleForTailCallOptimization.
bool CeespuTargetLowering::IsEligibleForTailCallOptimization(
    CCState &CCInfo, CallLoweringInfo &CLI, MachineFunction &MF,
    const SmallVector<CCValAssign, 16> &ArgLocs) const {
  auto &Callee = CLI.Callee;
  auto CalleeCC = CLI.CallConv;
  auto IsVarArg = CLI.IsVarArg;
  auto &Outs = CLI.Outs;
  auto &Caller = MF.getFunction();
  auto CallerCC = Caller.getCallingConv();

  // Do not tail call opt functions with "disable-tail-calls" attribute.
  if (Caller.getFnAttribute("disable-tail-calls").getValueAsString() == "true")
    return false;

  // Exception-handling functions need a special set of instructions to
  // indicate a return to the hardware. Tail-calling another function would
  // probably break this.
  // TODO: The "interrupt" attribute isn't currently defined by RISC-V. This
  // should be expanded as new function attributes are introduced.
  if (Caller.hasFnAttribute("interrupt")) return false;

  // Do not tail call opt functions with varargs.
  if (IsVarArg) return false;

  // Do not tail call opt if the stack is used to pass parameters.
  if (CCInfo.getNextStackOffset() != 0) return false;

  // Do not tail call opt if any parameters need to be passed indirectly.
  // Since long doubles (fp128) and i128 are larger than 2*XLEN, they are
  // passed indirectly. So the address of the value will be passed in a
  // register, or if not available, then the address is put on the stack. In
  // order to pass indirectly, space on the stack often needs to be allocated
  // in order to store the value. In this case the CCInfo.getNextStackOffset()
  // != 0 check is not enough and we need to check if any CCValAssign ArgsLocs
  // are passed CCValAssign::Indirect.
  for (auto &VA : ArgLocs)
    if (VA.getLocInfo() == CCValAssign::Indirect) return false;

  // Do not tail call opt if either caller or callee uses struct return
  // semantics.
  auto IsCallerStructRet = Caller.hasStructRetAttr();
  auto IsCalleeStructRet = Outs.empty() ? false : Outs[0].Flags.isSRet();
  if (IsCallerStructRet || IsCalleeStructRet) return false;

  // Externally-defined functions with weak linkage should not be
  // tail-called. The behaviour of branch instructions in this situation (as
  // used for tail calls) is implementation-defined, so we cannot rely on the
  // linker replacing the tail call with a return.
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    const GlobalValue *GV = G->getGlobal();
    if (GV->hasExternalWeakLinkage()) return false;
  }

  // The callee has to preserve all registers the caller needs to preserve.
  const CeespuRegisterInfo *TRI = Subtarget.getRegisterInfo();
  const uint32_t *CallerPreserved = TRI->getCallPreservedMask(MF, CallerCC);
  if (CalleeCC != CallerCC) {
    const uint32_t *CalleePreserved = TRI->getCallPreservedMask(MF, CalleeCC);
    if (!TRI->regmaskSubsetEqual(CallerPreserved, CalleePreserved))
      return false;
  }

  // Byval parameters hand the function a pointer directly into the stack area
  // we want to reuse during a tail call. Working around this *is* possible
  // but less efficient and uglier in LowerCall.
  for (auto &Arg : Outs)
    if (Arg.Flags.isByVal()) return false;

  return true;
}

static SDValue packIntoRegLoc(SelectionDAG &DAG, SDValue Val,
                              const CCValAssign &VA, const SDLoc &DL) {
  EVT LocVT = VA.getLocVT();

  switch (VA.getLocInfo()) {
    default:
      llvm_unreachable("Unexpected CCValAssign::LocInfo");
    case CCValAssign::Full:
      break;
    case CCValAssign::BCvt:
      Val = DAG.getNode(ISD::BITCAST, DL, LocVT, Val);
      break;
  }
  return Val;
}

const char *CeespuTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((CeespuISD::NodeType)Opcode) {
    case CeespuISD::FIRST_NUMBER:
      break;
    case CeespuISD::RET_FLAG:
      return "CeespuISD::RET_FLAG";
    case CeespuISD::CALL:
      return "CeespuISD::CALL";
    case CeespuISD::SELECT_CC:
      return "CeespuISD::SELECT_CC";
    case CeespuISD::TAIL:
      return "CeespuISD::TAIL";
  }
  return nullptr;
}

std::pair<unsigned, const TargetRegisterClass *>
CeespuTargetLowering::getRegForInlineAsmConstraint(
    const TargetRegisterInfo *TRI, StringRef Constraint, MVT VT) const {
  // First, see if this is a constraint that directly corresponds to a
  // Ceespu register class.
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
      case 'r':
        return std::make_pair(0U, &Ceespu::GPRRegClass);
      default:
        break;
    }
  }

  return TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);
}
