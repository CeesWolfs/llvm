//===-- CeespuISelLowering.cpp - Ceespu DAG Lowering Implementation  ------------===//
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
#include "CeespuTargetMachine.h"
#include "CeespuSubtarget.h"
#include "CeespuMachineFunction.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
using namespace llvm;

#define DEBUG_TYPE "Ceespu-lower"

namespace {

// Diagnostic information for unimplemented or unsupported feature reporting.
class DiagnosticInfoUnsupported : public DiagnosticInfo {
private:
  // Debug location where this diagnostic is triggered.
  DebugLoc DLoc;
  const Twine &Description;
  const Function &Fn;
  SDValue Value;

  static int KindID;

  static int getKindID() {
    if (KindID == 0)
      KindID = llvm::getNextAvailablePluginDiagnosticKind();
    return KindID;
  }

public:
  DiagnosticInfoUnsupported(SDLoc DLoc, const Function &Fn, const Twine &Desc,
                            SDValue Value)
      : DiagnosticInfo(getKindID(), DS_Error), DLoc(DLoc.getDebugLoc()),
        Description(Desc), Fn(Fn), Value(Value) {}

  void print(DiagnosticPrinter &DP) const override {
    std::string Str;
    raw_string_ostream OS(Str);

    if (DLoc) {
      auto DIL = DLoc.get();
      StringRef Filename = DIL->getFilename();
      unsigned Line = DIL->getLine();
      unsigned Column = DIL->getColumn();
      OS << Filename << ':' << Line << ':' << Column << ' ';
    }

    OS << "in function " << Fn.getName() << ' ' << *Fn.getFunctionType() << '\n'
       << Description;
    if (Value)
      Value->print(OS);
    OS << '\n';
    OS.flush();
    DP << Str;
  }

  static bool classof(const DiagnosticInfo *DI) {
    return DI->getKind() == getKindID();
  }
};

int DiagnosticInfoUnsupported::KindID = 0;
}

CeespuTargetLowering::CeespuTargetLowering(const TargetMachine &TM,
                                     const CeespuSubtarget &STI)
    : TargetLowering(TM) {

  // Set up the register classes.
  addRegisterClass(MVT::i32, &Ceespu::GPRRegClass);

  // Compute derived properties from the register classes
  computeRegisterProperties(STI.getRegisterInfo());

  setStackPointerRegisterToSaveRestore(Ceespu::SP);

  setOperationAction(ISD::BR_CC, MVT::i32, Custom);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  setOperationAction(ISD::BRIND, MVT::Other, Expand);
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  setOperationAction(ISD::SETCC, MVT::i32, Expand);
  setOperationAction(ISD::SELECT, MVT::i32, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i32, Custom);

  setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
  setOperationAction(ISD::ExternalSymbol,MVT::i32,   Custom);

  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32, Expand);
  setOperationAction(ISD::STACKSAVE, MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE, MVT::Other, Expand);

  setOperationAction(ISD::VAARG,             MVT::Other, Expand);
  setOperationAction(ISD::VACOPY,            MVT::Other, Expand);
  setOperationAction(ISD::VAEND,             MVT::Other, Expand);

  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIV, MVT::i32, Expand);
  setOperationAction(ISD::SDIV, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::UREM, MVT::i32, Expand);

  setOperationAction(ISD::MULHU, MVT::i32, Expand);
  setOperationAction(ISD::MULHS, MVT::i32, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i32, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i32, Expand);

  setOperationAction(ISD::ROTR, MVT::i32, Expand);
  setOperationAction(ISD::ROTL, MVT::i32, Expand);
  setOperationAction(ISD::SHL_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRL_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRA_PARTS, MVT::i32, Expand);

  setOperationAction(ISD::CTTZ, MVT::i32, Expand);
  setOperationAction(ISD::CTLZ, MVT::i32, Expand);
  setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i32, Expand);
  setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i32, Expand);
  setOperationAction(ISD::CTPOP, MVT::i32, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1, Expand);
  //setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8, Expand);
  //setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);

  // Extended load operations for i1 types must be promoted
  for (MVT VT : MVT::integer_valuetypes()) {
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::i1, Promote);
    setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::i1, Promote);
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i1, Promote);
  }

  setBooleanContents(ZeroOrOneBooleanContent);

  // Function alignments (log2)
  setMinFunctionAlignment(2);
  setPrefFunctionAlignment(2);

  // inline memcpy() for kernel to see explicit copy
  MaxStoresPerMemset = MaxStoresPerMemsetOptSize = 128;
  MaxStoresPerMemcpy = MaxStoresPerMemcpyOptSize = 128;
  MaxStoresPerMemmove = MaxStoresPerMemmoveOptSize = 128;
}

SDValue CeespuTargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG);
  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG);
  case ISD::ExternalSymbol:
    return LowerExternalSymbol(Op, DAG);
  case ISD::SELECT_CC:
    return LowerSELECT_CC(Op, DAG);
  default:
    llvm_unreachable("unimplemented operand");
  }
}

// Calling Convention Implementation
#include "CeespuGenCallingConv.inc"

SDValue CeespuTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, SDLoc DL, SelectionDAG &DAG,
    SmallVectorImpl<SDValue> &InVals) const {
  switch (CallConv) {
  default:
    llvm_unreachable("Unsupported calling convention");
  case CallingConv::C:
  case CallingConv::Fast:
    break;
  }

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  CeespuFunctionInfo *CeespuFI = MF.getInfo<CeespuFunctionInfo>();

  int LastFI = 0;

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_Ceespu);

  for (auto &VA : ArgLocs) {
	  EVT ValVT = VA.getValVT();

    if (VA.isRegLoc()) {
      // Arguments passed in registers
      EVT RegVT = VA.getLocVT();
      switch (RegVT.getSimpleVT().SimpleTy) {
      default: {
        errs() << "LowerFormalArguments Unhandled argument type: "
               << RegVT.getSimpleVT().SimpleTy << '\n';
        llvm_unreachable(0);
      }
      case MVT::i32:
        unsigned VReg = RegInfo.createVirtualRegister(&Ceespu::GPRRegClass);
        RegInfo.addLiveIn(VA.getLocReg(), VReg);
        SDValue ArgValue = DAG.getCopyFromReg(Chain, DL, VReg, RegVT);

        // If this is an 8/16/32-bit value, it is really passed promoted to 32
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
      }
    } else {
      // sanity check
      assert(VA.isMemLoc());
      // The stack pointer offset is relative to the caller stack frame.
      // Since the real stack size is unknown here, a negative SPOffset
      // is used so there's a way to adjust these offsets when the stack
      // size get known (on EliminateFrameIndex). A dummy SPOffset is
      // used instead of a direct negative address (which is recorded to
      // be used on emitPrologue) to avoid mis-calc of the first stack
      // offset on PEI::calculateFrameObjectOffsets.
      // Arguments are always 32-bit.
      unsigned ArgSize = VA.getLocVT().getSizeInBits()/8;
      unsigned StackLoc = VA.getLocMemOffset();
      int FI = MFI->CreateFixedObject(ArgSize, -4, true);
      CeespuFI->recordLoadArgsFI(FI, -StackLoc);
      CeespuFI->recordLiveIn(FI);
      // Create load nodes to retrieve arguments from the stack
      SDValue FIN = DAG.getFrameIndex(FI, getPointerTy(MF.getDataLayout()));
      InVals.push_back(DAG.getLoad(VA.getValVT(), DL, Chain, FIN,
                                   MachinePointerInfo::getFixedStack(MF, FI),
                                   false, false, false, 0));
    }
  }

  if (IsVarArg) {
    DiagnosticInfoUnsupported Err(
        DL, *MF.getFunction(),
        "functions with VarArgs or StructRet are not supported", SDValue());
    DAG.getContext()->diagnose(Err);
  }

  return Chain;
}

SDValue CeespuTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                     SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  auto &Outs = CLI.Outs;
  auto &OutVals = CLI.OutVals;
  auto &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  bool &IsTailCall = CLI.IsTailCall;
  SDLoc &DL = CLI.DL;
  CallingConv::ID CallConv = CLI.CallConv;
  bool IsVarArg = CLI.IsVarArg;
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  EVT PtrVT = getPointerTy(DAG.getDataLayout());
  int byvalArgIdx = 0;

  // Ceespu target does not support tail call optimization.
  IsTailCall = false;

  switch (CallConv) {
  default:
    report_fatal_error("Unsupported calling convention");
  case CallingConv::Fast:
  case CallingConv::C:
    break;
  }

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());

  CCInfo.AnalyzeCallOperands(Outs, CC_Ceespu);

  unsigned NumBytes = CCInfo.getNextStackOffset();

  for (auto &Arg : Outs) {
    ISD::ArgFlagsTy Flags = Arg.Flags;
    if (1)//!Flags.isByVal())
      continue;

    DiagnosticInfoUnsupported Err(CLI.DL, *MF.getFunction(),
                                  "pass by value not supported ", Callee);
    DAG.getContext()->diagnose(Err);
  }


  SmallVector<std::pair<unsigned, SDValue>, 5> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;
  SmallVector<SDValue, 8> ByValArgs;
  for (unsigned i = 0,  e = Outs.size(); i != e; ++i) {
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    if (!Flags.isByVal())
      continue;

    SDValue Arg = OutVals[i];
    unsigned Size = Flags.getByValSize();
    unsigned Align = Flags.getByValAlign();

    int FI = MFI->CreateStackObject(Size, Align, false);
    SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
    SDValue SizeNode = DAG.getConstant(Size, DL, MVT::i32);

    Chain = DAG.getMemcpy(Chain, DL, FIPtr, Arg, SizeNode, Align,
                          false,        // isVolatile,
                          (Size <= 32), // AlwaysInline if size <= 32,
                          false,        // isTailCall
                          MachinePointerInfo(), MachinePointerInfo());
    ByValArgs.push_back(FIPtr);
  }

  Chain = DAG.getCALLSEQ_START(
	  Chain, DAG.getConstant(NumBytes, CLI.DL, PtrVT, true), CLI.DL);

  // Walk arg assignments
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
	  CCValAssign &VA = ArgLocs[i];
	  SDValue Arg = OutVals[i];
	  SDValue ArgValue = OutVals[i];
	  ISD::ArgFlagsTy Flags = Outs[i].Flags;
	  SDValue StackPtr;
	  if (Flags.isByVal())
		  Arg = ByValArgs[byvalArgIdx++];
	  // Promote the value if needed.
	  switch (VA.getLocInfo()) {
	  default:
		  llvm_unreachable("Unknown loc info");
	  case CCValAssign::Full:
		  break;
	  case CCValAssign::SExt:
		  Arg = DAG.getNode(ISD::SIGN_EXTEND, CLI.DL, VA.getLocVT(), Arg);
		  break;
	  case CCValAssign::ZExt:
		  Arg = DAG.getNode(ISD::ZERO_EXTEND, CLI.DL, VA.getLocVT(), Arg);
		  break;
	  case CCValAssign::AExt:
		  Arg = DAG.getNode(ISD::ANY_EXTEND, CLI.DL, VA.getLocVT(), Arg);
		  break;
	  }

	  // Push arguments into RegsToPass vector
	  if (VA.isRegLoc())
		  RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
	  else {
		  assert(VA.isMemLoc() && "Argument not register or memory this makes no sense");
		  // Work out the address of the stack slot.  Unpromoted ints and
		  // floats are passed as right-justified 8-byte values.
		  if (!StackPtr.getNode())
			  StackPtr = DAG.getCopyFromReg(Chain, DL, Ceespu::SP, PtrVT);
		  // Create the frame index object for this incoming parameter
		  unsigned ArgSize = VA.getValVT().getSizeInBits() / 8;
		  unsigned StackLoc = VA.getLocMemOffset();
		  int FI = MFI->CreateFixedObject(ArgSize, StackLoc, true);

		  SDValue PtrOff = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));

		  // emit ISD::STORE whichs stores the
		  // parameter value to a stack Location
		  MemOpChains.push_back(DAG.getStore(Chain, DL, Arg, PtrOff,
			  MachinePointerInfo(),
			  false, false, 0));
	  }
  }
  SDValue InFlag;
  if (!MemOpChains.empty()) {Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOpChains); }
  // Build a sequence of copy-to-reg nodes chained together with token chain and
  // flag operands which copy the outgoing args into registers.  The InFlag in
  // necessary since all emitted instructions must be stuck together.
  for (auto &Reg : RegsToPass) {
    Chain = DAG.getCopyToReg(Chain, CLI.DL, Reg.first, Reg.second, InFlag);
    InFlag = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), CLI.DL, PtrVT,
                                        G->getOffset(), 0);
  else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), PtrVT, 0);

  // Returns a chain & a flag for retval copy to use.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (auto &Reg : RegsToPass)
    Ops.push_back(DAG.getRegister(Reg.first, Reg.second.getValueType()));

  if (InFlag.getNode())
    Ops.push_back(InFlag);

  Chain = DAG.getNode(CeespuISD::CALL, CLI.DL, NodeTys, Ops);
  InFlag = Chain.getValue(1);

  // Create the CALLSEQ_END node.
  Chain = DAG.getCALLSEQ_END(
      Chain, DAG.getConstant(NumBytes, CLI.DL, PtrVT, true),
      DAG.getConstant(0, CLI.DL, PtrVT, true), InFlag, CLI.DL);
  InFlag = Chain.getValue(1);

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(Chain, InFlag, CallConv, IsVarArg, Ins, CLI.DL, DAG,
                         InVals);
}

SDValue
CeespuTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                               bool IsVarArg,
                               const SmallVectorImpl<ISD::OutputArg> &Outs,
                               const SmallVectorImpl<SDValue> &OutVals,
                               SDLoc DL, SelectionDAG &DAG) const {

  // CCValAssign - represent the assignment of the return value to a location
  SmallVector<CCValAssign, 16> RVLocs;
  MachineFunction &MF = DAG.getMachineFunction();

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());

  if (MF.getFunction()->getReturnType()->isAggregateType()) {
    DiagnosticInfoUnsupported Err(DL, *MF.getFunction(),
                                  "only integer returns supported", SDValue());
    DAG.getContext()->diagnose(Err);
  }

  // Analize return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_Ceespu);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutVals[i], Flag);

    // Guarantee that all emitted copies are stuck together,
    // avoiding something bad.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  unsigned Opc = CeespuISD::RET_FLAG;
  RetOps[0] = Chain; // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  return DAG.getNode(Opc, DL, MVT::Other, RetOps);
}

SDValue CeespuTargetLowering::LowerCallResult(
    SDValue Chain, SDValue InFlag, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, SDLoc DL, SelectionDAG &DAG,
    SmallVectorImpl<SDValue> &InVals) const {

  MachineFunction &MF = DAG.getMachineFunction();
  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());

  if (Ins.size() >= 4) {
    DiagnosticInfoUnsupported Err(DL, *MF.getFunction(),
                                  "only small returns supported", SDValue());
    DAG.getContext()->diagnose(Err);
  }

  CCInfo.AnalyzeCallResult(Ins, RetCC_Ceespu);

  // Copy all of the result registers out of their specified physreg.
  // Copy all of the result registers out of their specified physreg.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    Chain = DAG.getCopyFromReg(Chain, DL, RVLocs[i].getLocReg(),
                               RVLocs[i].getValVT(), InFlag).getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

static void NegateCC(SDValue &LHS, SDValue &RHS, ISD::CondCode &CC) {
  switch (CC) {
  default:
    break;
  case ISD::SETULT:
  case ISD::SETULE:
  case ISD::SETLT:
  case ISD::SETLE:
    CC = ISD::getSetCCSwappedOperands(CC);
    std::swap(LHS, RHS);
    break;
  }
}

SDValue CeespuTargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);
  SDLoc DL(Op);

  NegateCC(LHS, RHS, CC);

  return DAG.getNode(CeespuISD::BR_CC, DL, Op.getValueType(), Chain, LHS, RHS,
                     DAG.getConstant(CC, DL, MVT::i32), Dest);
}

SDValue CeespuTargetLowering::LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue TrueV = Op.getOperand(2);
  SDValue FalseV = Op.getOperand(3);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();
  SDLoc DL(Op);

  NegateCC(LHS, RHS, CC);

  SDValue TargetCC = DAG.getConstant(CC, DL, MVT::i32);

  SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::Glue);
  SDValue Ops[] = {LHS, RHS, TargetCC, TrueV, FalseV};

  return DAG.getNode(CeespuISD::SELECT_CC, DL, VTs, Ops);
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
  case CeespuISD::BR_CC:
    return "CeespuISD::BR_CC";
  case CeespuISD::Wrapper:
    return "CeespuISD::Wrapper";
  }
  return nullptr;
}

SDValue CeespuTargetLowering::LowerGlobalAddress(SDValue Op,
                                              SelectionDAG &DAG) const {
  SDLoc DL(Op);
  const GlobalValue *GV = cast<GlobalAddressSDNode>(Op)->getGlobal();
  SDValue GA = DAG.getTargetGlobalAddress(GV, DL, MVT::i32);

  return DAG.getNode(CeespuISD::Wrapper, DL, MVT::i32, GA);
}

SDValue CeespuTargetLowering::LowerExternalSymbol(SDValue Op,
                                                  SelectionDAG &DAG) const {
  SDLoc dl(Op);
  const char *Sym = cast<ExternalSymbolSDNode>(Op)->getSymbol();
  auto PtrVT = getPointerTy(DAG.getDataLayout());
  SDValue Result = DAG.getTargetExternalSymbol(Sym, PtrVT);
  return DAG.getNode(llvm::CeespuISD::Wrapper, dl, PtrVT, Result);
}

MachineBasicBlock *
CeespuTargetLowering::EmitInstrWithCustomInserter(MachineInstr *MI,
                                               MachineBasicBlock *BB) const {
  const TargetInstrInfo &TII = *BB->getParent()->getSubtarget().getInstrInfo();
  DebugLoc DL = MI->getDebugLoc();
  DEBUG(dbgs() << "EmitInstrWithCustomInserter \n");
  assert(MI->getOpcode() == Ceespu::Select && "Unexpected instr type to insert");

  // To "insert" a SELECT instruction, we actually have to insert the diamond
  // control-flow pattern.  The incoming instruction knows the destination vreg
  // to set, the condition code register to branch on, the true/false values to
  // select between, and a branch opcode to use.
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
  unsigned LHS = MI->getOperand(1).getReg();
  unsigned RHS = MI->getOperand(2).getReg();
  int CC = MI->getOperand(3).getImm();
  switch (CC) {
  case ISD::SETGT:
    BuildMI(BB, DL, TII.get(Ceespu::JSGT_rr))
        .addReg(LHS)
        .addReg(RHS)
        .addMBB(Copy1MBB);
    break;
  case ISD::SETUGT:
    BuildMI(BB, DL, TII.get(Ceespu::JUGT_rr))
        .addReg(LHS)
        .addReg(RHS)
        .addMBB(Copy1MBB);
    break;
  case ISD::SETGE:
    BuildMI(BB, DL, TII.get(Ceespu::JSGE_rr))
        .addReg(LHS)
        .addReg(RHS)
        .addMBB(Copy1MBB);
    break;
  case ISD::SETUGE:
    BuildMI(BB, DL, TII.get(Ceespu::JUGE_rr))
        .addReg(LHS)
        .addReg(RHS)
        .addMBB(Copy1MBB);
    break;
  case ISD::SETEQ:
    BuildMI(BB, DL, TII.get(Ceespu::JEQ_rr))
        .addReg(LHS)
        .addReg(RHS)
        .addMBB(Copy1MBB);
    break;
  case ISD::SETNE:
    BuildMI(BB, DL, TII.get(Ceespu::JNE_rr))
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
  if (MI->getOperand(0).getReg() == Ceespu::R0) {
	  unsigned VReg = F->getRegInfo().createVirtualRegister(&Ceespu::GPRRegClass);
	  BuildMI(*Copy0MBB, Copy0MBB->begin(), DL, TII.get(TargetOpcode::COPY), VReg)
		  .addReg(MI->getOperand(0).getReg());
	  BuildMI(*BB, BB->begin(), DL, TII.get(Ceespu::PHI), VReg)
		  .addReg(MI->getOperand(5).getReg())
		  .addMBB(Copy0MBB)
		  .addReg(MI->getOperand(4).getReg())
		  .addMBB(ThisMBB);
  }
  else if (MI->getOperand(4).getReg() == Ceespu::R0) {
	  unsigned VReg = F->getRegInfo().createVirtualRegister(&Ceespu::GPRRegClass);
	  BuildMI(*Copy0MBB, Copy0MBB->begin(), DL, TII.get(TargetOpcode::COPY), VReg)
		  .addReg(MI->getOperand(4).getReg());
	  BuildMI(*BB, BB->begin(), DL, TII.get(Ceespu::PHI), MI->getOperand(0).getReg())
		  .addReg(MI->getOperand(5).getReg())
		  .addMBB(Copy0MBB)
		  .addReg(VReg)
		  .addMBB(ThisMBB);
  }
  else if (MI->getOperand(5).getReg() == Ceespu::R0) {
	  unsigned VReg = F->getRegInfo().createVirtualRegister(&Ceespu::GPRRegClass);
	  BuildMI(*Copy0MBB, Copy0MBB->begin(), DL, TII.get(TargetOpcode::COPY), VReg)
		  .addReg(MI->getOperand(5).getReg());
	  BuildMI(*BB, BB->begin(), DL, TII.get(Ceespu::PHI), MI->getOperand(0).getReg())
		  .addReg(VReg)
		  .addMBB(Copy0MBB)
		  .addReg(MI->getOperand(4).getReg())
		  .addMBB(ThisMBB);
  }
  else {
	  BuildMI(*BB, BB->begin(), DL, TII.get(Ceespu::PHI), MI->getOperand(0).getReg())
		  .addReg(MI->getOperand(5).getReg())
		  .addMBB(Copy0MBB)
		  .addReg(MI->getOperand(4).getReg())
		  .addMBB(ThisMBB);
  }
  MI->eraseFromParent(); // The pseudo instruction is gone now.
  return BB;
}
