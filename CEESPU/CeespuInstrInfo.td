//===-- CeespuInstrInfo.td - Target Description for Ceespu Target ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes the Ceespu instructions in TableGen format.
//
//===----------------------------------------------------------------------===//

include "CeespuInstrFormats.td"

// Instruction Operands and Patterns

// These are target-independent nodes, but have target-specific formats.
def SDT_CeespuCallSeqStart : SDCallSeqStart<[SDTCisVT<0, iPTR>]>;
def SDT_CeespuCallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, iPTR>, SDTCisVT<1, iPTR>]>;
def SDT_CeespuCall         : SDTypeProfile<0, -1, [SDTCisVT<0, iPTR>]>;
def SDT_CeespuSetFlag      : SDTypeProfile<0, 3, [SDTCisSameAs<0, 1>]>;
def SDT_CeespuSelectCC     : SDTypeProfile<1, 5, [SDTCisSameAs<1, 2>,
                                               SDTCisSameAs<0, 4>,
                                               SDTCisSameAs<4, 5>]>;
def SDT_CeespuBrCC         : SDTypeProfile<0, 4, [SDTCisSameAs<0, 1>,
                                               SDTCisVT<3, OtherVT>]>;
def SDT_CeespuWrapper      : SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>,
                                               SDTCisPtrTy<0>]>;

def Ceespucall         : SDNode<"CeespuISD::CALL", SDT_CeespuCall,
                             [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                              SDNPVariadic]>;
def Ceespuretflag      : SDNode<"CeespuISD::RET_FLAG", SDTNone,
                             [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def Ceespucallseq_start: SDNode<"ISD::CALLSEQ_START", SDT_CeespuCallSeqStart,
                             [SDNPHasChain, SDNPOutGlue]>;
def Ceespucallseq_end  : SDNode<"ISD::CALLSEQ_END",   SDT_CeespuCallSeqEnd,
                             [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
def Ceespubrcc         : SDNode<"CeespuISD::BR_CC", SDT_CeespuBrCC,
                             [SDNPHasChain, SDNPOutGlue, SDNPInGlue]>;

def Ceespuselectcc     : SDNode<"CeespuISD::SELECT_CC", SDT_CeespuSelectCC, [SDNPInGlue]>;
def CeespuWrapper      : SDNode<"CeespuISD::Wrapper", SDT_CeespuWrapper>;

def brtarget : Operand<OtherVT>;
def calltarget : Operand<i16>;

def u32imm   : Operand<i32> {
  let PrintMethod = "printImm32Operand";
}

def i32immSExt16 : PatLeaf<(imm),
                [{return isInt<16>(N->getSExtValue()); }]>;

// Addressing modes.
def ADDRri : ComplexPattern<i32, 2, "SelectAddr", [], []>;
def FIri : ComplexPattern<i32, 2, "SelectFIAddr", [add, or], []>;

// Address operands
def MEMri : Operand<i32> {
  let PrintMethod = "printMemOperand";
  let EncoderMethod = "getMemoryOpValue";
  let MIOperandInfo = (ops GPR, i16imm);
}

// Conditional code predicates - used for pattern matching for jump instructions
def Ceespu_CC_EQ  : PatLeaf<(imm),
                         [{return (N->getZExtValue() == ISD::SETEQ);}]>;
def Ceespu_CC_NE  : PatLeaf<(imm),
                         [{return (N->getZExtValue() == ISD::SETNE);}]>;
def Ceespu_CC_GE  : PatLeaf<(imm),
                         [{return (N->getZExtValue() == ISD::SETGE);}]>;
def Ceespu_CC_GT  : PatLeaf<(imm),
                         [{return (N->getZExtValue() == ISD::SETGT);}]>;
def Ceespu_CC_GTU : PatLeaf<(imm),
                         [{return (N->getZExtValue() == ISD::SETUGT);}]>;
def Ceespu_CC_GEU : PatLeaf<(imm),
                         [{return (N->getZExtValue() == ISD::SETUGE);}]>;

// jump instructions
class JMP_RR<bits<6> Opc, string OpcodeStr, PatLeaf Cond>
    : InstCeespu<(outs), (ins GPR:$dst, GPR:$src, brtarget:$BrDst),
              !strconcat(OpcodeStr, " $dst, $src, $BrDst"),
              [(Ceespubrcc i32:$dst, i32:$src, Cond, bb:$BrDst)]> {
  bits<6> op;
  bits<5> dst;
  bits<5> src;
  bits<16> BrDst;

  let Inst{31-26} = Opc;
  let Inst{25-21} = src;
  let Inst{20-16} = dst;
  let Inst{15-0} = BrDst;

  let op = Opc;
}
/*
class JMP_RI<bits<4> Opc, string OpcodeStr, PatLeaf Cond>
    : InstCeespu<(outs), (ins GPR:$dst, i64imm:$imm, brtarget:$BrDst),
              !strconcat(OpcodeStr, "i $dst, $imm goto $BrDst"),
              [(Ceespubrcc i64:$dst, i64immSExt32:$imm, Cond, bb:$BrDst)]> {
  bits<4> op;
  bits<1> CeespuSrc;
  bits<4> dst;
  bits<16> BrDst;
  bits<32> imm;

  let Inst{63-60} = op;
  let Inst{59} = CeespuSrc;
  let Inst{51-48} = dst;
  let Inst{47-32} = BrDst;
  let Inst{31-0} = imm;

  let op = Opc;
  let CeespuSrc = 0;
  let CeespuClass = 5; // Ceespu_JMP
}
*/
multiclass J<bits<6> Opc, string OpcodeStr, PatLeaf Cond> {
  def _rr : JMP_RR<Opc, OpcodeStr, Cond>;
  //def _ri : JMP_RI<Opc, OpcodeStr, Cond>;
}

let isBranch = 1, isTerminator = 1, hasDelaySlot=0 in {
// branch instructions
defm JEQ  : J<0x1, "beq",  Ceespu_CC_EQ>;
defm JUGT : J<0x2, "bgu", Ceespu_CC_GTU>;
defm JUGE : J<0x3, "bgeu", Ceespu_CC_GEU>;
defm JNE  : J<0x5, "bne",  Ceespu_CC_NE>;
defm JSGT : J<0x6, "bg", Ceespu_CC_GT>;
defm JSGE : J<0x7, "bge", Ceespu_CC_GE>;
}

// ALU instructions
class ALU_RI<bits<6> Opc, string OpcodeStr, SDNode OpNode>
    : InstCeespu<(outs GPR:$dst), (ins GPR:$src2, i32imm:$imm),
              !strconcat(OpcodeStr, "i $dst, $src2, $imm"),
              [(set GPR:$dst, (OpNode GPR:$src2, imm:$imm))]> {
  bits<6> Opc;
  bits<5> dst;
  bits<5> src;
  bits<16> imm;
  let Inst{31-26} = Opc;
  let Inst{25-21} = dst;
  let Inst{20-16} = src;
  let Inst{15-0}  = imm;
}

class ALU_RR<bits<6> Opc, string OpcodeStr, SDNode OpNode>
    : InstCeespu<(outs GPR:$dst), (ins GPR:$src2, GPR:$src),
              !strconcat(OpcodeStr, " $dst, $src, $src2"),
              [(set GPR:$dst, (OpNode i32:$src2, i32:$src))]> {
  bits<6> op;
  bits<5> dst;
  bits<5> src;
  bits<5> src2;

  let Inst{31-26} = op;
  let Inst{25-21} = dst;
  let Inst{20-16} = src;
  let Inst{15-11} = src2;

  let op = Opc;
}

multiclass ALU<bits<6> Opc, string OpcodeStr, SDNode OpNode> {
  def _rr : ALU_RR<Opc, OpcodeStr, OpNode>;
  def _ri : ALU_RI<Opc, OpcodeStr, OpNode>;
}

let isAsCheapAsAMove = 1 in {
  defm ADD : ALU<0x0, "add", add>;
  defm ADC : ALU<0x0, "adc", adde>;
  defm ADE : ALU<0x0, "add", addc>;
  defm SUB : ALU<0x1, "sub", sub>;
  defm SBB : ALU<0x1, "sub", subc>;
  defm SBE : ALU<0x1, "sbb", sube>;
  defm OR  : ALU<0x4, "or", or>;
  defm AND : ALU<0x5, "and", and>;
  defm SLL : ALU<0x6, "shl", shl>;
  defm SRL : ALU<0x7, "shr", srl>;
  defm XOR : ALU<0xa, "xor", xor>;
  defm SRA : ALU<0xc, "sar", sra>;
}
  defm MUL : ALU<0x2, "mul", mul>;

  
def NOPAL : InstAlias<"nop", (ADD_ri R0, R0, 0)>;
def NOT : InstAlias<"not $dst, $src", (XOR_ri GPR:$dst, GPR:$src, -1)>;
def SEXT8 : InstCeespu<(outs GPR:$dst), (ins GPR:$src),
              !strconcat("seb", " $dst, $src"),
              [(set GPR:$dst, (sext_inreg GPR:$src, i8))]>;
def SEXT16 : InstCeespu<(outs GPR:$dst), (ins GPR:$src),
              !strconcat("seh", " $dst, $src"),
              [(set GPR:$dst, (sext_inreg GPR:$src, i16))]>;


class MOV_RR<string OpcodeStr>
    : InstCeespu<(outs GPR:$dst), (ins GPR:$src),
              !strconcat(OpcodeStr, " $dst, $src, c0"),
              []> {
  bits<6> op;
  bits<5> dst;
  bits<5> src;

  let Inst{31-26} = 0x0;
  let Inst{25-21} = dst;
  let Inst{20-16} = src;

  let op = 0x0;     // Ceespu_MOV
}

class MOV_RI<string OpcodeStr>
    : InstCeespu<(outs GPR:$dst), (ins i32imm:$imm),
              !strconcat(OpcodeStr, " $dst, c0, $imm"),
              [(set GPR:$dst, (i32 imm:$imm))]> {
  bits<6> op;
  bits<5> dst;
  bits<16> imm;

  let Inst{31-26} = op;
  let Inst{25-21} = dst;
  let Inst{15-0} = imm;

  let op = 0x20; 
}

let isReMaterializable = 1, isAsCheapAsAMove = 1 in {
def MOV_rr : MOV_RR<"add">;
def MOV_ri : MOV_RI<"addi">;
}

def FI_ri
    : InstCeespu<(outs GPR:$dst), (ins MEMri:$addr),
               "lea $dst, $addr",
               [(set i32:$dst, FIri:$addr)]> {
  // This is a tentative instruction, and will be replaced
  // with MOV_rr and ADD_ri in PEI phase
}

/*
def LD_pseudo
    : InstCeespu<(outs GPR:$dst), (ins u32imm:$pseudo, u32imm:$imm),
              "ld_pseudo $dst, $pseudo, $imm",
              [(set GPR:$dst, (int_Ceespu_pseudo imm:$pseudo, imm:$imm))]> {

  bits<3> mode;
  bits<2> size;
  bits<4> dst;
  bits<64> imm;
  bits<4> pseudo;

  let Inst{63-61} = mode;
  let Inst{60-59} = size;
  let Inst{51-48} = dst;
  let Inst{55-52} = pseudo;
  let Inst{47-32} = 0;
  let Inst{31-0} = imm{31-0};

  let mode = 0;     // Ceespu_IMM
  let size = 3;     // Ceespu_DW
  let CeespuClass = 0; // Ceespu_LD
}
*/
// STORE instructions
class STORE<bits<6> op, string OpcodeStr, list<dag> Pattern>
    : InstCeespu<(outs), (ins GPR:$src, MEMri:$addr),
              !strconcat(OpcodeStr, " $src, $addr"), Pattern> {
  bits<6> op;
  bits<5> src;
  bits<21> addr;

  let Inst{31-26} = op;
  let Inst{25-21} = src;
  let Inst{20-0} = addr{20-0}; // base reg offset
}

class STOREi32<bits<6> Opc, string OpcodeStr, PatFrag OpNode>
    : STORE<Opc, OpcodeStr, [(OpNode i32:$src, ADDRri:$addr)]>;

def STH : STOREi32<0x1, "sh", truncstorei16>;
def STB : STOREi32<0x2, "sb", truncstorei8>;
def STW : STOREi32<0x3, "sw", store>;

// LOAD instructions
class LOAD<bits<6> op, string OpcodeStr, list<dag> Pattern>
    : InstCeespu<(outs GPR:$dst), (ins MEMri:$addr),
              !strconcat(OpcodeStr, " $dst, $addr"), Pattern> {
  bits<6> op;
  bits<5> dst;
  bits<21> addr;

  let Inst{31-26} = op;
  let Inst{25-21} = dst;
  let Inst{20-0} = addr{20-0}; // base reg offset
}

class LOADi32<bits<6> Opc, string OpcodeStr, PatFrag OpNode>
    : LOAD<Opc, OpcodeStr, [(set i32:$dst, (OpNode ADDRri:$addr))]>;

def LDW : LOADi32<0x0, "lw", load>;
def LDH : LOADi32<0x1, "lh", sextloadi16>;
def LDHU: LOADi32<0x4, "lhu",zextloadi16>;
def LDB : LOADi32<0x2, "lb", sextloadi8>;
def LDBU: LOADi32<0x3, "lbu",zextloadi8>;

class BRANCH<bits<6> Opc, string OpcodeStr, list<dag> Pattern>
    : InstCeespu<(outs), (ins brtarget:$BrDst),
              !strconcat(OpcodeStr, " $BrDst"), Pattern> {
  bits<6> op;
  bits<16> BrDst;

  let Inst{31-26} = op;
  let Inst{15-0}  = BrDst;

  let op = Opc;
}


class CALL<string OpcodeStr>
    : InstCeespu<(outs), (ins calltarget:$BrDst),
              !strconcat(OpcodeStr, " $BrDst"), []> {
  bits<16> BrDst;
  let Inst{31-26} = 0x3c;
  let Inst{15-0}  = BrDst;

}

class CALLR<string OpcodeStr>
    : InstCeespu<(outs), (ins GPR:$dst),
              !strconcat(OpcodeStr, " $dst"), [(Ceespucall GPR:$dst)]> {
  bits<5> dst;
  let Inst{31-26} = 0x3c;
  let Inst{20-16} = dst;

}

// Jump always
let isBranch = 1, isTerminator = 1, hasDelaySlot=0, isBarrier = 1 in {
  def JMP : BRANCH<0x3c, "b", [(br bb:$BrDst)]>;
}

// Jump and link
let isCall=1, hasDelaySlot=0, Defs = [LR, R20, R21, R22, R23, R24, R25, R13, R14, R15, R16, R26, R27, R28, R29, R30, R31] in {
  def JAL  : CALL<"call">;
  def JALR : CALLR<"call">;
}

class NOP_I<string OpcodeStr>
    : InstCeespu<(outs), (ins u32imm:$imm),
              !strconcat(OpcodeStr, " $imm"), []> {
  // mov r0, r0 == nop
  bits<6> op;
  
  bits<5> dst;
  bits<5> src;

  let Inst{31-26} = op;

  let op = 0x3c;     // Ceespu_MOV
  let src = 0;      // R0
  let dst = 0;      // R0
}

let hasSideEffects = 0 in {
  def NOP : NOP_I<"nop">;
}
class RET<string OpcodeStr>
    : InstCeespu<(outs), (ins),
              !strconcat(OpcodeStr, ""), [(Ceespuretflag)]> {
  bits<6> op;

  let Inst{31-26} = op;

  let op = 9;      
}

let isReturn = 1, isTerminator = 1, hasDelaySlot=0, isBarrier = 1, Uses = [LR], 
    isNotDuplicable = 1 in {
  def RET : RET<"bx clr">;
}

// ADJCALLSTACKDOWN/UP pseudo insns
let Defs = [SP], Uses = [SP, LR] in {
def ADJCALLSTACKDOWN : Pseudo<(outs), (ins u32imm:$amt),
                              "#ADJCALLSTACKDOWN $amt",
                              [(Ceespucallseq_start timm:$amt)]>;
def ADJCALLSTACKUP   : Pseudo<(outs), (ins u32imm:$amt1, u32imm:$amt2),
                              "#ADJCALLSTACKUP $amt1 $amt2",
                              [(Ceespucallseq_end timm:$amt1, timm:$amt2)]>;
}


let usesCustomInserter = 1 in {
  def Select : Pseudo<(outs GPR:$dst),
                      (ins GPR:$lhs, GPR:$rhs, i32imm:$imm, GPR:$src, GPR:$src2),
                      "# Select PSEUDO $dst = $lhs $imm $rhs ? $src : $src2",
                      [(set i32:$dst,
                       (Ceespuselectcc i32:$lhs, i32:$rhs, (i32 imm:$imm), i32:$src, i32:$src2))]>;
}

// load global addr into register
def : Pat<(CeespuWrapper tglobaladdr:$in), (MOV_ri tglobaladdr:$in)>;

// 
def : Pat<(i32 0), (i32 R0)>;
def : Pat<(not GPR:$dst),
 (XOR_ri GPR:$dst, -1)>;

// 0xffff doesn't fit into simm16, optimize common case
def : Pat<(i32 (and (i32 GPR:$src), 0xffff)),
          (SRL_ri (SLL_ri (i32 GPR:$src), 16), 16)>;

// Calls
def : Pat<(Ceespucall tglobaladdr:$dst), (JAL tglobaladdr:$dst)>;
def : Pat<(Ceespucall imm:$dst), (JAL imm:$dst)>;

// Loads
def : Pat<(extloadi8  tglobaladdr:$src), (i32 (LDB ADDRri:$src))>;
def : Pat<(extloadi16 tglobaladdr:$src), (i32 (LDH ADDRri:$src))>;
def : Pat<(extloadi32 tglobaladdr:$src), (i32 (LDW ADDRri:$src))>;

def : Pat<(extloadi8  ADDRri:$src), (i32 (LDB ADDRri:$src))>;
def : Pat<(extloadi16 ADDRri:$src), (i32 (LDH ADDRri:$src))>;
def : Pat<(extloadi32 ADDRri:$src), (i32 (LDW ADDRri:$src))>;


