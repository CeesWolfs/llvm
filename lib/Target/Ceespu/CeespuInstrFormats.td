//===-- CeespuInstrFormats.td - Ceespu Instruction Formats -------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

// Opcodes as stated in the assembler
class CeespuOpcode<bits<6> val> {
  bits<6> Value = val;
}
def OPC_ADD     : CeespuOpcode<0b000000>;
def OPC_ADDI    : CeespuOpcode<0b010000>;
def OPC_ADC     : CeespuOpcode<0b000001>;
def OPC_ADCI    : CeespuOpcode<0b010001>;
def OPC_SUB     : CeespuOpcode<0b000010>;
def OPC_SUBI    : CeespuOpcode<0b010010>;
def OPC_SBB     : CeespuOpcode<0b000011>;
def OPC_SBBI    : CeespuOpcode<0b010011>;
def OPC_OR      : CeespuOpcode<0b000100>;
def OPC_ORI     : CeespuOpcode<0b010100>;
def OPC_AND     : CeespuOpcode<0b000101>;
def OPC_ANDI    : CeespuOpcode<0b010101>;
def OPC_XOR     : CeespuOpcode<0b000110>;
def OPC_XORI    : CeespuOpcode<0b010110>;
def OPC_SE      : CeespuOpcode<0b000111>;
def OPC_SHF     : CeespuOpcode<0b001000>;
def OPC_SHFI    : CeespuOpcode<0b011000>;
def OPC_MUL     : CeespuOpcode<0b001001>;
def OPC_MULI    : CeespuOpcode<0b011001>;
def OPC_SETHI   : CeespuOpcode<0b101010>;
def OPC_LW      : CeespuOpcode<0b100000>;
def OPC_LH      : CeespuOpcode<0b100001>;
def OPC_LHU     : CeespuOpcode<0b100011>;
def OPC_LB      : CeespuOpcode<0b100010>;
def OPC_LBU     : CeespuOpcode<0b100100>;
def OPC_SW      : CeespuOpcode<0b110100>;
def OPC_SH      : CeespuOpcode<0b110101>;
def OPC_SB      : CeespuOpcode<0b110110>;
def OPC_BEQ     : CeespuOpcode<0b111000>;
def OPC_BNE     : CeespuOpcode<0b111001>;
def OPC_BGU     : CeespuOpcode<0b111010>;
def OPC_BGEU    : CeespuOpcode<0b111011>;
def OPC_BG      : CeespuOpcode<0b111100>;
def OPC_BGE     : CeespuOpcode<0b111101>;
def OPC_BC      : CeespuOpcode<0b111110>;
def OPC_JMP     : CeespuOpcode<0b111111>;

class InstCeespu<CeespuOpcode opc, dag outs, dag ins, string opcstr, string argstr,  list<dag> pattern>
    : Instruction {
  field bits<32> Inst;
  let Namespace = "Ceespu";
  let Inst{31-26} = opc.Value;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString   = opcstr # "\t" # argstr;
  let Pattern = pattern;
  let Size = 4;
}

// Type A0 instruction (opcode rd, ra, rb)
class CeespuA0<CeespuOpcode opc, dag outs, dag ins, string opcstr, string argstr, list<dag> pattern>
: InstCeespu<opc, outs, ins, opcstr, argstr, pattern>
{
  bits<5> rd;
  bits<5> ra;
  bits<5> rb;

  let Inst{25-21} = rd;
  let Inst{20-16} = ra;
  let Inst{15-11} = rb;
}

// Type A1 instruction (opcode rd, ra)
class CeespuA1<CeespuOpcode opc, dag outs, dag ins, string opcstr, string argstr, list<dag> pattern>
: InstCeespu<opc, outs, ins, opcstr, argstr, pattern>
{
  bits<5> rd;
  bits<5> ra;

  let Inst{25-21} = rd;
  let Inst{20-16} = ra;
  
}

// Type A2 instruction (opcode ra)
class CeespuA2<CeespuOpcode opc, dag outs, dag ins, string opcstr, string argstr, list<dag> pattern>
: InstCeespu<opc, outs, ins, opcstr, argstr, pattern>
{
  bits<5> ra;

  let Inst{20-16} = ra; 
}

// Type B0 instruction (opcode rd, ra, imm)
class CeespuB0<CeespuOpcode opc, dag outs, dag ins, string opcstr, string argstr, list<dag> pattern>
: InstCeespu<opc, outs, ins, opcstr, argstr, pattern>
{
  bits<5> rd;
  bits<5> ra;
  bits<16> imm;

  let Inst{25-21} = rd;
  let Inst{20-16} = ra;
  let Inst{15-0}  = imm;
}

// Type B1 instruction (opcode ra, rb, imm)
class CeespuB1<CeespuOpcode opc, dag outs, dag ins, string opcstr, string argstr, list<dag> pattern>
: InstCeespu<opc, outs, ins, opcstr, argstr, pattern>
{
  bits<5> ra;
  bits<5> rb;
  bits<16> imm;

  let Inst{25-21} = imm{15-11};
  let Inst{20-16} = ra;
  let Inst{15-11} = rb;
  let Inst{10-0}  = imm{10-0};
}

// Type B2 instruction (opcode imm)
class CeespuB2<CeespuOpcode opc, dag outs, dag ins, string opcstr, string argstr, list<dag> pattern>
: InstCeespu<opc, outs, ins, opcstr, argstr, pattern>
{
  bits<16> imm;

  let Inst{15-0}  = imm{15-0};
}

class Pseudo<dag outs, dag ins, string opcstr="", string argstr="", list<dag> pattern> : Instruction {
  let isPseudo = 1;
}
