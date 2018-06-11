//===-- CeespuInstrFormats.td - Ceespu Instruction Formats -------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

class InstCeespu<dag outs, dag ins, string asmstr, list<dag> pattern>
    : Instruction {
  field bits<32> Inst;
  let Namespace = "Ceespu";
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString   = asmstr;
  let Pattern = pattern;
  let Size = 4;
}

class Pseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
    : InstCeespu<outs, ins, asmstr, pattern> {
  let isPseudo = 1;
}
