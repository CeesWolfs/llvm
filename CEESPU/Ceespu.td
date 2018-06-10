//===-- Ceespu.td - Describe the Ceespu Target Machine -----------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

include "CeespuRegisterInfo.td"
include "CeespuCallingConv.td"
include "CeespuInstrInfo.td"

def CeespuInstrInfo : InstrInfo;

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"generic", []>;

def CeespuInstPrinter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  bit isMCAsmWriter = 1;
}

def CeespuAsmParserVariant : AsmParserVariant {
  int Variant = 0;
  string Name = "Ceespu";
  string BreakCharacters = "";
}

def Ceespu : Target {
  let InstructionSet = CeespuInstrInfo;
  let AssemblyWriters = [CeespuInstPrinter];
  let AssemblyParserVariants = [CeespuAsmParserVariant];
}
