//===-- CeespuInstPrinter.cpp - Convert Ceespu MCInst to asm syntax
//---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints an Ceespu MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "CeespuInstPrinter.h"
#include "MCTargetDesc/CeespuBaseInfo.h"
#include "MCTargetDesc/CeespuMCExpr.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

// Include the auto-generated portion of the assembly writer.
#define PRINT_ALIAS_INSTR
#include "CeespuGenAsmWriter.inc"

// Include the auto-generated portion of the compress emitter.
//#define GEN_UNCOMPRESS_INSTR
//#include "CeespuGenCompressInstEmitter.inc"

static cl::opt<bool> NoAliases(
    "ceespu-no-aliases",
    cl::desc("Disable the emission of assembler pseudo instructions"),
    cl::init(false), cl::Hidden);

void CeespuInstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                                  StringRef Annot, const MCSubtargetInfo &STI) {
  printInstruction(MI, O);
  printAnnotation(O, Annot);
}

void CeespuInstPrinter::printRegName(raw_ostream &O, unsigned RegNo) const {
  O << getRegisterName(RegNo);
}

void CeespuInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                     raw_ostream &O, const char *Modifier) {
  assert((Modifier == 0 || Modifier[0] == 0) && "No modifiers supported");
  const MCOperand &MO = MI->getOperand(OpNo);

  if (MO.isReg()) {
    printRegName(O, MO.getReg());
    return;
  }

  if (MO.isImm()) {
    O << MO.getImm();
    return;
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(O, &MAI);
}

void CeespuInstPrinter::printMemOperand(const MCInst *MI, int OpNo,
                                        raw_ostream &O, const char *Modifier) {
  const MCOperand &RegOp = MI->getOperand(OpNo);
  const MCOperand &OffsetOp = MI->getOperand(OpNo + 1);
  // offset
  if (OffsetOp.isImm())
    O << formatDec(OffsetOp.getImm());
  else
    assert(0 && "Expected an immediate");

  // register
  assert(RegOp.isReg() && "Register operand not a register");
  O << '(' << getRegisterName(RegOp.getReg()) << ')';
}
