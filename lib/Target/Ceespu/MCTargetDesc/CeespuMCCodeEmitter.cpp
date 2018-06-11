//===-- CeespuMCCodeEmitter.cpp - Convert Ceespu code to machine code -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the CeespuMCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/CeespuMCTargetDesc.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

namespace {
class CeespuMCCodeEmitter : public MCCodeEmitter {
  CeespuMCCodeEmitter(const CeespuMCCodeEmitter &) = delete;
  void operator=(const CeespuMCCodeEmitter &) = delete;
  const MCRegisterInfo &MRI;
  bool IsLittleEndian;

public:
  CeespuMCCodeEmitter(const MCRegisterInfo &mri, bool IsLittleEndian)
    : MRI(mri), IsLittleEndian(IsLittleEndian) {}

  ~CeespuMCCodeEmitter() {}

  // getBinaryCodeForInstr - TableGen'erated function for getting the
  // binary encoding for an instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  // getMachineOpValue - Return binary encoding of operand. If the machin
  // operand requires relocation, record the relocation and return zero.
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  uint64_t getMemoryOpValue(const MCInst &MI, unsigned Op,
                            SmallVectorImpl<MCFixup> &Fixups,
                            const MCSubtargetInfo &STI) const;

  void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;
};
}

MCCodeEmitter *llvm::createCeespuMCCodeEmitter(const MCInstrInfo &MCII,
                                            const MCRegisterInfo &MRI,
                                            MCContext &Ctx) {
  return new CeespuMCCodeEmitter(MRI, true);
}

MCCodeEmitter *llvm::createCeespubeMCCodeEmitter(const MCInstrInfo &MCII,
                                              const MCRegisterInfo &MRI,
                                              MCContext &Ctx) {
  return new CeespuMCCodeEmitter(MRI, false);
}

unsigned CeespuMCCodeEmitter::getMachineOpValue(const MCInst &MI,
                                             const MCOperand &MO,
                                             SmallVectorImpl<MCFixup> &Fixups,
                                             const MCSubtargetInfo &STI) const {
  if (MO.isReg())
    return MRI.getEncodingValue(MO.getReg());
  if (MO.isImm())
    return static_cast<unsigned>(MO.getImm());

  assert(MO.isExpr());

  const MCExpr *Expr = MO.getExpr();

  assert(Expr->getKind() == MCExpr::SymbolRef);

  if (MI.getOpcode() == Ceespu::JAL)
    // func call name
    Fixups.push_back(MCFixup::create(0, Expr, FK_SecRel_4));
  else
    // bb label
    Fixups.push_back(MCFixup::create(0, Expr, FK_PCRel_2));

  return 0;
}

static uint8_t SwapBits(uint8_t Val)
{
  return (Val & 0x0F) << 4 | (Val & 0xF0) >> 4;
}

void CeespuMCCodeEmitter::encodeInstruction(const MCInst &MI, raw_ostream &OS,
                                         SmallVectorImpl<MCFixup> &Fixups,
                                         const MCSubtargetInfo &STI) const {
  unsigned Opcode = MI.getOpcode();
  support::endian::Writer<support::little> LE(OS);
  support::endian::Writer<support::big> BE(OS);

  if (0) {
    uint64_t Value = getBinaryCodeForInstr(MI, Fixups, STI);
    LE.write<uint8_t>(Value >> 56);
    if (IsLittleEndian)
      LE.write<uint8_t>((Value >> 48) & 0xff);
    else
      LE.write<uint8_t>(SwapBits((Value >> 48) & 0xff));
    LE.write<uint16_t>(0);
    if (IsLittleEndian)
      LE.write<uint32_t>(Value & 0xffffFFFF);
    else
      BE.write<uint32_t>(Value & 0xffffFFFF);

    const MCOperand &MO = MI.getOperand(1);
    uint64_t Imm = MO.isImm() ? MO.getImm() : 0;
    LE.write<uint8_t>(0);
    LE.write<uint8_t>(0);
    LE.write<uint16_t>(0);
    if (IsLittleEndian)
      LE.write<uint32_t>(Imm >> 32);
    else
      BE.write<uint32_t>(Imm >> 32);
  } else {
    // Get instruction encoding and emit it
    uint64_t Value = getBinaryCodeForInstr(MI, Fixups, STI);
    LE.write<uint8_t>(Value >> 56);
    if (IsLittleEndian) {
      LE.write<uint8_t>((Value >> 48) & 0xff);
      LE.write<uint16_t>((Value >> 32) & 0xffff);
      LE.write<uint32_t>(Value & 0xffffFFFF);
    } else {
      LE.write<uint8_t>(SwapBits((Value >> 48) & 0xff));
      BE.write<uint16_t>((Value >> 32) & 0xffff);
      BE.write<uint32_t>(Value & 0xffffFFFF);
    }
  }
}

// Encode Ceespu Memory Operand
uint64_t CeespuMCCodeEmitter::getMemoryOpValue(const MCInst &MI, unsigned Op,
                                            SmallVectorImpl<MCFixup> &Fixups,
                                            const MCSubtargetInfo &STI) const {
  uint64_t Encoding;
  const MCOperand Op1 = MI.getOperand(1);
  assert(Op1.isReg() && "First operand is not register.");
  Encoding = MRI.getEncodingValue(Op1.getReg());
  Encoding <<= 16;
  MCOperand Op2 = MI.getOperand(2);
  assert(Op2.isImm() && "Second operand is not immediate.");
  Encoding |= Op2.getImm() & 0xffff;
  return Encoding;
}

#include "CeespuGenMCCodeEmitter.inc"
