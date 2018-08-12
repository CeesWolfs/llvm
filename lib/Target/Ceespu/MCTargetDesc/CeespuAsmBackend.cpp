//===-- CeespuAsmBackend.cpp - Ceespu Assembler Backend
//---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/CeespuFixupKinds.h"
#include "MCTargetDesc/CeespuMCTargetDesc.h"
#include "llvm/ADT/APInt.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
class CeespuAsmBackend : public MCAsmBackend {
  const MCSubtargetInfo &STI;
  uint8_t OSABI;
  bool Is64Bit;

 public:
  CeespuAsmBackend(const MCSubtargetInfo &STI, uint8_t OSABI, bool Is64Bit)
      : MCAsmBackend(support::little), STI(STI), OSABI(OSABI), Is64Bit(false) {}
  ~CeespuAsmBackend() override {}

  // Generate diff expression relocations if the relax feature is enabled,
  // otherwise it is safe for the assembler to calculate these internally.
  bool requiresDiffExpressionRelocations() const override { return false; }
  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved,
                  const MCSubtargetInfo *STI) const override;

  std::unique_ptr<MCObjectTargetWriter> createObjectTargetWriter()
      const override;

  // If linker relaxation is enabled, always emit relocations even if the fixup
  // can be resolved. This is necessary for correctness as offsets may change
  // during relaxation.
  bool shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                             const MCValue &Target) override {
    return false;
  }

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override {
    llvm_unreachable("Handled by fixupNeedsRelaxationAdvanced");
  }

  bool fixupNeedsRelaxationAdvanced(const MCFixup &Fixup, bool Resolved,
                                    uint64_t Value,
                                    const MCRelaxableFragment *DF,
                                    const MCAsmLayout &Layout,
                                    const bool WasForced) const override;

  unsigned getNumFixupKinds() const override {
    return Ceespu::NumTargetFixupKinds;
  }

  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override {
    const static MCFixupKindInfo Infos[] = {
        // This table *must* be in the order that the fixup_* kinds are defined
        // in
        // CeespuFixupKinds.h.
        //
        // name                      offset bits  flags
        {"fixup_ceespu_hi16", 0, 16, 0},
        {"fixup_ceespu_lo16", 0, 16, 0},
        {"fixup_ceespu_lo12", 0, 32, 0},
        {"fixup_ceespu_cbranch", 0, 32, MCFixupKindInfo::FKF_IsPCRel},
        {"fixup_ceespu_lo22", 2, 22, MCFixupKindInfo::FKF_IsPCRel}};
    static_assert((array_lengthof(Infos)) == Ceespu::NumTargetFixupKinds,
                  "Not all fixup kinds added to Infos array");

    if (Kind < FirstTargetFixupKind)
      return MCAsmBackend::getFixupKindInfo(Kind);

    assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
           "Invalid kind!");
    return Infos[Kind - FirstTargetFixupKind];
  }

  bool mayNeedRelaxation(const MCInst &Inst,
                         const MCSubtargetInfo &STI) const override;
  unsigned getRelaxedOpcode(unsigned Op) const;

  void relaxInstruction(const MCInst &Inst, const MCSubtargetInfo &STI,
                        MCInst &Res) const override;

  bool writeNopData(raw_ostream &OS, uint64_t Count) const override;
};

bool CeespuAsmBackend::fixupNeedsRelaxationAdvanced(
    const MCFixup &Fixup, bool Resolved, uint64_t Value,
    const MCRelaxableFragment *DF, const MCAsmLayout &Layout,
    const bool WasForced) const {
  // Return true if the symbol is actually unresolved.
  // Resolved could be always false when shouldForceRelocation return true.
  // We use !WasForced to indicate that the symbol is unresolved and not forced
  // by shouldForceRelocation.
  if (!Resolved && !WasForced) return true;

  int64_t Offset = int64_t(Value);
  switch ((unsigned)Fixup.getKind()) {
    default:
      return false;
    case Ceespu::fixup_ceespu_cbranch:
      // For compressed jump instructions the immediate must be
      // in the range [-2048, 2046].
      return Offset > 32767 || Offset < -32768;
  }
}

void CeespuAsmBackend::relaxInstruction(const MCInst &Inst,
                                        const MCSubtargetInfo &STI,
                                        MCInst &Res) const {
  /* TODO: replace this with call to auto generated uncompressinstr() function.
  switch (Inst.getOpcode()) {
    default:
      llvm_unreachable("Opcode not expected!");
    case Ceespu::C_BEQZ:
      // c.beqz $rs1, $imm -> beq $rs1, X0, $imm.
      Res.setOpcode(Ceespu::BEQ);
      Res.addOperand(Inst.getOperand(0));
      Res.addOperand(MCOperand::createReg(Ceespu::X0));
      Res.addOperand(Inst.getOperand(1));
      break;
    case Ceespu::C_BNEZ:
      // c.bnez $rs1, $imm -> bne $rs1, X0, $imm.
      Res.setOpcode(Ceespu::BNE);
      Res.addOperand(Inst.getOperand(0));
      Res.addOperand(MCOperand::createReg(Ceespu::X0));
      Res.addOperand(Inst.getOperand(1));
      break;
    case Ceespu::C_J:
      // c.j $imm -> jal X0, $imm.
      Res.setOpcode(Ceespu::JAL);
      Res.addOperand(MCOperand::createReg(Ceespu::X0));
      Res.addOperand(Inst.getOperand(0));
      break;
    case Ceespu::C_JAL:
      // c.jal $imm -> jal X1, $imm.
      Res.setOpcode(Ceespu::JAL);
      Res.addOperand(MCOperand::createReg(Ceespu::X1));
      Res.addOperand(Inst.getOperand(0));
      break;
  } */
}

// Given a compressed control flow instruction this function returns
// the expanded instruction.
unsigned CeespuAsmBackend::getRelaxedOpcode(unsigned Op) const {
  /*  switch (Op) {
    default:
      return Op;
    case Ceespu::C_BEQZ:
      return Ceespu::BEQ;
    case Ceespu::C_BNEZ:
      return Ceespu::BNE;
    case Ceespu::C_J:
    case Ceespu::C_JAL: // fall through.
      return Ceespu::JAL;
    }*/
}

bool CeespuAsmBackend::mayNeedRelaxation(const MCInst &Inst,
                                         const MCSubtargetInfo &STI) const {
  return getRelaxedOpcode(Inst.getOpcode()) != Inst.getOpcode();
}

bool CeespuAsmBackend::writeNopData(raw_ostream &OS, uint64_t Count) const {
  bool HasStdExtC = false;  // STI.getFeatureBits()[Ceespu::FeatureStdExtC];
  unsigned MinNopLen = HasStdExtC ? 2 : 4;

  if ((Count % MinNopLen) != 0) return false;

  // The canonical nop on RISC-V is addi x0, x0, 0.
  uint64_t Nop32Count = Count / 4;
  for (uint64_t i = Nop32Count; i != 0; --i) OS.write("\x13\0\0\0", 4);

  // The canonical nop on RVC is c.nop.
  if (HasStdExtC) {
    uint64_t Nop16Count = (Count - Nop32Count * 4) / 2;
    for (uint64_t i = Nop16Count; i != 0; --i) OS.write("\x01\0", 2);
  }

  return true;
}

static uint64_t adjustFixupValue(const MCFixup &Fixup, uint64_t Value,
                                 MCContext &Ctx) {
  unsigned Kind = Fixup.getKind();
  switch (Kind) {
    default:
      llvm_unreachable("Unknown fixup kind!");
    case FK_Data_1:
    case FK_Data_2:
    case FK_Data_4:
    case FK_Data_8:
      return Value;
    case Ceespu::fixup_ceespu_hi16:
      return (Value >> 16) & 0xffff;
    case Ceespu::fixup_ceespu_lo16:
      return Value & 0xffff;
    case Ceespu::fixup_ceespu_lo12:
      return (Value & 0x7ff) | ((Value & 0xF800) << 10);
    case Ceespu::fixup_ceespu_lo22:
      return (Value & 0x1fffffc);
    case Ceespu::fixup_ceespu_cbranch: {
      if (!isInt<16>(Value))
        Ctx.reportError(Fixup.getLoc(), "fixup value out of range");
      if (Value & 0x3)
        Ctx.reportError(Fixup.getLoc(), "fixup value must be 4-byte aligned");
      return (Value & 0x7ff) | ((Value & 0xF800) << 10);
    }
  }
}

void CeespuAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                  const MCValue &Target,
                                  MutableArrayRef<char> Data, uint64_t Value,
                                  bool IsResolved,
                                  const MCSubtargetInfo *STI) const {
  MCContext &Ctx = Asm.getContext();
  MCFixupKindInfo Info = getFixupKindInfo(Fixup.getKind());
  if (!Value) return;  // Doesn't change encoding.
  // Apply any target-specific value adjustments.
  Value = adjustFixupValue(Fixup, Value, Ctx);

  // Shift the value into position.
  Value <<= Info.TargetOffset;

  unsigned Offset = Fixup.getOffset();
  unsigned NumBytes = alignTo(Info.TargetSize + Info.TargetOffset, 8) / 8;

  assert(Offset + NumBytes <= Data.size() && "Invalid fixup offset!");

  // For each byte of the fragment that the fixup touches, mask in the
  // bits from the fixup value.
  for (unsigned i = 0; i != NumBytes; ++i) {
    Data[Offset + i] |= uint8_t((Value >> (i * 8)) & 0xff);
  }
}

std::unique_ptr<MCObjectTargetWriter>
CeespuAsmBackend::createObjectTargetWriter() const {
  return createCeespuELFObjectWriter(OSABI, Is64Bit);
}

}  // end anonymous namespace

MCAsmBackend *llvm::createCeespuAsmBackend(const Target &T,
                                           const MCSubtargetInfo &STI,
                                           const MCRegisterInfo &MRI,
                                           const MCTargetOptions &Options) {
  const Triple &TT = STI.getTargetTriple();
  uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(TT.getOS());
  return new CeespuAsmBackend(STI, OSABI, TT.isArch64Bit());
}
