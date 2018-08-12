//===-- CeespuELFObjectWriter.cpp - Ceespu ELF Writer
//-----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/CeespuFixupKinds.h"
#include "MCTargetDesc/CeespuMCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class CeespuELFObjectWriter : public MCELFObjectTargetWriter {
 public:
  CeespuELFObjectWriter(uint8_t OSABI, bool Is64Bit);

  ~CeespuELFObjectWriter() override;

  // Return true if the given relocation must be with a symbol rather than
  // section plus offset.
  bool needsRelocateWithSymbol(const MCSymbol &Sym,
                               unsigned Type) const override {
    // TODO: this is very conservative, update once RISC-V psABI requirements
    //       are clarified.
    return true;
  }

 protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};
}  // namespace

CeespuELFObjectWriter::CeespuELFObjectWriter(uint8_t OSABI, bool Is64Bit)
    : MCELFObjectTargetWriter(Is64Bit, OSABI, ELF::EM_CEESPU,
                              /*HasRelocationAddend*/ true) {}

CeespuELFObjectWriter::~CeespuELFObjectWriter() {}

unsigned CeespuELFObjectWriter::getRelocType(MCContext &Ctx,
                                             const MCValue &Target,
                                             const MCFixup &Fixup,
                                             bool IsPCRel) const {
  // Determine the type of the relocation
  switch ((unsigned)Fixup.getKind()) {
    default:
      llvm_unreachable("invalid fixup kind!");
    case FK_Data_4:
      return ELF::R_CEESPU_NONE;
    case FK_Data_8:
      return ELF::R_CEESPU_NONE;
    case FK_Data_Add_1:
      return ELF::R_CEESPU_NONE;
    case FK_Data_Add_2:
      return ELF::R_CEESPU_NONE;
    case FK_Data_Add_4:
      return ELF::R_CEESPU_NONE;
    case FK_Data_Add_8:
      return ELF::R_CEESPU_NONE;
    case FK_Data_Sub_1:
      return ELF::R_CEESPU_NONE;
    case FK_Data_Sub_2:
      return ELF::R_CEESPU_NONE;
    case FK_Data_Sub_4:
      return ELF::R_CEESPU_NONE;
    case FK_Data_Sub_8:
      return ELF::R_CEESPU_NONE;
    case Ceespu::fixup_ceespu_lo22:
      return ELF::R_CEESPU_LO_22;
    case Ceespu::fixup_ceespu_lo12:
      return ELF::R_CEESPU_LO_12;
    case Ceespu::fixup_ceespu_lo16:
      return ELF::R_CEESPU_LO_16;
    case Ceespu::fixup_ceespu_hi16:
      return ELF::R_CEESPU_HI_16;
    case Ceespu::fixup_ceespu_cbranch:
      return ELF::R_CEESPU_RJMP;
  }
}

std::unique_ptr<MCObjectTargetWriter> llvm::createCeespuELFObjectWriter(
    uint8_t OSABI, bool Is64Bit) {
  return llvm::make_unique<CeespuELFObjectWriter>(OSABI, false);
}
