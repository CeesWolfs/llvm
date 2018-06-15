//===-- CeespuELFObjectWriter.cpp - Ceespu ELF Writer -----------------------===//
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
}

CeespuELFObjectWriter::CeespuELFObjectWriter(uint8_t OSABI, bool Is64Bit)
    : MCELFObjectTargetWriter(Is64Bit, OSABI, ELF::EM_Ceespu,
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
    return ELF::R_Ceespu_32;
  case FK_Data_8:
    return ELF::R_Ceespu_64;
  case FK_Data_Add_1:
    return ELF::R_Ceespu_ADD8;
  case FK_Data_Add_2:
    return ELF::R_Ceespu_ADD16;
  case FK_Data_Add_4:
    return ELF::R_Ceespu_ADD32;
  case FK_Data_Add_8:
    return ELF::R_Ceespu_ADD64;
  case FK_Data_Sub_1:
    return ELF::R_Ceespu_SUB8;
  case FK_Data_Sub_2:
    return ELF::R_Ceespu_SUB16;
  case FK_Data_Sub_4:
    return ELF::R_Ceespu_SUB32;
  case FK_Data_Sub_8:
    return ELF::R_Ceespu_SUB64;
  case Ceespu::fixup_ceespu_hi20:
    return ELF::R_Ceespu_HI20;
  case Ceespu::fixup_ceespu_lo12_i:
    return ELF::R_Ceespu_LO12_I;
  case Ceespu::fixup_ceespu_lo12_s:
    return ELF::R_Ceespu_LO12_S;
  case Ceespu::fixup_ceespu_pcrel_hi20:
    return ELF::R_Ceespu_PCREL_HI20;
  case Ceespu::fixup_ceespu_pcrel_lo12_i:
    return ELF::R_Ceespu_PCREL_LO12_I;
  case Ceespu::fixup_ceespu_pcrel_lo12_s:
    return ELF::R_Ceespu_PCREL_LO12_S;
  case Ceespu::fixup_ceespu_jal:
    return ELF::R_Ceespu_JAL;
  case Ceespu::fixup_ceespu_branch:
    return ELF::R_Ceespu_BRANCH;
  case Ceespu::fixup_ceespu_rvc_jump:
    return ELF::R_Ceespu_RVC_JUMP;
  case Ceespu::fixup_ceespu_rvc_branch:
    return ELF::R_Ceespu_RVC_BRANCH;
  case Ceespu::fixup_ceespu_call:
    return ELF::R_Ceespu_CALL;
  case Ceespu::fixup_ceespu_relax:
    return ELF::R_Ceespu_RELAX;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createCeespuELFObjectWriter(uint8_t OSABI, bool Is64Bit) {
  return llvm::make_unique<CeespuELFObjectWriter>(OSABI, Is64Bit);
}
