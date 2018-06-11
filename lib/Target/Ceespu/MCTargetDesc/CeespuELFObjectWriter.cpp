//===-- CeespuELFObjectWriter.cpp - Ceespu ELF Writer ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/CeespuMCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class CeespuELFObjectWriter : public MCELFObjectTargetWriter {
public:
  CeespuELFObjectWriter(uint8_t OSABI);

  ~CeespuELFObjectWriter() override;

protected:
  unsigned GetRelocType(const MCValue &Target, const MCFixup &Fixup,
                        bool IsPCRel) const override;
};
}

CeespuELFObjectWriter::CeespuELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(/*Is64Bit*/ true, OSABI, ELF::EM_NONE,
                              /*HasRelocationAddend*/ false) {}

CeespuELFObjectWriter::~CeespuELFObjectWriter() {}

unsigned CeespuELFObjectWriter::GetRelocType(const MCValue &Target,
                                          const MCFixup &Fixup,
                                          bool IsPCRel) const {
  // determine the type of the relocation
  switch ((unsigned)Fixup.getKind()) {
  default:
    llvm_unreachable("invalid fixup kind!");
  case FK_SecRel_8:
    return ELF::R_X86_64_64;
  case FK_SecRel_4:
    return ELF::R_X86_64_PC32;
  case FK_Data_8:
    return IsPCRel ? ELF::R_X86_64_PC64 : ELF::R_X86_64_64;
  case FK_Data_4:
    return IsPCRel ? ELF::R_X86_64_PC32 : ELF::R_X86_64_32;
  }
}

MCObjectWriter *llvm::createCeespuELFObjectWriter(raw_pwrite_stream &OS,
                                               uint8_t OSABI, bool IsLittleEndian) {
  MCELFObjectTargetWriter *MOTW = new CeespuELFObjectWriter(OSABI);
  return createELFObjectWriter(MOTW, OS, IsLittleEndian);
}
