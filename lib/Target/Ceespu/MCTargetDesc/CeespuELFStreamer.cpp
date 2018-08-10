//===-- CeespuELFStreamer.cpp - Ceespu ELF Target Streamer Methods ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Ceespu specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "CeespuELFStreamer.h"
#include "CeespuMCTargetDesc.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCSubtargetInfo.h"

using namespace llvm;

// This part is for ELF object output.
CeespuTargetELFStreamer::CeespuTargetELFStreamer(MCStreamer &S,
                                               const MCSubtargetInfo &STI)
    : CeespuTargetStreamer(S) {
  MCAssembler &MCA = getStreamer().getAssembler();

  const FeatureBitset &Features = STI.getFeatureBits();

  unsigned EFlags = MCA.getELFHeaderEFlags();

  if (Features[Ceespu::FeatureStdExtC])
    EFlags |= ELF::EF_Ceespu_RVC;

  MCA.setELFHeaderEFlags(EFlags);
}

MCELFStreamer &CeespuTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}

void CeespuTargetELFStreamer::emitDirectiveOptionRVC() {}
void CeespuTargetELFStreamer::emitDirectiveOptionNoRVC() {}
