//===-- CeespuELFStreamer.h - Ceespu ELF Target Streamer ---------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_CeespuELFSTREAMER_H
#define LLVM_LIB_TARGET_Ceespu_CeespuELFSTREAMER_H

#include "CeespuTargetStreamer.h"
#include "llvm/MC/MCELFStreamer.h"

namespace llvm {

class CeespuTargetELFStreamer : public CeespuTargetStreamer {
public:
  MCELFStreamer &getStreamer();
  CeespuTargetELFStreamer(MCStreamer &S, const MCSubtargetInfo &STI);

  virtual void emitDirectiveOptionRVC();
  virtual void emitDirectiveOptionNoRVC();
};
}
#endif
