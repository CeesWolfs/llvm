//===-- CeespuTargetStreamer.h - Ceespu Target Streamer ----------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_CeespuTARGETSTREAMER_H
#define LLVM_LIB_TARGET_Ceespu_CeespuTARGETSTREAMER_H

#include "llvm/MC/MCStreamer.h"

namespace llvm {

class CeespuTargetStreamer : public MCTargetStreamer {
public:
  CeespuTargetStreamer(MCStreamer &S);

  virtual void emitDirectiveOptionRVC() = 0;
  virtual void emitDirectiveOptionNoRVC() = 0;
};

// This part is for ascii assembly output
class CeespuTargetAsmStreamer : public CeespuTargetStreamer {
  formatted_raw_ostream &OS;

public:
  CeespuTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);

  void emitDirectiveOptionRVC() override;
  void emitDirectiveOptionNoRVC() override;
};

}
#endif
