//===-- CeespuTargetStreamer.cpp - Ceespu Target Streamer Methods -----------===//
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

#include "CeespuTargetStreamer.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

CeespuTargetStreamer::CeespuTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

// This part is for ascii assembly output
CeespuTargetAsmStreamer::CeespuTargetAsmStreamer(MCStreamer &S,
                                               formatted_raw_ostream &OS)
    : CeespuTargetStreamer(S), OS(OS) {}

void CeespuTargetAsmStreamer::emitDirectiveOptionRVC() {
  OS << "\t.option\trvc\n";
}

void CeespuTargetAsmStreamer::emitDirectiveOptionNoRVC() {
  OS << "\t.option\tnorvc\n";
}
