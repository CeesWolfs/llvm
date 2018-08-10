//===-- CeespuMCTargetDesc.h - Ceespu Target Descriptions ---------*- C++
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Ceespu specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuMCTARGETDESC_H
#define LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuMCTARGETDESC_H

#include <memory>
#include "llvm/Config/config.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Support/DataTypes.h"

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class StringRef;
class Target;
class Triple;
class raw_ostream;
class raw_pwrite_stream;

Target &getTheCeespuTarget();
Target &getTheCeespuebTarget();

MCCodeEmitter *createCeespuMCCodeEmitter(const MCInstrInfo &MCII,
                                         const MCRegisterInfo &MRI,
                                         MCContext &Ctx);

MCAsmBackend *createCeespuAsmBackend(const Target &T,
                                     const MCSubtargetInfo &STI,
                                     const MCRegisterInfo &MRI,
                                     const MCTargetOptions &Options);

std::unique_ptr<MCObjectTargetWriter> createCeespuELFObjectWriter(uint8_t OSABI,
                                                                  bool Is64Bit);
}  // namespace llvm

// Defines symbolic names for Ceespu registers.
#define GET_REGINFO_ENUM
#include "CeespuGenRegisterInfo.inc"

// Defines symbolic names for Ceespu instructions.
#define GET_INSTRINFO_ENUM
#include "CeespuGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "CeespuGenSubtargetInfo.inc"

#endif
