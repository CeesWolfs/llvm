//===-- CeespuMCTargetDesc.h - Ceespu Target Descriptions -------------*- C++ -*-===//
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

#include "llvm/Support/DataTypes.h"
#include "llvm/Config/config.h"

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class StringRef;
class Target;
class Triple;
class raw_ostream;
class raw_pwrite_stream;

extern Target TheCeespuleTarget;
extern Target TheCeespubeTarget;
extern Target TheCeespuTarget;

MCCodeEmitter *createCeespuMCCodeEmitter(const MCInstrInfo &MCII,
                                      const MCRegisterInfo &MRI,
                                      MCContext &Ctx);
MCCodeEmitter *createCeespubeMCCodeEmitter(const MCInstrInfo &MCII,
                                        const MCRegisterInfo &MRI,
                                        MCContext &Ctx);

MCAsmBackend *createCeespuAsmBackend(const Target &T, const MCRegisterInfo &MRI,
                                  const Triple &TT, StringRef CPU);
MCAsmBackend *createCeespubeAsmBackend(const Target &T, const MCRegisterInfo &MRI,
                                    const Triple &TT, StringRef CPU);

MCObjectWriter *createCeespuELFObjectWriter(raw_pwrite_stream &OS,
                                         uint8_t OSABI, bool IsLittleEndian);
}

// Defines symbolic names for Ceespu registers.  This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "CeespuGenRegisterInfo.inc"

// Defines symbolic names for the Ceespu instructions.
//
#define GET_INSTRINFO_ENUM
#include "CeespuGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "CeespuGenSubtargetInfo.inc"

#endif
