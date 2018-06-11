##===- lib/Target/Ceespu/Makefile -----------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

LEVEL = ../../..
LIBRARYNAME = LLVMCeespuCodeGen
TARGET = Ceespu

# Make sure that tblgen is run, first thing.
BUILT_SOURCES = CeespuGenRegisterInfo.inc CeespuGenInstrInfo.inc \
		CeespuGenAsmWriter.inc CeespuGenAsmMatcher.inc CeespuGenDAGISel.inc \
		CeespuGenMCCodeEmitter.inc CeespuGenSubtargetInfo.inc CeespuGenCallingConv.inc

DIRS = InstPrinter TargetInfo MCTargetDesc

include $(LEVEL)/Makefile.common
