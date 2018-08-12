set(LLVM_TARGET_DEFINITIONS Ceespu.td)

tablegen(LLVM CeespuGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM CeespuGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM CeespuGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM CeespuGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM CeespuGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM CeespuGenCallingConv.inc -gen-callingconv)
tablegen(LLVM CeespuGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM CeespuGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM CeespuGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(CeespuCommonTableGen)

add_llvm_target(CeespuCodeGen
  CeespuAsmPrinter.cpp
  CeespuFrameLowering.cpp
  CeespuInstrInfo.cpp
  CeespuISelDAGToDAG.cpp
  CeespuISelLowering.cpp
  CeespuMCInstLower.cpp
  CeespuMachineFunctionInfo.cpp
  CeespuRegisterInfo.cpp
  CeespuSubtarget.cpp
  CeespuTargetMachine.cpp
  CeespuTargetObjectFile.cpp
  )

add_subdirectory(InstPrinter)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
