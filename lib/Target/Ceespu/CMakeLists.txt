set(LLVM_TARGET_DEFINITIONS Ceespu.td)

tablegen(LLVM CeespuGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM CeespuGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM CeespuGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM X86GenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM CeespuGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM CeespuGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM CeespuGenCallingConv.inc -gen-callingconv)
tablegen(LLVM CeespuGenSubtargetInfo.inc -gen-subtarget)
add_public_tablegen_target(CeespuCommonTableGen)

add_llvm_target(CeespuCodeGen
  CeespuAsmPrinter.cpp
  CeespuFrameLowering.cpp
  CeespuInstrInfo.cpp
  CeespuISelDAGToDAG.cpp
  CeespuISelLowering.cpp
  CeespuMachineFunction.cpp
  CeespuMCInstLower.cpp
  CeespuRegisterInfo.cpp
  CeespuSubtarget.cpp
  CeespuTargetMachine.cpp
  )

add_subdirectory(InstPrinter)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
