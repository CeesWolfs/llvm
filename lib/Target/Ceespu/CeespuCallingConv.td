//===-- CeespuCallingConv.td - Calling Conventions Ceespu --------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This describes the calling conventions for the Ceespu architecture.
//
//===----------------------------------------------------------------------===//

// Ceespu 32-bit C return-value convention.
def RetCC_Ceespu : CallingConv<[
  // i32 are returned in registers R20, R21, R22, R23
  CCIfType<[i32], CCAssignToReg<[R20, R21, R22, R23]>>,
  
  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;

// Ceespu 32-bit C Calling convention.
def CC_Ceespu : CallingConv<[
  // Promote i8/i16 arguments to i32.
  CCIfType<[i8, i16], CCPromoteToType<i32>>,
  // The first 6 integer arguments are passed in integer registers.
  CCIfType<[i32], CCAssignToReg<[R20, R21, R22, R23, R24, R25]>>,

  CCIfByVal<CCPassByVal<4, 4>>,
  
  // Integer values get stored in stack slots that are four bytes in
  // size and 4-byte aligned.
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;

def CSR : CalleeSavedRegs<(add LR, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)>;

def CSR_NoRegs : CalleeSavedRegs<(add)>;