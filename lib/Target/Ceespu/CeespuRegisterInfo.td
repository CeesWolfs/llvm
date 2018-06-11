//===-- CeespuRegisterInfo.td - Ceespu Register defs -------------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the Ceespu register file
//===----------------------------------------------------------------------===//

class CeespuReg<bits<16> Enc, string n> : Register<n> {
  let HWEncoding = Enc;
  let Namespace = "Ceespu";
}

// CPU registers
def R0  : CeespuReg< 0, "c0">;
def R1  : CeespuReg< 1, "c1">;
def R2  : CeespuReg< 2, "c2">;
def R3  : CeespuReg< 3, "c3">;
def R4  : CeespuReg< 4, "c4">;
def R5  : CeespuReg< 5, "c5">;
def R6  : CeespuReg< 6, "c6">;
def R7  : CeespuReg< 7, "c7">;
def R8  : CeespuReg< 8, "c8">;
def R9  : CeespuReg< 9, "c9">;
def R10 : CeespuReg< 10, "c10">;
def R11 : CeespuReg< 11, "c11">;
def R12 : CeespuReg< 12, "c12">;
def R13 : CeespuReg< 13, "c13">;
def R14 : CeespuReg< 14, "c14">;
def R15 : CeespuReg< 15, "c15">;
def R16 : CeespuReg< 16, "c16">;
def R17 : CeespuReg< 17, "cir">;
def SP  : CeespuReg< 18, "csp">;
def LR  : CeespuReg< 19, "clr">;
def R20 : CeespuReg< 20, "c20">;
def R21 : CeespuReg< 21, "c21">;
def R22 : CeespuReg< 22, "c22">;
def R23 : CeespuReg< 23, "c23">;
def R24 : CeespuReg< 24, "c24">;
def R25 : CeespuReg< 25, "c25">;
def R26 : CeespuReg< 26, "c26">;
def R27 : CeespuReg< 27, "c27">;
def R28 : CeespuReg< 28, "c28">;
def R29 : CeespuReg< 29, "c29">;
def R30 : CeespuReg< 30, "c30">;
def R31 : CeespuReg< 31, "c31">;

// Register classes.
//
def GPR : RegisterClass<"Ceespu", [i32], 32, (add 
	// Reserved
	R0, 
    // Return Values and Arguments
	R20, R21, R22, R23, R24, R25,
	// Not preserved across procedure calls
	R12, R13, R14, R15, R16, R26, R27, R28, R29, R30, R31,
	// Callee save
	R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11,
	// Reserved 
  LR, SP, R17)>;
