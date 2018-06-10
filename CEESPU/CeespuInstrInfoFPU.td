class FA<bits<5> Opc, string OpcodeStr, SDNode OpNode>
    : InstCeespu<(outs GPR:$dst), (ins GPR:$src2, GPR:$src),
              !strconcat(OpcodeStr, " $dst, $src, $src2"),
              [(set GPR:$dst, (OpNode f32:$src2, f32:$src))]> {
  bits<5> op;
  bits<5> dst;
  bits<5> src;
  bits<5> src2;

  let Inst{31-26} = 0x1A;
  let Inst{25-21} = dst;
  let Inst{20-16} = src;
  let Inst{15-11} = src2;
  let Inst{4-0}   = op;
  let op = Opc;
}

def FADD : FA<0x0, "fadd", fadd>;
def FSUB : FA<0x0, "fsub", fsub>;
def FMUL : FA<0x0, "fmul", fmul>;

let isAsCheapAsAMove = 1 in {
  def FCMP_UN : CmpFN<0x16, 0x200, "fcmp.un", IIC_FPUc>;
  def FCMP_LT : CmpFN<0x16, 0x210, "fcmp.lt", IIC_FPUc>;
  def FCMP_EQ : CmpFN<0x16, 0x220, "fcmp.eq", IIC_FPUc>;
  def FCMP_LE : CmpFN<0x16, 0x230, "fcmp.le", IIC_FPUc>;
  def FCMP_GT : CmpFN<0x16, 0x240, "fcmp.gt", IIC_FPUc>;
  def FCMP_NE : CmpFN<0x16, 0x250, "fcmp.ne", IIC_FPUc>;
  def FCMP_GE : CmpFN<0x16, 0x260, "fcmp.ge", IIC_FPUc>;
}


let usesCustomInserter = 1 in {
  def Select_FCC : MBlazePseudo<(outs GPR:$dst),
    (ins GPR:$T, GPR:$F, GPR:$CMP, i32imm:$CC),
    "; SELECT_FCC PSEUDO!",
    []>;
}