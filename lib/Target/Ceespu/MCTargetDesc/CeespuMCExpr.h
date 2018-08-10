//===-- CeespuMCExpr.h - Ceespu specific MC expression classes ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes Ceespu-specific MCExprs, used for modifiers like
// "%hi" or "%lo" etc.,
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuMCEXPR_H
#define LLVM_LIB_TARGET_Ceespu_MCTARGETDESC_CeespuMCEXPR_H

#include "llvm/MC/MCExpr.h"

namespace llvm {

class StringRef;
class MCOperand;
class CeespuMCExpr : public MCTargetExpr {
public:
  enum VariantKind {
    VK_Ceespu_None,
    VK_Ceespu_LO,
    VK_Ceespu_HI,
    VK_Ceespu_PCREL_LO,
    VK_Ceespu_PCREL_HI,
    VK_Ceespu_CALL,
    VK_Ceespu_Invalid
  };

private:
  const MCExpr *Expr;
  const VariantKind Kind;

  int64_t evaluateAsInt64(int64_t Value) const;

  explicit CeespuMCExpr(const MCExpr *Expr, VariantKind Kind)
      : Expr(Expr), Kind(Kind) {}

public:
  static const CeespuMCExpr *create(const MCExpr *Expr, VariantKind Kind,
                                   MCContext &Ctx);

  VariantKind getKind() const { return Kind; }

  const MCExpr *getSubExpr() const { return Expr; }

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;
  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  // There are no TLS CeespuMCExprs at the moment.
  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override {}

  bool evaluateAsConstant(int64_t &Res) const;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  static bool classof(const CeespuMCExpr *) { return true; }

  static VariantKind getVariantKindForName(StringRef name);
  static StringRef getVariantKindName(VariantKind Kind);
};

} // end namespace llvm.

#endif
