-- |

module Language.Imperative.C where

import Language.Imperative
import qualified Language.C as C

type CType a = C.CDeclaration a
type CLit a = C.CConstant a
type CExpr e s a = Expr (CLit a) (CType a) e s a
type CStmt e s a = Statement (CLit a) (CType a) e s a
type CCase s a = Case (CLit a) s a
type CPat a = Pattern (CLit a) a

newtype Fix f = Fix (f (Fix f))

newtype FCExpr a = FCExpr (CExpr (FCExpr a) (FCStmt a) a)
newtype FCStmt a = FCStmt (CStmt (FCExpr a) (FCStmt a) a)

fixS s = FCStmt s
fixE e = FCExpr e

fromC :: C.CStatement a -> FCStmt a
fromC (C.CLabel i stmt attrs a) =
    fixS $
    SBlock a [ fixS $ SLabel a (Label $ C.identToString i)
             , fromC stmt
             ]
fromC (C.CSwitch expr stmt a) =
    fixS $ SSwitch a (fromCE expr) (toCases stmt) Nothing

fromCE :: C.CExpression a -> FCExpr a
fromCE (C.CAssign assignOp e1 e2 a) =
    fixE $
    EAssign a (fromCE e1) (fromCE e2) -- TODO use assignOp

toCases :: C.CStatement a -> [CCase (FCStmt a) a]
toCases (C.CCase expr next@(C.CCase{}) a) =
    (Case a (toPattern expr) (fixS $ SBlock a []))
    : toCases next

toPattern :: C.CExpression a -> CPat a
toPattern (C.CConst c) = Pattern (C.annotation c) c
