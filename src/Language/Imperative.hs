{-# LANGUAGE DeriveFunctor #-}
module Language.Imperative where

newtype Identifier
    = Identifier String
    deriving (Show, Eq)

data Var t a = Var a Identifier t
    deriving (Show, Eq, Functor)

data Pattern l a = Pattern a l
    deriving (Show, Eq, Functor)

data Case l s a = Case a (Pattern l a) s
    deriving (Show, Eq, Functor)

newtype Label = Label String
    deriving (Show, Eq)

data Statement l t e s a
    = SBlock a [s]
    | SExpr a e
    | SIf a e s s
    | SWhile a e s -- for, do-while, etc. can be desugared to 'while'
    | SBreak a
    | SContinue a
    | STry a s (Maybe s) (Maybe s)
    | SThrow a e
    | SForEach a e e s
    | SSwitch a e [Case l s a] (Maybe s) --default
    | SReturn a e
    | SLabel a Label
    | SGoto a Label
    deriving (Show, Eq, Functor)

data Expr l t e s a
    = EVar a Identifier
    | EAssign a e e -- type doesn't enforce lvalue
    | ELiteral a l
    | ECall a e [e]
    | ETCall a e t -- pass a type to a function
    | EFunction a t [Var t a] s
    deriving (Show, Eq, Functor)
