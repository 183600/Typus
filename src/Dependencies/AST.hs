module Dependencies.AST (
  AST(..),
  Statement(..),
  TypeExpr(..),
  Constraint(..)
) where

import Data.Text (Text)

-- | Program AST used by the dependent type checker pipeline.
data AST = Program [Statement]
  deriving (Show, Eq)

-- | Top-level statements supported by the dependent type checker.
data Statement
  = STypeDef Text [Text] [Constraint]
  | STypeAlias Text TypeExpr [Constraint]
  | SVarDecl Text TypeExpr
  | SFuncDecl Text [(Text, TypeExpr)] (Maybe TypeExpr)
  | SConstraintDef Text Constraint
  | SExistsDecl [Text] Statement
  deriving (Show, Eq)

-- | Type expressions that appear in declarations and annotations.
data TypeExpr
  = SimpleT Text
  | GenericT Text [TypeExpr]
  | FuncT [(Text, TypeExpr)] TypeExpr
  | RefineT TypeExpr [Constraint]
  deriving (Show, Eq)

-- | Logical constraints used to refine or relate types.
data Constraint
  = SizeGT Text Int
  | SizeGE Text Int
  | RangeC Text Int Int
  | PredC Text [TypeExpr]
  deriving (Show, Eq)
