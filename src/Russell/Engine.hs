module Russell.Engine where

import Data.Map
import Russell.AST

type Assignments = Data.Map.Map Char Bool

eval :: ASTNode -> Assignments -> Maybe Bool
eval node assignments = case node of
  Literal val -> Just val
  Variable name -> Data.Map.lookup name assignments
  Negation node -> fmap not (eval node assignments)
  Conjunction lhs rhs -> liftA2 (&&) (eval lhs assignments) (eval rhs assignments)
  Disjunction lhs rhs -> liftA2 (||) (eval lhs assignments) (eval rhs assignments)
  Equality lhs rhs -> liftA2 (==) (eval lhs assignments) (eval rhs assignments)

computeTruthTable :: ASTNode -> [Assignments]
computeTruthTable node = []

isTautology :: ASTNode -> Bool
isTautology node =
  all
    (\l -> eval node l == Just True)
    (computeTruthTable node)

isContingency :: ASTNode -> Bool
isContingency node =
  all
    (\l -> eval node l == Just False)
    (computeTruthTable node)
