module Russell.AST where

data ASTNode
  = Literal Bool
  | Variable Char
  | Negation ASTNode
  | Conjunction ASTNode ASTNode
  | Disjunction ASTNode ASTNode
  | Equality ASTNode ASTNode
