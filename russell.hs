import Data.Map

data ASTNode
  = Literal Bool
  | Variable Char
  | Negation ASTNode
  | Conjunction ASTNode ASTNode
  | Disjunction ASTNode ASTNode
  | -- | XOR ASTNode ASTNode
    -- | Implication ASTNode ASTNode
    Equality ASTNode ASTNode

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

ast =
  Equality
    ( Disjunction
        (Variable 'p')
        ( Conjunction
            (Variable 'p')
            (Variable 'q')
        )
    )
    (Variable 'p')

main = print (eval ast (Data.Map.fromList [('p', True), ('q', True)]))
