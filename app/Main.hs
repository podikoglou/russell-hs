module Main where

import Data.Map
import Russell.AST
import Russell.Engine

ast :: ASTNode
ast = Equality (Disjunction (Variable 'p') (Conjunction (Variable 'p') (Variable 'q'))) (Variable 'p')

main :: IO ()
main = print (eval ast (Data.Map.fromList [('p', True), ('q', True)]))
