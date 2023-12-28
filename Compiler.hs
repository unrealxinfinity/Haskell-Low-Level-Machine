module Compiler where

import Interpreter


data Aexp = T | F | Var String | Const Integer | ADDexp Aexp Aexp | SUBexp Aexp Aexp | MULTexp Aexp Aexp deriving Show 

data Bexp = BexpA Aexp | EQexp Bexp Bexp | BoolEQexp Bexp Bexp | LEQexp Bexp Bexp | ANDexp Bexp Bexp | NEGexp Bexp deriving Show
data Stm = Assign String Aexp | Lp Bexp Program | Conditional Bexp Program Program | Print Bexp deriving Show
type Program = [Stm]




isBooleanEQ :: Bexp -> Bool
isBooleanEQ (BexpA T) = True
isBooleanEQ (BexpA F) = True
isBooleanEQ (BexpA _) = False
isBooleanEQ _ = True

compDoubleSubtraction :: Aexp -> Code
compDoubleSubtraction (SUBexp elem1 (SUBexp elem2 elem3)) = compDoubleSubtraction (SUBexp elem2 elem3) ++ compA elem1 ++ [Add]
compDoubleSubtraction (SUBexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Add]
compDoubleSubtraction _ = error "Error while doing double subtraction"


compA :: Aexp -> Code
compA T = [Tru]
compA F = [Fals]
compA (Var var) = [Fetch var]
compA (Const const) = [Push const]
compA (ADDexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Add]
compA (SUBexp elem1 (SUBexp elem2 elem3)) = compDoubleSubtraction (SUBexp elem2 elem3) ++ compA elem1 ++ [Sub]
compA (SUBexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Sub]
compA (MULTexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Mult]

compB :: Bexp -> Code
compB (BexpA aexp) = compA aexp
compB (EQexp elem1 elem2) 
      | not (isBooleanEQ elem1) && not (isBooleanEQ elem2) = compB elem2 ++ compB elem1 ++ [Equ]
compB (BoolEQexp elem1 elem2)
      | isBooleanEQ elem1 && isBooleanEQ elem2 = compB elem1 ++ compB elem2 ++ [Equ]
compB (LEQexp elem1 elem2) = compB elem2 ++ compB elem1 ++ [Le]
compB (NEGexp elem) = compB elem ++ [Neg]
compB (ANDexp elem1 elem2) = compB elem2 ++ compB elem1 ++ [And]

compB _ = error "Error Compiling code"


compile :: Program -> Code
compile [] = []
compile (Assign var aexp:program) = compA (aexp) ++ [Store var] ++ compile (program)
compile (Conditional bexp stm1 stm2:program) = compB bexp ++ [Branch progCalculated progCalculated2] ++ compile (program)
   where 
    progCalculated = compile stm1
    progCalculated2 = compile stm2
compile (Lp bexp prog:program) = [Loop (compB bexp) (compile prog)]


