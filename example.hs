-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

import Data.List
import qualified Data.Map as Map

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Pair a b = (a,b)
first::Pair a b -> a
first (x,_) = x
second::Pair a b -> b 
second (_,y) = y


data StackElement = TT | FF | Intgr Integer deriving (Eq, Show)
-- Mixed list that the last element is the top of the stack
type Stack = [StackElement]  

-- Map of pairs of String key and StackElement value
type State = Map.Map String StackElement
 
-- Extracts Value from result of lookup on a map, which has Maybe as result
extractValueFromState:: Maybe StackElement -> StackElement
extractValueFromState (Just a) = a                                    
extractValueFromState Nothing = error "Nothing found"

-- Converts a StackElement to an ordinary data type
stackElemToString::StackElement -> String
stackElemToString (Intgr n) = show n
stackElemToString (TT) = "True"
stackElemToString (FF) = "False"



createEmptyStack::Stack
createEmptyStack = [] 
createEmptyState::State
createEmptyState = Map.empty

-- Converts State element of a map as pair to a string
stateElemToString::Pair String StackElement->String
stateElemToString (a,b) = a ++"="++ (stackElemToString b)

--Converts Stack elements to string
stack2Str :: Stack -> String
stack2Str s = intercalate "," [stackElemToString x | x <- s]

--Converts State elements to string
state2Str :: State -> String
state2Str s = intercalate "," (sort [stateElemToString x | x <- (Map.toList s)])


-- Run fuction to execute instructions according to state and stack
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

-- Executes instruction Push n
run (Push n:code, stack, state) = run (code, Intgr n:stack, state)
-- Executes instruction Tru
run (Tru:code, stack, state) = run (code, TT:stack, state)
-- Executes instruction Fals
run (Fals:code, stack, state) = run (code, FF:stack, state)

-- Executes instruction Add
run (Add:code, Intgr elem1:Intgr elem2:stack, state) = run (code, Intgr (elem1+elem2):stack, state)

-- Executes instruction Sub
run (Sub:code, Intgr elem1:Intgr elem2:stack, state) = run (code, Intgr (elem1-elem2):stack, state)

-- Executes instruction Mult
run (Mult:code, Intgr elem1:Intgr elem2:stack, state) = run (code, Intgr (elem1*elem2):stack, state)

-- Executes instruction And
run (And:code, TT:TT:stack, state) = run (code, TT:stack, state)
run (And:code, FF:TT:stack, state) = run (code, FF:stack, state)
run (And:code, TT:FF:stack, state) = run (code, FF:stack, state)
run (And:code, FF:FF:stack, state) = run (code, FF:stack, state)

-- Executes instruction Neg
run (Neg:code, TT:stack, state) = run (code, FF:stack, state)
run (Neg:code, FF:stack, state) = run (code, TT:stack, state)

-- Executes instruction Equ
run (Equ:code, elem1:elem2:stack, state)
        | elem1 == elem2 = run (code, TT:stack, state)
        | otherwise = run (code, FF:stack, state)

-- Executes instruction Le
run (Le:code, Intgr elem1:Intgr elem2:stack, state)
        | elem1 <= elem2 = run (code, TT:stack, state)
        | otherwise = run (code, FF:stack, state)

-- Executes instruction Branch
run (Branch condition _:code, TT:stack, state) = run (condition ++ code, stack, state)
run (Branch _ condition:code, FF:stack, state) = run (condition ++ code, stack, state)

-- Executes instruction Fetch
run (Fetch n:code,stack,state) =
  let 
    element = extractValueFromState $ Map.lookup n state
  in run (code,(element:stack),state)

-- Executes instruction Store
run (Store n:code,(topStack:stack),state) =
  let 
    newState = Map.insert n topStack state
  in run(code,stack,newState)
run (Store n:code, [],state) = error "Nothing to store"

-- Executes instruction Loop
run (Loop condition logic:code, stack, state) = run (condition ++ [Branch (logic ++ [Loop condition logic]) [Noop]] ++ code, stack, state)

-- Executes instruction Noop
run (Noop:code, stack, state) = run (code, stack, state)

-- In case the pattern matching doesn match any of above cases, its a runtime error
run (_, _, _) = error "Runtime error"


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

run_tests :: Integer -> (String, String)
run_tests 1 = testAssembler [Push 10,Push 4,Push 3,Sub,Mult]
run_tests 2 = testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"]
run_tests 3 = testAssembler [Fals,Store "var",Fetch "var"]
run_tests 4 = testAssembler [Push (-20),Tru,Fals]
run_tests 5 = testAssembler [Push (-20),Tru,Tru,Neg]
run_tests 6 = testAssembler [Push (-20),Tru,Tru,Neg,Equ]
run_tests 7 = testAssembler [Push (-20),Push (-21), Le]
run_tests 8 = testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"]
run_tests 9 = testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]
run_tests 10 = testAssembler [Push 1,Push 2,And]
run_tests 11 = testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
run_tests _ = error "Please submit right input number"
  
-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, store2Str store)
  --where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1") --}
