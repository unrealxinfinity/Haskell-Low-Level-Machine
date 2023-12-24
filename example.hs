-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

import Data.List
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

data StackElement = T | F | Intgr Integer deriving (Eq, Show)
-- Mixed list that the last element is the top of the stack
type Stack = [StackElement]  

-- List of pairs of String key and StackElement value
type State = [Pair String StackElement]
 
                                    

-- Converts a StackElement to an ordinary data type
stackElemToString::StackElement -> String
stackElemToString (Intgr n) = show n
stackElemToString (T) = "True"
stackElemToString (F) = "False"


-- Checks for the data type for StackElements
--isStr:: StackElement->Bool
--isStr (Str a) = True
--isStr _ = False
--isIntgr::StackElement->Bool
--isIntgr (Intgr a)= True
--isIntgr _ = False
--isBoole::StackElement -> Bool
--isBoole (Boole a) = True
--isBoole _ = False


createEmptyStack::Stack
createEmptyStack = [] 
createEmptyState::State
createEmptyState = []

stateElemToString::Pair String StackElement->String
stateElemToString (a,b) = a ++"="++ (stackElemToString b)


stack2Str :: Stack -> String
stack2Str s = 
  let rev = reverse s
  in intercalate "," [stackElemToString x | x<-rev]

state2Str :: State -> String
state2Str s = intercalate "," (sort [stateElemToString x | x <- s])
  
  
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (Push n:code, stack, state) = run (code, Intgr n:stack, state)

run (Add:code, Intgr elem1:Intgr elem2:stack, state) = run (code, Intgr (elem1+elem2):stack, state)
run (Sub:code, Intgr elem1:Intgr elem2:stack, state) = run (code, Intgr (elem1-elem2):stack, state)
run (Mult:code, Intgr elem1:Intgr elem2:stack, state) = run (code, Intgr (elem1*elem2):stack, state)

run (Equ:code, elem1:elem2:stack, state)
        | elem1 == elem2 = run (code, T:stack, state)
        | otherwise = run (code, F:stack, state)

run (Le:code, Intgr elem1:Intgr elem2:stack, state)
        | elem1 <= elem2 = run (code, T:stack, state)
        | otherwise = run (code, F:stack, state)

run(_, _, _) = error "Runtime error"





-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)
  
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
