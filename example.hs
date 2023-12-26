-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

import Data.List
import qualified Data.Map as Map
import Data.Char (isSpace, isDigit, digitToInt, isAlpha)

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
run (Fetch n:code,stack,state)
        | Just element <- Map.lookup n state = run (code, element:stack, state)

-- Executes instruction Store
run (Store key:code, elem:stack, state) = run(code,stack,newState)
        where newState = Map.insert key elem state

-- Executes instruction Loop
run (Loop condition logic:code, stack, state) = run (condition ++ [Branch (logic ++ [Loop condition logic]) [Noop]] ++ code, stack, state)

-- Executes instruction Noop
run (Noop:code, stack, state) = run (code, stack, state)

-- In case the pattern matching doesn match any of above cases, its a runtime error
run (_, _, _) = error "Run-time error"


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

data Aexp = T | F | Var String | Const Integer | ADDexp Aexp Aexp | SUBexp Aexp Aexp | MULTexp Aexp Aexp deriving Show 

data Bexp = Aexp | EQexp Aexp Aexp | LEQexp Aexp Aexp | ANDexp Aexp Aexp | NEGexp Aexp deriving Show

data Stm = Assign Aexp Aexp deriving Show

type Program = [Stm]

data Token = EqualTok | PlusTok | MinusTok | TimesTok | VarTok String | IntTok Integer deriving Show


compA :: Aexp -> Code
compA T = [Tru]
compA F = [Fals]
compA (Var var) = [Fetch var]
compA (Const const) = [Push const]
compA (ADDexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Add]
compA (SUBexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Sub]
compA (MULTexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Mult]

-- compB :: Bexp -> Code
compB (EQexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Equ]
compB (LEQexp elem1 elem2) = compA elem2 ++ compA elem1 ++ [Le]
compB (NEGexp elem) = compA elem ++ [Neg]

-- compile :: Program -> Code
compile [] = []
compile (Assign (Var var) aexp:program) = compA (aexp) ++ [Store var] ++ compile (program)

stringToInt :: String -> Integer
stringToInt = foldl (\acc chr->10*acc+ toInteger (digitToInt chr)) 0


lexer :: String -> [Token]

lexer [] = []

lexer ('+':restStr) = PlusTok:lexer restStr
lexer ('-':restStr) = MinusTok:lexer restStr
lexer ('*':restStr) = TimesTok:lexer restStr
lexer (':':'=':restStr) = EqualTok:lexer restStr
lexer (chr:restStr)
      | isSpace chr = lexer restStr
lexer str@(chr : _)
      | isDigit chr = IntTok (stringToInt digitStr) : lexer restStr
      where
        (digitStr, restStr) = break (not . isDigit) str

lexer str@(chr : _)
      | isAlpha chr = VarTok varStr : lexer restStr
      where (varStr, restStr) = break (not . isAlpha) str
      

lexer (chr : restString) = error ("unexpected character: '" ++ show chr ++ "'")

parseVar :: [Token] -> Maybe (Aexp, [Token])
parseVar (VarTok elem:tokens) =
      Just (Var elem, tokens)

parseVar _ =
      Nothing

parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (IntTok elem:tokens) =
      Just (Const elem, tokens)

parseInt _ =
      Nothing

parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens =
      case parseInt tokens of 
        Just (aexp1, TimesTok:tokens1) ->
          case parseProdOrInt tokens1 of
            Just (aexp2, tokens2) ->
              Just (MULTexp aexp1 aexp2, tokens2)
            Nothing -> Nothing
        result -> result


parseSumOrSubOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSumOrSubOrProdOrInt tokens =
      case parseProdOrInt tokens of
        Just (aexp1, PlusTok:tokens1) ->
          case parseSumOrSubOrProdOrInt tokens1 of
            Just(aexp2, tokens2) ->
              Just (ADDexp aexp1 aexp2, tokens2)
            Nothing -> Nothing

        Just (aexp1, MinusTok:tokens1) ->
          case parseSumOrSubOrProdOrInt tokens1 of
            Just(aexp2, tokens2) ->
              Just(SUBexp aexp1 aexp2, tokens2)
            Nothing -> Nothing

        result -> result

parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm tokens = 
      case parseVar tokens of
          Just (varStr, EqualTok:tokens1) ->
            case parseSumOrSubOrProdOrInt tokens1 of
              Just (aexp, tokens2) ->
                Just (Assign varStr aexp, tokens2)
              Nothing -> Nothing


  

buildData :: [Token] -> Program
buildData tokens = 
      case parseStm tokens of
          Just (stm, []) -> [stm]
          _ -> error "Parse error"


parse :: String -> Program
parse programCode = buildData . lexer $ programCode

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1") --}



