module Extra where

import Compiler
import Parser
import Interpreter
import qualified Data.Map as Map

type Functions = Map.Map String (Pair [String] [Stm])


getParamValues :: [String] -> [Bexp] -> State -> Program

getParamValues [] [] _ = []

getParamValues (elem1:paramNames) (elem2:paramValues) state =
        case compB elem2 of 
            code ->
                case run (code, createEmptyStack, state) of
                    (_, TT:_, _) -> Assign elem1 T:getParamValues paramNames paramValues state
                    (_, FF:_, _) -> Assign elem1 F:getParamValues paramNames paramValues state
                    (_, Intgr num:_, _) -> Assign elem1 (Const num):getParamValues paramNames paramValues state


compA' :: Aexp -> Code -> Functions -> (Code, String)

compA' (FuncCall funcName bexp) program functions
        | Just (param, stm) <- Map.lookup funcName functions = 
            case (length param == length bexp) of
                True -> 
                    case run (program, createEmptyStack, createEmptyState) of
                        (_, _, state) ->
                            case getPrintResult (getParamValues param bexp state ++ stm) [] functions of
                                (code, str) ->
                                    case run (code, createEmptyStack, createEmptyState) of
                                        (_, TT:_, _) -> ([Tru], str)
                                        (_, FF:_, _) -> ([Fals], str)
                                        (_, Intgr num:_, _) -> ([Push (num)], str)
                                    
                False -> error "Incorrect number of arguments being passed to function"
        | otherwise = error "Error while calling function"

compA' (ADDexp elem1 elem2) program functions = 
        case compA' elem2 program functions of
            (code, str) ->
                case compA' elem1 program functions of
                    (code1, str1) ->
                        (code ++ code1 ++ [Add], str1 ++ str)

compA' (MULTexp elem1 elem2) program functions =
        case compA' elem2 program functions of
            (code, str) ->
                case compA' elem1 program functions of
                    (code1, str1) ->
                        (code ++ code1 ++ [Mult], str1 ++ str)

compA' elem _ _ = (compA elem, "")


compB' :: Bexp -> Code -> Functions -> (Code, String)
compB' (BexpA aexp) program functions = compA' aexp program functions
compB' (EQexp elem1 elem2) program functions
      | not (isBooleanEQ elem1) && not (isBooleanEQ elem2) = 
            case compB' elem2 program functions of
                (code, str) ->
                    case compB' elem1 program functions of
                        (code1, str1) ->
                            (code ++ code1 ++ [Equ], str1 ++ str)


                             
            
                --compB' elem2 program functions ++ compB' elem1 program functions ++ [Equ]
compB' (BoolEQexp elem1 elem2) program functions =
            case compB' elem2 program functions of
                (code, str) ->
                    case compB' elem1 program functions of
                        (code1, str1) ->
                            case last code1 of
                                Tru -> (code ++ code1 ++ [Equ], str1 ++ str)
                                Fals -> (code ++ code1 ++ [Equ], str1 ++ str)
                                _
                                    | isBooleanEQ elem1 && isBooleanEQ elem2 -> (code ++ code1 ++ [Equ], str1 ++ str)
                                    | otherwise -> error "Error while making a boolean expression"
        
      
      --compB' elem1 program functions ++ compB' elem2 program functions ++ [Equ]
compB' (LEQexp elem1 elem2) program functions = --compB' elem2 program functions ++ compB' elem1 program functions ++ [Le]
            case compB' elem2 program functions of
                (code, str) ->
                    case compB' elem1 program functions of
                        (code1, str1) ->
                            (code ++ code1 ++ [Le], str1 ++ str)
compB' (NEGexp elem) program functions = --compB' elem program functions ++ [Neg]
            case compB' elem program functions of
                (code, str) ->
                    (code ++ [Neg], str)
compB' (ANDexp elem1 elem2) program functions = --compB' elem2 program functions ++ compB' elem1 program functions ++ [And]
            case compB' elem2 program functions of
                (code, str) ->
                    case compB' elem1 program functions of
                        (code1, str1) ->
                            (code ++ code1 ++ [And], str1 ++ str)

compB' _ _ _ = error "Error Compiling code"



getPrintResult :: Program -> Code -> Functions -> (Code, String)
getPrintResult [] acc _ = (acc,"")
getPrintResult (Print bexp:program) acc functions =
          case run (acc, createEmptyStack, createEmptyState) of
            (_, stack, state) -> 
                case formPrintString bexp acc stack state functions of
                    printString -> 
                        case getPrintResult program acc functions of
                            (code, str) -> (code, printString ++ str)



getPrintResult (Conditional bexp stm1 stm2:program) acc functions =
            case run (acc ++ compB bexp, createEmptyStack, createEmptyState) of
                (_, TT:_, _) ->
                    case getPrintResult stm1 acc functions of
                        (_, str) ->
                            case getPrintResult program (acc ++ compile stm1) functions of
                                (code1, str1) ->
                                    (code1, str ++ str1)
                (_, FF:_, _) -> 
                    case getPrintResult stm2 acc functions of
                        (_, str) ->
                            case getPrintResult program (acc ++ compile stm2) functions of
                                (code1, str1) ->
                                    (code1, str ++ str1)


getPrintResult (While bexp logic:program) acc functions =
            case run (acc ++ compB bexp, createEmptyStack, createEmptyState) of
                (_, TT:_, _) -> 
                    case getPrintResult logic acc functions of
                        (_, str) ->
                            case getPrintResult (While bexp logic:program) (acc ++ compile logic) functions of
                                (code1, str1) ->
                                    (code1, str ++ str1)
                (_, FF:_, _) ->
                    getPrintResult program acc functions

getPrintResult (For stm bexp stm1 stm2:program) acc functions =
            case run (acc ++ compile [stm] ++ compB bexp, createEmptyStack, createEmptyState) of
                (_, TT:_, _) ->
                    case getPrintResult (stm2 ++ [stm1]) (acc ++ compile [stm]) functions of
                        (_, str) ->
                            case getPrintResult ((While bexp (stm2 ++ [stm1])):program) (acc ++ compile([stm] ++ stm2 ++ [stm1])) functions of
                                (code1, str1) ->
                                    (code1, str ++ str1)
                (_, FF:_, _) ->
                    getPrintResult program acc functions

getPrintResult (Function funcName param stm:program) acc functions =
            case Map.lookup funcName functions of
                Nothing ->
                    getPrintResult program acc (Map.insert funcName (param, stm) functions)
                _ -> error "Error while declaring function"

getPrintResult (Return bexp:program) acc functions = 
            case compB' bexp acc functions of
                (code, str) ->
                    (acc ++ code, str)


getPrintResult (Assign var aexp:program) acc functions= 
            case compA' aexp acc functions of
                (aexp1, printStr) ->
                    case getPrintResult program (acc ++ aexp1 ++ [Store var]) functions of
                        (code, str) ->
                            (code, printStr ++ str)
                







runFileProgram :: [Token] -> String
runFileProgram tokens =
          case buildData tokens of
            (program, tokens1) ->
                case getPrintResult program [] Map.empty of
                    (_, str) -> str


testParserFile :: String -> String
testParserFile programCode = runFileProgram (lexer (programCode))



checkEndLines :: String -> String

checkEndLines [] = ""

checkEndLines ('\\':'n':str) = "\n" ++ checkEndLines str

checkEndLines (elem:str) = elem:checkEndLines str


formPrintString :: [Bexp] -> Code -> Stack -> State -> Functions -> String

formPrintString [] _ _ _ _= ""

formPrintString (BexpA (PrintStr str):bexps) program stack state functions =  checkEndLines str ++ formPrintString bexps program stack state functions

formPrintString (bexp:bexps) program stack state functions = 
            case compB' bexp program functions of
                (code, str) ->
                    case run (code, stack, state) of
                        (_, elem:_, _) ->
                            str ++ stackElemToString elem ++ formPrintString bexps program stack state functions
                        _ -> error "Error while parsing print"






