module Extra where

import Compiler
import Parser
import Interpreter
import qualified Data.Map as Map

type Functions = Map.Map String (Pair [String] Program)

type Classes = Map.Map String Program


getParamValues :: [String] -> [Bexp] -> State -> Code -> Functions -> Classes -> (Program, [String], String, Code)

getParamValues _ [] _ _ _ _= ([], [], [], [])

getParamValues (elem1:paramNames) (BexpA (Var ('&':varName)):paramValues) state program functions classes = 
        case getParamValues paramNames paramValues state program functions classes of
            (program, refVars, funcStr, refCode) -> (program, varName:refVars, funcStr, refCode)

getParamValues (elem1:paramNames) (elem2:paramValues) state program functions classes =
        case compB' elem2 program functions classes of 
            (code, funcStr, refCode) ->
                case run (code, createEmptyStack, state) of
                    (_, TT:_, _) -> 
                        case getParamValues paramNames paramValues state program functions classes of
                            (program, refVars, funcStr1, refCode1) -> (Assign elem1 T:program, " ":refVars, funcStr ++ funcStr1, refCode ++ refCode1)
                    (_, FF:_, _) -> 
                        case getParamValues paramNames paramValues state program functions classes of
                            (program, refVars, funcStr1, refCode1) -> (Assign elem1 F:program, " ":refVars, funcStr ++ funcStr1, refCode ++ refCode1)
                    (_, Intgr num:_, _) -> 
                        case getParamValues paramNames paramValues state program functions classes of
                            (program, refVars, funcStr1, refCode1) -> (Assign elem1 (Const num):program, " ":refVars, funcStr ++ funcStr1, refCode ++ refCode1)


getPrintResult :: Program -> Code -> Functions -> Classes -> (Code, String)
getPrintResult [] acc _ _= (acc,"")
getPrintResult (Print bexp:program) acc functions classes =
          case run (acc, createEmptyStack, createEmptyState) of
            (_, stack, state) -> 
                case formPrintString bexp acc stack state functions classes of
                    (code, printString) -> 
                        case getPrintResult program (acc ++ code) functions classes of
                            (code1, '§':str) -> (code1, '§':printString ++ str)
                            (code1, str) -> (code1, printString ++ str)



getPrintResult (Conditional bexp stm1 stm2:program) acc functions classes =
            case compB' bexp acc functions classes of
                (code, funcStr, refCode) ->
                    case run (acc ++ code, createEmptyStack, createEmptyState) of
                        (_, TT:_, _) ->
                            case getPrintResult stm1 (acc ++ refCode) functions classes of
                                (code1, '§':str) -> (code1, '§':funcStr ++ str)
                                (code1, str) ->
                                    case getPrintResult program (code1) functions classes of
                                        (code2, '§':str1) -> (code2, '§':funcStr ++ str ++ str1)
                                        (code2, str1) ->
                                            (code2, funcStr ++ str ++ str1)
                        (_, FF:_, _) -> 
                            case getPrintResult stm2 (acc ++ refCode) functions classes of
                                (code1, '§':str) -> (code1, '§':funcStr ++ str)
                                (code1, str) ->
                                    case getPrintResult program (code1) functions classes of
                                        (code2, '§':str1) -> (code2, '§':funcStr ++ str ++ str1)
                                        (code2, str1) ->
                                            (code2, funcStr ++ str ++ str1)


getPrintResult (While bexp logic:program) acc functions classes =
            case compB' bexp acc functions classes of
                (code, funcStr, refCode) ->
                    case run (acc ++ code, createEmptyStack, createEmptyState) of
                        (_, TT:_, _) -> 
                            case getPrintResult logic (acc ++ refCode) functions classes of
                                (code1, '§':str) -> (code1, '§':funcStr ++ str)
                                (code1, str) ->
                                    case getPrintResult (While bexp logic:program) (code1) functions classes of
                                        (code2, '§':str1) -> (code2, '§':funcStr ++ str ++ str1)
                                        (code2, str1) ->
                                            (code2, funcStr ++ str ++ str1)
                        (_, FF:_, _) ->
                            case getPrintResult program (acc ++ refCode) functions classes of
                                (code1, '§':str) -> (code1, '§':funcStr ++ str)
                                (code1, str) ->
                                    (code1, funcStr ++ str)

getPrintResult (For stm bexp stm1 stm2:program) acc functions classes =
            case getPrintResult [stm] acc functions classes of
                (code, str) ->
                    case compB' bexp code functions classes of
                        (code1, funcStr, refCode) ->
                            case run (code ++ code1, createEmptyStack, createEmptyState) of
                                (_, TT:_, _) ->
                                    case getPrintResult (stm2 ++ [stm1]) (code ++ refCode) functions classes of
                                        (code2, '§':str1) -> (code2, '§':str ++ funcStr ++ str)
                                        (code2, str1) ->
                                            case getPrintResult ((While bexp (stm2 ++ [stm1])):program) (code ++ code2) functions classes of
                                                (code3, '§':str2) -> (code3, '§':str ++ funcStr ++ str1 ++ str2)
                                                (code3, str2) ->
                                                    (code3, str ++ funcStr ++ str1 ++ str2)
                                (_, FF:_, _) ->
                                    case getPrintResult program (acc ++ refCode) functions classes of
                                        (code2, '§':str1) -> (code2, '§':str ++ funcStr ++ str1)
                                        (code2, str1) ->
                                            (code2, str ++ funcStr ++ str1)

getPrintResult (Function funcName param stm:program) acc functions classes =
            case Map.lookup funcName functions of
                Nothing ->
                    getPrintResult program acc (Map.insert funcName (param, stm) functions) classes
                _ -> error "Error while declaring function"

getPrintResult (Return bexp:program) acc functions classes = 
            case compB' bexp acc functions classes of
                (code, str, refCode) ->
                    (acc ++ refCode ++ code, '§':str)


getPrintResult (VoidFuncCall funcName bexp:program) acc functions classes
            | Just (param, stm) <- Map.lookup funcName functions = 
                case (length param == length bexp) of
                    True -> 
                        case run (acc, createEmptyStack, createEmptyState) of
                            (_, _, state) ->
                                case getParamValues param bexp state acc functions classes of
                                    (program1, refVars, '§':funcStr, refCode1) ->
                                        case getPrintResult (program1 ++ stm) [] functions classes of
                                            (code, '§':str) ->
                                                case run (code, createEmptyStack, createEmptyState) of
                                                    (_, _, state1) -> 
                                                        case getRefVariablesValues refVars param state1 of
                                                            refCode -> 
                                                                case getPrintResult program (acc ++ refCode1 ++ refCode) functions classes of
                                                                    (code1, str1) -> (code1, funcStr ++ str ++ str1)
                                            (code, str) ->
                                                case run (code, createEmptyStack, createEmptyState) of
                                                    (_, _, state1) -> 
                                                        case getRefVariablesValues refVars param state1 of
                                                            refCode -> 
                                                                case getPrintResult program (acc ++ refCode1 ++ refCode) functions classes of
                                                                    (code1, str1) -> (code1, funcStr ++ str ++ str1)
                                    (program1, refVars, funcStr, refCode1) ->
                                        case getPrintResult (program1 ++ stm) [] functions classes of
                                            (code, '§':str) ->
                                                case run (code, createEmptyStack, createEmptyState) of
                                                    (_, _, state1) -> 
                                                        case getRefVariablesValues refVars param state1 of
                                                            refCode -> 
                                                                case getPrintResult program (acc ++ refCode1 ++ refCode) functions classes of
                                                                    (code1, str1) -> (code1, funcStr ++ str ++ str1)
                                            (code, str) ->
                                                case run (code, createEmptyStack, createEmptyState) of
                                                    (_, _, state1) -> 
                                                        case getRefVariablesValues refVars param state1 of
                                                            refCode -> 
                                                                case getPrintResult program (acc ++ refCode1 ++ refCode) functions classes of
                                                                    (code1, str1) -> (code1, funcStr ++ str ++ str1)
                    
                    False -> error "Incorrect number of arguments being passed to function"

            | otherwise = error "Error while calling function"


getPrintResult (Assign var (FuncCall name []):program) acc functions classes
            | Just (["§"], stm) <- Map.lookup name functions = 
                            getPrintResult program acc functions (Map.insert var stm classes)

getPrintResult (Assign var aexp:program) acc functions classes = 
            case compA' aexp acc functions classes of
                (aexp1, printStr, refCode) ->
                    case getPrintResult program (acc ++ refCode ++ aexp1 ++ [Store var]) functions classes of
                        (code, str) ->
                            (code, printStr ++ str)


getPrintResult (Class className stm:program) acc functions classes = 
            case Map.lookup className functions of
                Nothing ->
                    getPrintResult program acc (Map.insert className (["§"], stm) functions) classes
                _ -> error "Error while declaring class"
            



        





runFileProgram :: [Token] -> String
runFileProgram tokens =
          case buildData tokens of
            (program, tokens1) ->
                case getPrintResult program [] Map.empty Map.empty of
                    (_, str) -> str


testParserFile :: String -> String
testParserFile programCode = runFileProgram (lexer (programCode))



checkEndLines :: String -> String

checkEndLines [] = ""

checkEndLines ('\\':'n':str) = "\n" ++ checkEndLines str

checkEndLines (elem:str) = elem:checkEndLines str


formPrintString :: [Bexp] -> Code -> Stack -> State -> Functions -> Classes -> (Code, String)

formPrintString [] _ _ _ _ _= ([], "")

formPrintString (BexpA (PrintStr str):bexps) program stack state functions classes = 
            case formPrintString bexps program stack state functions classes of
                (code, printStr) -> (code, checkEndLines str ++ printStr)

formPrintString (bexp:bexps) program stack state functions classes = 
            case compB' bexp program functions classes of
                (code, str, refCode) ->
                    case run (code, stack, state) of
                        (_, elem:_, _) ->
                            case formPrintString bexps program stack state functions classes of
                                (code1, printStr) -> (refCode, str ++ stackElemToString elem ++ printStr)
                        _ -> error "Error while parsing print"


getRefVariablesValues :: [String] -> [String] -> State -> Code

getRefVariablesValues [] [] _ = []

getRefVariablesValues (elem1:refVars) (elem2:funcVars) state
        | head elem1 == ' ' = getRefVariablesValues refVars funcVars state
        | otherwise = 
            case Map.lookup elem2 state of
                Just TT ->
                    (compile [Assign elem1 T]) ++ getRefVariablesValues refVars funcVars state
                Just FF ->
                    (compile [Assign elem1 F]) ++ getRefVariablesValues refVars funcVars state
                Just (Intgr element) ->
                    (compile [Assign elem1 (Const element)]) ++ getRefVariablesValues refVars funcVars state
                Nothing -> 
                    getRefVariablesValues refVars funcVars state



compA' :: Aexp -> Code -> Functions -> Classes -> (Code, String, Code)

compA' (ClassCall className (Var elem1:aexp)) program functions classes
        | Just (stm) <- Map.lookup className classes =

            case getPrintResult (stm ++ [Return (BexpA (Var elem1))])  [] functions classes of
                (code, '§':str) ->
                    case run (code, createEmptyStack, createEmptyState) of
                            (_, TT:_, state1) -> 
                                ([Tru], str, [])
                            (_, FF:_, state1) -> 
                                ([Fals], str, [])
                            (_, Intgr num:_, state1) -> 
                                ([Push (num)], str, [])

        | otherwise = error "Error while calling a class"

compA' (ClassCall className (FuncCall funcName bexp:aexp)) program functions classes
        | Just (stm) <- Map.lookup className classes =
                        case getPrintResult (stm ++ [Return (BexpA (FuncCall funcName bexp))])  [] functions classes of
                            (code, '§':str) ->
                                case run (code, createEmptyStack, createEmptyState) of
                                    (_, TT:_, state1) -> 
                                        ([Tru], str, [])
                                    (_, FF:_, state1) -> 
                                        ([Fals], str, [])
                                    (_, Intgr num:_, state1) -> 
                                        ([Push (num)], str, [])

        | otherwise = error "Error while calling a class"

compA' (FuncCall funcName bexp) program functions classes
        | Just (param, stm) <- Map.lookup funcName functions = 
            case (length param == length bexp) of
                True -> 
                    case run (program, createEmptyStack, createEmptyState) of
                        (_, _, state) ->
                            case getParamValues param bexp state program functions classes of
                                (program1, refVars, funcStr, refCode1) ->
                                    case getPrintResult (program1 ++ stm) [] functions classes of
                                        (code, '§':str) ->
                                            case run (code, createEmptyStack, createEmptyState) of
                                                (_, TT:_, state1) -> 
                                                    case getRefVariablesValues refVars param state1 of
                                                        refCode -> ([Tru], funcStr ++ str, refCode1 ++ refCode)
                                                (_, FF:_, state1) -> 
                                                    case getRefVariablesValues refVars param state1 of
                                                        refCode -> ([Fals], funcStr ++ str, refCode1 ++ refCode)
                                                (_, Intgr num:_, state1) -> 
                                                    case getRefVariablesValues refVars param state1 of
                                                        refCode -> ([Push (num)], funcStr ++ str, refCode1 ++ refCode)
                                    
                False -> error "Incorrect number of arguments being passed to function"
        | otherwise = error "Error while calling function"

compA' (ADDexp elem1 elem2) program functions classes = 
        case compA' elem2 program functions classes of
            (code, str, refCode) ->
                case compA' elem1 program functions classes of
                    (code1, str1, refCode1) ->
                        (code ++ code1 ++ [Add], str1 ++ str, refCode ++ refCode1)

compA' (MULTexp elem1 elem2) program functions classes =
        case compA' elem2 program functions classes of
            (code, str, refCode) ->
                case compA' elem1 program functions classes of
                    (code1, str1, refCode1) ->
                        (code ++ code1 ++ [Mult], str1 ++ str, refCode ++ refCode1)

compA' elem _ _ _ = (compA elem, "", [])


compB' :: Bexp -> Code -> Functions -> Classes -> (Code, String, Code)
compB' (BexpA aexp) program functions classes = compA' aexp program functions classes
compB' (EQexp elem1 elem2) program functions classes
      | not (isBooleanEQ elem1) && not (isBooleanEQ elem2) = 
            case compB' elem2 program functions classes of
                (code, str, refCode) ->
                    case compB' elem1 program functions classes of
                        (code1, str1, refCode1) ->
                            (code ++ code1 ++ [Equ], str1 ++ str, refCode ++ refCode1)


                        
compB' (BoolEQexp elem1 elem2) program functions classes =
            case compB' elem2 program functions classes of
                (code, str, refCode) ->
                    case compB' elem1 program functions classes of
                        (code1, str1, refCode1) ->
                            case last code1 of
                                Tru -> (code ++ code1 ++ [Equ], str1 ++ str, refCode ++ refCode1)
                                Fals -> (code ++ code1 ++ [Equ], str1 ++ str, refCode ++ refCode1)
                                _
                                    | isBooleanEQ elem1 && isBooleanEQ elem2 -> (code ++ code1 ++ [Equ], str1 ++ str, refCode ++ refCode1)
                                    | otherwise -> error "Error while making a boolean expression"
        

compB' (LEQexp elem1 elem2) program functions classes = 
            case compB' elem2 program functions classes of
                (code, str, refCode) ->
                    case compB' elem1 program functions classes of
                        (code1, str1, refCode1) ->
                            (code ++ code1 ++ [Le], str1 ++ str, refCode ++ refCode1)
compB' (NEGexp elem) program functions classes = 
            case compB' elem program functions classes of
                (code, str, refCode) ->
                    (code ++ [Neg], str, refCode)
compB' (ANDexp elem1 elem2) program functions classes = 
            case compB' elem2 program functions classes of
                (code, str, refCode) ->
                    case compB' elem1 program functions classes of
                        (code1, str1, refCode1) ->
                            (code ++ code1 ++ [And], str1 ++ str, refCode ++ refCode1)

compB' _ _ _ _= error "Error Compiling code"






