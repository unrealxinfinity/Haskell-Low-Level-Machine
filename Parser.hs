module Parser where

import Compiler
import Interpreter
import Data.Char (isSpace, isDigit, digitToInt, isAlpha, isUpper)


data Token = EqualTok | PlusTok | TimesTok | IneqTok | EqTok | NotTok | BoolEqTok | AndTok | OpenParTok | CloseParTok | TrueTok | FalseTok | VarTok String | IntTok Integer | IfTok | ThenTok | ElseTok | ColonTok | WhileTok | DoTok | ForTok | PrintTok | FuncTok | RetTok | QuoteTok | RefTok | ClassTok | CommaTok | DotTok deriving Show


stringToInt :: String -> Integer
stringToInt = foldl (\acc chr->10*acc+ toInteger (digitToInt chr)) 0


lexer :: String -> [Token]

lexer [] = []

lexer (chr:restStr)
      | isSpace chr = lexer restStr
lexer ('£':'.':chr:restStr)
      | (isAlpha chr && not (isUpper chr)) || chr == '_' = DotTok:lexer (chr:restStr)
lexer ('£':restStr) = lexer restStr
lexer ('+':restStr) = PlusTok:lexer restStr
lexer ('-':restStr) = PlusTok:IntTok (-1):TimesTok:lexer restStr
lexer ('*':restStr) = TimesTok:lexer restStr
lexer (':':'=':restStr) = EqualTok:lexer restStr
lexer ('<':'=':restStr) = IneqTok:lexer restStr
lexer ('=':'=':restStr) = EqTok:lexer restStr
lexer ('=':restStr) = BoolEqTok:lexer restStr
lexer ('a':'n':'d':restStr) = AndTok:lexer restStr
lexer (';':restStr) = ColonTok:lexer restStr
lexer ('(':restStr) = OpenParTok:lexer restStr
lexer (')':restStr) = CloseParTok:lexer restStr
lexer ('i':'f':restStr) = IfTok:lexer restStr
lexer ('t':'h':'e':'n':restStr) = ThenTok:lexer restStr
lexer ('e':'l':'s':'e':restStr) = ElseTok:lexer restStr
lexer ('n':'o':'t':restStr) = NotTok:lexer restStr
lexer ('T':'r':'u':'e':restStr) = TrueTok:lexer restStr
lexer ('F':'a':'l':'s':'e':restStr) = FalseTok:lexer restStr
lexer ('w':'h':'i':'l':'e':restStr) = WhileTok:lexer restStr
lexer ('f':'o':'r':restStr) = ForTok:lexer restStr
lexer ('d':'o':restStr) = DoTok:lexer restStr
lexer ('p':'r':'i':'n':'t':'(':restStr) = PrintTok:lexer restStr
lexer ('f':'u':'n':'c':'t':'i':'o':'n':' ':restStr) = FuncTok:lexer restStr
lexer ('r':'e':'t':'u':'r':'n':restStr) = RetTok:lexer restStr
lexer ('c':'l':'a':'s':'s':' ':restStr) = ClassTok:lexer restStr
lexer ('&':restStr) = RefTok:lexer restStr
lexer (',':restStr) = CommaTok:lexer restStr
lexer ('\'':restStr) = QuoteTok:lexer ('§':restStr)

lexer str@(chr : _)
      | isDigit chr = IntTok (stringToInt digitStr) : lexer restStr
      where
        (digitStr, restStr) = break (not . isDigit) str


lexer str@('§': _) = VarTok varStr : QuoteTok : lexer restStr
      where ('§':varStr, '\'':restStr) = break (=='\'') str


lexer str@(chr : _)
      | (isAlpha chr && not (isUpper chr)) || chr == '_' = VarTok varStr : lexer ('£':restStr)
      where (varStr, restStr) = break (\c -> not (isAlpha c) && not (isDigit c) && c /= '_') str

lexer (chr : restStr) = error ("unexpected character: '" ++ show chr ++ "'")

parseAexpType :: [Token] -> Maybe (Aexp, [Token])
parseAexpType (TrueTok:tokens) =
      Just (T, tokens)

parseAexpType (FalseTok:tokens) =
      Just (F, tokens)


parseAexpType (IntTok elem:tokens) =
      Just (Const elem, tokens)

parseAexpType (VarTok elem:OpenParTok:tokens) =
      case getPrintVariables tokens of
        (bexp, tokens1) ->
          Just (FuncCall elem bexp, tokens1)

parseAexpType (VarTok elem:DotTok:tokens) =
      case getClassConnectors (DotTok:tokens) of
        (aexp, tokens1) ->
          Just (ClassCall elem aexp, tokens1)

parseAexpType (VarTok elem:tokens) =
      Just (Var elem, tokens)



parseAexpType (OpenParTok:tokens) =
      case parseSumOrSubOrProdOrInt tokens of
        Just (aexp, CloseParTok:tokens1) ->
          Just (aexp, tokens1)
        Just _ -> Nothing
        Nothing -> Nothing

parseAexpType _ =
      Nothing

getClassConnectors :: [Token] -> ([Aexp], [Token])

getClassConnectors (DotTok:VarTok elem:OpenParTok:tokens) =
      case getPrintVariables tokens of
        (bexp, tokens1) ->
          case getClassConnectors tokens1 of
            (aexp, tokens2) ->
              (FuncCall elem bexp:aexp, tokens2)

getClassConnectors (DotTok:VarTok elem:tokens) =
      case getClassConnectors tokens of
        (aexp, tokens2) ->
          (Var elem:aexp, tokens2)

getClassConnectors tokens = ([], tokens)


parseBexpType :: [Token] -> Maybe (Bexp, [Token])
parseBexpType (OpenParTok:tokens) =
      case parseAndOrBoolEqOrNotOrEqOrIneq tokens of
        Just (bexp, CloseParTok:tokens1) ->
          Just (bexp, tokens1)
        _ -> 
          case parseAexpType (OpenParTok:tokens) of
            Just (aexp, tokens1) ->
              Just (BexpA aexp, tokens1)
            Nothing -> Nothing


parseBexpType (tokens) =
      case parseSumOrSubOrProdOrInt tokens of
        Just (aexp, tokens1) ->
          Just (BexpA aexp, tokens1)
        Nothing -> Nothing



parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens =
      case parseAexpType tokens of 
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

        result -> result

parseIneq :: [Token] -> Maybe (Bexp, [Token])

parseIneq tokens =
      case parseBexpType tokens of 
        Just (bexp1, IneqTok:tokens1) ->
          case parseIneq tokens1 of
            Just (bexp2, tokens2) ->
              Just (LEQexp bexp1 bexp2, tokens2)
            Nothing -> Nothing
        result -> result

parseEqOrIneq :: [Token] -> Maybe (Bexp, [Token])
parseEqOrIneq tokens =
      case parseIneq tokens of
        Just (bexp1, EqTok:tokens1) ->
          case parseEqOrIneq tokens1 of 
            Just (bexp2, tokens2) ->
              Just (EQexp bexp1 bexp2, tokens2)
            Nothing -> Nothing
        result -> result

parseNotOrEqOrIneq :: [Token] -> Maybe (Bexp, [Token])
parseNotOrEqOrIneq (NotTok:tokens) =
      case parseNotOrEqOrIneq tokens of
        Just (bexp, tokens1) ->
          Just (NEGexp bexp, tokens1)
        result -> result


parseNotOrEqOrIneq tokens = 
      case parseEqOrIneq tokens of
        Just (bexp, tokens1) -> Just (bexp, tokens1)
        result -> result

parseBoolEqOrNotOrEqOrIneq :: [Token] -> Maybe (Bexp, [Token])
parseBoolEqOrNotOrEqOrIneq tokens =
      case parseNotOrEqOrIneq tokens of
        Just (bexp, BoolEqTok:tokens1) ->
          case parseBoolEqOrNotOrEqOrIneq tokens1 of
            Just (bexp1, tokens2) ->
              Just (BoolEQexp bexp bexp1, tokens2)
            Nothing -> Nothing

        result -> result

parseAndOrBoolEqOrNotOrEqOrIneq :: [Token] -> Maybe (Bexp, [Token])
parseAndOrBoolEqOrNotOrEqOrIneq tokens =
      case parseBoolEqOrNotOrEqOrIneq tokens of
        Just (bexp, AndTok:tokens1) ->
          case parseAndOrBoolEqOrNotOrEqOrIneq tokens1 of
            Just (bexp1, tokens2) ->
              Just (ANDexp bexp bexp1, tokens2)
            Nothing -> Nothing

        result -> result




branchFormat :: [Token] -> Pair Program [Token]
branchFormat (CloseParTok:_) = error "Bad Format of Conditional" 
branchFormat tokens = 
      case getBranch tokens 1 of
        (stm, tokens1) -> (stm, tokens1)



parseStm :: [Token] -> Maybe (Stm, [Token])

parseStm [] = Nothing


parseStm (IfTok:tokens)=
              case parseAndOrBoolEqOrNotOrEqOrIneq tokens of
                Just (bexp, ThenTok:tokens1) ->
                  case branchFormat tokens1 of
                    (stm1, ElseTok:tokens2) ->
                      case branchFormat tokens2 of
                        (stm2, ColonTok:tokens3) ->
                          Just (Conditional (bexp) (stm1) (stm2), ColonTok:tokens3)
                        (stm2, tokens3) ->
                          Just (Conditional (bexp) (stm1) (stm2), ColonTok:tokens3)
                    (stm1, tokens2) ->
                      Just (Conditional (bexp) (stm1) [], ColonTok:tokens2)
              
                result -> Nothing
        
parseStm (VarTok varName:EqualTok:tokens) = 
          case parseSumOrSubOrProdOrInt tokens of
            Just (aexp, tokens1) ->
              Just (Assign (varName) aexp, tokens1)
            result -> Nothing


parseStm (WhileTok:tokens) =
    case parseAndOrBoolEqOrNotOrEqOrIneq tokens of
      Just (bexp,DoTok:tokens1) ->
        case branchFormat tokens1 of
          (stm1, ColonTok:tokens2) ->
              Just (While (bexp) (stm1), ColonTok:tokens2)
          (stm1, tokens2) ->
              Just (While (bexp) (stm1), ColonTok:tokens2)
      result -> error "parse error"

parseStm (ForTok:OpenParTok:tokens) = 
      case parseStm tokens of
        Just (Assign var aexp, ColonTok:tokens1) -> 
          case parseAndOrBoolEqOrNotOrEqOrIneq tokens1 of 
            Just (bexp, ColonTok:tokens2) ->
              case parseStm tokens2 of
                Just (Assign var1 aexp1, CloseParTok:DoTok:tokens3) ->
                  case branchFormat tokens3 of
                    (stm, tokens4) ->
                      Just (For (Assign var aexp) (bexp) (Assign var1 aexp1) (stm), ColonTok:tokens4)
                _ -> Nothing
            _ -> Nothing
        result -> Nothing

parseStm (PrintTok:tokens) =
      case getPrintVariables tokens of
        (bexp, ColonTok:tokens1) ->
          Just (Print bexp, ColonTok:tokens1)
        _ -> Nothing

parseStm (FuncTok:VarTok funcName:OpenParTok:tokens) =
      case getFunctionParam tokens of
        (param, DoTok:tokens1) ->
          case branchFormat tokens1 of
            (stm, tokens2) ->
              Just (Function funcName param stm, ColonTok:tokens2)
        _ -> Nothing

parseStm (RetTok:tokens) =
      case parseSumOrSubOrProdOrInt tokens of
            Just (aexp, ColonTok:tokens1) ->
              Just (Return (BexpA aexp), ColonTok:tokens1)
            _ -> 
              case parseAndOrBoolEqOrNotOrEqOrIneq tokens of
                Just (bexp, ColonTok:tokens1) ->
                  Just (Return bexp, ColonTok:tokens1)
                _ -> Nothing

parseStm (VarTok funcName:OpenParTok:tokens) =
      case getPrintVariables tokens of
        (bexp, tokens1) ->
          Just (VoidFuncCall funcName bexp, tokens1)

parseStm (ClassTok:VarTok className:OpenParTok:tokens) = 
      case branchFormat (OpenParTok:tokens) of
        (stm, tokens1) ->
          case justAssignOrFuncOrClass stm of
            True -> 
              Just (Class className stm, ColonTok:tokens1)
            False ->
              error "Using illegal statements inside a class"


          

parseStm _ = Nothing

justAssignOrFuncOrClass :: Program -> Bool

justAssignOrFuncOrClass [] = True

justAssignOrFuncOrClass (Assign _ _:program) = justAssignOrFuncOrClass program

justAssignOrFuncOrClass (Class _ _:program) = justAssignOrFuncOrClass program

justAssignOrFuncOrClass (Function _ _ _:program) = justAssignOrFuncOrClass program

justAssignOrFuncOrClass _ = False



getBranch :: [Token] -> Int -> Pair Program [Token]
getBranch (tokens) 0 = ([], tokens)
getBranch [] _ = error "Error while making the condition"
getBranch (CloseParTok:tokens) _ = ([], tokens)
getBranch (OpenParTok:OpenParTok:tokens) counter = error "Error while opening parenthesis"
getBranch (OpenParTok:tokens) counter = getBranch tokens (-1)
getBranch tokens counter =
      case parseStm tokens of
          Just (stm, ColonTok:tokens1) -> 
             (stm:program, tokens2)
             where (program, tokens2) = getBranch (tokens1) (counter-1)
          _ -> error "Parse error"
          

buildData :: [Token] -> Pair Program [Token]
buildData [] = ([], [])
buildData tokens = 
      case parseStm tokens of
          Just (stm, ColonTok:tokens1) -> 
             (stm:program, tokens2)
             where (program, tokens2) = buildData tokens1
          _ -> error "Parse error"


parse :: String -> Program
parse programCode = 
            let
              (program, tokens) = buildData . lexer $ programCode
            in 
              if length tokens > 0 then
                 error "Error Parsing Code"
              else program

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








getPrintVariables :: [Token] -> Pair [Bexp] [Token]

getPrintVariables (CloseParTok:tokens) = ([], tokens)

getPrintVariables (QuoteTok:VarTok varName:QuoteTok:CommaTok:tokens) =
        case getPrintVariables tokens of
          (bexp, tokens1) ->
            ((BexpA (PrintStr varName)):bexp, tokens1)

getPrintVariables (QuoteTok:VarTok varName:QuoteTok:CloseParTok:tokens) = ([BexpA (PrintStr varName)], tokens)


getPrintVariables (RefTok:VarTok varName:CommaTok:tokens) = 
        case getPrintVariables tokens of
          (bexp, tokens1) ->
            (BexpA (Var ('&':varName)):bexp, tokens1)

getPrintVariables (RefTok:VarTok varName:CloseParTok:tokens) = ([BexpA (Var ('&':varName))], tokens)

getPrintVariables (elem:tokens) =
          case parseSumOrSubOrProdOrInt (elem:tokens) of
            Just (aexp, CommaTok:tokens1) ->
              case getPrintVariables tokens1 of
                (bexp, tokens2) ->
                  (BexpA aexp:bexp, tokens2)
            Just (aexp, CloseParTok:tokens1) ->
              ([BexpA aexp], tokens1)

            _ -> 
              case parseAndOrBoolEqOrNotOrEqOrIneq (elem:tokens) of
                Just (bexp, CommaTok:tokens1) ->
                  case getPrintVariables tokens1 of
                    (bexp1, tokens2) ->
                      (bexp:bexp1, tokens2)
                Just (bexp, CloseParTok:tokens1) ->
                  ([bexp], tokens1)
                _ -> error "Error while getting print variable"



getPrintVariables _ = error "Error while getting print variables"


getFunctionParam :: [Token] -> Pair [String] [Token]

getFunctionParam (CloseParTok:tokens) = ([], tokens)

getFunctionParam (VarTok varName:CloseParTok:tokens) = ([varName], tokens)

getFunctionParam (VarTok varName:CommaTok:tokens) =
        case getFunctionParam tokens of
          (str, tokens1) -> (varName:str, tokens1)

getFunctionParam _ = error "Error while getting function parameters"

