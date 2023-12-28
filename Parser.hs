module Parser where

import Compiler
import Interpreter
import Data.Char (isSpace, isDigit, digitToInt, isAlpha)


stringToInt :: String -> Integer
stringToInt = foldl (\acc chr->10*acc+ toInteger (digitToInt chr)) 0


lexer :: String -> [Token]

lexer [] = []

lexer (chr:restStr)
      | isSpace chr = lexer restStr
lexer ('+':restStr) = PlusTok:lexer restStr
lexer ('-':restStr) = MinusTok:lexer restStr
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
lexer ('d':'o':restStr) = DoTok:lexer restStr

lexer str@(chr : _)
      | isDigit chr = IntTok (stringToInt digitStr) : lexer restStr
      where
        (digitStr, restStr) = break (not . isDigit) str

lexer str@(chr : _)
      | isAlpha chr = VarTok varStr : lexer restStr
      where (varStr, restStr) = break (not . isAlpha) str

lexer (chr : restStr) = error ("unexpected character: '" ++ show chr ++ "'")

parseAexpType :: [Token] -> Maybe (Aexp, [Token])
parseAexpType (TrueTok:tokens) =
      Just (T, tokens)

parseAexpType (FalseTok:tokens) =
      Just (F, tokens)


parseAexpType (IntTok elem:tokens) =
      Just (Const elem, tokens)

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
      case parseAexpType tokens of
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

        Just (aexp1, MinusTok:tokens1) ->
          case parseSumOrSubOrProdOrInt tokens1 of
            Just(aexp2, tokens2) ->
              Just(SUBexp aexp1 aexp2, tokens2)
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



isValidConditionFormat :: [Token] -> Int -> Bool
isValidConditionFormat _ 0 = False
isValidConditionFormat (ThenTok:_) _ = True
isValidConditionFormat (ElseTok:_) _ = True
isValidConditionFormat (DoTok :_) _ = True
isValidConditionFormat [] _ = True
isValidConditionFormat (IntTok elem:tokens) (counter) = isValidConditionFormat tokens counter
isValidConditionFormat (VarTok elem:tokens) (counter) = isValidConditionFormat tokens counter
isValidConditionFormat (_:tokens) (counter) = isValidConditionFormat tokens (counter-1)

branchFormat :: [Token] -> Pair Program [Token]
branchFormat (CloseParTok:_) = error "Bad Format of Conditional" 
branchFormat tokens = 
      case getBranch tokens 1 of
        (stm, tokens1) -> (stm, tokens1)



parseStm :: [Token] -> Maybe (Stm, [Token])

parseStm [] = Nothing

parseStm (IfTok:OpenParTok:tokens) = 
      case parseAndOrBoolEqOrNotOrEqOrIneq tokens of
        Just (bexp, CloseParTok:ThenTok:tokens1) ->
          case branchFormat tokens1 of
            (stm1, ElseTok:tokens2) ->
              case branchFormat tokens2 of
                (stm2, tokens3) ->
                  Just (Conditional (bexp) (stm1) (stm2), ColonTok:tokens3)
            (stm1, tokens2) ->
              Just (Conditional (bexp) (stm1) [], ColonTok:tokens2)
        
        result -> Nothing

parseStm (IfTok:tokens)
      | isValidConditionFormat (tokens) (2) == True =
              case parseAndOrBoolEqOrNotOrEqOrIneq tokens of
                Just (bexp, ThenTok:tokens1) ->
                  case branchFormat tokens1 of
                    (stm1, ElseTok:tokens2) ->
                      case branchFormat tokens2 of
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

parseStm (WhileTok:OpenParTok:tokens) = 
          case parseAndOrBoolEqOrNotOrEqOrIneq tokens of
              Just (bexp, CloseParTok:DoTok:tokens1) ->
                case branchFormat tokens1 of
                  (stm1, tokens2) ->
                    Just (Lp (bexp) (stm1), ColonTok:tokens2)
              result -> Nothing

parseStm (WhileTok:tokens) 
  |isValidConditionFormat tokens 2 == True =
    case parseAndOrBoolEqOrNotOrEqOrIneq tokens of
      Just (bexp,DoTok:tokens1) ->
        case branchFormat tokens1 of
          (stm1, tokens2) ->
              Just (Lp (bexp) (stm1), ColonTok:tokens2)
      result -> error "parse error"

parseStm _ = Nothing




getBranch :: [Token] -> Int -> Pair Program [Token]
getBranch (tokens) 0 = ([], tokens)
getBranch [] _ = error "Error while making the condition"
getBranch (CloseParTok:tokens) _ = ([], tokens)
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