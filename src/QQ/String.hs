{-# LANGUAGE NamedFieldPuns, RecordWildCards, TemplateHaskell, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Interpolated here docs
module QQ.String (istring) where

import Control.Monad.State
import Data.Char

import Language.Haskell.Meta
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Text.Parsec
import Text.Parsec.String

data StringPart = Lit String | Esc Char | Anti (Q Exp)

data HsChompState = HsChompState { quoteState :: QuoteState
                                 , braceCt :: Int
                                 , consumed :: String
                                 , prevCharWasIdentChar :: Bool
                                 }

data QuoteState = None | Single EscapeState | Double EscapeState

data EscapeState = Escaped | Unescaped

-- | Quote a here doc with embedded antiquoted expressions
--
-- Any expression occurring between @${@ and @}@ (for which the type must have
-- 'Show' and 'Typeable' instances) will be interpolated into the quoted
-- string.
--
-- Characters preceded by a backslash are treated literally. This enables the
-- inclusion of the literal substring @${@ within your quoted text by writing
-- it as @\\${@. The literal sequence @\\${@ may be written as @\\\\${@.
istring :: QuasiQuoter
istring = QuasiQuoter {quoteExp = quoteInterp . dropWhile (== '\n') }

quoteInterp :: String -> Q Exp
quoteInterp s = either (handleError s) combineParts (parseInterp s)

handleError :: String -> ParseError -> Q Exp
handleError expStr parseError = error $
  "Failed to parse interpolated expression in string: "
    ++ expStr
    ++ "\n"
    ++ show parseError

combineParts :: [StringPart] -> Q Exp
combineParts = foldr (\x y -> [|$x <> $y|]) [|""|] . fmap toExpQ
  where
    toExpQ = \ case
        Lit s -> stringE s
        Esc c -> stringE [c]
        Anti expq -> expq

parseInterp :: String -> Either ParseError [StringPart]
parseInterp = parse p_interp ""

p_interp :: Parser [StringPart]
p_interp = manyTill p_stringPart eof

p_stringPart :: Parser StringPart
p_stringPart = p_anti <|> p_esc <|> p_lit

p_anti :: Parser StringPart
p_anti = Anti <$> between (try p_antiOpen) p_antiClose p_antiExpr

p_antiOpen :: Parser String
p_antiOpen = string "${"

p_antiClose :: Parser String
p_antiClose = string "}"

p_antiExpr :: Parser (Q Exp)
p_antiExpr = p_untilUnbalancedCloseBrace >>= either fail (return . return) . parseExp

p_untilUnbalancedCloseBrace :: Parser String
p_untilUnbalancedCloseBrace = evalStateT go $ HsChompState None 0 "" False
  where
    go = do
      c <- lift anyChar
      modify $ \st@HsChompState {consumed} -> st {consumed = c:consumed}
      HsChompState {..} <- get
      let next = setIdentifierCharState c >> go
      case quoteState of
        None -> case c of
          '{' -> incBraceCt 1 >> next
          '}' | braceCt > 0 -> incBraceCt (-1) >> next
              | otherwise -> stepBack >> return (reverse $ tail consumed)
          '\'' -> unless prevCharWasIdentChar (setQuoteState $ Single Unescaped)
               >> next
          '"' -> setQuoteState (Double Unescaped) >> next
          _ -> next
        Single Unescaped -> do case c of '\\' -> setQuoteState (Single Escaped)
                                         '\'' -> setQuoteState None
                                         _ -> return ()
                               next
        Single Escaped -> setQuoteState (Single Unescaped) >> next
        Double Unescaped -> do case c of '\\' -> setQuoteState (Double Escaped)
                                         '"' -> setQuoteState None
                                         _ -> return ()
                               next
        Double Escaped -> setQuoteState (Double Unescaped) >> next
    stepBack = lift do
      _ <- updateParserState \s -> s {statePos = incSourceColumn (statePos s) (-1)}
      getInput >>= setInput . ('}':)
    incBraceCt n = modify $ \st@HsChompState {braceCt} ->
      st {braceCt = braceCt + n}
    setQuoteState qs = modify $ \st -> st {quoteState = qs}
    setIdentifierCharState c = modify $ \st ->
      st {prevCharWasIdentChar = or [isLetter c, isDigit c, c == '_', c == '\'']}

p_esc :: Parser StringPart
p_esc = Esc <$> (char '\\' *> anyChar)

p_lit :: Parser StringPart
p_lit = fmap Lit $
  try (litCharTil $ try $ lookAhead p_antiOpen <|> lookAhead (string "\\"))
    <|> litCharTil eof
  where litCharTil = manyTill $ noneOf ['\\']
