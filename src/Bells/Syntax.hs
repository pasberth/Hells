module Bells.Syntax where

import Bells.Syntax.Statements
import qualified Text.ParserCombinators.Parsec as P

parse fname input = case P.parse parser fname (concat ["\n", input]) of
       Left err -> do{ putStr "parse error at "
                       ; print err
                       }
       Right x  -> do { print x }
parser = do
	stats <- P.many statement
	; return (toplevelMacro stats)
	
toplevelMacro :: [Stat] -> Stat
toplevelMacro stats = (Macro (Sym "eval") stats)