
module Bells.Syntax.Statements where

import qualified Text.ParserCombinators.Parsec as P

data Stat
  = Sym String
  | Str String
  | Macro Stat [Stat]
  deriving (Show, Eq)

statement :: P.Parser Stat
statement = stat ""

stat :: String -> P.Parser Stat
stat indent = P.choice
       [ (macro indent)
	   , (literal indent)
	   , (blankLine indent)
       ]

args i = P.try $
	do
		; a <- stat i
		; as <- args i
		; return (a:as)
	P.<|> return []

blankLine indent = P.try $
	do {
		macro indent >>= return
	} P.<|> do {
		P.char '\n'
		; P.many $ P.char ' '
		; blankLine indent >>= return
	}

macro indent = P.try $
	do {
		; P.char '\n'
		; P.string indent
		; deep <- P.many (P.char ' ')
	    ; macro <- stat (concat [indent, deep, " "])
		; P.many $ P.char ' '
		; args <- args (concat [indent, deep, " "])
		; return (Macro macro args)
	} P.<|> do {
		; P.char '$'
		; P.many $ P.char ' '
		; macro <- stat indent
		; args <- args indent
		; return (Macro macro args)
	}
	
string i = P.try $
	do
		; str <- P.between (P.char '"') (P.char '"') scan
		; P.many $ P.char ' '
		; return (Str str)
	where
		scan = P.many $ P.noneOf "\""

symbol i = P.try $ do
	; a <- P.noneOf "$\n"
	; sym <- P.many (P.noneOf " \n")
	; P.many $ P.char ' '
	; return (Sym $ concat [[a], sym])

literal i = string i P.<|> symbol i