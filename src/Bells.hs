module Bells( bells ) where
	
import Bells.Syntax

bells fname = do
  input <- readFile fname
  ; putStrLn input
  ; parse fname input
