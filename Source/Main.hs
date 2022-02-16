import Interpreter 
import Parser (parser, lexer)



interpreter :: String -> State -> State
interpreter sequence state = executor ((parser . lexer) sequence) state

{-
emptyState :: State
emptyState = []

interpreter :: String -> Maybe State -> State
interpreter s state = executor ((parser . lexer) s) $ case state of 
  Just state ->  state
  Nothing -> emptyState
  -}