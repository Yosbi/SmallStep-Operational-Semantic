module Interpreter where
import RandomGen

type Program = Statement
type Variable = String
type Value = Int
type State = [(Variable, Value)]

data Expression = Num Value
                | Sum Expression Expression
                | Diff Expression Expression
                | Mult Expression Expression
                | Var Variable
                deriving Show


data Statement = Assing Variable Expression
                | Sequence [Statement]
                | Par [Statement] [Statement]
                | Skip
                deriving Show

evaluator :: Expression -> State -> Value
evaluator expression state = case expression of
    Var x -> case lookup x state of 
                Nothing -> error "unbound variable"
                Just value -> value
    Num value -> value
    Sum expression1 expression2 -> (evaluator expression1 state) + (evaluator expression2 state)
    Diff expression1 expression2 -> (evaluator expression1 state) - (evaluator expression2 state)
    Mult expression1 expression2 ->  (evaluator expression1 state) * (evaluator expression2 state)

executor :: Statement -> State -> State
executor statement state = case statement of
    Assing variable expression -> case lookup variable state of
                                    Nothing -> (variable, evaluator expression state):state 
                                    Just _ -> map (\p@(f, _) -> if f == variable then (variable, (evaluator expression state)) else p) state
    Sequence [] -> state
    Sequence (statement : statementQueue) -> if (check statement Skip) then state
                         else executor (Sequence statementQueue) (executor statement state)
    Par s1 s2 ->  executor (Sequence (planExecution s1 s2 0)) state
    Skip -> state

planExecution :: [a] -> [a] ->Int -> [a]
planExecution (x:xs) [] _ = (x:xs)
planExecution [] (y:ys) _ = (y:ys)
planExecution (x:xs) (y:ys) num = do
                            let random = getRandomN(num)
                            if random < 50 then
                               x:planExecution xs (y:ys) random
                            else
                               y:planExecution (x:xs) ys random

check :: Statement -> Statement-> Bool
check Skip Skip = True
check _ _ = False

test :: Program
test = Sequence 
    [
        Assing "x" (Num 1),
        --Skip,
        Assing "z" (Sum (Var "x") (Var "y")),
        [Assing "x" (Num 1)] `Par` [Assing "x" (Num 2),Assing "x" (Sum (Var "x") (Num 2))]
    ]