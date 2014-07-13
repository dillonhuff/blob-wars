module TestUtils

%access public

data Pair a b = MkPair a b

fst : Pair a b -> a
fst (MkPair a b) = a

snd : Pair a b -> b
snd (MkPair a b) = b

testOutcome : (Eq b, Show b) => b -> b -> String
testOutcome expected actual = if expected == actual
  then ""
  else "FAILED: Expected " ++ show expected ++ " but got " ++ show actual ++ "\n"

concatStrs : Vect n String -> String
concatStrs Nil = ""
concatStrs (x::xs) = x ++ (concatStrs xs)

testFunction : (Eq b, Show b) => (a -> b) -> Vect n (Pair a b) -> String
testFunction f testCases = let inputs = map fst testCases in
  let expectedOutputs = map snd testCases in
  let results = map f inputs in
  concatStrs $ zipWith testOutcome expectedOutputs results