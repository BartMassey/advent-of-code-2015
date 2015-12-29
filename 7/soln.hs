-- Copyright © 2015 Bart Massey

import qualified Data.Map.Strict as M

import Soln

-- | Value or variable.
data Term = TermConst Word16
          | TermVar String

-- | Gate calculation.
data Gate = GateConst Word16
          | GateUnary (Word16 -> Word16) Term
          | GateBinary (Word16 -> Word16 -> Word16) Term Term

-- | List of current mappings of variables to calculations.
type GateMap = M.Map String Gate

-- | Parse a single term.
parseTerm :: String -> Term
parseTerm t@(t1 : _)
    | isDigit t1 = TermConst (read t)
    | isAlpha t1 = TermVar t
    | otherwise = error "malformed term"
parseTerm _ = error "empty term"

-- | 'fromIntegral' the RHS, because Haskell has no promotions
-- and 'shiftL' and 'shiftR' require 'Int' RHS for some reason.
fir :: (Word16 -> Int -> Word16) -> (Word16 -> Word16 -> Word16)
fir f a1 a2 = f a1 (fromIntegral a2)

-- | Parse a single gate.
parseGate :: [String] -> (String, Gate)
parseGate [a, "->", r] =
    (r, GateUnary id (parseTerm a))
parseGate ["NOT", a, "->", r] =
    (r, GateUnary complement (parseTerm a))
parseGate [a1, "LSHIFT", a2, "->", r] =
    (r, GateBinary (fir shiftL) (parseTerm a1) (parseTerm a2))
parseGate [a1, "RSHIFT", a2, "->", r] =
    (r, GateBinary (fir shiftR) (parseTerm a1) (parseTerm a2))
parseGate [a1, "AND", a2, "->", r] =
    (r, GateBinary (.&.) (parseTerm a1) (parseTerm a2))
parseGate [a1, "OR", a2, "->", r] =
    (r, GateBinary (.|.) (parseTerm a1) (parseTerm a2))
parseGate _ = error "bad gate"

-- | Parse the list of gates.
parseGates :: String -> GateMap
parseGates stuff =
    M.fromList $ map parseGate $ map words $ lines stuff

-- | Evaluate a single 'Term' in the context of
-- a 'GateMap', returning a new 'GateMap' with
-- the term substituted for its value, together
-- with the value.
evalTerm :: GateMap -> Term -> (GateMap, Word16)
evalTerm circuit (TermConst n) =
    (circuit, n)
evalTerm circuit (TermVar a) =
    let (circuit', n) = eval circuit a in
    (M.insert a (GateConst n) circuit', n)

-- | Evaluate a variable in the context of
-- a 'GateMap', returning the result of the
-- evaluation plus a new 'GateMap' with
-- all substitutions made.
eval :: GateMap -> String -> (GateMap, Word16)
eval circuit target =
    case M.lookup target circuit of
      Just (GateConst n) ->
          (circuit, n)
      Just (GateUnary op a) ->
          let (circuit', n) = evalTerm circuit a in
          (circuit', op n)
      Just (GateBinary op a1 a2) ->
          let (circuit', n1) = evalTerm circuit a1 in
          let (circuit'', n2) = evalTerm circuit' a2 in
          (circuit'', op n1 n2)
      Nothing -> error "bad target"

-- | Parse the gates, solve for "a", print the answer.
solna :: String -> IO ()
solna stuff = do
  let circuit = parseGates stuff
  print $ snd $ eval circuit "a"

-- | Strategy: Evaluate for "a" in the original circuit,
-- substitute the resulting value for "b" in the original
-- circuit, evaluate for "b".
solnb :: String -> IO ()
solnb stuff = do
  let circuit = parseGates stuff
  let n = snd $ eval circuit "a"
  let circuit' = M.insert "b" (GateConst n) circuit
  print $ snd $ eval circuit' "a"

main :: IO ()
main = makeMain solna solnb
