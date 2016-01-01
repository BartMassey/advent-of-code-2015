-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Soln

-- | Give registers symbolic names for type safety.
data Reg = RegA | RegB

-- | State of the machine.
data State = State {
      rA, rB, pc :: Int }
             deriving Show

-- | Execute an ALU instruction.
updateALU :: (Int -> Int) -> Reg -> State -> State
updateALU fn RegA state =
    state { rA = fn (rA state),
            pc = pc state + 1 }
updateALU fn RegB state =
    state { rB = fn (rB state),
            pc = pc state + 1 }

-- | Execute a control instruction.
updateCtl :: Int -> State -> State
updateCtl off state =
    state { pc = pc state + off }

-- | Execute a conditional instruction.
updateCond :: (Int -> Bool) -> Reg -> Int -> State -> State
updateCond fn reg off state =
    case fn (readReg reg state) of
      True -> state { pc = pc state + off }
      False -> state { pc = pc state + 1 }
    where
      readReg :: Reg -> State -> Int
      readReg RegA s = rA s
      readReg RegB s = rB s

-- | Parse and genericize a program's instructions.
parseInsn :: [String] -> State -> State
parseInsn ["hlf", reg] =
  updateALU (`div` 2) (parseReg reg) 
parseInsn ["inc", reg] =
  updateALU (+ 1) (parseReg reg)
parseInsn ["tpl", reg] =
  updateALU (* 3) (parseReg reg)
parseInsn ["jmp", off] =
  updateCtl (parseOff off)
parseInsn ["jie", reg, off] | last reg == ',' =
  updateCond even (parseReg (init reg)) (parseOff off)
parseInsn ["jio", reg, off] | last reg == ',' =
  updateCond (== 1) (parseReg (init reg)) (parseOff off)
parseInsn _ = error "illegal instruction"

-- | Parse a register name.
parseReg :: String -> Reg
parseReg "a" = RegA
parseReg "b" = RegB
parseReg _ = error "illegal register"

-- | Parse an offset, paying careful attention to
-- the required sign.
parseOff :: String -> Int
parseOff ('+' : off) = read off
parseOff ('-' : off) = negate (read off)
parseOff _ = error "illegal offset"

-- | Parse a whole program.
parseProgram :: String -> [State -> State]
parseProgram stuff =
    map (parseInsn . words) $ lines stuff

-- | Run given program from given starting state.
--
-- The use of `!!` here is not going to win any efficiency
-- prizes. An array would be a better plan, but Haskell
-- arrays are a pain and it doesn't matter for short runs of
-- small programs.
runProgram :: [State -> State] -> State -> State
runProgram insns state0 =
    case find terminated $ iterate runInsn state0 of
      Just state -> state
      Nothing -> error "program terminated early"
    where
      terminated state = pc state >= length insns
      runInsn state
          | pc state < 0 = error "pc off top"
          | terminated state = state
          | otherwise =
               (insns !! pc state) state

solna :: String -> IO ()
solna stuff = do
  print $ runProgram (parseProgram stuff) $ State 0 0 0

solnb :: String -> IO ()
solnb stuff = do
  print $ runProgram (parseProgram stuff) $ State 1 0 0

main :: IO ()
main = makeMain solna solnb
