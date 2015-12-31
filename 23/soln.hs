-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

{-# LANGUAGE ExistentialQuantification #-}

import Soln

-- | Give registers symbolic names for type safety.
data Reg = RegA | RegB

-- | State of the machine.
data State = State {
      rA, rB, pc :: Int }
             deriving Show

-- | Form of an arithmetic instruction.
data ALUInsn = ALUInsn {
      aluOpcode :: String,
      aluRegister :: Reg,
      aluOp :: Reg -> State -> State }

-- | Form of a control instruction.
data CtlInsn = CtlInsn {
      ctlOpcode :: String,
      ctlOffset :: Int,
      ctlOp :: Int -> State -> State }

-- | Form of a conditional instruction.
data CondInsn = CondInsn {
      condOpcode :: String,
      condRegister :: Reg,
      condOffset :: Int,
      condOp :: Reg -> Int -> State -> State }

-- | Form of instructions in general.
class Insn a where
    insnOpcode :: a -> String
    insnOp :: a -> State -> State

-- | How to view an ALU instruction.
instance Insn ALUInsn where
    insnOpcode = aluOpcode
    insnOp (ALUInsn {aluOp = i, aluRegister = r}) s =
        i r s

-- | How to view a control instruction.
instance Insn CtlInsn where
    insnOpcode = ctlOpcode
    insnOp (CtlInsn {ctlOp = i, ctlOffset = o}) s =
        i o s

-- | How to view a conditional instruction.
instance Insn CondInsn where
    insnOpcode = condOpcode
    insnOp (CondInsn {condOp = i, condRegister = r, condOffset = o}) s =
        i r o s

-- | Type of generic instructions.
data InsnT = forall a . Insn a => InsnT a

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
parseInsn :: [String] -> InsnT
parseInsn ["hlf", reg] =
  InsnT $ ALUInsn "hlf" (parseReg reg) (updateALU (`div` 2))
parseInsn ["inc", reg] =
  InsnT $ ALUInsn "inc" (parseReg reg) (updateALU (+ 1))
parseInsn ["tpl", reg] =
  InsnT $ ALUInsn "tpl" (parseReg reg) (updateALU (* 3))
parseInsn ["jmp", off] =
  InsnT $ CtlInsn "jmp" (parseOff off) updateCtl
parseInsn ["jie", reg, off] | last reg == ',' =
  InsnT $ CondInsn "jie" (parseReg (init reg)) (parseOff off)
          (updateCond even)
parseInsn ["jio", reg, off] | last reg == ',' =
  InsnT $ CondInsn "jio" (parseReg (init reg)) (parseOff off)
          (updateCond (== 1))
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
parseProgram :: String -> [InsnT]
parseProgram stuff =
    map (parseInsn . words) $ lines stuff

-- | Run given program from given starting state.
--
-- The use of `!!` here is not going to win any efficiency
-- prizes. An array would be a better plan, but Haskell
-- arrays are a pain and it doesn't matter for short runs of
-- small programs.
runProgram :: [InsnT] -> State -> State
runProgram insns state
    | pc state < 0 = error "pc off top"
    | pc state >= length insns = state
    | otherwise =
        case insns !! pc state of
          InsnT insn -> runProgram insns (insnOp insn state)

solna :: String -> IO ()
solna stuff = do
  print $ runProgram (parseProgram stuff) $ State 0 0 0

solnb :: String -> IO ()
solnb stuff = do
  print $ runProgram (parseProgram stuff) $ State 1 0 0

main :: IO ()
main = makeMain solna solnb
