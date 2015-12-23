{-# LANGUAGE ExistentialQuantification #-}
-- Copyright © 2015 Bart Massey

import Soln

data Reg = RegA | RegB

data State = State {
      rA, rB, pc :: Int }
             deriving Show

data ALUInsn = ALUInsn {
      aluOpcode :: String,
      aluRegister :: Reg,
      aluOp :: Reg -> State -> State }

data CtlInsn = CtlInsn {
      ctlOpcode :: String,
      ctlOffset :: Int,
      ctlOp :: Int -> State -> State }

data CondInsn = CondInsn {
      condOpcode :: String,
      condRegister :: Reg,
      condOffset :: Int,
      condOp :: Reg -> Int -> State -> State }

class Insn a where
    insnOpcode :: Insn a => a -> String
    insnOp :: Insn a => a -> State -> State

instance Insn ALUInsn where
    insnOpcode = aluOpcode
    insnOp (ALUInsn {aluOp = i, aluRegister = r}) s =
        i r s

instance Insn CtlInsn where
    insnOpcode = ctlOpcode
    insnOp (CtlInsn {ctlOp = i, ctlOffset = o}) s =
        i o s

instance Insn CondInsn where
    insnOpcode = condOpcode
    insnOp (CondInsn {condOp = i, condRegister = r, condOffset = o}) s =
        i r o s

data InsnT = forall a . Insn a => InsnT a

updateALU :: (Int -> Int) -> Reg -> State -> State
updateALU fn RegA state =
    state { rA = fn (rA state),
            pc = pc state + 1 }
updateALU fn RegB state =
    state { rB = fn (rB state),
            pc = pc state + 1 }

updateCtl :: Int -> State -> State
updateCtl off state =
    state { pc = pc state + off }

updateCond :: (Int -> Bool) -> Reg -> Int -> State -> State
updateCond fn reg off state =
    case fn (readReg reg state) of
      True -> state { pc = pc state + off }
      False -> state { pc = pc state + 1 }
    where
      readReg :: Reg -> State -> Int
      readReg RegA s = rA s
      readReg RegB s = rB s

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

parseReg :: String -> Reg
parseReg "a" = RegA
parseReg "b" = RegB
parseReg _ = error "illegal register"

parseOff :: String -> Int
parseOff ('+' : off) = read off
parseOff ('-' : off) = negate (read off)
parseOff _ = error "illegal offset"

parseProgram :: String -> [InsnT]
parseProgram stuff =
    map (parseInsn . words) $ lines stuff

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
