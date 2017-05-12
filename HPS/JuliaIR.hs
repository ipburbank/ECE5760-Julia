module JuliaIR(Register, Instruction, schedule) where

import Control.Monad.Trans.State.Lazy

import qualified JuliaParser
import Data.UUID

type Register = Int -- 0 is 'z'
zReg = 1 :: Register

data Instruction =
  --     SRC1     SRC2     DEST
    Add  Register Register Register
  | Mul  Register Register Register
  --     SRC      DEST
  | Neg  Register Register
  --     Val      DEST
  | Load Int      Register
  --     DEST
  | Var  Register
                 deriving (Show)

----------------- PRESCHEDULE -----------------
emptySchedule :: [[Instruction]]
emptySchedule = [] -- repeat([])

mergeSchedules :: [[Instruction]] -> [[Instruction]] -> [[Instruction]]
mergeSchedules a b = map (\(l, r) ->  concat [l,r]) $ zip a b

-- for getting register numbers
tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n

-- make a list of lists
-- post condition: operation is put in register file on/by the lowest-index cycle
preschedule :: JuliaParser.Exp -> State Int ([[Instruction]], Register)
preschedule (JuliaParser.Num x  ) = do
  returnReg <- tick
  return ([Load x returnReg]:emptySchedule, returnReg)
preschedule (JuliaParser.Var "z") = do
  return ([]:emptySchedule, zReg) -- no additional cycles TODO: verify
preschedule (JuliaParser.Pos a  ) = do -- a NOP
  prescheduleA <- preschedule a
  return prescheduleA
preschedule (JuliaParser.Neg a  ) = do
  returnReg  <- tick
  (subSchedule, subReturn) <- preschedule a
  return ([Neg subReturn returnReg]:subSchedule, returnReg)
preschedule (JuliaParser.Add a b) = do
  returnReg    <- tick
  (subA, retA) <- preschedule a
  (subB, retB) <- preschedule b
  return ([]:[]:[Add retA retB returnReg]:(mergeSchedules subA subB), returnReg)
preschedule (JuliaParser.Sub a b) = do
  returnReg    <- tick
  (subA, retA) <- preschedule a
  (subB, retB) <- preschedule b
  return ([]:[]:[Add retA retB returnReg]:[Neg retB retB]:(mergeSchedules subA subB),
       returnReg)
preschedule (JuliaParser.Mul a b) = do
  returnReg    <- tick
  (subA, retA) <- preschedule a
  (subB, retB) <- preschedule b
  return ([Mul retA retB returnReg]:(mergeSchedules subA subB), returnReg)
preschedule (JuliaParser.Div a b) = do
  return ([], 0)
preschedule (JuliaParser.Pow a b) = do
  return ([], 0)

----------------- FLATTEN SCHEDULE -----------------

data Cycle = Cycle { load :: Maybe Instruction
                   , add  :: Maybe Instruction
                   , mul  :: Maybe Instruction
                   , neg  :: Maybe Instruction
                   } deriving (Show)

nopCycle = Cycle{ load = Nothing
                , add  = Nothing
                , mul  = Nothing
                , neg  = Nothing
                }

instructionToCycle :: Instruction -> Cycle
instructionToCycle instruction@(Add _ _ _) = Cycle{ add  = Just instruction
                                                  , mul  = Nothing
                                                  , load = Nothing
                                                  , neg  = Nothing
                                                  }
instructionToCycle instruction@(Mul _ _ _) = Cycle{ add  = Nothing
                                                  , mul  = Just instruction
                                                  , load = Nothing
                                                  , neg  = Nothing
                                                  }
instructionToCycle instruction@(Load _ _)  = Cycle{ add  = Nothing
                                                  , mul  = Nothing
                                                  , load = Just instruction
                                                  , neg  = Nothing
                                                  }
instructionToCycle instruction@(Neg _ _)   = Cycle{ add  = Nothing
                                                  , mul  = Nothing
                                                  , load = Nothing
                                                  , neg  = Just instruction
                                                  }


scheduleNext :: ([Cycle], [Instruction]) -> [Instruction] -> ([Cycle], [Instruction])
scheduleNext (scheduled, unscheduled) newInstructions =
  case unscheduled ++ newInstructions of
    []   -> (scheduled ++ [nopCycle],             [])
    x:xs -> (scheduled ++ [instructionToCycle x], xs)

flattenSchedule :: [[Instruction]] -> [Cycle]
flattenSchedule prescheduled =
  let (scheduled, unscheduled) = foldl scheduleNext ([], []) prescheduled in
    scheduled ++ (map instructionToCycle unscheduled)

----------------- SCHEDULE -----------------

schedule :: JuliaParser.Exp -> [Cycle]
schedule input = flattenSchedule prescheduled
  where (prescheduled, _) = evalState (preschedule input) zReg -- start at reg 1
