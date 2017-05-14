module JuliaIR(Register, Instruction(..), Cycle(..), schedule) where

import Control.Monad.Trans.State.Lazy

import qualified JuliaParser
import Data.UUID

type Register = Int -- 0 is 'z'
zRegRe       = 0 :: Register
zRegIm       = 1 :: Register
magnitudeReg = 2 :: Register

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

-- taken without shame from: http://stackoverflow.com/questions/22403029/how-to-zip-lists-with-different-length
zipWithPadding :: [[Instruction]] -> [[Instruction]] -> [([Instruction],[Instruction])]
zipWithPadding (x:xs) (y:ys) = (x,y) : zipWithPadding xs ys
zipWithPadding []     ys     = zip (repeat []) ys
zipWithPadding xs     []     = zip xs (repeat [])

mergeSchedules :: [[Instruction]] -> [[Instruction]] -> [[Instruction]]
mergeSchedules a b = map (\(l, r) ->  concat [l,r]) $ zipWithPadding a b

-- for getting register numbers
tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n

regFileNumElements = 128 -- 256 / 2, 256 regs times two for complex

type Complex = (Register, Register)

-- make a list of lists
-- post condition: operation is put in register file on/by the lowest-index cycle
-- return the locations of the real and complex parts
preschedule :: JuliaParser.Exp -> State Int ([[Instruction]], Complex)
preschedule (JuliaParser.Num x  ) = do
  returnRegRe <- tick
  returnRegIm <- tick
  return ([Load 0 returnRegIm]:[Load x returnRegRe]:emptySchedule,
          (returnRegRe, returnRegIm))
preschedule (JuliaParser.Cpx (JuliaParser.Num re) (JuliaParser.Num im)) = do
  returnRegRe <- tick
  returnRegIm <- tick
  return ([Load im returnRegIm]:[Load re returnRegRe]:emptySchedule,
          (returnRegRe, returnRegIm))
preschedule (JuliaParser.Var "z") = do
  return (emptySchedule, (zRegRe, zRegIm)) -- no additional cycles TODO: verify
preschedule (JuliaParser.Pos a  ) = do -- a NOP
  prescheduleA <- preschedule a
  return prescheduleA
preschedule (JuliaParser.Neg a  ) = do
  returnRegReNoOffset  <- tick
  returnRegImNoOffset  <- tick
  (subSchedule, (subReturnRe, subReturnIm)) <- preschedule a
  let returnRegRe = returnRegReNoOffset + (1*regFileNumElements) in
    let returnRegIm = returnRegImNoOffset + (1*regFileNumElements) in
      return ([Neg subReturnIm returnRegIm]:[Neg subReturnRe returnRegRe]:subSchedule,
              (returnRegRe, returnRegIm))
preschedule (JuliaParser.Add a b) = do
  returnRegReNoOffset    <- tick
  returnRegImNoOffset    <- tick
  (subA, (retARe, retAIm)) <- preschedule a
  (subB, (retBRe, retBIm)) <- preschedule b
  let returnRegRe = returnRegReNoOffset + (2*regFileNumElements) in
    let returnRegIm = returnRegImNoOffset + (2*regFileNumElements) in
    return ([]:[]:[Add retAIm retBIm returnRegIm]:[Add retARe retBRe returnRegRe]:
            (mergeSchedules subA subB), (returnRegRe, returnRegIm))
preschedule (JuliaParser.Sub a b) = do
  returnRegReNoOffset    <- tick
  returnRegImNoOffset    <- tick
  negTmpNoOffset         <- tick
  (subA, (retARe, retAIm)) <- preschedule a
  (subB, (retBRe, retBIm)) <- preschedule b
  let returnRegRe = returnRegReNoOffset + (2*regFileNumElements) in
    let returnRegIm = returnRegImNoOffset + (2*regFileNumElements) in
      let negTmp = negTmpNoOffset + (1*regFileNumElements) in
    return ([]:[]:[Add retAIm negTmp returnRegIm]:[Neg retBIm negTmp]:
            [Add retARe negTmp returnRegRe]:[Neg retBRe negTmp]:(mergeSchedules subA subB),
            (returnRegRe, returnRegIm))
preschedule (JuliaParser.Mul l r) = do
  returnRegReNoOffset    <- tick
  returnRegImNoOffset    <- tick
  mulTmp1NoOffset        <- tick
  mulTmp2NoOffset        <- tick
  negTmpNoOffset         <- tick
  (subA, (a, b)) <- preschedule r
  (subB, (c, d)) <- preschedule l
  let returnRegRe = returnRegReNoOffset + (2*regFileNumElements) in
    let returnRegIm = returnRegImNoOffset + (2*regFileNumElements) in
      let mulTmp1 = mulTmp1NoOffset + (3*regFileNumElements) in
        let mulTmp2 = mulTmp2NoOffset + (3*regFileNumElements) in
          let negTmp = negTmpNoOffset + (1*regFileNumElements) in
            return ([]:[]:[Add mulTmp1 mulTmp2 returnRegIm]:[Mul b c mulTmp2]:[Mul a d mulTmp1]:
                    [Add negTmp mulTmp1 returnRegRe]:[Neg mulTmp2 negTmp]:[Mul b d mulTmp2]:[Mul a c mulTmp1]:
                    (mergeSchedules subA subB),
                    (returnRegRe, returnRegIm))
-- preschedule (JuliaParser.Div a b) = do
--   return ([], 0)
-- preschedule (JuliaParser.Pow a b) = do
--   return ([], 0)

----------------- Save to Z ----------------

saveZMagnitude :: Complex -> [[Instruction]]
saveZMagnitude (reReg, imReg) = []:[]:[Add 4 5 3]:[Mul 0 0 4, Mul 1 1 5]: -- calc/save magnitude
                                []:[]:[Add 4 reReg 0, Add 4 imReg 1]:[Load 0 4]:[] -- save z

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
schedule input = flattenSchedule $ (saveZMagnitude zResult) ++ prescheduled
  where (prescheduled, zResult) = evalState (preschedule input) 10 -- start at reg 10
