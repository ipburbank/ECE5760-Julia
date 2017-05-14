module JuliaAssembler(assemble) where

import qualified Data.BitVector as BV
import Data.LargeWord

import qualified JuliaIR as IR

encodeLoad :: Maybe IR.Instruction -> BV.BV
encodeLoad (Just(IR.Load val reg)) = (BV.bitVec 1  1  ) BV.#
                                     (BV.bitVec 27 val) BV.#
                                     (BV.bitVec 10 reg)
encodeLoad Nothing                 = (BV.bitVec 1  0  ) BV.#
                                     (BV.bitVec 27 0  ) BV.#
                                     (BV.bitVec 10 0  )

encodeNeg :: Maybe IR.Instruction -> BV.BV
encodeNeg (Just(IR.Neg src dest))  = (BV.bitVec 1  1   ) BV.#
                                     (BV.bitVec 10 src ) BV.#
                                     (BV.bitVec 10 dest)
encodeNeg Nothing                  = (BV.bitVec 1  0   ) BV.#
                                     (BV.bitVec 10 0   ) BV.#
                                     (BV.bitVec 10 0   )

encodeAdd :: Maybe IR.Instruction -> BV.BV
encodeAdd (Just(IR.Add src1 src2 dest)) = (BV.bitVec 1  1   ) BV.#
                                          (BV.bitVec 10 src1) BV.#
                                          (BV.bitVec 10 src2) BV.#
                                          (BV.bitVec 10 dest)
encodeAdd Nothing                       = (BV.bitVec 1  0   ) BV.#
                                          (BV.bitVec 10 0   ) BV.#
                                          (BV.bitVec 10 0   ) BV.#
                                          (BV.bitVec 10 0   )

encodeMul :: Maybe IR.Instruction -> BV.BV
encodeMul (Just(IR.Mul src1 src2 dest)) = (BV.bitVec 1  1   ) BV.#
                                          (BV.bitVec 10 src1) BV.#
                                          (BV.bitVec 10 src2) BV.#
                                          (BV.bitVec 10 dest)
encodeMul Nothing                       = (BV.bitVec 1  0   ) BV.#
                                          (BV.bitVec 10 0   ) BV.#
                                          (BV.bitVec 10 0   ) BV.#
                                          (BV.bitVec 10 0   )

encodeCycle :: IR.Cycle -> Word128
encodeCycle cyc = fromIntegral ((encodeLoad (IR.load cyc)) BV.#
                                (encodeNeg  (IR.neg  cyc)) BV.#
                                (encodeAdd  (IR.add  cyc)) BV.#
                                (encodeMul  (IR.mul  cyc)))

assemble :: [IR.Cycle] -> [Word128]
assemble cycles = map encodeCycle cycles
