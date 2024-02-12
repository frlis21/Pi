{-# LANGUAGE TypeOperators #-}

import Pi
import Test.HUnit

type PiBool = (() :+: ())

piNot :: PiBool -> PiBool
piNot = swapP

piIf :: (a -> a) -> PiBool :*: a -> PiBool :*: a
piIf c = distrib |.| ((id |*| c) |+| id) |.| factor

cnot :: PiBool :*: PiBool -> PiBool :*: PiBool
cnot = piIf piNot

toffoli :: PiBool :*: (PiBool :*: PiBool) -> PiBool :*: (PiBool :*: PiBool)
toffoli = piIf cnot

fullAdder :: (PiBool :*: (PiBool :*: (PiBool :*: PiBool))) -> (PiBool :*: (PiBool :*: (PiBool :*: PiBool)))
fullAdder =
  assocTL
    |.| assocTL
    |.| (assocTR |*| id)
    |.| (toffoli |*| id)
    |.| (assocTL |*| id)
    |.| assocTR
    |.| (cnot |*| swapT)
    |.| assocTR
    |.| (id |*| toffoli)
    |.| (id |*| assocTL)
    |.| (id |*| (cnot |*| id))
    |.| (id |*| assocTR)

testFullAdder :: String -> (PiBool :*: (PiBool :*: PiBool)) -> (PiBool :*: PiBool) -> Assertion
testFullAdder preface (x, (y, z)) correct = assertEqual ("for " ++ preface ++ ",") correct result
  where
    (_, (_, result)) = fullAdder (x, (y, (Right (), z)))

main :: IO Counts
main =
  runTestTT $
    test
      [ testFullAdder "000" (Right (), (Right (), Right ())) (Right (), Right ()),
        testFullAdder "100" (Left (), (Right (), Right ())) (Left (), Right ()),
        testFullAdder "010" (Right (), (Left (), Right ())) (Left (), Right ()),
        testFullAdder "110" (Left (), (Left (), Right ())) (Right (), Left ()),
        testFullAdder "001" (Right (), (Right (), Left ())) (Left (), Right ()),
        testFullAdder "101" (Left (), (Right (), Left ())) (Right (), Left ()),
        testFullAdder "011" (Right (), (Left (), Left ())) (Right (), Left ()),
        testFullAdder "111" (Left (), (Left (), Left ())) (Left (), Left ())
      ]
