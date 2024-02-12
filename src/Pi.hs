{-# LANGUAGE TypeOperators #-}

module Pi
  ( (:+:),
    (:*:),
    (|.|),
    (|+|),
    (|*|),
    swapP,
    assocPL,
    assocPR,
    swapT,
    assocTL,
    assocTR,
    unitE,
    unitI,
    distrib,
    factor,
  )
where

import Data.Bifunctor

type (:+:) = Either

type (:*:) = (,)

infixl 6 |.|

infixl 6 |+|

infixl 7 |*|

(|.|) :: (a -> b) -> (b -> c) -> (a -> c)
f |.| g = g . f

(|+|) :: (a -> b) -> (c -> d) -> (a :+: c -> b :+: d)
(|+|) = bimap

(|*|) :: (a -> b) -> (c -> d) -> (a :*: c -> b :*: d)
(|*|) = bimap

-- Disjunctive/Additive
swapP :: a :+: b -> b :+: a
swapP = Right `either` Left

assocPL :: a :+: (b :+: c) -> (a :+: b) :+: c
assocPL = (Left . Left) `either` ((Left . Right) `either` (Right))

assocPR :: (a :+: b) :+: c -> a :+: (b :+: c)
assocPR = ((Left) `either` (Right . Left)) `either` (Right . Right)

-- Conjunctive/Multiplicative
swapT :: a :*: b -> b :*: a
swapT (x, y) = (y, x)

assocTL :: a :*: (b :*: c) -> (a :*: b) :*: c
assocTL (x, (y, z)) = ((x, y), z)

assocTR :: (a :*: b) :*: c -> a :*: (b :*: c)
assocTR ((x, y), z) = (x, (y, z))

unitE :: () :*: b -> b
unitE ((), x) = x

unitI :: b -> () :*: b
unitI x = ((), x)

-- Distributivity and factoring
distrib :: (a :+: b) :*: c -> (a :*: c) :+: (b :*: c)
distrib (Left x, y) = Left (x, y)
distrib (Right x, y) = Right (x, y)

factor :: (a :*: c) :+: (b :*: c) -> (a :+: b) :*: c
factor (Left (x, y)) = (Left x, y)
factor (Right (x, y)) = (Right x, y)
