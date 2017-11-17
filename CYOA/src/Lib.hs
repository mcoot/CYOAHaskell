module Lib where

import StoryData

-- | String duplication 
duplicate :: [a] -> Int -> [a]
duplicate = flip $ (concat .) . replicate

-- | Test if a floating point value is an integer (to n decimal places to help deal with floating point errors)
isIntPrec :: (RealFrac a, Integral b) => a -> b -> Bool
isIntPrec x prec = (round $ 10^(fromIntegral prec) * (x - (fromIntegral $ round x))) == 0

-- | Test if a floating point value is an integer
isInt :: (RealFrac a) => a -> Bool
isInt = flip isIntPrec 7

-- | logical xor
xor :: Bool -> Bool -> Bool
xor p q = (p || q) && (not (p && q))

-- | logical implies
implies :: Bool -> Bool -> Bool
implies p q = (not p) || q

-- | logical iff
iff :: Bool -> Bool -> Bool
iff p q = (p `implies` q) && (q `implies` p) 