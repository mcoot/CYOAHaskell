module Lib (duplicate) where

import StoryData

-- | String duplication 
duplicate :: [a] -> Int -> [a]
duplicate = flip $ (concat .) . replicate