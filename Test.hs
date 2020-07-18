import qualified Data.List
import qualified Data.Maybe
import qualified Debug.Trace
type LwCodeBook = [String]
type LwEncoding = String

--lwCompress :: String -> LwComponents -> LwEncoding
--lwCompress i c = 

findSubset needle haystack
    | has = True
    | otherwise = False
    where
       has = needle `elem` haystack 

{-
(
    [
        "A"   -- 0
        "AB"  -- 1
        "ABA" -- 2
    ], 
    [
        2,
        2,
        0
        -- ABAABAA
    ]
    )
-}

{-

findUniqueSubset :: String -> [String] -> Int
findUniqueSubset = 

findUniqueSubset' :: String -> Int

-}

findUniqueSubset :: LwCodeBook -> String -> String
findUniqueSubset cb t = findUniqueSubset' (Debug.Trace.traceShow ("cb ", cb) cb) (tail (Debug.Trace.traceShow ("t ", t) t)) [head t]

findUniqueSubset' :: LwCodeBook -> String -> String -> String
findUniqueSubset' [] _ n = n
findUniqueSubset' cb (t:ts) n
    | exists = findUniqueSubset' cb ts (n ++ [t])
    | otherwise = n
    where 
        exists = Data.List.elem n cb

abc :: String -> String
abc = abc' []

abc' :: LwCodeBook -> String -> String
abc' codebook [] = []
abc' codebook target = abc' (codebook ++ [nc]) (drop l target) ++ encoded
    where
        nc = findUniqueSubset codebook target
        c = head' nc
        l = length nc
        k = Data.List.elemIndex (head' nc) codebook
        encoded = show k ++ nc

head' :: [a] -> [a]
head' a = (take $ length a - 1) a