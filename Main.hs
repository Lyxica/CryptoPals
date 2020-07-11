import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Char
import qualified Data.Ord
import qualified Data.Sort
import qualified CryptoPals

filterCRchar a = [x | x <- a, x /= '\r']

filterSuite :: [String] -> [String]
filterSuite = lineLength . wordCount . nonAsciiCharacters
  where
    wordCount = filter (\x -> length (words x) >= 3) :: [String] -> [String]
    lineLength = filter (\x -> length x >= 5)  :: [String] -> [String]
    nonAsciiCharacters a = [ filterNonAscii x | x <- a]
      where
        filterNonAscii = filter (\x -> let ord = Data.Char.ord x in ord >= 32 && ord <= 126)

main :: IO ()
main = do
  content <- readFile "challenge4.txt"
  let lines' = lines content
  let newlines = [CryptoPals.asciiSearch $ filterCRchar x | x <- lines']
  let outputdata = concat newlines 
  let scoreddata = scoreStrings $ filterSuite outputdata
  let sorted = Data.Sort.sortBy sortFunction scoreddata
  writeFile "output.txt" (unlines [x | (x, _) <- sorted])

scoreStrings :: [String] -> [(String, Float)]
scoreStrings strings = zip strings scores
  where 
    scores = [score x | x <- strings]

score :: String -> Float
score str = sum [ quad (weights Map.! x) (freqs Map.! x) | x <- ['a'..'z'] ++ [' ']]
  where
    weights = CryptoPals.createWeights
    freqs = CryptoPals.getfreqs str

quad :: Float -> Float -> Float
quad w f = (f-w)^2/w

sortFunction :: (String, Float) -> (String, Float) -> Data.Ord.Ordering
sortFunction a b
  | score' > 0 = GT
  | score' == 0 = EQ
  | score' < 0 = LT
  | otherwise = LT
  where score' = snd a - snd b