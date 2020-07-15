import qualified CryptoPals
import qualified Text.Hex
import qualified Data.Text
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.Maybe
import qualified Data.Bits
import qualified GHC.Word
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Char
import qualified Data.Ord
import qualified Data.Sort
import qualified Debug.Trace
import qualified Data.ByteString.UTF8


filterCRchar a = [x | x <- a, x /= '\r']

filterSuite :: [String] -> [String]
filterSuite = lineLength . wordCount . nonAsciiCharacters
  where
    wordCount = filter (\x -> length (words x) >= 3)
    lineLength = filter (\x -> length x >= 5) 
    nonAsciiCharacters a = [ filterNonAscii x | x <- a]
      where
        filterNonAscii = filter (\x -> let ord = Data.Char.ord x in ord >= 32 && ord <= 126)

main :: IO ()
main = do
  content <- readFile "challenge4.txt"
  let lines' = lines content
  let newlines = [CryptoPals.singleCharXORBruteForce $ filterCRchar x | x <- lines']
  let outputdata = concat newlines 
  let scoreddata = scoreStrings $ filterSuite outputdata
  let sorted = Data.Sort.sortBy sortFunction scoreddata
  writeFile "output.txt" (unlines [x | (x, _) <- sorted])

scoreStrings :: [String] -> [(String, Float)]
scoreStrings strings = zip strings scores
  where 
    scores = [score x | x <- strings]

score :: String -> Float
score str = sum [ chi (weights Map.! x) (freqs Map.! x) | x <- ['a'..'z'] ++ [' ']]
  where
    weights = CryptoPals.englishCharacterFrequencies
    freqs = CryptoPals.characterFrequencies str

chi :: Float -> Float -> Float
chi w f = (f-w)^2/w

sortFunction :: (String, Float) -> (String, Float) -> Data.Ord.Ordering
sortFunction a b
  | score' > 0 = GT
  | score' == 0 = EQ
  | score' < 0 = LT
  | otherwise = LT  
  where score' = snd a - snd b

-- "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
-- XOR rotating key: ICE

-- string representing key, take key and get its bytestring value, we take message and gets byte string, repeat key  
rotatingKey :: String -> String -> Text.Hex.Text
rotatingKey key message = test
  where
    keybytes = f key
    messagebytes = f message
    f = Data.ByteString.unpack . Data.ByteString.UTF8.fromString
    tupledata = (zip messagebytes (cycle keybytes))
    xoreddata = [Data.Bits.xor x y | (x, y) <- tupledata]
    test = Text.Hex.encodeHex . Data.ByteString.pack $ xoreddata


hammingDistance :: Eq a => [a] -> [a] -> Maybe Int
hammingDistance xs ys count
  |length xs /= length ys = Nothing
  |length xs == 0 Just count
  |last xs == last ys = hammingDistance (init xs) (init ys) count




