module CryptoPals where
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

englishCharacterFrequencies :: Map Char Float
englishCharacterFrequencies = Map.fromList a
  where
    a = zip letters weights
    letters = ['a'..'z'] ++ [' ']
    weights = 8.167 : 1.492 : 2.782 : 4.253 : 12.702 : 2.228 : 2.015 : 6.094 : 6.966 : 0.153 : 0.772 : 4.025 : 2.406 : 6.749 : 7.507 : 1.929 : 0.095 : 5.987 : 6.327 : 9.056 : 2.758 : 0.978 : 2.360 : 0.150 : 1.974 : 0.074 : 12.8 : []

unsafeTextToBytestring :: String -> [GHC.Word.Word8]
unsafeTextToBytestring = Data.ByteString.unpack . Data.Maybe.fromJust . Text.Hex.decodeHex . Data.Text.pack

mapXORByte :: [GHC.Word.Word8] -> [[GHC.Word.Word8]]
mapXORByte a = 
  [ map (`Data.Bits.xor` byte) a 
  | byte <- [0..255]
  ]

characterFrequencies :: String -> Map Char Float
characterFrequencies str = Map.fromList a
    where 
        alphabet = ['a'..'z'] ++ [' ']
        f = characterFrequency str
        a = [(chr, f chr) | chr <- alphabet]

characterFrequency :: String -> Char -> Float
characterFrequency a b = freq 
    where
        f = Data.Char.toLower
        len = fromIntegral $ length a
        freq = sum [1 :: Float | chr <- a, f chr == f b] / len

wordsToStrings :: [[GHC.Word.Word8]] -> [String]
wordsToStrings a = 
  [ Data.ByteString.Char8.unpack . Data.ByteString.pack $ str 
  | str <- a
  ]

singleCharXORBruteForce :: String -> [String]
singleCharXORBruteForce strs = results
  where 
    results = wordsToStrings . mapXORByte . unsafeTextToBytestring $ strs