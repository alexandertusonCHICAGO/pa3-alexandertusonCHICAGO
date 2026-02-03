import System.Environment (getArgs)
import System.Exit (die)
import Data.Char (ord, chr, isAscii, isLower, isUpper, isAlpha, toLower)
import Data.List (maximumBy)
import Data.Ord (comparing)
import System.IO.Unsafe (unsafePerformIO)


main :: IO ()
main = do
    putStrLn "Starting program..."
    args <- getArgs
    interact (processor args)
  where
    processor ("--encrypt":password:_) = \input -> format (encrypt input password)
    processor ("--decrypt":password:_) = \input -> format (decrypt input password)
    processor ("--decrypt-auto":_)    = \input -> format (decryptWithoutPassword input)
    processor _                        = \_ -> "usage: vigenere (--encrypt PASSWORD | --decrypt PASSWORD | --decrypt-auto)\n"


encrypt :: String -> String -> String
encrypt input password = 
    zipWith encipherChar cleaned keystream
    where
        cleaned = clean input
        keystream = makeKeystream password (length cleaned)


decrypt :: String -> String -> String
decrypt input password = 
    zipWith decipherChar cleaned keystream
    where
        cleaned = clean input
        keystream = makeKeystream password (length cleaned)


alphaToInt :: Char -> Int
alphaToInt c
    | isAscii c && isLower c = ord c - ord 'a'
    | isAscii c && isUpper c = ord c - ord 'A'
    | otherwise = error "alphaToInt -- unexpected character"


encipherChar :: Char -> Char -> Char
encipherChar c p = chr (ord 'A' + (alphaToInt c + alphaToInt p) `mod` 26)


decipherChar :: Char -> Char -> Char
decipherChar c p = chr (ord 'a' + (alphaToInt c - alphaToInt p) `mod` 26)


makeKeystream :: String -> Int -> String
makeKeystream key n = take n (cycle key)


clean :: String -> String
clean c = map toLower (filter isAlpha c)


format :: String -> String
format [] = []
format s  = take 40 s ++ "\n" ++ format (drop 40 s)


-- compares two strings, counts how many characters in common
common :: String -> String -> Int -> Int
common [] _ count = count
common _ [] count = count
common (x:xs) (y:ys) count
    | x == y = common xs ys (count + 1)
    | otherwise = common xs ys count


shift :: String -> Int -> String
shift message 0 = message
shift message n = shift ('.' : message) (n - 1)

-- shift message then compare to og, add value to list, repeat n - 1 times
_commonCharList :: String -> Int -> Int -> [Int]
_commonCharList message n maxshift
    | n > maxshift = []
    | otherwise = common message (shift message n) 0 : _commonCharList message (n + 1) maxshift


commonCharList :: String -> [Int]
commonCharList message = _commonCharList message 1 (length(message) - 1)


calcMean :: [Int] -> Double
calcMean nums = fromIntegral (sum nums) / fromIntegral (length nums)


calcStdev :: [Int] -> Double
calcStdev xs = sqrt $ sum (map (\x -> (fromIntegral x - m) ^ 2) xs) / fromIntegral (length xs)
  where
    m = calcMean xs


findOutliers :: [Int] -> Double -> Double -> Int -> [Int]
findOutliers list mean stdev index
    | index >= length list = []
    | fromIntegral (list !! index) < (mean + 3*stdev) = findOutliers list mean stdev (index + 1)
    | otherwise = index : findOutliers list mean stdev (index + 1)


distances :: [Int] -> [Int]
distances xs = zipWith (-) (tail xs) xs


findKeyLength :: [Int] -> Int
findKeyLength [] = error "empty list"
findKeyLength xs = foldl1 gcd xs


getPhase :: String -> Int -> Int -> Int -> [Char]
getPhase message phase index numPhases
    | index >= length message = []
    | index `mod` numPhases == phase = message !! index : getPhase message phase (index + 1) numPhases
    | otherwise = getPhase message phase (index + 1) numPhases


shiftChar :: Int -> Char
shiftChar n = chr (ord 'a' + n `mod` 26)


-- turn candidate phase into frequency vector
makeFrequencyVector :: String -> Int -> [Int]
makeFrequencyVector phase shift =
    let shifted = map (`decipherChar` shiftChar shift) phase
        letters = ['a' .. 'z']
    in [ length (filter (== l) shifted) | l <- letters ]


-- evaluate strength of candidate frequency vector (take dot product)
dot :: [Int] -> [Double] -> Double
dot xs ys = sum $ zipWith (\x y -> fromIntegral x * y) xs ys


-- converts a number 0-25 to a letter a-z
shiftToChar :: Int -> Char
shiftToChar n = chr (ord 'a' + n)


-- find most likely shift based on strongest frequency vector
findMostLikely :: [Double] -> String -> Char
findMostLikely english phase = 
    let candidates = [0..25]
        scored = [(shiftToChar shift, dot (makeFrequencyVector phase shift) english) | shift <- candidates]
        (best, _) = maximumBy (comparing snd) scored
    in best


-- assembles the key
assembleKey :: String -> Int -> [Double] -> String
assembleKey ciphertext keylen english = 
    [ findMostLikely english (getPhase ciphertext phase 0 keylen) | phase <- [0..keylen - 1]]


englishFrequency :: [Double]
englishFrequency =
  [ 8200, 1500, 2800, 4300, 12700, 2200
  , 2000, 6100, 7000, 160, 770, 4000
  , 2400, 6700, 7500, 1900, 120, 6000
  , 6300, 9100, 2800, 980, 2400, 150
  , 2000, 74
  ]


decryptWithoutPassword :: String -> String
decryptWithoutPassword input =
    let cleaned      = clean input
        coincidences = commonCharList cleaned
        meanVal      = calcMean coincidences
        stdevVal     = calcStdev coincidences
        peaks        = findOutliers coincidences meanVal stdevVal 0
        dists        = distances peaks
        keyLen       = findKeyLength dists
        key          = assembleKey cleaned keyLen englishFrequency
    in unsafePerformIO $ do
        putStrLn $ "Cleaned text: " ++ take 100 cleaned ++ "..."
        putStrLn $ "Coincidences: " ++ show coincidences
        putStrLn $ "Mean: " ++ show meanVal ++ ", Stdev: " ++ show stdevVal
        putStrLn $ "Peaks: " ++ show peaks
        putStrLn $ "Distances: " ++ show dists
        putStrLn $ "Guessed key length: " ++ show keyLen
        putStrLn $ "Guessed key: " ++ key
        return $ decrypt cleaned key













