module Common (module Common {-, module IOExts-}, module Colors) where

import Colors
-- import IOExts(trace)
import Debug.Trace (trace)
import qualified Data.IntMap as IntMap
import Char(isSpace)
import GHC.Float(formatRealFloat, FFFormat(FFFixed))
import List

-- | return a list of all cartesian products for a list of lists
--   e.g. products [[1,2],[3,4]] = [[1,3],[1,4],[2,3],[2,4]]
products :: [[a]] -> [[a]]
products [] = [[]]
products (xs:xss) = [ x:prod | x <- xs, prod <- products xss]

trees :: Show a => String -> a -> a
trees msg a = trace ("{" ++ msg ++ ":" ++ show a ++ "}") a

foreach :: Monad m => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

foreach_ :: Monad m => [a] -> (a -> m b) -> m ()
foreach_ list fun = do
    mapM fun list
    return ()

ifJust :: Monad m => Maybe a -> (a -> m b) -> m ()
ifJust ma f =
    case ma of
        Nothing -> return ()
        Just a  -> do { f a; return () }

internalError :: String -> String -> String -> a

internalError moduleName functionName errorString =
    error (moduleName ++ "." ++ functionName ++ ": " ++ errorString)

parseDouble :: String -> Maybe Double
parseDouble string =
    case reads (commasToDots . trim $ string) of
        ((double, []):_) -> Just double
        _                -> Nothing
  where
    commasToDots = map (\c -> if c == ',' then '.' else c)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | A NumberMap maps integers to integers
type NumberMap = IntMap.IntMap Int

-- | A NumberMap can be inverted (keys become values and values become keys)
invertMap :: NumberMap -> NumberMap
invertMap theMap =
    let list = IntMap.toList theMap
        invertedList = map (\(x, y) -> (y, x)) list
    in IntMap.fromList invertedList

-- | commasAnd combines a list of strings to one string by placing
--   commas in between and the word "and" just before the last element
commasAnd :: [String] -> String
commasAnd [] = ""
commasAnd [x] = x
commasAnd [x, y] = x ++ " and " ++ y
commasAnd (x:xs) = x ++ ", " ++ commasAnd xs


-- TODO: is niceFloat 2 0.0001 = "0.0" correct? (as opposed to "0.00")
-- | niceFloat prints a floating-point value with maximum
--   number of decimals
niceFloat :: Int -> Double -> String
niceFloat nrOfDigits f =
    let s = formatRealFloat FFFixed (Just nrOfDigits) f
        s' = reverse s -- s -- dropWhile (== '0') (reverse s)
        s'' = if head s' == '.' then '0':s' else s'
    in reverse s''

-- | niceFloatFix prints a floating-point value with fixed
--   number of decimals
niceFloatFix :: Int -> Double -> String
niceFloatFix nrOfDigits f =
    let s = formatRealFloat FFFixed (Just nrOfDigits) f
    in  if head s == '.' then '0':s else s

-- Compute the average of a list of fractionals, with average [] equal to 0.
average :: Fractional a => [a] -> a
average [] = 0
average xs = (sum xs) / fromIntegral (length xs)

-- | updateList changes the element at the given zero-based index in a list
--   Example: updateList 2 "yes" ["no","maybe","often","always"] ==>
--                    ["no","maybe","yes","always"]
updateList :: Int -> a -> [a] -> [a]
updateList i x l = take i l ++ [x] ++ drop (i+1) l

-- | groups splits a list into groups of given length. The
--   last group might be shorter.
--   Example: groups 3 [1..10] ==> [[1,2,3],[4,5,6],[7,8,9],[10]]
groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n xs = let (col, rest) = splitAt n xs
              in  col: groups n rest

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- remove the extension from a file name (or path).
removeExtension :: String -> String
removeExtension filename =
  case break (=='.') $ reverse filename of
    (_ , _ {- dot -}:properName) -> reverse properName
    (_ , []) -> filename

tabDelimited :: [[String]] -> String
tabDelimited = unlines . map (concat . intersperse "\t")

singleton :: a -> [a]
singleton x = [x]

-- | a version of Prelude.lookup that fails when the element is not present in the assoc-list
unsafeLookup :: (Show k, Eq k) => k -> [(k,v)] -> v
unsafeLookup x assocs =
  case lookup x assocs of
    Just v  -> v
    Nothing -> internalError "Common" "unsafeLookup" ("element " ++ show x ++ " not in list.")

-- | a version of Prelude.elemIndex that fails when the element is not present in the list
unsafeElemIndex :: (Show a, Eq a) => a -> [a] -> Int
unsafeElemIndex x xs =
  case elemIndex x xs of
    Just i  -> i
    Nothing -> internalError "Common" "unsafeElemIndex" ("element " ++ show x ++ " not in list")

-- Approximately equals
(~=) :: Double -> Double -> Bool
(~=) d1 d2 = abs (d1 - d2) < 0.000001

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

safeIndex :: String -> [a] -> Int -> a
safeIndex msg xs i
    | i >= 0 && i < length xs = xs !! i
    | otherwise = internalError "Common" "safeIndex" msg

-- reorderList [0,2,1] "hoi" ==> "hio"
reorderList :: Show a => [Int] -> [a] -> [a]
reorderList order xs
    | sort order /= [0..length xs-1] =
        internalError "Common" "reorderList" ("order = " ++ show order ++ ", list = " ++ show xs)
    | otherwise =
        [ xs !! i | i <- order ]
