import Data.Bits
import Data.Map (fromList, (!))
import System.Environment (getArgs)

bitty :: Int -> Int -> [Int]
bitty width n = [min 1 (n .&. x) | x <- (take width (map (2^) [0..]))]

rule = 110

ruleLookup = let ruleBitmap  = bitty 8 rule
                 ruleMatches = map (bitty 3) [0..7]
             in
                 fromList (zip ruleMatches ruleBitmap)

evolveOne (x, y, z) = ruleLookup ! [x, y, z]

evolve row = let extendedRow = [0, 0] ++ row ++ [0, 0]
                 slidingWindowTuples = (zip3 (drop 0 extendedRow)
                                             (drop 1 extendedRow)
                                             (drop 2 extendedRow))
             in map evolveOne slidingWindowTuples

stringify width s = let blankOffset = (take ((width - (length s)) `div` 2) (repeat ' '))
                        prettyState = (map (\x -> if x == 0 then ' ' else '.') s)
                    in
                        blankOffset ++ prettyState

go 0 width s = putStrLn (stringify width s)
go n width s = putStrLn (stringify width s) >> go (n - 1) width (evolve s)

