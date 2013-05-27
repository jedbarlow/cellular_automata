import Data.Bits
import Data.Map (fromList, (!))
import System.Environment (getArgs)

bitty :: Int -> Int -> [Int]
bitty width n = [min 1 (n .&. x) | x <- (take width (map (2^) [0..]))]

rule = 110

ruleLookup = fromList (zip ruleMatches ruleBitmap)
             where ruleBitmap  = bitty 8 rule
                   ruleMatches = map (bitty 3) [0..7]

evolveOne (x, y, z) = ruleLookup ! [x, y, z]

evolve row = map evolveOne slidingWindowTuples
             where slidingWindowTuples = (zip3 (drop 0 extendedRow)
                                               (drop 1 extendedRow)
                                               (drop 2 extendedRow))
                   extendedRow = [0, 0] ++ row ++ [0, 0]

stringify width s = blankOffset ++ prettyState
                    where blankOffset = (take ((width - (length s)) `div` 2) (repeat ' '))
                          prettyState = (map (\x -> if x == 0 then ' ' else '.') s)


go 0 width s = putStrLn (stringify width s)
go n width s = putStrLn (stringify width s) >> go (n - 1) width (evolve s)

