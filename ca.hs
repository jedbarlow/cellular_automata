import Data.Bits
import Data.Map

bitty :: Int -> Int -> [Int]
bitty width n = [ min 1 (n .&. x) | x <- (take width (Prelude.map (2^) [0..]))]

rule = 110

ruleBitmap = bitty 8 rule

ruleLookup = fromList (zip (Prelude.map (bitty 3) [0..7]) (bitty 8 37))

toRuleIndex previous3 = Prelude.foldr (+) 0 (zipWith (*) previous3 (take 3 (Prelude.map (2^) [0..])))
toRuleIndexT (x, y, z) = toRuleIndex [z, y, x]

evolveStep triple = ruleBitmap !! (toRuleIndexT triple)

evolve row = let state = [0, 0] ++ row ++ [0, 0]
             in Prelude.map evolveStep
                    (zip3 state
                          (drop 1 state)
                          (drop 2 state))

stringify :: Int -> [Int] -> [Char]
stringify width s = (take ((width - (length s)) `div` 2) (repeat ' ')) ++ (Prelude.map (\x -> if x == 0 then ' ' else '.') s)

go 0 _ _ = putStr ""
go n width s = let next = evolve s
         in do
              putStrLn (stringify width next)
              go (n - 1) width next


