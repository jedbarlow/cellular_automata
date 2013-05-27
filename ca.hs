import Data.Bits
import Data.Map

bitty :: Int -> Int -> [Int]
bitty width n = [ min 1 (n .&. x) | x <- (take width (Prelude.map (2^) [0..]))]

rule = 36

ruleBitmap = bitty 8 rule

ruleLookup = fromList (zip (Prelude.map (bitty 3) [0..7]) (bitty 8 37))

toRuleIndex previous3 = Prelude.foldr (+) 0 (zipWith (*) previous3 (take 3 (Prelude.map (2^) [0..])))
toRuleIndexT (x, y, z) = toRuleIndex [x, y, z]

evolveStep triple = ruleBitmap !! (toRuleIndexT triple)

evolve row = let state = [0, 0] ++ row ++ [0, 0]
             in Prelude.map evolveStep
                    (zip3 state
                          (tail state)
                          (tail (tail state)))

go s = let next = evolve s
       in do
            print next
            go next
