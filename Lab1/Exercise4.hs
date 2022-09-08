import Lab1

reversibleStream :: [Integer]
reversibleStream = [x | x <- [1..10000], prime x, prime $ reversal x]

reversal_present :: Bool
reversal_present = all ( < 10000) reversibleStream