import Lab1

sum_squares :: Int -> Int
sum_squares n = sum [m^2 | m <- [1..n]]

sum_squares' :: Int -> Int
sum_squares' n = n * (n+1) * (2*n+1) `div` 6

test_sum_squares :: Int -> Property
test_sum_squares n = n > 0 ==> sum_squares' n == sum_squares n

sum_cubes :: Int -> Int
sum_cubes n = sum [m^3 | m <- [1..n]]

sum_cubes' :: Int -> Int
sum_cubes' n = (n * (n+1) `div` 2)^2

test_sum_cubes :: Int -> Property
test_sum_cubes n = n > 0 ==> sum_cubes' n == sum_cubes n