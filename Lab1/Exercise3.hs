import Lab1

n_perms :: Int -> Int
n_perms n = length (permutations [1..n])

factorial :: Int -> Int
factorial n = product [1..n]

test_perms :: Int -> Property
test_perms n = n >= 0 ==> factorial n == n_perms n