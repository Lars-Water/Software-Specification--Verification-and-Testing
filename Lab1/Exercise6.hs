import Lab1

counterexamples :: [([Integer], Integer)]
counterexamples = determine 1 []

determine :: Int -> [([Integer], Integer)] -> [([Integer], Integer)]
determine n xs =
    if  length xs > 9
        then xs
    else if  (prime $ (+) 1 prod_cons)
        then determine (n + 1) xs
    else
        determine (n + 1) (xs ++ [(take n primes, (+) 1 prod_cons)])
    where prod_cons = product $ take n primes