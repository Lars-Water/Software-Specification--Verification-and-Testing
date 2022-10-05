import Lab1

consecutive101Prime :: Integer
consecutive101Prime = determine 101

determine :: Int -> Integer
determine i =
    if prime $ sum cons_primes
        then sum cons_primes
    else
        determine $ i+1
    where cons_primes = take 101 $ reverse $ take i primes