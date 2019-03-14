

divisors n =[d | d <- [1..(n `div` 2)], n `rem` d == 0]
sumdivisors n=sum $ divisors n 
perfectnumber  = [n | n <- [1..10000] , n == sumdivisors n]
