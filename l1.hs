
euler n=sum[1|1<-gcd n<$>[1..n]]

divisors n =[d | d <- [1..(n `div` 2], n `rem` d == 0]
suma n = sum[euler x|x<- divisors n]
sumdivisors n=sum $ divisors n 

newton n k = if k == 0 || k==n
then 1
else newton (n-1) (k-1) + newton (n-1) k

pythagoreantriple y = [(n^2-m^2,2*n*m,n^2+m^2) | n <- [1..y], m <- [1..n-1], gcd n m == 1, odd (n+m), (n*n-m*m) <=y ]



fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibbonacifast n = take n fibs

fibbonaci n | n==0 =0 | n==1 = 1 | n > 1 = fibbonaci (n-1) + fibbonaci (n-2)

fibbonacilist n = map fibbonaci[1..n]

divisors n =[d | d <- [1..(n `div` 2)], n `rem` d == 0]
sumdivisors n=sum $ divisors n 
perfectnumber  = [n | n <- [1..10000] , n == sumdivisors n]
