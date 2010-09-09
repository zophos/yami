fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n-2)

fibs = 1:1:[a + b|(a,b) <- zip fibs (tail fibs)]


fib2 n = fib2_iter 1 0 n

fib2_iter a b 0 = b
fib2_iter a b count = fib2_iter (a + b) a (count - 1)


pascal_tri 1 1 = 1
pascal_tri k 1 = 1
pascal_tri k n 
           | k == n = 1
           | otherwise = pascal_tri (k -1) n + pascal_tri (k-1) (n-1)

count_change amount =
    cc amount 5

cc 0 kinds_of_coins = 1
cc amount 0 = 0
cc amount kinds_of_coins 
   | amount < 0 = 0
   | otherwise =  cc amount (kinds_of_coins - 1)
                  + cc (amount - first_denomination kinds_of_coins) kinds_of_coins
                    

first_denomination kinds_of_coins 
    | kinds_of_coins == 1 = 1
    | kinds_of_coins == 2 = 5
    | kinds_of_coins == 3 = 10
    | kinds_of_coins == 4 = 25
    | kinds_of_coins == 5 = 50

     
cube x = x * x * x

p x = 3 * x - 4 * cube x

sine angle = if not (abs angle > 0.1) 
             then angle
             else p (sine (angle / 3.0))
     

expt b n 
         | n == 0 = 1
         | otherwise = b * expt b (n - 1)

expt2 b n = expt2_iter b n 1

expt2_iter b counter product 
     | counter == 0 = product
     | otherwise = expt2_iter b (counter - 1) (b * product)

square n = n * n

fast_expt b n 
    | n == 0 = 1
    | even n = square (fast_expt b (quot n 2))
    | otherwise = (b * fast_expt b (n - 1))

fast_expt2 b n = fast_expt2_iter b n 1

fast_expt2_iter b n a
    | n == 0 = a
    | even n = fast_expt2_iter (b * b) (quot n 2) a
    | otherwise = fast_expt2_iter b (n - 1) (a * b)

mul a b
    | b == 0 = 0
    | otherwise = a + (mul a (b -1))

halve = quot
double n = n * 2

mul2 a b 
    | b == 0 = 0
    | even b = (double (mul a (halve b 2)))
    | otherwise = (a + mul a (b -1))

mul3 a b = mul3_iter a b 0
           where 
             mul3_iter a b z
                       | b == 0 = z
                       | even b = mul3_iter (double a) (halve b 2) z
                       | otherwise = mul3_iter a (b - 1) (z + a)
                       
mygcd a b 
    | b == 0 = a
    | otherwise = mygcd b (rem a b)
