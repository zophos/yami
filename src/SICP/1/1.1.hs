my_sqrt x = my_sqrt_iter 1.0 x


my_sqrt_iter guess x = if good_enough guess x
                       then guess
                       else my_sqrt_iter (improve guess x) x

good_enough guess x =
    if (abs ((guess * guess) - x)) < 0.001
    then True
    else False

improve guess x = average guess (x / guess)

average x y = (x + y) / 2


{-
my_sqrt_iter guess x = if (abs ((guess * guess) - x)) < 0.001
                       then guess
                       else my_sqrt_iter (improve guess x) x
-}

--- ex 1.6
new_if predicate then_clause else_clause =
    if predicate 
    then then_clause
    else else_clause

my_sqrt_iter2 guess x = new_if (good_enough guess x)
                        guess
                        (my_sqrt_iter2 (improve guess x) x)

my_sqrt2 x = my_sqrt_iter2 1.0 x

--- ex 1.7
my_sqrt3 x = my_sqrt_iter3 100000000000 1.0 x


my_sqrt_iter3 pre_guess guess x = if good_enough3 pre_guess guess
                                  then guess
                                  else my_sqrt_iter3 guess (improve guess x) x

good_enough3 pre_guess guess =
    if abs (pre_guess - guess) < 0.001
    then True
    else False



factorial 1 = 1
factorial n = n * factorial (n-1)

factorial2 n = fact_iter 1 1 n

fact_iter product counter max_count =
    if counter > max_count then product
    else fact_iter (counter * product) (counter + 1) max_count

-- main = my_sqrt 2.0

my_sqrt4 x = my_sqrt_iter4 1.0 x
             where my_sqrt_iter4 guess x =
                       if good_enough guess x
                       then guess
                       else my_sqrt_iter4 (improve guess x) x
                       
my_sqrt5 x = my_sqrt_iter5 1.0
    where my_sqrt_iter5 guess =
                        if good_enough guess x
                        then guess
                        else my_sqrt_iter5 (improve guess x)
                       
                       
                                 
