module Physics.Utilities.Basics where

-- Swaps first and second arguments of a function
swap1_2 : (a -> b -> c ) -> (b -> a -> c)
swap1_2 = flip

-- Swaps first and third arguments of a function
swap1_3 : (c -> b -> a -> d) -> (a -> b -> c -> d)
swap1_3 f x y z = f z y x

-- Swaps first and second arguments of a function
swap2_1 : (a -> b -> c) -> (b -> a -> c)
swap2_1 = swap1_2


-- Swaps second and third arguments of a function
swap2_3 : (a -> c -> b -> d) -> (a -> b -> c -> d)
swap2_3 f x y z = f x z y

-- Swaps first and third arguments of a function
swap3_1 : (c -> b -> a -> d) -> (a -> b -> c -> d)
swap3_1 = swap1_3

-- Swaps second and third arguments of a function
swap3_2 : (a -> c -> b -> d) -> (a -> b -> c -> d)
swap3_2 = swap2_3

{- Turns second argument into the first argument
   pushing all other arguments -}
toFront_2 : (a -> b -> c) -> (b -> a -> c)
toFront_2 = swap1_2

{- Turns third argument into the first argument
   pushing all other arguments -}
toFront_3 : (c -> a -> b -> d) -> (a -> b -> c -> d)
toFront_3 = swap2_3 swap1_3

{- Turns the first argument into the second argument
   pulling all other arguments -}
toBack_2 : (a -> b -> c) -> (b -> a -> c)
toBack_2 = swap1_2

{- Turns the first argument into the third argument
   pulling all other arguments -}
toBack_3 : (b -> c -> a -> d) -> (a -> b -> c -> d)
toBack_3 =  swap1_2 . swap2_3
