module Lists where

shoppingList :: [ String ]
shoppingList =
    [ "Carrots"
    , "Oats"
    , "Butter"
    , "Apples"
    , "Milk"
    , "Cereal"
    , "Chocolate"
    , "Bananas"
    , "Broccoli"
    ]


main :: IO ()
main = putStrLn ("There are "
    ++ show (length shoppingList)
    ++ " items on the shopping list."
    ++ " and the list is: "
    ++ joinedWithCommas shoppingList)


joinedWithCommas :: [ String ] -> String
joinedWithCommas []     = ""
joinedWithCommas [x]    = x
joinedWithCommas (x:xs) = x ++ ", " ++ joinedWithCommas xs
