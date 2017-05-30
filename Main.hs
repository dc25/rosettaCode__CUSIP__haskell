import Data.List(elemIndex)

data Result = Valid | BadCheck | TooLong | TooShort | InvalidContent deriving Show

prependMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
prependMaybe (Just v) (Just vs) = Just (v:vs)
prependMaybe _ _ = Nothing

-- convert a list of Maybe to a Maybe list.
-- result is Nothing if any of values from the original list are Nothing
allMaybe :: [Maybe a] -> Maybe [a]
allMaybe = foldr prependMaybe (Just []) 

toValue :: Char -> Maybe Int
toValue c = elemIndex c $ ['0'..'9'] ++ ['A'..'Z'] ++ "*&#" 

-- check a list of ints to see if they represent a valid CUSIP
valid :: [Int] -> Bool
valid ns = 
    let -- multiply first 8 values with even index by 2
        s0 = zipWith (\i n -> ((if odd i then 1 else 2) * n)) [1..] $ take 8 ns

        -- apply div/mod formula from site and sum up results
        s1 = sum $ fmap (\s -> ( s `div` 10 ) + s `mod` 10) s0

    in  -- apply mod/mod formula from site and compare to 9th value ( 0 index ) 
        ns!!8 == (10 - (s1 `mod` 10)) `mod` 10

-- check a String to see if it represents a valid CUSIP
checkCUSIP :: String -> Result
checkCUSIP cs 
       | l == 9    = case allMaybe $ fmap toValue cs of
                              Nothing -> InvalidContent
                              Just ns -> if valid ns then Valid else BadCheck
       | l < 9     = TooShort
       | otherwise = TooLong
    where l = length cs

testData =  
    [ "037833100"
    , "17275R102"
    , "38259P508"
    , "594918104"
    , "68389X106"
    , "68389X105"
    ]

main = mapM_ putStrLn (fmap (\t -> t ++ ": " ++ show (checkCUSIP t)) testData)
