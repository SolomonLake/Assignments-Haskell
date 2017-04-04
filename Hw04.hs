{- CS380 Assignment 4
   Name: Solomon Lake Giffen-Hunter
   College email: sgiffenh@haverford.edu
   Resources / collaborators:

used these:
https://artyom.me/aeson#fromjson-instances-for-other-types
https://en.wikibooks.org/wiki/Haskell/do_notation
https://wiki.haskell.org/Monoid
IO:
http://stackoverflow.com/questions/1675366/a-haskell-function-of-type-io-string-string
Hoogle and Hackage also


   **DUE ON GRADESCOPE BEFORE CLASS ON MONDAY, FEBRUARY 27, 2017.**
-}


{-# LANGUAGE GADTSyntax, StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, GADTSyntax #-}


module Hw04 where

import Data.Aeson
import Data.Vector
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Processing JSON
--

--f <- B.readFile m
m = "markets.json"

displayJSON :: B.ByteString -> B.ByteString
displayJSON input = input

-- 1.
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object obj) = Object (fmap ynToBool obj)
ynToBool (Array arr) =  Array (fmap ynToBool arr)
ynToBool val = val


-- 2.
parseData :: B.ByteString -> Maybe Value
parseData input = fmap ynToBool (decode input)


-- 3.
data Market where
    Market :: {marketname   :: T.Text
             , x      :: Double
             , y      :: Double
             , state  :: T.Text
             , cheese :: Bool} -> Market
    deriving (Show, Generic, FromJSON)

p :: Maybe Market
p = decode "{\"marketname\":\"lala\",\"x\":80,\"y\":20.0001,\"hello\":\"ya\",\"state\":\"Hawaai\",\"cheese\":false}"

samp :: Maybe Market
samp = decode "{\"season4time\":\"\",\"nuts\":false,\"wiccash\":false,\"plants\":false,\"vegetables\":false,\"snap\":false,\"state\":\"Missouri\",\"county\":\"Platte\",\"season2time\":\"\",\"location\":\"\",\"trees\":false,\"meat\":false,\"jams\":false,\"soap\":false,\"maple\":false,\"prepared\":false,\"wine\":false,\"season1date\":\"\",\"marketname\":\"Zona Roasa Farmers' Market\",\"season3time\":\"\",\"zip\":\"64153\",\"website\":\"\",\"flowers\":false,\"fmid\":1002206,\"poultry\":false,\"crafts\":false,\"street\":\"Corner of Barry Rd and I-29\",\"updatetime\":\"2009\",\"nursery\":false,\"city\":\"Kansas City\",\"season3date\":\"\",\"credit\":true,\"wic\":false,\"seafood\":false,\"cheese\":false,\"honey\":false,\"season4date\":\"\",\"x\":-94.7071,\"season2date\":\"\",\"eggs\":false,\"sfmnp\":false,\"herbs\":false,\"bakedgoods\":false,\"season1time\":\"\",\"y\":39.288}"


noMarket = Market "None" 0 0 "" False


-- 4. trying to create list of objects
parseMarkets :: B.ByteString -> Maybe [Market]
parseMarkets input = makeMarkets (parseData input)

makeMarkets :: Maybe Value -> Maybe [Market]
makeMarkets (Just (Array arr)) = Just (toList (fmap getMarket (fmap objTrim arr)))
makeMarkets Nothing = Nothing

objTrim :: Value -> Maybe Market
objTrim obj = decode (encode obj)

getMarket :: Maybe Market -> Market
getMarket m = case m of
    Just mx -> mx
    Nothing -> error "No Market"



-- 5. 
loadData :: IO [Market]
loadData = do
    f <- B.readFile "markets.json"
    let out = parseMarkets f
    case out of
        (Just a) -> return $ a
        Nothing -> fail "parsing error"


-- 6.
data OrdList a where
    OrdList :: {getOrdList :: [a]} -> OrdList a
    deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    mappend x y = OrdList (sortMax x y)

sortMax :: Ord a => OrdList a -> OrdList a -> [a]
sortMax (OrdList []) (OrdList []) = []
sortMax (OrdList []) (OrdList [y]) = [y]
sortMax (OrdList [x]) (OrdList []) = [x]
sortMax (OrdList []) (OrdList (y:ys)) = (y:ys)
sortMax (OrdList (x:xs)) (OrdList []) = (x:xs)
sortMax (OrdList (x:xs)) (OrdList (y:ys)) | x <= y    = [x] Prelude.++ (sortMax (OrdList xs) (OrdList (y:ys)))
sortMax (OrdList (x:xs)) (OrdList (y:ys)) | otherwise = [y] Prelude.++ (sortMax (OrdList (x:xs)) (OrdList ys))

evens :: OrdList Integer
evens = OrdList [2,4,6]

odds :: OrdList Integer
odds = OrdList [1,3,5]

combined :: OrdList Integer
combined = evens <> odds


-- 7.
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search fn name [] = mempty
search fn name [x] | T.isInfixOf name (marketWithName x) = fn x
                   | otherwise                       = mempty
search fn name (x:xs) | T.isInfixOf name (marketWithName x) = mappend (fn x) (search fn name xs)
search fn name (x:xs) | otherwise = search fn name xs

marketWithName :: Market -> T.Text
marketWithName mkt@(Market {marketname = n}) = n


-- 8.
firstFound :: Searcher (Maybe Market)
firstFound name [] = Nothing
firstFound name mList | Prelude.null result = Nothing
                      | otherwise = Just (Prelude.head result)
                      where
                        result = search fn name mList
                        fn :: Market -> [Market]
                        fn m = [m]

-- 9.
lastFound :: Searcher (Maybe Market)
lastFound name [] = Nothing
lastFound name mList | Prelude.null result = Nothing
                      | otherwise = Just (Prelude.last result)
                      where
                        result = search fn name mList
                        fn :: Market -> [Market]
                        fn m = [m]


-- 10.
allFound :: Searcher [Market]
allFound name [] = []
allFound name mList = result
                      where
                        result = search fn name mList
                        fn :: Market -> [Market]
                        fn m = [m]

-- 11.
numberFound :: Searcher Int
numberFound name [] = 0
numberFound name mList | Prelude.null result = 0
                       | otherwise = Prelude.length result
                       where
                         result = search fn name mList
                         fn :: Market -> [Market]
                         fn m = [m]

-- 12.
orderedNtoS :: Searcher [Market]
orderedNtoS name [] = []
orderedNtoS name mList = result--(Prelude.map marketWithY result)
                         where
                            result = search fn name mList
                            fn :: Market -> [Market]
                            fn m = [m]

marketWithY :: Market -> (Double, Market)
marketWithY mkt@(Market {y = yCoor}) = (yCoor, mkt)

{-
Wasn't able to finish number 12. This is what I have for it...


instance Monoid (OrdList [(Double,Market)]) where
    mempty = OrdList []
    mappend x y = OrdList (sortY x y)

sortY :: Ord a => OrdList (a,b) -> OrdList (a,b) -> [(a,b)]
sortY (OrdList []) (OrdList []) = []
sortY (OrdList []) (OrdList [y]) = [y]
sortY (OrdList [x]) (OrdList []) = [x]
sortY (OrdList []) (OrdList (y:ys)) = (y:ys)
sortY (OrdList (x:xs)) (OrdList []) = (x:xs)
sortY (OrdList ((x:xs):xss)) (OrdList ((y:ys):yss)) | x <= y    = (x:xs) Prelude.++ (sortY (OrdList xss) (OrdList (y:ys):yss))
sortY (OrdList ((x:xs):xss)) (OrdList ((y:ys):yss)) | otherwise = (y:ys) Prelude.++ (sortY (OrdList (x:xs):xss) (OrdList yss))

-}









-- Testing purposes
mileM :: T.Text
mileM = "1"

change :: Maybe [Market] -> [Market]
change m = case m of
    Just mx -> mx
    Nothing -> error "No Market"





{-
Scratchwork


trimMarkets :: Maybe Value -> Maybe [Market]
trimMarkets v = case v of
    Just (Object obj) -> decode (encode (Object obj))
    Just (Array arr)  -> arrTrim (Array arr)--Just (toList (fmap getMarket (fmap arrTrim arr)))
    Nothing           -> Nothing
--case arrayItem of
    --(Array a) -> Just [noMarket]
    --(Object obj) -> Just (toList (fmap getMarket (fmap objTrim arr)))
    --where
      --arrayItem = Prelude.head (toList (arr))
-}

