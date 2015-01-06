{-# LANGUAGE OverloadedStrings, GADTs, TypeSynonymInstances, FlexibleInstances #-} 
module Main where
import Data.Typeable

data Field a where 
  Year :: Field Int 
  Studio :: Field String
  Rating :: Field Float
 
class ToDBField a where
  toDBField :: a -> String

instance ToDBField (Field a) where
  toDBField Year = "year" 
  toDBField Studio = "studio" 
  toDBField Rating = "rating" 

data Constraint where
  Equals :: (ToQVal a, Eq a) => Field a -> a -> Constraint 
  LessThan :: (ToQVal a, Ord a) => Field a -> a -> Constraint 
  (:>:) :: (ToQVal a, Num a, Ord a) => Field a -> a -> Constraint
  Match :: Field String -> String -> Constraint 
  Range :: (Show a, Num a) => Field a -> (a, a) -> Constraint 

-- for quoting in query if necessary
class ToQVal a where
    toQVal :: a -> String

instance ToQVal String where 
    toQVal x = "S'" ++ show x ++ "'"

instance ToQVal Int where 
    toQVal = show 
instance ToQVal Double where 
    toQVal = show 
instance ToQVal Float where 
    toQVal = show 

toQuery :: Constraint -> String
toQuery (Equals f v) = toDBField f ++ " == " ++ toQVal v
toQuery (LessThan f v) = toDBField f ++ " < " ++ toQVal v
toQuery ((:>:) f v) = toDBField f ++ " > " ++ toQVal v
toQuery (Match f s) = toDBField f ++ " =~ " ++ toQVal s
toQuery (Range f s@(x,y)) = toDBField f ++ " between " ++ show x ++ " and " ++ show y

fromPairs :: (String, [String]) -> Constraint
fromPairs ("year",[x]) = Equals Year (read x)
fromPairs ("year",x:y:_) = Range Year ((read x), (read y))
fromPairs ("studio",[x]) = Equals Studio x
fromPairs ("rating",[x]) = Rating :>: (read x)
fromPairs _ = undefined

main = do
  let c = Equals Year 1999
  -- let d = Match Studio 11 -- FAILS
  let d = Match Studio "Paramount" 
  print $ map toQuery [Equals Year 1999, Match Studio "MGM", Range Rating (1,4)]
  mapM_ print $ map (toQuery . fromPairs) [("year", ["2000"]), ("studio", ["Miramax"])]
  -- "year == 2000"
  -- "studio == \"Miramax\""
  mapM_ print $ map (toQuery . fromPairs) [("year", ["2000", "2002"])]
  mapM_ print $ map (toQuery . fromPairs) [("rating", ["3"])]
  -- "year between (2000,2002)"

  return ()
