{-# LANGUAGE OverloadedStrings, GADTs #-} 
module Main where
import Data.Typeable

data Field a where 
  Year :: Field Int 
  Studio :: Field String
  Rating :: Num a => Field a
 
class ToDBField a where
  toDBField :: a -> String

instance ToDBField (Field a) where
  toDBField Year = "year" 
  toDBField Studio = "studio" 
  toDBField Rating = "rating" 

data Constraint where
  Equals :: (Show a, Eq a) => Field a -> a -> Constraint 
  LessThan :: (Show a, Ord a) => Field a -> a -> Constraint 
  Match :: Field String -> String -> Constraint 
  Range :: (Show a, Ord a) => Field a -> (a, a) -> Constraint 

class ToQuery a where
  toQuery :: a -> String

instance ToQuery Constraint  where
  toQuery (Equals f v) = toDBField f ++ " == " ++ show v
  toQuery (LessThan f v) = toDBField f ++ " < " ++ show v
  toQuery (Match f s) = toDBField f ++ " =~ " ++ show s
  toQuery (Range f s@(x,y)) = toDBField f ++ " between " ++ show s



main = do
  let c = Equals Year 1999
  -- let d = Match Studio 11 -- FAILS
  let d = Match Studio "Paramount" 
  print $ map toQuery [c,d]
  return ()
