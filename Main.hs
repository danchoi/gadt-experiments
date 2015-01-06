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
  Range :: (Show a, Num a) => Field a -> a -> a -> Constraint 

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
toQuery (f :>: v) = toDBField f ++ " > " ++ toQVal v
toQuery (Match f s) = toDBField f ++ " =~ " ++ toQVal s
toQuery (Range f x y) = toDBField f ++ " between " ++ show x ++ " and " ++ show y

fromPairs :: (String, [String]) -> Constraint
fromPairs ("year",xs) = numConstraint Year xs
fromPairs ("studio",xs) = strConstraint Studio xs 
fromPairs ("rating",xs) = numConstraint Rating xs
fromPairs _ = undefined

toParamParser :: String -> ([String] -> Constraint)
toParamParser "year" = numConstraint Year
toParamParser "studio" = strConstraint Studio
toParamParser "rating" = strConstraint Studio

numConstraint :: (Num a, Read a, ToQVal a, Eq a, Show a) => Field a -> [String] -> Constraint
numConstraint field [x] = Equals field (read x)
numConstraint field (x:y:_) = Range field (read x) (read y)

strConstraint :: Field String -> [String] -> Constraint
strConstraint field (x:_) = Equals field x

gtConstraint :: (ToQVal a, Ord a, Read a, Num a) => Field a -> [String] -> Constraint
gtConstraint field (x:_) = field :>: read x

main = do
  let c = Equals Year 1999
  -- let d = Match Studio 11 -- FAILS
  let d = Match Studio "Paramount" 
  print $ map toQuery [Equals Year 1999, Match Studio "MGM", Range Rating 1 4]
  mapM_ print $ map (toQuery . fromPairs) [("year", ["2000"]), ("studio", ["Miramax"])]
  -- "year == 2000"
  -- "studio == \"Miramax\""
  mapM_ print $ map (toQuery . fromPairs) [("year", ["2000", "2002"])]
  mapM_ print $ map (toQuery . fromPairs) [("rating", ["3"])]
  mapM_ print $ map (toQuery . fromPairs) [("rating", ["3", "5"])]
  -- "year between (2000,2002)"

  return ()
