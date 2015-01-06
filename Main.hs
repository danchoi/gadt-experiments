{-# LANGUAGE OverloadedStrings, GADTs #-} 
module Main where

data Field a where 
  Year :: Field Int 
  Studio :: Field String
  Rating :: Num a => Field a
 
instance Show (Field a) where
  show Year = "Year" 
  show Studio = "Studio" 
  show Rating = "Rating" 

data Constraint a where
  Equals :: Eq a => Field a -> a -> Constraint a
  LessThan :: Ord a => Field a -> a -> Constraint a
  Match :: Field String -> String -> Constraint String
  Range :: Field a -> (a, a) -> Constraint a

instance (Show a) => Show (Constraint a) where
  show (Equals f v) = show f ++ " == " ++ show v
  show (LessThan f v) = show f ++ " < " ++ show v
  show (Match f s) = show f ++ " =~ " ++ show s
  show (Range f s@(x,y)) = show f ++ " between " ++ show s

main = do
  let c = Equals Year 1999
  let d = LessThan Year 2001
  let e = Match Studio "Miramax"
  -- let f = Range Studio (1,10) -- invalid
  let f' = Range Year (1990, 2010)
  -- let invalid = Match 2002 -- fails
  let g = Equals Rating (2.0 :: Float)
  let h = LessThan Rating (20 :: Int)
  print c
  print d
  print e
  print f'
  print g
  print h


