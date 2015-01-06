{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving #-} 
module Main where

data Field a where 
  Year :: Field Int 
  Studio :: Field String
 
instance Show (Field a) where
  show Year = "Year" 
  show Studio = "Studio" 

data Constraint a where
  Equals :: Eq a => Field a -> a -> Constraint a
  LessThan :: Ord a => Field a -> a -> Constraint a
  Match :: Field String -> String -> Constraint String

instance (Show a) => Show (Constraint a) where
  show (Equals f v) = show f ++ " == " ++ show v
  show (LessThan f v) = show f ++ " < " ++ show v
  show (Match f s) = show f ++ " =~ " ++ show s

main = do
  let c = Equals Year 1999
  let d = LessThan Year 2001
  let e = Match Studio "Miramax"
  -- let invalid = Match 2002 -- fails
  print c
  print d
  print e


