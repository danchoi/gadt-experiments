{-# LANGUAGE OverloadedStrings, GADTs, ExistentialQuantification #-} 
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
  Range :: Ord a => Field a -> (a, a) -> Constraint a

instance (Show a) => Show (Constraint a) where
  show (Equals f v) = show f ++ " == " ++ show v
  show (LessThan f v) = show f ++ " < " ++ show v
  show (Match f s) = show f ++ " =~ " ++ show s
  show (Range f s@(x,y)) = show f ++ " between " ++ show s


eval :: Constraint a -> a -> Bool
eval (Equals f v) v' = v == v'
eval (LessThan f v) v' = v < v'
eval (Match f v) v' = v == v'
eval (Range f (x,y)) v   = v >= x && v <= y



data AnyField = forall a. Show a => AnyField a
instance Show AnyField where
  showsPrec p (AnyField a) = showsPrec p a

toField :: String -> AnyField
toField "year" = AnyField Year
toField "studio" = AnyField Studio
toField "rating" = AnyField Rating


data Facetable = forall a. Show a => Facetable a
toFacet' :: Facetable -> String
toFacet' (Facetable x) = show x

main = do
  let c = Equals Year 1999
  let d = LessThan Year 2001
  let e = Match Studio "Miramax"
  -- let invalid = Match 2002 -- fails

  -- let f = Range Studio (1,10) -- invalid
  let f' = Range Year (1990, 2010)

  let g = Equals Rating (2.0 :: Float)
  let h = LessThan Rating (20 :: Int)
  -- let j = LessThan Rating 't' -- invalid
  print c
  print d
  print e
  print f'
  print g
  print h
  print $ eval c 1999
  print $ eval c 2000
  print $ eval h 21
  -- print $ eval h 21.0 -- type mismatch
  print $ eval f' 1991
  print $ eval f' 1980
  -- print $ eval f' 'a'  -- type mismatch
  print $ eval e "Miramax"
  -- print $ eval e 200  -- invalid

  print $ toField "year"
  print $ [toField "year", toField "studio"]
  print $ map toField ["year", "studio"]


  -- Existentials https://www.haskell.org/haskellwiki/Existential_type

  -- print $ map toFacet [Year, Studio] -- invalid
  print $ map toFacet' [Facetable Year, Facetable Studio] -- WORKS


