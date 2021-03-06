{-# LANGUAGE OverloadedStrings, GADTs, ExistentialQuantification #-} 
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
  -- compiles forces you to exhaust patterns

data Constraint a where
  Equals :: Eq a => Field a -> a -> Constraint a
  LessThan :: Ord a => Field a -> a -> Constraint a
  Match :: Field String -> String -> Constraint String
  Range :: Ord a => Field a -> (a, a) -> Constraint a

data AnyConstraint = forall a. ToQuery a => AnyConstraint a

fromPairs :: (String, String) -> AnyConstraint
fromPairs ("year",x) = AnyConstraint $ Equals Year (read x)
fromPairs ("studio",x) = AnyConstraint $ Equals Studio x
fromPairs _ = undefined
 
class ToQuery a where
  toQuery :: a -> String

instance Show a => ToQuery (Constraint a) where
  toQuery (Equals f v) = toDBField f ++ " == " ++ show v
  toQuery (LessThan f v) = toDBField f ++ " < " ++ show v
  toQuery (Match f s) = toDBField f ++ " =~ " ++ show s
  toQuery (Range f s@(x,y)) = toDBField f ++ " between " ++ show s

instance ToQuery AnyConstraint where
  toQuery (AnyConstraint x) = toQuery x



data AnyField = forall a. ToDBField  a => AnyField a

instance ToDBField AnyField where
  toDBField (AnyField a) = toDBField a

-- conversion from param to Field type
-- does not enforce exhaustive patterns
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
  print $ toQuery c
  print $ toQuery d
  print $ toQuery e
  print $ toQuery f'
  print $ toQuery g
  print $ toQuery h
  print $ toDBField $ toField "year"
  print $ map toDBField $ [toField "year", toField "studio"]
  print $ map (toDBField . toField) ["year", "studio"]


  -- Existentials https://www.haskell.org/haskellwiki/Existential_type

  -- print $ map toFacet [Year, Studio] -- invalid
  -- print $ map toFacet' [Facetable Year, Facetable Studio] -- WORKS

  print $ [ toDBField x | AnyField x <- [AnyField Year, AnyField Studio] ]

  -- uses instance ToQuery AnyConstraint, and fromPairs
  mapM_ print $ map (toQuery . fromPairs) [("year", "2000"), ("studio", "Miramax")]
  -- prints
  -- "year == 2000"
  -- "studio == \"Miramax\""

{-

"year == 1999"
"year < 2001"
"studio =~ \"Miramax\""
"year between (1990,2010)"
"rating == 2.0"
"rating < 20"
"year"
["year","studio"]
["year","studio"]
["year","studio"]
"year == 2000"
"studio == \"Miramax\""
-}

