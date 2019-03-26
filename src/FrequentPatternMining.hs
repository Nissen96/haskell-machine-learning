{-# LANGUAGE MonadComprehensions #-}

module FrequentPatternMining where

import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Data.List
import Data.Maybe (catMaybes)


--------------- Types -------------
newtype Item a = Item a deriving (Eq, Ord)
instance Show a => Show (Item a) where
    show (Item a) = show a

type Itemset a     = Set (Item a)
type Transaction a = Itemset a
type Database a    = [Transaction a]

data Rule a = Rule {
    antecedent :: Itemset a,
    consequent :: Itemset a
} deriving (Eq, Ord)

instance (Ord a, Show a) => Show (Rule a) where
    show (Rule x y) = showSet x ++ " => " ++ showSet y


----------- Helpers ---------------
showSet :: (Ord a, Show a) => Set a -> String
showSet set = "{" ++ (intercalate ", " . map show . Set.toList) set ++ "}"

showListOfSets :: (Ord a, Show a) => [Set a] -> String
showListOfSets ls = "[\n  " ++ (intercalate ",\n  " . map showSet) ls ++ "\n]"

showSetOfSets :: (Ord a, Show a) => Set (Set a) -> String
showSetOfSets set = "{\n  " ++ (intercalate ",\n  " . map showSet . Set.toList) set ++ "\n}"

printSet :: (Ord a, Show a) => Set a -> IO ()
printSet set = putStrLn $ showSet set

printListOfSets :: (Ord a, Show a) => [Set a] -> IO ()
printListOfSets ls = putStrLn $ showListOfSets ls

printSetOfSets :: (Ord a, Show a) => Set (Set a) -> IO ()
printSetOfSets set = putStrLn $ showSetOfSets set

printDatabase :: (Ord a, Show a) => Database a -> IO ()
printDatabase db = putStrLn $ "DB = " ++ showListOfSets db

toRealDiv :: (Integral a, Fractional b) => a -> a -> b
toRealDiv a b = fromIntegral a / fromIntegral b

cartesian :: Set a -> Set a -> Set (a, a)
cartesian a b = [(x, y) | x <- a, y <- b]

cartesianNonreflexive :: Eq a => Set a -> Set a -> Set (a, a)
cartesianNonreflexive a b = [(x, y) | x <- a, y <- b, x /= y]

powerSet :: Ord a => Set a -> Set (Set a)
powerSet set
    | null set  = Set.singleton Set.empty
    | otherwise = Set.map (Set.insert x) powset `Set.union` powset
        where
            (x, xs) = Set.deleteFindMin set
            powset  = powerSet xs

setTake :: Ord a => Int -> Set a -> Set a
setTake n = Set.fromDistinctAscList . take n . Set.toAscList

setCatMaybes :: Ord a => Set (Maybe a) -> Set a
setCatMaybes = Set.fromDistinctAscList . catMaybes . Set.toAscList

---------------- Itemsets ----------------
createItemset :: Ord a => [a] -> Itemset a
createItemset ls = Set.fromList [Item x | x <- ls]

cover :: Ord a => Itemset a -> Database a -> [Transaction a]
cover itemset database = [trans | trans <- database, itemset `Set.isSubsetOf` trans]

support :: Ord a => Itemset a -> Database a -> Int
support itemset database = length $ cover itemset database

frequency :: Ord a => Itemset a -> Database a -> Float
frequency itemset database = (support itemset database) `toRealDiv` (length database)

isFrequent :: Ord a => Itemset a -> Database a -> Int -> Bool
isFrequent itemset database threshold = (support itemset database) >= threshold

extractItems :: Ord a => Database a -> Itemset a
extractItems database = Set.unions database


------------------ Rules ------------------
ruleUnion :: Ord a => Rule a -> Itemset a
ruleUnion rule = antecedent rule `Set.union` consequent rule

ruleSupport :: Ord a => Rule a -> Database a -> Int
ruleSupport rule database = support (ruleUnion rule) database

ruleFrequency :: Ord a => Rule a -> Database a -> Float
ruleFrequency rule database = frequency (ruleUnion rule) database

confidence :: Ord a => Rule a -> Database a -> Float
confidence rule database = (ruleSupport rule database)
                           `toRealDiv`
                           (support (antecedent rule) database)


-------------------- Apriori ------------------


generateCandidates :: Ord a => Set (Itemset a) -> Set (Itemset a)
generateCandidates freqSets = setCatMaybes
                            . Set.map (uncurry join)
                            $ cartesianNonreflexive freqSets freqSets

join :: Ord a => Set (Item a) -> Set (Item a) -> Maybe (Itemset a)
join p q
    | preP == preQ = Just $ p `Set.union` q
    | otherwise    = Nothing
    where
        preP = setTake k p
        preQ = setTake k q
        k    = Set.size p - 1
