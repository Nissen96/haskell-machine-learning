module FrequentPatternMining where

import qualified Data.Set as Set
import Data.List
import Data.Maybe (catMaybes)


--------------- Types -------------
newtype Item a = Item a deriving (Eq, Ord)
instance Show a => Show (Item a) where
    show (Item a) = show a

type Itemset a = Set.Set (Item a)
type Transaction a = Itemset a
type Database a = [Transaction a]

data Rule a = Rule {
    antecedent :: Itemset a,
    consequent :: Itemset a
} deriving (Eq, Ord)

instance Show a => Show (Rule a) where
    show (Rule x y) = showSet x ++ " => " ++ showSet y


----------- Helpers ---------------
showSet :: Show a => Set.Set a -> String
showSet set = "{" ++ (intercalate ", " . map show . Set.toList) set ++ "}"

showListOfSets :: Show a => [Set.Set a] -> String
showListOfSets ls = "[\n  " ++ (intercalate ",\n  " . map showSet) ls ++ "\n]"

showSetOfSets :: Show a => Set.Set (Set.Set a) -> String
showSetOfSets set = "{\n  " ++ (intercalate ",\n  " . map showSet . Set.toList) set ++ "\n}"

printSet :: Show a => Set.Set a -> IO ()
printSet set = putStrLn $ showSet set

printListOfSets :: Show a => [Set.Set a] -> IO ()
printListOfSets ls = putStrLn $ showListOfSets ls

printSetOfSets :: Show a => Set.Set (Set.Set a) -> IO ()
printSetOfSets set = putStrLn $ showSetOfSets set

printDatabase :: Show a => Database a -> IO ()
printDatabase db = putStrLn $ "DB = " ++ showListOfSets db

toRealDiv :: (Integral a, Fractional b) => a -> a -> b
toRealDiv a b = fromIntegral a / fromIntegral b

cartesian :: [a] -> [a] -> [(a, a)]
cartesian a b = [(x, y) | x <- a, y <- b]

cartesianNonreflexive :: Eq a => [a] -> [a] -> [(a, a)]
cartesianNonreflexive a b = [(x, y) | x <- a, y <- b, x /= y]

powerSet :: Ord a => Set.Set a -> Set.Set (Set.Set a)
powerSet set
    | null set = Set.singleton Set.empty
    | otherwise = Set.map (Set.insert x) powset `Set.union` powset
        where
            (x, xs) = Set.deleteFindMin set
            powset = powerSet xs


---------------- Itemsets ----------------
createItemset :: Ord a => [a] -> Itemset a
createItemset ls = Set.fromList [Item x | x <- ls]

cover :: Ord a => Itemset a -> Database a -> Set.Set (Transaction a)
cover itemset database = Set.fromList [trans | trans <- database, itemset `Set.isSubsetOf` trans]

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
confidence rule database = (ruleSupport rule database) `toRealDiv` (support (antecedent rule) database)


-------------------- Apriori ------------------


generateCandidates :: Ord a => Set.Set (Itemset a) -> Set.Set (Itemset a)
generateCandidates freqSets = Set.fromList $ catMaybes $ map (\(x, y) -> join x y) $ cartesianNonreflexive sorted sorted
    where sorted = map (sort . Set.toList) $ Set.toList freqSets

join :: Ord a => [Item a] -> [Item a] -> Maybe (Itemset a)
join p q
    | preP == preQ = Just $ (Set.fromList p) `Set.union` (Set.fromList q)
    | otherwise    = Nothing
    where
        preP = take (length p - 1) p
        preQ = take (length q - 1) q
