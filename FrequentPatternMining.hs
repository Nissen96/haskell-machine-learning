module FrequentPatternMining where

import qualified Data.Set as Set
import Data.List
import Data.Maybe (catMaybes)

--------------- Types -------------
type Item = String
type Itemset = Set.Set Item
type Transaction = Itemset
type Database = [Transaction]

data Rule = Rule {
    antecedent :: Itemset,
    consequent :: Itemset
} deriving (Eq, Ord)

instance Show Rule where
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

printDatabase :: Database -> IO ()
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
cover :: Itemset -> Database -> Set.Set Transaction
cover itemset database = Set.fromList [trans | trans <- database, itemset `Set.isSubsetOf` trans]

support :: Itemset -> Database -> Int
support itemset database = length $ cover itemset database

frequency :: Itemset -> Database -> Float
frequency itemset database = (support itemset database) `toRealDiv` (length database)

isFrequent :: Itemset -> Database -> Int -> Bool
isFrequent itemset database threshold = (support itemset database) >= threshold

extractItems :: Database -> Itemset
extractItems database = Set.unions database

------------------ Rules ------------------
ruleUnion :: Rule -> Itemset
ruleUnion rule = antecedent rule `Set.union` consequent rule

ruleSupport :: Rule -> Database -> Int
ruleSupport rule database = support (ruleUnion rule) database

ruleFrequency :: Rule -> Database -> Float
ruleFrequency rule database = frequency (ruleUnion rule) database

confidence :: Rule -> Database -> Float
confidence rule database = (ruleSupport rule database) `toRealDiv` (support (antecedent rule) database)


-------------------- Apriori ------------------


generateCandidates :: Set.Set Itemset -> Set.Set Itemset
generateCandidates freqSets = Set.fromList $ catMaybes $ map (\(x, y) -> join x y) $ cartesianNonreflexive sorted sorted
    where sorted = map (sort . Set.toList) $ Set.toList freqSets

join :: [Item] -> [Item] -> Maybe Itemset
join p q
    | preP == preQ = Just $ (Set.fromList p) `Set.union` (Set.fromList q)
    | otherwise    = Nothing
    where
        preP = take (length p - 1) p
        preQ = take (length q - 1) q
