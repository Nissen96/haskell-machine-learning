module FrequentPatternMining where

import qualified Data.Set as Set
import Data.List
--data Item        = Item String            deriving (Eq, Ord, Show)
--data Itemset     = Itemset (Set.Set Item) deriving (Eq, Ord, Show)
--data Transaction = Transaction Itemset    deriving (Eq, Ord, Show)
--data Database    = Database [Transaction] deriving (Show)

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

main = do
    let butter = "butter"
    let milk = "milk"
    let bread = "bread"
    let sugar = "sugar"
    let flour = "flour"
    let eggs = "eggs"
    let salt = "salt"

    putStrLn butter
    putStrLn bread
    putStrLn sugar

    let t1 = Set.fromList [bread, butter, milk, sugar]
    let t2 = Set.fromList [butter, flour, milk, sugar]
    let t3 = Set.fromList [butter, eggs, milk, salt]
    let t4 = Set.fromList [eggs]
    let t5 = Set.fromList [butter, flour, milk, salt, sugar]
    printSet t1

    let db = [t1, t2, t3, t4, t5]

    printDatabase db

    let test = Set.fromList [milk, sugar, butter]

    printSetOfSets $ cover test db
    print $ support test db
    print $ frequency test db

    print $ isFrequent test db 3
    printSetOfSets $ powerSet $ Set.fromList [1, 2, 3, 4]

    let ant = Set.fromList [butter, milk]
    let cons = Set.fromList [salt]
    let rule = Rule ant cons
    print rule

    print $ ruleSupport rule db
    print $ ruleFrequency rule db
    print $ confidence rule db

    let items = extractItems db
    printSet items
    printSetOfSets $ powerSet items
