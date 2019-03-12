module FrequentPatternMiningTest where

import qualified Data.Set as Set

--------------- Types -------------
newtype Item a = Item a deriving (Eq, Ord)
instance Show a => Show (Item a) where
    show (Item a) = show a

type Itemset a = Set.Set (Item a)
type Transaction a = Itemset a
type Database a    = [Transaction a]

data Rule a = Rule {
    antecedent :: Itemset a,
    consequent :: Itemset a
} deriving (Eq, Ord, Show)


----------- Helpers ---------------
showItemset :: Show a => Itemset a -> String
showItemset itemset = "{" ++ (unwords . map show . Set.toList) itemset ++ "}"

showDatabase :: Show a => Database a -> String
showDatabase db = "DB = [\n" ++ (unlines . map ("  " ++) . map showItemset) db ++ "]"

toRealDiv :: (Integral a, Fractional b) => a -> a -> b
toRealDiv a b = fromIntegral a / fromIntegral b

powerSet :: Ord a => Set.Set a -> Set.Set (Set.Set a)
powerSet set
    | null set = Set.singleton Set.empty
    | otherwise = Set.map (Set.insert x) powset `Set.union` powset
        where
            (x, xs) = Set.deleteFindMin set
            powset = powerSet xs

extractItems :: Ord a => Database a -> Itemset a
extractItems database = (Set.unions database)


---------------- Itemsets ----------------
cover :: Ord a => Itemset a -> Database a -> Set.Set (Transaction a)
cover itemset database = Set.fromList [trans | trans <- database, itemset `Set.isSubsetOf` trans]

support :: Ord a => Itemset a -> Database a -> Int
support itemset database = length $ cover itemset database

frequency :: Ord a => Itemset a -> Database a -> Float
frequency itemset database = (support itemset database) `toRealDiv` (length database)

isFrequent :: Ord a => Itemset a -> Database a -> Int -> Bool
isFrequent itemset database threshold = (support itemset database) >= threshold


------------------ Rules ------------------
ruleUnion :: Ord a => Rule a -> Itemset a
ruleUnion rule = antecedent rule `Set.union` consequent rule

ruleSupport :: Ord a => Rule a -> Database a -> Int
ruleSupport rule database = support (ruleUnion rule) database

ruleFrequency :: Ord a => Rule a -> Database a -> Float
ruleFrequency rule database = frequency (ruleUnion rule) database

confidence :: Ord a => Rule a -> Database a -> Float
confidence rule database = (ruleSupport rule database) `toRealDiv` (support (antecedent rule) database)

main = do
    let butter = Item "butter"
    let milk = Item "milk"
    let bread = Item "bread"
    let sugar = Item "sugar"
    let flour = Item "flour"
    let eggs = Item "eggs"
    let salt = Item "salt"
    let test1 = Item 1
    let test2 = Item 4
    let test3 = Item [1, 2]
    let test4 = Item 'a'

    print butter
    print bread
    print sugar
    print test1
    print test2
    print test3
    print test4

    let t1 = Set.fromList [bread, butter, milk, sugar]
    let t2 = Set.fromList [butter, flour, milk, sugar]
    let t3 = Set.fromList [butter, eggs, milk, salt]
    let t4 = Set.fromList [eggs]
    let t5 = Set.fromList [butter, flour, milk, salt, sugar]
    putStrLn $ showItemset t1

    let db = [t1, t2, t3, t4, t5]

    putStrLn $ showDatabase db

    let test = Set.fromList [milk, sugar, butter]

    putStrLn $ unwords $ map showItemset $ Set.toList $ cover test db
    print $ support test db
    print $ frequency test db

    print $ isFrequent test db 3
    print $ powerSet $ Set.fromList [1, 2, 3]

    let ant = Set.fromList [butter, milk]
    let cons = Set.fromList [salt]
    let rule = Rule ant cons

    print $ rule
    print $ ruleSupport rule db
    print $ ruleFrequency rule db
    print $ confidence rule db

    putStrLn $ showItemset $ extractItems db
