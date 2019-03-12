import FrequentPatternMining
import qualified Data.Set as Set

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
