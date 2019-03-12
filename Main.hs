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

    let i1 = "1"
    let i2 = "2"
    let i3 = "3"
    let i4 = "4"
    let i5 = "5"

    let s1 = Set.fromList [i1, i2, i3]
    let s2 = Set.fromList [i1, i2, i4]
    let s3 = Set.fromList [i1, i3, i4]
    let s4 = Set.fromList [i1, i3, i5]
    let s5 = Set.fromList [i2, i3, i4]

    let freqSets = Set.fromList [s1, s2, s3, s4, s5]
    printSetOfSets $ generateCandidates freqSets
