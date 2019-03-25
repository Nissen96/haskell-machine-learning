import FrequentPatternMining
import qualified Data.Set.Monad as Set

main = do
    let butter = "butter"
    let milk = "milk"
    let bread = "bread"
    let sugar = "sugar"
    let flour = "flour"
    let eggs = "eggs"
    let salt = "salt"

    print butter
    print bread
    print sugar

    let t1 = createItemset [bread, butter, milk, sugar]
    let t2 = createItemset [butter, flour, milk, sugar]
    let t3 = createItemset [butter, eggs, milk, salt]
    let t4 = createItemset [eggs]
    let t5 = createItemset [butter, flour, milk, salt, sugar]
    printSet t1

    let db = [t1, t2, t3, t4, t5]

    printDatabase db

    let test = createItemset [milk, sugar, butter]

    printListOfSets $ cover test db
    print $ support test db
    print $ frequency test db

    print $ isFrequent test db 3
    printSetOfSets $ powerSet $ Set.fromList [1, 2, 3, 4]

    let ant = createItemset [butter, milk]
    let cons = createItemset [salt]
    let rule = Rule ant cons
    print rule

    print $ ruleSupport rule db
    print $ ruleFrequency rule db
    print $ confidence rule db

    let items = extractItems db
    printSet items
    printSetOfSets $ powerSet items

    let s1 = createItemset [1, 2, 3]
    let s2 = createItemset [1, 2, 4]
    let s3 = createItemset [1, 3, 4]
    let s4 = createItemset [1, 3, 5]
    let s5 = createItemset [2, 3, 4]

    printSet s1

    let freqSets = Set.fromList [s1, s2, s3, s4, s5]
    printSetOfSets freqSets
    let candidates = generateCandidates freqSets
    printSetOfSets candidates
