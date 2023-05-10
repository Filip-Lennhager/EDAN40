{-Assignment N2 String Alignment-}
module StringAlignment where
import qualified Data.Bifunctor as Bi
--2a)
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
    | x == y    = scoreMatch
    | otherwise = scoreMismatch

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y,
                                        similarityScore xs (y:ys) + score x '-',
                                        similarityScore (x:xs) ys + score '-' y]

newSimilarityScore :: String -> String -> Int
newSimilarityScore xs ys = simScore (length xs) (length ys)
  where
    simScore i j = simScoreTable !! i !! j
    simScoreTable = [[ simEntry i j | j <- [0..]] | i <- [0..] ]

    simEntry :: Int -> Int -> Int
    simEntry 0 _ = 0
    simEntry _ 0 = 0
    simEntry i j = maximum [simScore (i-1) (j-1) + score (xs !! (i-1)) (ys !! (j-1)),
                            simScore i (j-1) + score (xs !! (i-1)) '-',
                            simScore (i-1) j + score '-' (ys !! (j-1))]

--2b)
{-The list comprehension [(h1:xs, h2:ys) | (xs, ys) <- aList] generates a new 
list of tuples. For each tuple (xs, ys) in aList, it creates a new tuple where 
the first list has h1 added to its beginning and the second list has h2 added 
to its beginning.-}
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

--2c)
{-The maximaBy function first computes maxValue, which is the maximum value 
produced by applying valueFcn to each element of xs. Then it filters xs to 
keep only the elements for which valueFcn produces maxValue.-}
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn [] = []
maximaBy valueFcn xs = filter (\x -> valueFcn x == maxValue) xs
    where maxValue = maximum $ map valueFcn xs
--maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"]

--2d)
{-Computes all possible alignments, then uses maximaBy to select the ones with the maximum score.-}
type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments [] ys = [(replicate (length ys) '-', ys)]
optAlignments xs [] = [(xs, replicate (length xs) '-')]
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) alignments
  where
    alignments = concat [attachHeads x y $ optAlignments xs ys,
                        attachHeads x '-' $ optAlignments xs (y:ys),
                        attachHeads '-' y $ optAlignments (x:xs) ys]


newOptAlignments :: String -> String -> [AlignmentType]
newOptAlignments xs ys = map (Bi.bimap reverse reverse) (snd (opt (length xs) (length ys)))
    where
        opt :: Int -> Int -> (Int, [AlignmentType])
        opt i j = optTable !! i !! j
        optTable = [[ optEntry i j | j <- [0..] ] | i <- [0..] ]

        optEntry :: Int -> Int -> (Int, [AlignmentType])
        optEntry 0 0 = (0, [([],[])])
        optEntry i 0 = (scoreSpace * i, [(take i xs, replicate i '-')])
        optEntry 0 j = (scoreSpace * j, [(replicate j '-', take j ys)])
        optEntry i j = (fst (head z), concatMap snd z)
            where
              (a, opta) = opt (i - 1) (j - 1)
              (b, optb) = opt (i - 1) j
              (c, optc) = opt i (j - 1)
              x = xs !! (i - 1)
              y = ys !! (j - 1)
              z = maximaBy fst [(a + score x y, attachHeads x y opta),
                                (b + score x '-', attachHeads x '-' optb),
                                (c + score '-' y, attachHeads '-' y optc)]

--2e)
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
    let alignments = newOptAlignments string1 string2
    --let alignments = optAlignments string1 string2
    let numAlignments = length alignments
    putStrLn $ "There are " ++ show numAlignments ++ " optimal alignments:\n"
    mapM_ printAlignment alignments
    putStrLn $ "\nThere were " ++ show numAlignments ++ " optimal alignments!"
  where
    printAlignment :: AlignmentType -> IO ()
    printAlignment (s1, s2) = do
        putStrLn s1
        putStrLn s2
        putStrLn ""