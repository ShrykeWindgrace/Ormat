module Main where

import Data.List
import Data.Foldable (fold)
import Control.Monad

main :: IO ()
main = do
    -- prettyPrinter ormats
    putStrLn "easyansw = "
    mapM_ (\sol -> putStr "solLen=" >> print (length sol)) easyAnsw
    writeFile "answer9.ormat.txt" (intercalate "\n" (showPair <$> answer))
    -- print $ subsequences $ filter (<= easy) ormats



prettyPrinter :: [Ormat] -> IO ()
prettyPrinter = mapM_ (\orm -> print orm >> putStrLn ",")

newtype Ormat = Ormat [Bool] deriving (Eq)

instance Show Ormat where
    show (Ormat l) = let simple = map (\b -> if b then 'X' else 'O') l in
        take 3 simple ++ "\n" ++ take 3 (drop 3 simple) ++ "\n" ++ drop 6 simple

instance Monoid Ormat where
    mempty = Ormat $ replicate 9 False
    mappend (Ormat left) (Ormat right) = Ormat $ zipWith (||) left right


instance Ord Ormat where
    (Ormat left) <= (Ormat right) = and $ zipWith (\l r -> r || not l) left right

ormatP :: [Ormat]
ormatP = [Ormat [ mod (i-j) 3 == k  | i<-[1..3], j<-[1..3] ] | k <- [0..2]]

ormatN :: [Ormat]
ormatN = [Ormat [ mod (i+j) 3 == k  | i<-[1..3], j<-[1..3] ] | k <- [0..2]]

ormats :: [Ormat]
ormats = ormatN ++ ormatP

ormatId = head ormatP :: Ormat

findPaths :: Ormat -> [[Ormat]]
findPaths target = map snd $ filter (\(a,_) -> target == a) $ map (\lst->(fold lst, lst) ) $ subsequences $ filter (<= target) ormats

easy :: Ormat
easy = Ormat $ replicate 9 True

easyAnsw = findPaths easy :: [[Ormat]]

bestLength :: Ormat -> Int
-- bestLength target = minimum $ length <$> findPaths target
bestLength target = findPaths target & map length & safeMinimum

(&) :: a -> (a->b) -> b
x & f = f x 
infixl 5 &

possibleOrmats :: [Ormat]
possibleOrmats = Ormat <$> replicateM 9 [True, False]

answer :: [(Ormat, Int)]
answer = map (\orm -> (orm, bestLength orm)) possibleOrmats


safeMinimum :: [Int] -> Int
safeMinimum [] = -1
safeMinimum xs = minimum xs


showPair :: (Ormat, Int) -> String
showPair (o,i) = "--------\n" ++ show o  ++ " => " ++ show i ++ "\n--------"