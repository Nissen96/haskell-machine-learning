module Clustering where

import Data.List
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mtx

type Point a = [a]

------- Utility Functions ---------
average :: (Fractional a) => [a] -> a
average ls = (sum ls) / genericLength ls

divide :: (Real a, Fractional b) => a -> a -> b
divide a b = realToFrac a / realToFrac b

split :: [a] -> Int -> [[a]]
split _ 0  = []
split [] _ = []
split ls k = lower : split upper (k - 1)
    where (lower, upper) = splitAt (length ls `div` k) ls

------- Distance Functions --------
norm :: Floating a => a -> Point a -> Point a -> a
norm n p q = (**(1 / n)) $ sum $ zipWith (\x y -> ((**n) . abs) (x - y)) p q

weightedNorm :: Floating a => a -> Point a -> Point a -> Point a -> a
weightedNorm n p q weights = (**(1 / n)) $ sum $ zipWith3 (\x y w -> ((*w) . (**n) . abs) (x - y)) p q weights

manhattanDist :: Floating a => Point a -> Point a -> a
manhattanDist p q = sum $ zipWith (\x y -> abs $ x - y) p q

euclideanDist :: Floating a => Point a -> Point a -> a
euclideanDist = norm 2

maxDist :: (Num a, Ord a) => Point a -> Point a -> a
maxDist p q = maximum $ zipWith (\x y -> abs $ x - y) p q

squaredDist :: Num a => Point a -> Point a -> a
squaredDist p q = sum $ zipWith (\x y -> (x - y)^2) p q

wEuclideanDist :: Floating a => Point a -> Point a -> Point a -> a
wEuclideanDist = weightedNorm 2

quadraticForm :: Floating a => Point a -> Point a -> Matrix a -> a
quadraticForm p q m = sqrt . (Mtx.getElem 1 1) $ rowSubbed `Mtx.multStd` m `Mtx.multStd` colSubbed
    where
        subbed = zipWith (-) p q
        len = length subbed
        rowSubbed = Mtx.fromList 1 len subbed
        colSubbed = Mtx.fromList len 1 subbed

---------- Clustering -----------
computeCentroid :: (Fractional a) => [Point a] -> Point a
computeCentroid points = map average $ transpose points

clusterCompactness :: Floating a => (Point a -> Point a -> a) -> [Point a] -> a
clusterCompactness dist cluster = sum $ map (dist centroid) cluster
    where centroid = computeCentroid cluster

clusteringCompactness :: Floating a => (Point a -> Point a -> a) -> [[Point a]] -> a
clusteringCompactness dist = sum . map (clusterCompactness dist)

kMeansFL :: (Eq a, Floating a) => Int -> (Point a -> Point a -> a) -> [Point a] -> Int -> [[Point a]]
kMeansFL k dist points maxIter
    | length points < k = error "Not enough points for k clusters"
    | otherwise         = kMeansFLRec k dist points (take k points) maxIter

kMeansFLRec :: (Eq a, Floating a) => Int -> (Point a -> Point a -> a) -> [Point a] -> [Point a] -> Int -> [[Point a]]
kMeansFLRec k dist points centroids maxIter
    | maxIter == 0 = clusters
    | centroids == newCentroids = clusters
    | otherwise = kMeansFLRec k dist points newCentroids maxIter
        where
            clusters = assignClusters k dist points centroids
            newCentroids = map computeCentroid clusters

-- TODO Create list of clusters based on smallest distance to any centroid for each point
assignClusters :: Int -> (Point a -> Point a -> a) -> [Point a] -> [Point a] -> [[Point a]]
assignClusters k dist points centroids = split points k--do
    --let map (map (\p = dist p) centroids) points --split points k

main = do
let p1 = [10, 1]
let p2 = [2, 3]
let p3 = [3, 4]
let p4 = [1, 5]
let p5 = [7, 7]
let p6 = [6, 8]
let p7 = [7, 8]
let p8 = [7, 9]

let c1 = [p1, p2, p3, p4]
let c2 = [p5, p6, p7, p8]

print $ computeCentroid c1
print $ computeCentroid c2

print $ clusterCompactness squaredDist c1
print $ clusterCompactness squaredDist c2

let points = [p1, p2, p3, p4, p5, p6, p7, p8]

print $ kMeansFL 3 squaredDist points 100

-- (black, red, yellow)
let p = [2, 3, 5]
let q = [4, 7, 8]
let w = [1, 1.5, 2.5]
let m1 = Mtx.identity 3
let m2 = Mtx.fromLists [[1, 0.9, 0.7], [0.9, 1, 0.8], [0.7, 0.8, 1]]

print $ euclideanDist p q
print $ manhattanDist p q
print $ maxDist p q
print $ wEuclideanDist p q w
print $ quadraticForm p q m1
print $ quadraticForm p q m2
