module Main where

import qualified Data.Maybe as Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import System.Random
import System.Environment
import Control.Monad

type Point = [Float]
type Cluster = [Point]

distance :: Point -> Point -> Float
distance p1 p2 = sqrt $ foldl (+) 0 $ zipWith (\x y -> (x - y) ** 2) p1 p2

centroid :: Cluster -> Point
centroid [] = []
centroid c = map (/ cSize) $ foldl sumCoords init c
  where sumCoords = zipWith (+)
        dimensions = length $ head c
        cSize = fromIntegral $ length c
        init = take dimensions [0,0..]

type ClustersMap = M.Map Int [Point]
type CentroidMap = M.Map Int Point


closestPoint :: [Point] -> Point -> Int
closestPoint clusters point = Maybe.fromJust $ L.elemIndex (L.minimum distances) distances
  where distances = map (distance point) clusters

assignClusters :: [Point] -> [Point] -> ClustersMap
assignClusters centroids [] = M.empty
assignClusters centroids (point:rest) = M.insertWith (++) idx [point] clusters
  where clusters = assignClusters centroids rest
        idx = closestPoint centroids point


centroidListFromClusters :: ClustersMap -> [Point]
centroidListFromClusters clusters = map (\x -> M.findWithDefault [] x centroidMap) keys
  where centroidMap = M.map centroid clusters
        keys = L.sort $ M.keys centroidMap

centroidChange :: [Point] -> [Point] -> Float
centroidChange c1 c2 = foldl (+) 0 $ zipWith distance c1 c2


kMeans :: [Point] -> [Point] -> ClustersMap
kMeans centroids points = if maxDist < 0.001 then
                            clusters
                          else kMeans newCentroids points
  where clusters = assignClusters centroids points
        newCentroids = centroidListFromClusters clusters
        maxDist = centroidChange centroids newCentroids


randPoints :: Int -> Int -> [Point] -> [Point]
randPoints seed n points = randPoints' (mkStdGen seed) (min n $ length points) (M.fromList pointsKV) S.empty
  where pointsKV = zip [1..length points] points

randPoints' :: StdGen -> Int -> M.Map Int Point -> S.Set Point -> [Point]
randPoints' g n points current = if (n == S.size current) then S.toList current
                                                          else randPoints' newG n points newCurrent
  where (num, newG) = next g
        idx = ((abs num) `mod` (M.size points)) + 1
        point = M.findWithDefault [] idx points
        newCurrent = S.insert point current

randPointsWeighted :: Int -> Int -> [Point] -> [Point]
randPointsWeighted seed n points = randPointsWeighted' newG n points [initPoint]
  where g = mkStdGen seed
        (num, newG) = next g
        lPoints = length points
        initIdx = (abs num) `mod` lPoints
        initPoint = points!!initIdx

randPointsWeighted' g n points current = do
  if (n == length current) then current
                           else randPointsWeighted' newG n points (chosen:current)
  where pointsDistances = map (\x -> (L.minimum (map (distance x) current))**2) points
        distancesSum = foldl (+) 0 pointsDistances
        (num, newG) = next g
        searchNum = (fromIntegral ((abs num) `mod` 1000) :: Float) / 1000 * distancesSum
        position = getPos searchNum pointsDistances 0
        chosen = points!!position

getPos :: Float -> [Float] -> Int -> Int
getPos _ [a] curr = curr
getPos sum dists curr = if sum < 0 then curr - 1
                                    else getPos (sum - head dists) (tail dists) (curr + 1)

clusterizeSimple numClusters points = mapping
  where
        initCenters = randPoints 42 numClusters points
        mapping = kMeans initCenters points

clusterizeWeighted numClusters points = mapping
  where
        initCenters = randPointsWeighted 42 numClusters points
        mapping = kMeans initCenters points

showListSpace :: Show a => [a] -> String
showListSpace [] = ""
showListSpace [h] = show h
showListSpace (h:t) = show h ++ " " ++ (showListSpace t)

printClusters :: ClustersMap -> IO ()
printClusters clusters = do
  mapM_ (\x -> do
              putStrLn $ (show $ fst x) ++ " " ++ (show . length $ snd x)
              mapM_ (putStrLn . showListSpace) $ snd x) $ M.toList clusters

main = do
  nPoints <- getLine
  numClusters <- getLine
  points <- replicateM (read nPoints) getLine
  pointsList <- return $ map (\x -> map (\x -> read x :: Float) $ words x) points
  clusters <- return $ clusterizeWeighted (read numClusters) pointsList
  printClusters clusters
