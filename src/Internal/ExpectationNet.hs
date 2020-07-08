module Internal.ExpectationNet where

import           Data.List
import           System.Random
import Data.Number.Erf

type Bern = (String, Double)

--type Factor = ([String], [[Bool]], [Double])

data Node = GNode Int String (Double,Double) [Int]  | GFNode Int String (Double,Double) [Int]
            | DGFNode Int String String (Double,Double) [Int]   | GTNode Int String (Int,Int) [Int] | VNode Int Bern [Int]  | ONode Int Bern [Int] deriving (Show, Eq)


data NodeDescription = GF String Double Double | DGF String String Double | GrT String (String,String) Double


type Graph = [Node]

generateGraph :: [NodeDescription] -> Graph
generateGraph descriptions =
  foldl (\graph des -> addNodePair graph des) [] descriptions

addNodePair :: Graph -> NodeDescription -> Graph
addNodePair graph (GF name mu std) =
  graph
    ++ [ GFNode nId name (mu, std) [nId + 1]
       , GNode (nId + 1) name (mu, std) [nId]
       ]
  where nId = length graph
addNodePair graph (DGF name1 name2 std) =
  n_graph
    ++ [ DGFNode nId name1 name2 (0, std) [id, nId + 1]
       , GNode (nId + 1) name1 (0, 0) [nId]
       ]
 where
  nId     = length graph
  id      = findId name2 graph
  n_graph = updateGraph nId [id] graph
addNodePair graph (GrT name (lesser, larger) value) =
  n_graph
    ++ [ GTNode nId name (n_lesser, n_larger) [nId + 1]
       , VNode (nId + 1) (name, value) [nId]
       ]
 where
  nId      = length graph
  n_lesser = findId lesser graph
  n_larger = findId larger graph
  n_graph  = updateGraph nId [n_lesser, n_larger] graph

findId :: String -> Graph -> Int
findId name graph = head $ concatMap (nodeToID name) graph

nodeToID :: String -> Node -> [Int]
nodeToID searched (GNode id name _ _) = if name == searched then [id] else []
nodeToID _        _                   = []

updateGraph :: Int -> [Int] -> Graph -> Graph
updateGraph id ids graph = foldl updateGraph' graph ids
  where updateGraph' graph to = map (addId id to) graph


addId :: Int -> Int -> Node -> Node
addId from to node@(GNode id n p ids) =
  if id == to then (GNode id n p (from : ids)) else node
addId from to node = node

type NormDist = (String,Double,Double)


normalProduct :: NormDist -> NormDist -> NormDist
normalProduct (name,mu1,std1) (__,mu2,std2)  = (name, mu, sqrt var)
  where 
    mu = (mu1 * std2^2 + mu2 * std1^2) / (std1^2 + std2^2)
    var = (std1*std2)^2 / (std1^2+std2^2)

normalDivision :: NormDist -> NormDist -> NormDist
normalDivision (name,mu1,std1) (__,mu2,std2)  = (name, mu, sqrt var)
  where 
    mu = (mu1 * std2^2 - mu2 * std1^2) / (std2^2 - std1^2)
    var = (std1*std2)^2 / (std2^2-std1^2)


convolution :: NormDist -> NormDist -> NormDist
convolution (name,mu1,std1) (__,mu2,std2) = (name, mu1+mu2, std1+std2)  

cumulative :: NormDist -> Bool -> (Double -> Double)
cumulative (_, mu, std) wins  = if wins then f else (1-) . f 
  where 
    f x = 1/2*(1+ erf( (x-mu)/(sqrt (2* std^2))))

normal :: NormDist -> (Double -> Double)
normal (_,mu,std) = (\x -> 1/ (sqrt ( 2* pi * std^2 )) * exp( - (x-mu)^2/(2*std^2)) )

norm :: [(Double,Double)] -> [(Double,Double)]
norm xs = map(\(a,b) -> (a,b/area)) xs
  where
    pairs = zip xs (tail xs)
    area = sum $ map(\((a,b),(c,d)) -> (c-a) * (b+d)/2 ) pairs
  
mean :: [(Double,Double)] -> Double
mean dist = mu
  where
    pairs = zip dist (tail dist)
    mu = sum $ map(\((a,b),(c,d)) -> (c-a) * (b+d)/2 * (a+c)/2 ) pairs

stdev :: [(Double,Double)] -> Double -> Double
stdev dist mu = sqrt $ eX2 - mu^2 
  where 
    pairs = zip dist (tail dist)
    eX2 = sum $ map(\((a,b),(c,d)) -> (c-a) * (b+d)/2 * ((a+c)/2)^2 ) pairs

approxGaussian :: NormDist -> (Double -> Double) -> NormDist
approxGaussian dist@(_,mu',std') cumul = normalDivision ("approx",mu, dev) dist
  where
    start = mu' - 4 * std'
    end = mu' + 4 *std'
    --xs = [start..end]
    xs = [start,(start+std'/100)..end]
    cum = map cumul xs
    normDist = map (normal dist) xs
    product = map(uncurry (*) ) $ zip cum normDist
    normProduct = norm $ zip xs product
    mu = mean normProduct
    dev = stdev normProduct mu
    

getMessage :: Int -> String -> Node -> Graph -> NormDist
getMessage sender name1 (GNode id name2 (mu,std) ids) graph = foldr1 normalProduct norms
  where 
    froms = filter (/=sender) ids 
    norms = map (\x -> getMessage id name2 (graph !! x) graph) froms   
getMessage sender name (GFNode _ _ (mu,std) _) graph = (name,mu,std)
getMessage sender name (DGFNode id own other (_,std) ids) graph = 
  convolution (name,0,std) dist 
  where
      from = head $ filter (/=sender) ids
      dist = getMessage id own (graph !! from) graph
      sdist = getMessage id other (graph !! sender) graph
getMessage sender name1 (GTNode id name2 (lesser,larger) ids) graph = (name1,mu,std)
  where
    (truthValue,_,_) = getMessage id name2 (graph !! (id+1)) graph
    isGreater = if truthValue == "true" then  True else False
    lessDist = getMessage id name2 (graph !! lesser) graph
    larDist = getMessage id name2 (graph !! larger) graph
    cumul = if sender == larger 
      then
        cumulative lessDist isGreater
      else
        cumulative larDist (not isGreater)  
    (_,mu,std) = if sender == larger
      then
        approxGaussian larDist cumul
      else
        approxGaussian lessDist cumul
 
getMessage sender _ (ONode id (_,value) ids) graph = 
  if value > 0.5
    then ("true", 0,0)
    else ("false" ,0,0)

getMessage sender _ (VNode id (_,value) ids) graph = 
  if value > 0.5
    then ("true",0,0)
    else ("false",0,0)

evalVariables :: [String] -> [String] -> [[Bool]] -> Graph -> (Graph,[NormDist])
evalVariables variables observations [[]] graph = (graph,dists)
  where 
    dists = map (readDistFromGraph graph) variables
evalVariables variables observations values graph = 
  evalVariables variables observations restValues upGraph 
  where
    actValues = map head values
    evals = map (\name -> evalVariable name observations actValues graph) variables
    restValues = map tail values
    upGraph = replaceVariables evals graph

readDistFromGraph :: Graph -> String-> NormDist
readDistFromGraph graph name = (name,mu,std)
  where
    id = findId name graph
    GFNode _ _ (mu,std) _  = graph !! (id-1)
  
replaceVariables :: [NormDist] -> Graph -> Graph
replaceVariables dists graph = foldr replaceVariable graph dists

replaceVariable :: NormDist -> Graph -> Graph
replaceVariable (name,mu,std) graph = before ++ (n_node:after)
  where
    id = findId name graph
    (before,(GFNode _ _ _ ids):after ) = splitAt (id-1) graph
    n_node = GFNode (id-1) name (mu,std) ids 

evalVariable :: String -> [String] -> [Bool] -> Graph -> NormDist
evalVariable variable observations values graph = getMessage (-1) variable (obsGraph !! id) obsGraph  
  where 
    obsGraph = foldr (\(obs,value) g -> setObservation obs value g) graph $ zip observations values 
    id = findId variable obsGraph
    

toId :: String -> Node -> [(Int,[Int])]
toId name1 node@(VNode id (name,_) ids ) = 
  if name == name1
    then [(id,ids)]
    else [] 
toId name1 node@(ONode id (name,_) ids ) = 
  if name == name1
    then [(id,ids)]
    else [] 
toId _ _ = []

findOVNodeId :: String -> Graph -> (Int,[Int])
findOVNodeId name graph = head $ concatMap (toId name) graph



setObservation :: String -> Bool -> Graph -> Graph
setObservation name obs graph = before ++ (n_node:xs)
  where
    (id,ids) = findOVNodeId name graph
    (before,found:xs) = splitAt id graph
    value = if obs then 1 else 0 
    n_node = ONode id (name,value) ids