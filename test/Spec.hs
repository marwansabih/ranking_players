import           Test.Hspec
import           Internal.ExpectationNet


--data NodeDescription = GF String Double Double | DGF String String Double | GrT String (String,String) Double

nodes =
        [ GF "Fred_Skill" 100 5
        , GF "Jill_Skill" 120 40
        , DGF "Fred_perf" "Fred_Skill" 5
        , DGF "Jill_perf" "Jill_Skill" 5
        , GrT "Jill_Win" ("Fred_perf", "Jill_perf") 0.5
        ]

graph = generateGraph nodes 

testGenerateGraph :: IO ()
testGenerateGraph = do
  let expectedGraph =
        [ GFNode 0 "Fred_Skill" (100.0, 5.0) [1]
        , GNode 1 "Fred_Skill" (100.0, 5.0) [4, 0]
        , GFNode 2 "Jill_Skill" (120.0, 40.0) [3]
        , GNode 3 "Jill_Skill" (120.0, 40.0) [6, 2]
        , DGFNode 4 "Fred_perf" "Fred_Skill" (0.0, 5.0) [1, 5]
        , GNode 5 "Fred_perf" (0.0, 0.0) [8, 4]
        , DGFNode 6 "Jill_perf" "Jill_Skill" (0.0, 5.0) [3, 7]
        , GNode 7 "Jill_perf" (0.0, 0.0) [8, 6]
        , GTNode 8 "Jill_Win" (5, 7) [9]
        , VNode 9 ("Jill_Win", 0.5) [8]
        ]
  print graph
  print $ setObservation "Jill_Win" False graph
  shouldBe graph expectedGraph

testDistFunctions :: IO ()
testDistFunctions = do
      let xs = [-30,-29.99..300]
      let gauss = norm $ zip xs (map (normal ("test", 100, 5)) xs)
      let mu = mean gauss
      print $ "mu " ++ show mu
      print $ "stdev " ++ show ( stdev gauss mu)

testDivisionGaussian :: IO ()
testDivisionGaussian = do
      let f_skill = ("Fskill", 110, 40)
      let j_skill = ("Jskill", 120, 5) 
      let norm1 = normal f_skill
      let norm2 = normal j_skill
      let xs = [-1000,-999.9..1000]
      let normal1 = zip xs $ map norm1 xs
      let normal2 = zip xs $ map norm2 xs
      let divide =  norm $ map (\((a,b),(c,d)) -> if(d == 0) then (a,0) else (a,b/d) ) $ zip normal2 normal1
      --print divide
      let mu = mean divide
      print $ "mu " ++ show mu
      print $ "stdev " ++ show (stdev divide mu)
      print $ normalDivision j_skill f_skill


testApproxGaussian :: IO ()
testApproxGaussian = do
      let f_skill = ("Fskill", 100, 5)
      let j_skill = ("Jskill", 120, 40)
      let cum = cumulative f_skill True
      let cum2 = cumulative j_skill True 
      print $ approxGaussian j_skill cum
      print $ approxGaussian f_skill cum2
      

testGetMessage :: IO ()
testGetMessage = do
  let n_graph = setObservation "Jill_Win" True graph
  print "After Jill wins"
  print $ getMessage (0) "Fred_skill" (n_graph !! 1) n_graph
  print $ getMessage (4) "Fred_skill" (n_graph !! 1) n_graph
  print $ getMessage (-1) "Fred_skill" (n_graph !! 1) n_graph
  print $ getMessage (-1) "Jill_skill" (n_graph !! 3) n_graph
  print $ getMessage 5 "Fred_perf" (n_graph !! 8) n_graph
  print $ getMessage 7 "Jill_perf" (n_graph !! 8) n_graph
  print "After Jill looses"
  let l_graph = setObservation "Jill_Win" False graph
  print $ getMessage (-1) "Fred_skill" (l_graph !! 1) l_graph
  print $ getMessage (-1) "Jill_skill" (l_graph !! 3) l_graph

-- evalVariable variable observations graph 
testEvalVariable :: IO ()
testEvalVariable = do
      print "Jill Wins:"
      print $ evalVariable "Fred_Skill" ["Jill_Win"] [True] graph
      print $ evalVariable "Jill_Skill" ["Jill_Win"] [True] graph
      print "Jill looses:"
      print $ evalVariable "Fred_Skill" ["Jill_Win"] [False] graph
      print $ evalVariable "Jill_Skill" ["Jill_Win"] [False] graph

testEvalVariables :: IO ()
testEvalVariables = do
      let tenWins = replicate 10 True
      let tenLooses = replicate 10 False
      print "Jill Wins 10:"
      print $ snd $ evalVariables ["Fred_Skill","Jill_Skill"] ["Jill_Win"] [tenWins] graph
      print "Jill looses 10:" 
      print $ snd $ evalVariables ["Fred_Skill","Jill_Skill"] ["Jill_Win"] [tenLooses] graph

main :: IO ()
main = do
  testGenerateGraph
  testDistFunctions
  testDivisionGaussian
  testApproxGaussian
  putStrLn "\nTest Get message\n"
  testGetMessage
  putStrLn "\nEvalVariable\n"
  testEvalVariable
  putStrLn "\nEvalVariabls\n"
  testEvalVariables
