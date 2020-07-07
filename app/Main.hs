module Main where

import           Data.Random.Distribution.Normal
import           Data.Random.RVar
import           Data.Random
import           Data.Random.Source.DevRandom
import           Control.Monad
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Plot.Lines
import Data.Number.Erf
import GHC.Float
import Data.List
import Internal.ExpectationNet
import Control.Monad.IO.Class
 

sampleStdNormal :: Int -> IO [Double]
sampleStdNormal nr = replicateM nr (runRVar stdNormal DevRandom :: IO Double)

percentageInDeviation :: Double -> [Double] -> Double
percentageInDeviation deviation sample = fromIntegral inDeviation
  / fromIntegral (length sample)
  where inDeviation = length $ filter (\x -> abs x < deviation) sample


task1 :: IO ()
task1 = do
  sample <- sampleStdNormal 1000000
  print $ "In 1 std " ++ ( show $ percentageInDeviation 1 sample )
  print $ "In 2 std " ++ ( show $ percentageInDeviation 2 sample )
  print $ "In 3 std " ++ ( show $ percentageInDeviation 3 sample )

hist :: [Double] -> PlotHist Double Double
hist sample = defaultNormedPlotHist
  { _plot_hist_title     = ""
  , _plot_hist_values    = sample
  , _plot_hist_bins      = 500
  }

stdNormalPdf :: [Double] -> [(Double, Double)]
stdNormalPdf xs  =
  [ (x, 1/ (sqrt (2*pi)) * exp (-x*x/2)) | x <- xs ]

chart sample = toFile def "../visualisation/hist_vs_beta.png" $ do
  layout_title .= "Standard-Normalverteilung"
  plot $ fmap histToPlot $ liftCState $ return $ hist sample 
  plot ( line "" [stdNormalPdf [-4,-3.999..4]])

task2 :: IO ()
task2 = do
    sample <- sampleStdNormal 100000
    chart sample

task3 :: IO ()
task3 = do
    sample <- sampleStdNormal 100000
    let nr = fromIntegral $ length sample
    let mean = sum sample / nr
    let var =  sum (map (\x -> (x-mean)^2) sample ) / nr
    let std =  sqrt var
    print $ "mean" ++ " " ++ show mean
    print $ "var" ++ " " ++ show var
    print $ "std" ++ " " ++ show std

sampleNormal :: Int -> Double -> Double -> IO [Double]
sampleNormal nr mean std = replicateM nr (runRVar (Data.Random.normal mean std) DevRandom :: IO Double)


skillPlot :: String -> Double -> [(Double,Double)] -> IO ()
skillPlot title var values = toFile def ("../visualisation/Jill_vs_Fred_std_" ++ show var) $ do
  layout_title .= title
  plot (points "" values)
  plot (line "" [[(-30,-30),(50,50) ]])
  
winRatio :: [(Double,Double)] -> String
winRatio sample = "Jill " ++ show winRatio ++ " vs " ++ "Fred " ++ show (1 - winRatio)
  where
    wins = fromIntegral $ length $ filter (uncurry (<)) sample
    winRatio =  wins / (fromIntegral $ length sample)

sampleGames :: Int -> Double-> IO [(Double,Double)]
sampleGames nr var = do
  sample_jill <- sampleNormal nr 15 var
  sample_fred <- sampleNormal nr 12.5 var
  return $ zip sample_fred sample_jill

generateGamePlot :: Int -> Double -> IO()
generateGamePlot nr var = do
  sample <- sampleGames nr var
  let title = winRatio sample
  skillPlot title var sample


task4 :: IO()
task4 = do
  generateGamePlot 10000 5
  generateGamePlot 10000 2
  generateGamePlot 10000 10

posteriorWinPropablity :: Double -> Double -> Double -> Double
posteriorWinPropablity jskill fskill std = normcdf ( (jskill-fskill)/( sqrt 2 * std))

task5 :: IO ()
task5 = do
  print $ posteriorWinPropablity 15 12.5 5
  print $ posteriorWinPropablity 15 12.5 2
  print $ posteriorWinPropablity 15 12.5 10

normalPDF :: Double -> Double -> [Double] -> [(Double, Double)]
normalPDF mu std xs  =
  [ (x, 1/ (sqrt (2*pi* (std^2))) * exp (-(x-mu)^2/(2 * std^2 ) ) ) | x <- xs ]


plotAverageConvolution :: Int -> IO()
plotAverageConvolution nr_average = do
  mus <- sampleNormal nr_average 100 5
  let normals = map (\mu -> normalPDF mu 5 [0,0.1..140]) mus
  let nr = int2Double nr_average
  let average = map (\xs -> (fst $ head xs, sum ( map snd xs )/nr )) $ transpose normals
  let title = "../visualisation/average_gaussion_" ++ show nr_average   ++ ".png"
  toFile def title $ plot ( line "" [average])
  

task6 :: IO ()
task6 = do
  plotAverageConvolution 3
  plotAverageConvolution 6
  plotAverageConvolution 100

plotDists cumul pdf product = toFile def "../visualisation/pdf_to_approx.png" $ do
  plot( line "cumulative input" [cumul] )
  plot ( line "sensitive pdf" [pdf])
  plot ( line "product" [product])

norm_sample :: [(Double,Double)] -> [(Double,Double)]
norm_sample sample = map (\(a,b) -> (a,b/area)) sample
  where 
    area = sum $ map snd sample

task7 :: IO ()
task7 = do
  let skill_fred = ("fred", 100::Double, 5::Double)
  let dist_perf = ("perf", 0::Double, 5::Double)
  let conv1 = convolution dist_perf skill_fred 
  let skill_jill = ("jill", 120, 40)
  let (_ , mu, std) = convolution dist_perf skill_jill
  print mu
  print std
  let cumul = cumulative conv1 True
  let sample = norm_sample $ map(\x -> (x, cumul x)) [-50..300]
  let pdf = norm_sample $ normalPDF mu 40 [-50..300]
  let product = norm_sample $ zipWith (\(a,b) (c,d) -> (a,b*d)) sample pdf
  plotDists sample pdf product
  
task8 :: IO ()
task8 = do 
  let skill_fred = ("fred", 100::Double, 5::Double)
  let dist_perf = ("perf", 0::Double, 5::Double)
  let conv1 = convolution dist_perf skill_fred 
  let skill_jill = ("jill", 120, 40)
  let (_ , mu, std) = convolution dist_perf skill_jill
  let cumul = cumulative conv1 True
  let sample = norm_sample $ map(\x -> (x, cumul x)) [-50..300]
  let pdf = norm_sample $ normalPDF mu std [-50..300]
  let product = norm_sample $ zipWith (\(a,b) (c,d) -> (a,b*d)) sample pdf
  let mean = sum $ map (\(a,b) -> a*b) product
  let mean_of_square = sum $ map (\(a,b) -> a^2*b) product
  let std = sqrt $ mean_of_square - mean^2
  let (_,mux,stdx) = normalDivision ("test", mean, std) skill_jill
  print mux
  print stdx

plotDivide input = toFile def "../visualisation/divide.png" $ plot ( line "test" [input])

test :: IO ()
test = do
  let normal1 = normalPDF 120 5 [100..140]
  let normal2 = normalPDF 120 40 [100..140]
  let divide = map (\((a,b),(c,d)) -> if d == 0 then (a,0) else (a, b /d)) $ zip normal2 normal1
  plotDivide divide

nodes =
        [ GF "Fred_Skill" 100 5
        , GF "Jill_Skill" 120 40
        , DGF "Fred_perf" "Fred_Skill" 5
        , DGF "Jill_perf" "Jill_Skill" 5
        , GrT "Jill_Win" ("Fred_perf", "Jill_perf") 0.5
        ]

graph = generateGraph nodes 

plotPriorVsPost :: String -> [(Double,Double)] -> [(Double,Double)] -> [(Double,Double)] -> [(Double,Double)]-> IO ()
plotPriorVsPost title fredPrior fredPosterior jillPrior jillPosterior = toFile def ("../visualisation/" ++ title) $ do
  layout_title .= title
  plot (line "Fred Prios" [fredPrior])
  plot (line "Fred Posterior" [fredPosterior])
  plot (line "Jill Prios" [jillPrior])
  plot (line "Jill Posterior" [jillPosterior])

task9 :: IO ()
task9 = do
  let [(fredSkill,mu1,std1),(jillSkill,mu2,std2)] =  snd $ evalVariables ["Fred_Skill","Jill_Skill"] ["Jill_Win"] [[True]] graph
  let [(fredSkill',mu1',std1'),(jillSkill',mu2',std2')] =  snd $ evalVariables ["Fred_Skill","Jill_Skill"] ["Jill_Win"] [[False]] graph

  let fredPrior = normalPDF 100 5 [-30..300]
  let jillPrior = normalPDF 120 40 [-30..300]
  -- Jill wins
  let fredPosterior = normalPDF mu1 std1 [-30..300]
  let jillPosterior = normalPDF mu2 std2 [-30..300]
  --  Jill looses
  let fredPosterior' = normalPDF mu1' std1' [-30..300]
  let jillPosterior' = normalPDF mu2' std2' [-30..300]
  plotPriorVsPost "Priors_vs_Posterios_Jill_Win.png" fredPrior fredPosterior jillPrior jillPosterior
  plotPriorVsPost "Priors_vs_Posterios_Jill_Loose.png" fredPrior fredPosterior' jillPrior jillPosterior'

main :: IO ()
main = do
  task1
