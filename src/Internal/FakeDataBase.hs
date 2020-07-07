{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, TypeOperators #-}
module Internal.FakeDataBase where
import           Database.Selda
import           Database.Selda.SQLite
import Database.Selda.Unsafe
import System.Random
import Control.Monad

playerStrengths :: IO [Double]
playerStrengths = replicateM (length names) (randomRIO (0::Double, 1))

results :: IO [(Int,Int,Int)]
results = replicateM 10000 generateResult

generateResult :: IO (Int,Int,Int)
generateResult = do
  let ids = [1..(length names +1)]
  pID1 <- randomRIO (1::Int, length names)
  let (xs,y:ys) = splitAt (pID1-1) ids
  pID2 <- (\nr ->  (xs ++ ys) !! nr) <$> randomRIO (0::Int, length names -2 )  
  str1 <- (\xs -> xs !! (pID1-1)) <$> playerStrengths
  str2 <- (\xs -> xs !! (pID2-1)) <$> playerStrengths
  perf1 <- randomRIO(0::Double,1)
  perf2 <- randomRIO(0::Double,1)
  return $ 
    if str1 + perf1 > str2 + perf2 
      then (pID1, pID2, pID1)
      else (pID1, pID2, pID2)

players :: Table (RowID :*: Text :*: Double :*: Double)
players = table "players" $ autoPrimary "id" :*: required "name" :*: required "mu" :*: required "std"

games :: Table (RowID :*: RowID :*: RowID :*: RowID)
games = table "games" $ autoPrimary "id" :*: required "player1" :*: required "player2" :*: required "winner"

numberGames :: IO Int
numberGames = withSQLite "my_database.sqlite" $ do
  plz <- query $ aggregate $ do
    (id :*: _) <- select games
    return $ count id
  games' <- query $ select games
  liftIO (print $ games')
  liftIO (return $ head plz)

getGameRecord :: Int -> IO (RowID,RowID,RowID,Double,Double,Double,Double) 
getGameRecord nr = withSQLite "my_database.sqlite" $ do
  values <- query $ getGameRecord' nr
  let (player1 :*: player2 :*: winner :*: mu :*: std :*: mu2 :*: std2) = head values
  liftIO (return  (player1,player2,winner,mu,std,mu2,std2))  

getGameRecord' nr = do
  (gid :*: player1 :*: player2 :*: winner) <- select games
  (id1 :*: _ :*: mu :*: std) <- select players
  restrict ( player1  .== id1 )
  (id2 :*: _ :*: mu2 :*: std2) <- select players
  restrict ( player2 .== id2 )
  restrict (gid .== literal (unsafeRowId nr))
  return (player1 :*: player2 :*: winner :*: mu :*: std :*: mu2 :*: std2)

setup :: SeldaM ()
setup = do
  createTable players
  createTable games
  

insertPlayers :: SeldaM ()
insertPlayers = do
  insert_ players $ map (\name -> def :*: name :*: (100::Double) :*: (50::Double)) names
  
insertGames :: [(Int,Int,Int)] -> SeldaM ()
insertGames results = do
  insert_ games $ map(\(a,b,c) -> def :*:  unsafeRowId a :*: unsafeRowId b :*: unsafeRowId c) results 

teardown :: SeldaM ()
teardown = do
  tryDropTable games
  tryDropTable players

names :: [Text]
names =
  [ "Elias"
  , "Liam"
  , "Linus"
  , "Julia"
  , "Jürgen"
  , "Jonas"
  , "CrazyCat"
  , "SleepyLion"
  , "Thomas"
  , "Pauls"
  , "Johannes"
  , "Marius"
  , "Tim"
  , "Maya"
  , "Simone"
  , "Andrea"
  , "Lukas"
  , "Verona"
  , "Kröte"
  , "KillerRabe"
  , "TheFrog"
  , "Grenui"
  , "Mirama"
  , "Miriam"
  , "Daniela"
  , "Natascha"
  , "Chantal"
  , "Tobias"
  , "Sebastian"
  , "Arnold"
  , "Mika"
  , "TheShadow"
  , "LuckyLuke"
  , "GraberPaba"
  , "HerrVonHöllestein"
  , "Winnie"
  , "Ursula"
  , "Gerald"
  , "Ulrike"
  , "PlayerBoy"
  , "Volker"
  , "Anna"
  , "Carla"
  , "James"
  , "Bond"
  , "Yogi"
  , "Napoleon"
  , "Riddler"
  , "Bierbauch"
  , "Butter"
  , "MisterX"
  , "Holger"
  , "Erich"
  , "Baldrian"
  , "Baldur"
  , "Jayn"
  , "Frank"
  , "Walter"
  , "Christiane"
  , "Matthias"
  , "Mina"
  , "Roscharch"
  , "Mika"
  , "Joe"
  , "Stepfhen"
  , "Dogbert"
  , "Dagobert"
  , "Hodor"
  , "Sam"
  , "Stefanie"
  , "TwitterGirl"
  , "Legolas"
  , "Kalle"
  , "LatteIgel"
  , "Barbara"
  , "Verona"
  , "Wilma"
  , "Sanders"
  , "Finn"
  , "Leopold"
  , "Bernie"
  , "Fritz"
  , "Peter"
  , "Harold"
  , "Sansa"
  , "Arya"
  , "Snow"
  , "Tyrion"
  , "Catelyn"
  , "Eddard"
  , "Robb"
  , "Bran"
  , "Tywin"
  , "Baratheon"
  , "Theon"
  , "Renly"
  , "Stannis"
  , "Tyenne"
  , "GreenEyes"
  , "FinalChapter"
  ]