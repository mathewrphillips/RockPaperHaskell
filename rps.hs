import System.Random
import Control.Monad.State

data Hand = Rock | Scissors | Paper  deriving(Show,Eq)

fight :: Hand -> Hand -> Result
fight h1 h2 | h1 == h2 = Tie
            | h1 == Rock && h2 == Scissors = Win
            | h1 == Paper && h2 == Rock = Win
            | h1 == Scissors && h2 == Paper = Win
            | otherwise = Lose

type Score = (Int,Int)
data Result = Win | Lose | Tie deriving(Show)

updateScore :: Result -> StateT Score IO Result 
updateScore r = do
    (h,c) <- get
    case r of
        Win -> put (h+1,c)
        Lose -> put (h,c+1)
        Tie -> put (h,c)
    return r

playR :: StateT Score IO ()
playR = do
    io $ putStrLn "(r)ock, (p)aper, or (s)cissors"
    r <- io $ liftM2 fight getrps genrps
    io $ print r
    updateScore r
    s <- get
    io $ putStrLn ((show . fst) s ++ " - " ++ (show . snd )s)
    if isGameOver s then endGame s else playR 

endGame :: Score -> StateT Score IO () 
endGame (3,_) = io $ putStrLn "You Win!"
endGame (_,3) = io $ putStrLn "Sorry You Lost"

isGameOver :: Score -> Bool
isGameOver (x,y) | x == 3 || y == 3 = True
                 | otherwise = False

io :: IO a -> StateT Score IO a
io = liftIO

genrps :: IO Hand
genrps = fmap rps rand
    where rps 1 = Rock
          rps 2 = Paper
          rps _ = Scissors
          rand = randomRIO (1,3) :: IO Int

getrps = fmap rps getLine
    where rps "r" = Rock
          rps "p" = Paper
          rps "s" = Scissors
          rps _ = error "invalid choice"

main = void $ runStateT playR (0,0)
    
