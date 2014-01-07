import System.Random

data Hand = Rock | Scissors | Paper  deriving(Show,Eq)
data Result = Win | Lose | Tie deriving(Show)

fight :: Hand -> Hand -> Result
fight h1 h2 | h1 == h2 = Tie
            | h1 == Rock && h2 == Scissors = Win
            | h1 == Paper && h2 == Rock = Win
            | h1 == Scissors && h2 == Paper = Win
            | otherwise = Lose

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

play = do
    putStrLn "(r)ock, (p)aper, or (s)cissors"
    human <- getrps
    computer <- genrps
    putStrLn (show human ++ " vs. " ++ show computer)
    let result = fight human computer
    case result of
        Win ->  putStrLn "You Win!" 
        Lose -> putStrLn "You Lose!"
        Tie -> putStrLn "You Tied!"
