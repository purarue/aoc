import           System.Environment             ( getArgs )

calculate :: Int -> Int
calculate n = -2 + quot n 3

main :: IO ()
main = do
  args     <- getArgs
  contents <- readFile $ head args
  print $ sum $ map (calculate . read) . lines $ contents
