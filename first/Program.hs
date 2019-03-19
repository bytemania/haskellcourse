module Program where

main :: IO ()
main = do
    content <- readFile "numbers.txt"
    putStrLn content
