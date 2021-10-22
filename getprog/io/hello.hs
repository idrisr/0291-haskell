helloPerson :: String -> String
helloPerson name = "Hello " ++ name

mmain :: Maybe String
mmain = do 
    name <- Just "YO"
    return (helloPerson name)

fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
    n <- getLine
    let m = read n
    putStrLn $ show (fib $ m)
