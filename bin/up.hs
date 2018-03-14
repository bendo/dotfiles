import Data.List.Split (splitOn)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs)

main :: IO ()
main = mapM_ up =<< getArgs

up :: String -> IO ()
up target = do
    pwd <- getCurrentDirectory
    putStrLn ((head $ splitOn target pwd) ++ target)

