module Main where
import qualified Discord as D
import qualified System.IO as I

main :: IO ()
main = do
  token <- I.withFile "secrets/token.txt" I.ReadMode I.hGetLine
  putStrLn token
