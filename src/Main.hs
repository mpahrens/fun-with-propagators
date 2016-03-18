module Main where
import Data.Propagator
import Control.Monad.ST

example1 :: Maybe Double
example1 = runST $ do {
  x <- cell
; y <- cell
; z <- cell
; _ <- lift2 (+) x y z
; _ <- write x 1.0
; _ <- write y 2.0
; content z
}

example2 :: Maybe Double
example2 = forwards (\c -> c * 9/5 + 32) 100

example3 :: Maybe Double
example3 = backwards (\c -> c * 9/5 + 32) 212

main :: IO ()
main = putStrLn "Hello, World!"
