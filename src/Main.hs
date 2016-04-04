module Main where
import Data.Propagator
import Control.Monad.ST
import Control.Monad.Trans.State

import Data.Functor.Identity
import Control.Monad.Trans.Class (lift)

import Data.STRef
import Control.Monad
mymerge :: Double -> Double -> Change Double
mymerge a b = Change (not (a == b)) b

mycell :: ST s (Cell s Double)
mycell = cellWith mymerge

example1 :: Maybe Double
example1 = runST $ do {
  x <- mycell
; y <- mycell
; z <- mycell
; _ <- lift2 (+) x y z
; _ <- lift2 (-) z y x
; _ <- lift2 (-) z x y
; _ <- write y (1.0 :: Double)
; _ <- write x (2.0 :: Double)
; content z
}
example2 :: Maybe Double
example2 = forwards (\c -> c * 9/5 + 32) 100

example3 :: Maybe Double
example3 = backwards (\c -> c * 9/5 + 32) 212

-- Monad Transformer
ttest1 :: StateT Integer Identity (Integer, Integer)
ttest1 = do {
  a <- get
; modify (+1)
; b <- get
; return (a,b)
}

ttest2 :: StateT String Identity (String, String)
ttest2 = do {
  a <- get
; modify (++"1")
; b <- get
; return (a,b)
}

tgo1 :: (Integer, Integer)
tgo1 = evalState ttest1 0
-- (0,1)
tgo2 :: (String, String)
tgo2 = evalState ttest2 "0"
-- ("0","01")

ttest3 :: StateT Integer (StateT String Identity) (Integer, String)
ttest3 = do {
  modify (+1)
; lift $ modify (++ "1")
; a <- get
; b <- lift get
; return (a,b)
}

tgo3 :: (Integer, String)
tgo3 = runIdentity $ evalStateT (evalStateT ttest3 0) "0"

ttest4 :: StateT Integer IO ()
ttest4 = do {
  modify (+1)
; a <- get
; lift (print a)
; modify (+1)
; b <- get
; lift (print b)
}

tgo4 :: IO ()
tgo4 = evalStateT ttest4 0

ttest5 :: StateT Integer (StateT String Identity) (Integer, String)
ttest5 = do {
  modify (+1)
; lift $ modify (++ "1")
; a <- get
; b <- lift get
; return (a,b)
}

tgo5 :: (Integer,String)
tgo5 = evalState (evalStateT ttest5 0) "0"

-- Matt Ahrens Examples
ttest6 :: FilePath -> StateT String IO String
ttest6 file = do {
  modify (++ "Read:\n")
; f <- lift $ readFile file
; modify (++ f)
; modify (++ "\nDone!\n")
; b <- get
; return b
}

tgo6 :: IO String
tgo6 = evalStateT (ttest6 "test.txt") ""

-- combine ST and IO
-- runST example
stexample1 :: Num a => [a] -> a
stexample1 xs = runST $ do {
  n <- newSTRef 0
; forM_ xs $ \x -> modifySTRef n (+x)
; readSTRef n
}

stexample2 = runST $ do {
  
}
main :: IO ()
main = putStrLn "Hello, World!"
