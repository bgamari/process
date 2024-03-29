{-# LANGUAGE BangPatterns #-}
module Main ( main ) where

-- base
import System.Environment
import System.IO

-- deepseq
import Control.DeepSeq
  ( force )

-- process
import System.Process.CommunicationHandle
  ( useCommunicationHandle )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ chRead, chWrite ] -> do
      hRead  <- useCommunicationHandle $ read chRead
      hWrite <- useCommunicationHandle $ read chWrite
      putStrLn "child stdout 1"
      input <- hGetContents hRead
      putStrLn "child stdout 2"
      let !output = force $ reverse input ++ "123"
      putStrLn "child stdout 3"
      hPutStr hWrite output
      putStrLn "child stdout 4"
      hClose hWrite
    _ -> error "expected two CommunicationHandle arguments"
