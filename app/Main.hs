{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)

import Techtron.JSON ()
import Techtron.JSON.Simple


main :: IO ()
main = do
  args <- getArgs
  if not $ null args
    then
      let arg0 = head args in
        if arg0 == "--"
          then do
            let fileName = args !! 1
            s <- readFile fileName
            let !jValue = parseJsonStr s
            print $ snd <$> jValue
            return ()
          else fail "args error: parser [--] [filename]"
    else do
      s <- getContents
      let !jValue = parseJsonStr s
      print $ snd <$> jValue
  return ()
