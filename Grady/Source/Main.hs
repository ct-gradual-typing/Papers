{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.State
import System.Console.Haskeline

import qualified Core.Repl as CG
import qualified Surface.Repl as SG

banner :: String
banner = "Welcome to Grady!"

helpMenu :: String
helpMenu =
      "----------------------------------------------------------\n"++
      "                  The Grady Help Menu                     \n"++
      "----------------------------------------------------------\n"++
      ":help          (:h)  Display the help menu\n"++
      ":quit          (:q)  exit Grady\n"++
      ":surface-grady (:sg) Surface Grady\n"++
      ":core-grady    (:sg) Core Grady\n"++
      "----------------------------------------------------------"

main :: IO ()
main = do
  putStrLn banner
  runInputT defaultSettings loop
   where 
       loop :: InputT IO ()
       loop = do           
           minput <- getInputLine "Grady> "
           case minput of
               Nothing -> return ()
               Just [] -> loop
               Just input | input == ":q" || input == ":quit"
                              -> liftIO $ putStrLn "Goodbye!" >> return ()
                          | input == ":h" || input == ":help"
                              -> (liftIO $ putStrLn helpMenu) >> loop
                          | input == ":sg" || input == ":surface-grady"
                              -> (liftIO $ SG.repl) >> loop
                          | input == ":cg" || input == ":core-grady"
                              -> (liftIO $ CG.repl) >> loop
                          | otherwise -> (liftIO $ putStrLn helpMenu) >> loop
                          