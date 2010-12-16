module Main where

import System.Environment
import System.IO
import Monad
import System.Console.Readline

import LispTypes
import LispParsing
import LispFunctions

readPrompt :: String -> IO (Maybe String)
readPrompt prompt = 
  do maybeLine <- readline prompt
     case maybeLine of
       Nothing -> return Nothing
       Just line -> do addHistory line
                       return $ Just line
       

evalStr :: Env -> String -> IO String
evalStr env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
           
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalStr env expr >>= putStrLn

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= stdlib >>= replLoop
  where replLoop env = do        
          input <- readPrompt "Barsut %> "
          case input of
            Nothing -> putStrLn "bye!" >> return ()
            Just "" -> replLoop env
            Just line -> do evalAndPrint env line
                            replLoop env



stdlib :: Env -> IO Env
stdlib env = evalAndPrint env "(load \"stdlib.scm\")" >> return env

main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> runOne args
            otherwise -> runUsage
            
runUsage :: IO ()            
runUsage = do
  name <- getProgName
  putStrLn $ "Usage: " ++ name ++ " [fname]"
  
