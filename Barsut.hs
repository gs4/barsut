module Main where

import System.Environment
import System.IO
import Monad

import LispTypes
import LispParsing
import LispFunctions

flushStr :: String -> IO ()           
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalStr :: Env -> String -> IO String
evalStr env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
           
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalStr env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= stdlib >>= until_ (== ":q") (readPrompt "Barsut>>> ") . evalAndPrint

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
  
