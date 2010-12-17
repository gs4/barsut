{-# LANGUAGE ExistentialQuantification #-}

module LispTypes (Unpacker (AnyUnpacker), 
                  LispVal (Atom, 
                           List,
                           DottedList,
                           Number,
                           String,
                           Bool,
                           PrimitiveFunc,
                           Func,
                           IOFunc, 
                           Port),
                  LispError (NumArgsErr,
                             TypeMismatchErr,
                             ParserErr,
                             BadSpecialFormErr,
                             NotFunctionErr,
                             UnboundVarErr,
                             DefaultErr),                  
                  Env,
                  ThrowsError,
                  IOThrowsError,
                  Continuation,
                  idCont,
                  trapError,
                  extractValue,
                  liftThrows,
                  runIOThrows,
                  nullEnv)
       where

import System.IO
import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec
       
-----

type Env = IORef [(String, IORef LispVal)]
type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO
type Continuation = LispVal -> IOThrowsError LispVal


-----

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params   :: [String],
                     vararg   :: (Maybe String),
                     body     :: [LispVal],
                     closure  :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle


data LispError = NumArgsErr Integer [LispVal]
               | TypeMismatchErr String LispVal
               | ParserErr ParseError
               | BadSpecialFormErr String LispVal
               | NotFunctionErr String String
               | UnboundVarErr String String
               | DefaultErr String


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)



-------------

idCont :: Continuation
idCont val = return val

-------------

showVal :: LispVal -> String  
showVal (String conts) = "\"" ++ conts ++ "\""
showVal (Atom name) = name
showVal (Number conts) = show conts
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List conts) = "(" ++ unwordsList conts ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env})
  = "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"        
showVal (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
                            
--------------              
                            

showError :: LispError -> String
showError (UnboundVarErr message varname) = message ++ ": " ++ varname
showError (BadSpecialFormErr message form) = message ++ ": " ++ show form
showError (NotFunctionErr message func) = message ++ ": " ++ show func
showError (NumArgsErr expected found) = "Expected " ++ show expected 
                     ++ " args; found values " ++ unwordsList found
showError (TypeMismatchErr expected found) = "Invalid type: expected " ++ expected
                     ++ ", found " ++ show found
showError (ParserErr parseErr) = "Parse error at " ++ show parseErr
                              
instance Show LispError where show = showError

instance Error LispError where
  noMsg = DefaultErr "An error occurred"
  strMsg = DefaultErr
  
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


--------------

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

