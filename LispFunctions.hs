module LispFunctions (apply,
                      eval,
                      bindVars,
                      primitiveBindings)
       where

import Control.Monad.Error
import Monad
import System.IO
import Data.IORef

import LispTypes
import LispParsing

----------

type LispFn = ([LispVal] -> ThrowsError LispVal)
type LispIOFn = ([LispVal] -> IOThrowsError LispVal)

------------

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVarErr "Unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do env <- liftIO $ readIORef envRef
                           maybe (throwError $ UnboundVarErr "Unbound variable" var)
                                 (liftIO . (flip writeIORef val))
                                 (lookup var env)
                           return val
                           
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal                           
defineVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var val >> return val
    else liftIO $ do
      valueRef <- newIORef val
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return val
      
bindVars :: Env -> [(String, LispVal)] -> IO Env      
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, val) = do ref <- newIORef val
                                   return (var, ref)
                                   
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ 
                                 map (makeFunc IOFunc) ioPrimitives
                                 ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . show

-------------------

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgsErr (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
 where         
   remainingArgs = drop (length params) args
   num = toInteger . length
   evalBody env = liftM last $ mapM (eval env) body
   bindVarArgs arg env = case arg of
     Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
     Nothing -> return env
apply badArg args = throwError $ NotFunctionErr "tried to apply a nonfunction" $ (show badArg) ++ " applied to [" ++ (unwords $ map show args) ++ "]"

-----

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "load", String fname]) =
  load fname >>= liftM last . mapM (eval env)
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =       
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =       
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List (function : args)) = do 
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialFormErr "Uncrecoginized special form" badForm

-------------------

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numNumBinop (+)),
              ("-", numNumBinop (-)),
              ("*", numNumBinop (*)),
              ("/", numNumBinop div),
              ("mod", numNumBinop mod),
              ("quotient", numNumBinop quot),
              ("remainder", numNumBinop rem),
              ("remainder", numNumBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),              
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string>?", strBoolBinop (>)),
              ("string<?", strBoolBinop (<)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("pair?", pair)]
             
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll),
                ("display", display)]
               
applyProc :: LispIOFn
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

closePort :: LispIOFn
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: LispIOFn
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

writeProc :: LispIOFn
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

display :: LispIOFn
display [String str] = writeProc [String str, Port stdout]

readContents :: LispIOFn
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [badArg] = liftThrows $ throwError $ TypeMismatchErr "expected filename " badArg
readContents badArgs = liftThrows $ throwError $ NumArgsErr 1 badArgs

makePort :: IOMode -> LispIOFn
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

readAll :: LispIOFn
readAll [String fname] = liftM List $ load fname

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

------------


car :: LispFn
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatchErr "pair" badArg
car badArgList = throwError $ NumArgsErr 1 badArgList

cdr :: LispFn
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatchErr "pair" badArg
cdr badArgList = throwError $ NumArgsErr 1 badArgList

cons :: LispFn
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgsErr 2 badArgList

eqv :: LispFn
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                 (and $ map eqvPair $ zip arg1 arg2)
                                   where eqvPair (x1, x2) = case eqv [x1, x2] of
                                           Left err -> False
                                           Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgsErr 2 badArgList

----

pair :: LispFn
pair [(DottedList _ _)] = return $ Bool True
pair [(List _)] = return $ Bool True
pair _ = return $ Bool False

----


equal :: LispFn
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgsErr 2 badArgList

----


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
 `catchError` (const $ return False)



boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgsErr 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right
                                     
numBoolBinop = boolBinop unpackNum                                     
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatchErr "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatchErr "boolean" notBool

numNumBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numNumBinop op singleVal@[_] = throwError $ NumArgsErr 2 singleVal
numNumBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
  if null parsed 
  then throwError $ TypeMismatchErr "number" $ String n
  else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatchErr "number" notNum


------

