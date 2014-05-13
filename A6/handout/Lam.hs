{-
Compiling the Lambda Calculus
====================================

Sending channels through channels is an interesting model, but how many
algorithms can we really implement this way?  It turns out that the
pi-calculus can implement any computable function.  

We could demonstrate this by compiling the untyped Lambda Calculus to the
untyped Pi Calculus but since we have a typed Pi Calculus as a target,
we will start with a typed Lambda Calculus.  It isn't Turing Complete,
but it's pretty cool none-the-less!

-}

module Lam where

import Pi hiding (Gamma)
import qualified Data.Map as M 
import qualified Data.IORef as R

-- The typed lambda calculus:

data LTyp  
  = LTUnit
  | LTArrow LTyp LTyp
  deriving (Eq, Show)

data Lam
  = LUnit              -- unit:  ()
  | LVar Name          -- variables:  x
  | LAbs Name LTyp Lam -- lambda abstraction: \x:t.e
  | Lam :@: Lam        -- application:  f :@: e executes f on argument e
  | LEff (IO ()) Lam   -- run an effectful computation of your choice
                       -- see printL below for a useful example

instance Show Lam where
  show LUnit = "()"
  show (LVar x) = x
  show (LAbs x t e) = "(\\" ++ x ++ " : " ++ (show t) ++ ". " ++ (show e) ++ ")"
  show (e1 :@: e2) = (show e1) ++ "(" ++ (show e2) ++ ")"
  show (LEff _ e) = "LEff _ (" ++ (show e) ++ ")"

-- Useful abbreviations:

-- printL s e is a lambda expression that prints s and then executes e
printL :: String -> Lam -> Lam
printL s e = LEff (putStr $ s ++ "\n") e

-- Environments for type checking Lambda expressions
type Gamma = M.Map Name LTyp

data Result a = Good a | Bad String

-- Lambda expression type checker
type_of :: Gamma -> Lam -> Result LTyp

type_of g LUnit = Good LTUnit

type_of g (LVar x) = 
  case M.lookup x g of
    Nothing -> Bad ("var not found: " ++ x)
    Just t -> Good t

type_of g (LAbs x t e) = 
  case type_of (M.insert x t g) e of
    Good t2 -> Good (LTArrow t t2)
    x -> x

type_of g (e1 :@: e2) = 
  case (type_of g e1, type_of g e2) of
    (Good (LTArrow targ tresult), Good t2) ->
       if targ == t2 then Good tresult
       else Bad "application type mismatch" 
    (Good t1, Good t2) -> Bad "not arrow type"
    (Bad x, _ ) -> Bad x
    (_, Bad y) -> Bad y

type_of g (LEff a e) = type_of g e

-- type check closed expressions
-- check :: Lam -> IO Bool
-- check e =
--   case type_of M.empty e of
--     Good x -> return True
--     Bad s -> putStr s >> return False

-- Linear lambda expression type checker
lintype_of :: Gamma -> Lam -> Result (Gamma, LTyp)

lintype_of g LUnit = Good (g, LTUnit)

lintype_of g (LVar x) = 
  case M.lookup x g of
    Nothing -> Bad ("var not found: " ++ x)
    Just t -> Good (M.delete x g, t)

lintype_of g (LAbs x t e) = 
  case lintype_of (M.insert x t g) e of
    Good (g',t2) -> 
      if M.member x g' then Bad ("var " ++ x ++ " not used")
      else 
        case M.lookup x g of
          Nothing -> Good (g', LTArrow t t2)
          Just tx -> Good (M.insert x tx g', LTArrow t t2)
    x -> x

lintype_of g (e1 :@: e2) = 
  case lintype_of g e1 of
    Good (g1, LTArrow targ tresult) ->
      case lintype_of g1 e2 of
        Good (g2, t2) ->
          if targ == t2 then Good (g2, tresult)
          else Bad "application type mismatch" 
        x -> x
    Good (g1, t) -> Bad "not arrow type" 
    x -> x

lintype_of g (LEff a e) = lintype_of g e

-- linear type check closed expressions
lincheck :: Lam -> IO Bool
lincheck e =
  case lintype_of M.empty e of
    Good (g,t) -> if M.null g then return True else return False
    Bad s -> putStr (s ++ "\n") >> return False

type Counter = R.IORef Integer

nameGenerator :: Counter -> IO Name
nameGenerator counter = do
  n <- R.readIORef counter
  R.modifyIORef' counter (+1)
  return ("x" ++ show n)


-- TASK!
-- Implement your lambda calculus to pi calculus compiler here!

-- First, generate a target Pi Calculus type
typeTrans :: LTyp -> Typ
typeTrans LTUnit = unitT
typeTrans (LTArrow t1 t2) = TTup [TChan (typeTrans t1), TChan (typeTrans t2)]

-- Second, implement your compiler
compile_lam :: IO Name -> Name -> Gamma -> Lam -> IO (LTyp, Pi)
compile_lam c ch g LUnit = do
  return (LTUnit, Out ch unitE)
compile_lam c ch g (LVar x) = do
  tp <- case type_of g (LVar x) of
    Good t -> return t
    Bad e -> error e
  return (tp, Out ch (EVar x))
compile_lam c ch g (LAbs x t1 e ) = do
  (t2, q2) <- compile_lam c "n2" (M.insert x t1 g) e
  tp <- return $ LTArrow t1 t2
  q1 <- return $ New "n1" (TChan (typeTrans t1)) ( --fresh variables
    New "n2" (TChan (typeTrans t2)) ( --fresh variables
       Out ch (ETup [EVar "n1", EVar "n2"]) :|:
       Inp "n1" (PVar x) q2))
  return (tp, q1) -- choose a fresh name --Check the channel name
compile_lam c ch g (e1 :@: e2 ) = do
  (t2, q2) <- compile_lam c "temp_channel" g e2
  (t1, q1) <- compile_lam c ch g e1
  tp <- case type_of g (e1 :@: e2 ) of
    Good t -> return t
    Bad e -> error e
  return (tp, New "temp_channel" (typeTrans t2) (q1 :|: q2))
compile_lam c ch g (LEff f e) = do
  (t,q) <- compile_lam c ch g e
  return (t, Embed (\_ -> f) q)

start_lam :: Lam -> IO ()
start_lam e = do
  b <- lincheck e
  if not b 
    then putStr "Source program does not type check.\n"
    else do
      r <- R.newIORef 0
      let fresh = nameGenerator r
      n <- fresh
      (t,pi) <- compile_lam fresh n M.empty e 
      let wrap = New n (TChan $ typeTrans t) $ pi :|: Inp n Wild (printer "done!")
      case check wrap of
        Left err -> do
          putStrLn $ "Translated program does not type check.  Program:"
          putStrLn $ show wrap
          putStrLn $ "Error: \n" ++ err
        Right () -> start wrap
