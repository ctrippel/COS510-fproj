{-# LANGUAGE FlexibleInstances #-}

-- Implementation of the Syntax and Operational Semantics of the Pi Calculus

module Pi where

-- For documentation, see the following pages:
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent.html
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent-Chan.html
-- http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map.html

import Concurrent

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Data.Map as M 
import qualified Data.List as L

-- Syntax of the Pi Calculus

type Name = String

instance Show (Chan Value) where
  show chan = "<channel>"

-- When reading through these data types, it is worth noting that *all* values
-- in this pi calculus are like locations in the STLC with references: they only
-- show up during evaluation, but *not* in programs a user might write.
--
-- In other words, the "abstract channel" object defined in your handout (as
-- "c" in the syntax) will actually be a Haskell channel (VChan below).  But
-- your translation will generate Pi terms, which only include expressions
-- (Exp), not values.

data Value
  = VChan (Chan Value)  -- channel value
  | VTup [Value]        -- tuple of values
  deriving Show    

data Exp
  = EVar Name           -- variable expression
  | ETup [Exp]          -- tuple of expressions
  deriving Show    

data Pattern
  = PVar Name           -- variable pattern
  | PTup [Pattern]      -- tuple pattern
  | Wild                -- wildcard pattern
  deriving Show    

data Typ
  = TChan Typ           -- channel type
  | TTup [Typ]          -- tuple type
  deriving Eq

instance Show Typ where
  show (TChan t) = "Chan " ++ (show t)
  show (TTup []) = "()"
  show (TTup (h:ts)) = "(" ++ (show h) ++
    (L.concatMap (\x -> ", " ++ (show x)) ts) ++ ")"

instance Show (Env -> IO ()) where
  show f = "<function>"

data Pi
  = Nil
  | Pi :|: Pi
  | New Name Typ Pi
  | Out Name Exp 
  | Inp Name Pattern Pi
  | RepInp Name Pattern Pi   -- repeated input
  | Embed (Env -> IO ()) Pi

instance Show Pi where
  show Nil = "0"
  show (p1 :|: p2) =
    "(" ++ (show p1) ++ ") | (" ++ (show p2) ++ ")"
  show (New x t p) =
    "new " ++ x ++ " : " ++ (show t) ++ ". " ++ (show p)
  show (Out x e) =
    "send " ++ x ++ "(" ++ (show e) ++ ")"
  show (Inp x pat p) =
    "rec " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (RepInp x pat p) =
    "rec! " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (Embed _ p) = "<function> " ++ (show p)

-- Useful Abbreviations

unitT :: Typ
unitT = TTup []

unitE :: Exp
unitE = ETup []

unitP :: Pattern
unitP = PTup []

printer :: String -> Pi
printer s = Embed (\_ -> putStr $ s ++ "\n") Nil

-- Static type checking

-- TASK!
-- Implement your pi calculus type checker here!

-- Recall that in Haskell, the Either type is used to represent computations
-- that may fail.  Either a b has two constructors: Left a, and Right b.
-- In the following functions, Left String represents an error with an error
-- message, and Right b represents success, returning a value of type b.

type Gamma = M.Map Name Typ

typeExp :: Gamma -> Exp -> Either String Typ
typeExp g (EVar e) = case (g M.! e) of
                         (TChan t) -> Right (TChan t)
                         (TTup t)  -> Right (TTup t)
typeExp g (ETup e) = let type_list = L.map (\x -> case (typeExp g x) of
                                                  Right (TChan t) -> (TChan t)
                                                  Right (TTup t)  -> (TTup t) ) e in Right (TTup type_list)

typePat :: Gamma -> Pattern -> Typ -> Either String Gamma
typePat g (PVar p) t         = Right (M.insert p t (M.empty))
typePat g (PTup p) (TChan t) = Left "Pattern did not type check" 
typePat g (PTup p) (TTup t)  = let tup_list = (L.zip p t) in
                               let tup_map = L.foldr (\(pat,typ) temp_map -> case (typePat g pat typ) of
                                                                                 Right gam -> M.union gam temp_map) M.empty tup_list in Right tup_map
typePat g Wild     t         = Right M.empty

checkPi :: Gamma -> Pi -> Either String ()
checkPi g Nil               = Right ()
checkPi g (p1 :|: p2)       = case (checkPi g p1, checkPi g p2) of
                            (Right (), Right ()) -> Right ()
                            _                    -> Left "Concurrent process did not type check"
checkPi g (New n t p)       = case (checkPi g p) of
                                  Right () -> if ( M.member n g == True ) 
                                                 then Left "New process did not type check"
                                              else  Right ()
                                  _        -> Left "New process did not type check"
checkPi g (Out n e)         = case (g M.! n) of
                                  (TChan c) -> case (typeExp g e) of
                                               Right typ -> if (typ == c) then Right () else Left "Out process did not type check"
                                               _         -> Left "Out process did not type check"
                                  _         -> Left "Out process did not type check"
checkPi g (Inp n pat p)     = case (g M.! n) of
                                  (TChan c) -> case (typePat g pat c) of 
                                                   Right gam -> case checkPi (M.union g gam) p of
                                                                    Right () -> Right ()
                                                                    _        -> Left "Imp process did not type check"
                                                   _         -> Left "Inp process did not type check"
                                  _         -> Left "Inp process did not type check"


checkPi g (RepInp n pat p)  = case (checkPi g (Inp n pat p)) of
                                  Right () -> Right ()
                                  _        -> Left "RepInp rocess did not type check"
checkPi g (Embed (envio) p) = checkPi g p




check :: Pi -> Either String ()
check p = checkPi M.empty p

-- Signals a dynamic error

type_error :: String -> a
type_error s = error $ "Run-time Type Error:" ++ s

-- Environments for interpreters

-- TASK!
-- Implement your interpreter here!

type Env = M.Map Name Value

-- eval_p env p v 
-- match a value v against a pattern p and extend environment env
eval_p :: Env -> Pattern -> Value -> Env
eval_p e (PVar n) (VChan c) = M.insert n (VChan c) e
eval_p e (PVar n) (VTup v) = M.insert n (VTup v) e
eval_p e (PTup n) (VChan c) = e
eval_p e (PTup n) (VTup v) = if (L.length n /= L.length v)
                                 then e
                             else if (L.length n == 0)
                                 then e
                             else let z = L.zip n v in L.foldr (\(x1,x2) y -> eval_p e x1 x2) e z
eval_p e (Wild) (VChan c) = e
eval_p e (Wild) (VTup v) = e

-- eval_e env e
-- evaluates e to a value in environment env
eval_e :: Env -> Exp -> Value
eval_e env (EVar x) = env M.! x
eval_e env (ETup es) = VTup (eval_es env es)
  where
    eval_es env [] = []
    eval_es env (e:es) = eval_e env e : eval_es env es

run :: Env -> Pi -> IO ()
run e Nil               = putStr "done\n"
run e (p1 :|: p2)       = parallel [run e p1, run e p2]
run e (New n t p)       = do
  c <- newChan
  run (M.insert n (VChan c) e) p
run e (Out n exp)       = case (e M.! n) of
                            (VChan c) -> writeChan c (eval_e e exp)
run e (Inp n pat p)     = do
  v <- readChan (case e M.! n of (VChan c) -> c)
  run (eval_p e pat v) p
run e (RepInp n pat p)  = do
  v <- readChan (case e M.! n of (VChan c) -> c)
  parallel [run (M.insert n v e) p, run e (RepInp n pat p)]
run e (Embed (envio) p) = run e p

start :: Pi -> IO ()
start p = run M.empty p
