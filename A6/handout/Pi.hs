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
typeExp = undefined

typePat :: Gamma -> Pattern -> Typ -> Either String Gamma
typePat = undefined

checkPi :: Gamma -> Pi -> Either String ()
checkPi = undefined

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
eval_p = undefined

-- eval_e env e
-- evaluates e to a value in environment env
eval_e :: Env -> Exp -> Value
eval_e env (EVar x) = env M.! x
eval_e env (ETup es) = VTup (eval_es env es)
  where
    eval_es env [] = []
    eval_es env (e:es) = eval_e env e : eval_es env es

run :: Env -> Pi -> IO ()
run = undefined

start :: Pi -> IO ()
start p = run M.empty p
