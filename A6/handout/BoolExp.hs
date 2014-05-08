{-
Syntax and Implementation of Boolean Expressions
================================================
-}

module BoolExp where

import Pi
import qualified Data.Map.Strict as M 

data BoolExp
  = BVar Name
  | BVal Bool
  | BoolExp :&&: BoolExp
  | BoolExp :||: BoolExp
  | Not BoolExp
  deriving Show

-- Environments for interpreting boolean expressions
type BEnv = M.Map Name Bool

-- TASK!
-- compile_b tchan fchan b
-- returns a process p that when juxtaposed with a compatible environment
-- sends a message on tchan if the boolean expression evaluates to true
-- sends a message on fchan if the boolean expression evaluates to false
compile_b :: Name -> Name -> BoolExp -> Pi
compile_b tchan fchan bexp = undefined
  

-- TASK!
-- compile a boolean variable environment into a process that
-- communicates with a compiled Boolean expression containing free
-- variables from the environment
compile_benv :: BEnv -> Pi -> Pi
compile_benv benv p = undefined

start_bool :: BEnv -> BoolExp -> IO ()
start_bool benv bexp = 
  start pi
    where
      tchan = "t"
      fchan = "f"   
      pi = New tchan unitT $ 
           New fchan unitT $ 
           compile_benv benv (compile_b tchan fchan bexp) :|:
           Inp tchan unitP (printer "true") :|:
           Inp fchan unitP (printer "false")
           
