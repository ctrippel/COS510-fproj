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
compile_b tchan fchan (BVar str) = Out (str ++ "_query_") unitE :|: relayU (str ++ "_True_") tchan :|: relayU (str ++ "_False_") fchan
compile_b tchan fchan (BVal b) = if b then (Out tchan unitE) else (Out fchan unitE)
compile_b tchan fchan (b1 :&&: b2) = let left_true = "_and_chan_left_true_" ++ (show b1)
                                         left_false = "_and_chan_left_false_" ++ (show b1)
                                         right_true = "_and_chan_right_true_" ++ (show b2)
                                         right_false = "_and_chan_right_false_" ++ (show b2)
                                     in newChs [left_true, left_false,right_true, right_false] unitT
                                        (compile_b left_true left_false b1 :|:
                                         compile_b right_true right_false b2 :|:
                                         relayU left_false fchan :|:
                                         relayU right_false fchan :|:
                                         multiRelay [left_true, right_true] tchan)
                                                       
compile_b tchan fchan (b1 :||: b2) = let left_true = "_or_chan_left_true_" ++ (show b1)
                                         left_false = "_or_chan_left_false_" ++ (show b1)
                                         right_true = "_or_chan_right_true_" ++ (show b2)
                                         right_false = "_or_chan_right_false_" ++ (show b2)
                                     in newChs [left_true, left_false,right_true, right_false] unitT
                                        (compile_b left_true left_false b1 :|:
                                         compile_b right_true right_false b2 :|:
                                         relayU left_true tchan :|:
                                         relayU right_true tchan :|:
                                         multiRelay [left_false, right_false] fchan)
compile_b tchan fchan (Not b) = let true = "_not_chan_true" ++ (show b)
                                    false = "_not_chan_false" ++ (show b)
                                in newChs [true, false] unitT
                                   (compile_b true false b :|:
                                    relayU true fchan :|:
                                    relayU false tchan)

--Some Helper funcitons:
--Creates new channels from a list
newChs :: [Name] -> Typ -> Pi -> Pi
newChs [] _ p = p
newChs (ch:lch) t p = New ch t (newChs lch t p)

--Receives unit from first channel and sends unit to the the second channel
relayU :: Name -> Name -> Pi
relayU ch1 ch2 = Inp ch1 (PVar "_temp_var!") (Out ch2 unitE)


--Server: Receives unit from first channel and sends unit to the the second channel
relayU_serv :: Name -> Name -> Pi
relayU_serv ch1 ch2 = RepInp ch1 (PVar "_temp_var!") (Out ch2 unitE)

--Receives unit from a list of channels and then sends unit to the the last channel
multiRelay :: [Name] -> Name -> Pi
multiRelay [] ch = (Out ch unitE)
multiRelay (ch1:lch) ch2 = Inp ch1 (PVar "_temp_var!") (multiRelay lch ch2)



-- TASK!
-- compile a boolean variable environment into a process that
-- communicates with a compiled Boolean expression containing free
-- variables from the environment
compile_benv :: BEnv -> Pi -> Pi
compile_benv benv p = compile_blist (M.toList benv) p where
  compile_blist [] p = p
  compile_blist ((str, b):lpair) p =  newChs [query_ch, true_ch, false_ch] unitT ( relayU_serv query_ch ans_ch :|: compile_blist lpair p) where
    query_ch = str++"_query_"
    ans_ch = str++"_"++show(b)++"_"
    true_ch = str++"_True_"
    false_ch = str++"_False_"


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
           

