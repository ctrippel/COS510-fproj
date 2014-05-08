import Pi
import Control.Exception.Base

n,i,o :: Name
n = "n"
i = "i"
o = "o"

test1 = printer "hello"

test2 = 
  New n unitT $
  (Inp n (PVar i) test1) :|: (Out n unitE)

test3 =
  New n unitT $
  (RepInp n (PVar i) test1) :|: (Out n unitE) :|: (Out n unitE):|: (Out n unitE)

rep n pi | n <= 0 = Nil
rep n pi | n > 0  = pi :|: rep (n-1) pi

test4 =
  New n unitT $
  (RepInp n (PVar i) test1) :|: (rep 100 (Out n unitE))

tests :: [Pi]
tests = [test1, test2, test3, test4]

run_tests :: [Pi] -> IO ()
run_tests ts = run_t 1 ts
  where
    run_t n []     = return ()
    run_t n (t:ts) = do
      putStr ("test " ++ show n ++ ":\n")
      start t
      putStr "\n"
      run_t (n+1) ts

main = run_tests tests