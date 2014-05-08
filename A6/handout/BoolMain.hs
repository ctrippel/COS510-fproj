import BoolExp
import qualified Data.Map as M 

t = BVal True
f = BVal False

x = "x"
y = "y"

env1 = M.empty
env2 = M.insert x True env1 
env3 = M.insert y False env2 

type Test = (BEnv, BoolExp)

test1 = (env1, t)
test2 = (env1, f)
test3 = (env1, t :&&: (f :||: t) :&&: (Not f))
test4 = (env2, BVar x)
test5 = (env3, BVar x :&&: BVar y)
test6 = (env3, BVar x :&&: (BVar y :||: (t :&&: (f :||: t))))
test7 = (env3, BVar x :&&: (BVar y :||: (t :&&: (f :||: f))))
test8 = (env3, Not (BVar y))
test9 = (env3, Not (BVar y :&&: BVar x))
test10 = (env3, BVar y :||: (BVar x :&&: BVar x))

tests = [test1, test2, test3, test4, test5]

run_tests :: [Test] -> IO ()
run_tests ts = run_t 1 ts
  where
    run_t n []     = return ()
    run_t n ((env,b):ts) = do
      putStr ("test " ++ show n ++ ":\n")
      start_bool env b
      putStr "\n"
      run_t (n+1) ts

main = run_tests tests