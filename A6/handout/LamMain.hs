import Lam
import qualified Data.Map as M 

type Test = Lam

u = LTUnit

infixr 5 ~>
t1 ~> t2 = LTArrow t1 t2

x = "x"
xv = LVar x
y = "y"
yv = LVar y
f = "f"
fv = LVar f

-- linear function
test1 = (LAbs x u $ printL "hello!\n" xv) :@: LUnit

app = 
  LAbs f (u ~> u) $
    LAbs x u $
      fv :@: xv 

test2 = 
  (app :@: (LAbs x u $ printL "hello!\n" xv)) :@: (printL "hi" LUnit)

-- non-linear function (uses y twice)
-- does not type check
test3 = LAbs y u $
          LAbs f (u ~> u ~> u) $ 
            (fv :@: yv) :@: yv

tests = [test1, test2, test3]

run_tests :: [Test] -> IO ()
run_tests ts = run_t 1 ts
  where
    run_t n []     = return ()
    run_t n (e:ts) = do
      putStr ("test " ++ show n ++ ":\n")
      start_lam e
      putStr "\n"
      run_t (n+1) ts

main = run_tests tests
