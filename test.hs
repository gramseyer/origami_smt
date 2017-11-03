module F where
import Data.SBV
import Data.SBV.Control

f :: Symbolic (AlgReal)
f = do
    x<- sReal "x"
    y <- sReal "y"
    constrain $ x.>= 0
    constrain $ y.>= 0
    constrain $ x*x.==10
    query $ do
      cs <- checkSat
      case cs of
          Unk -> error "foo"
          Unsat -> error "unsat"
          Sat -> do
                xv <- getValue x
                yv <- getValue y
                io $ putStrLn $ show (xv, yv)
                return xv

g :: AlgReal -> Symbolic (AlgReal)
g x = do
    z <- sReal "z"
    constrain $ literal x .== z
    query $ do
        cs <- checkSat
        case cs of
            Unk -> error "foo"
            Unsat -> error "unsat"
            Sat -> do
                 zv <- getValue z
                 io $ putStrLn $ show zv
                 return zv
