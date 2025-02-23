module Main where

import Data.Random (runRVar)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import MyLib (OrnstienUhlenbeck (..), chen, cir, vasicek)
import System.Random.MWC (create)

vasParam :: OrnstienUhlenbeck
vasParam = OrnstienUhlenbeck {theta = 1, mu = 0.05, sigma = 0.03}

cirParam :: OrnstienUhlenbeck
cirParam = OrnstienUhlenbeck {theta = 1, mu = 0.05, sigma = 0.3}

chenMeanParam :: OrnstienUhlenbeck
chenMeanParam = OrnstienUhlenbeck {theta = 2, mu = 0.05, sigma = 0.03}

chenVarianceParam :: OrnstienUhlenbeck
chenVarianceParam = OrnstienUhlenbeck {theta = 0.3, mu = 0.02, sigma = 0.1}

main :: IO ()
main = do
  mwc <- create
  vasicekRun <- runRVar (vasicek 0.14 vasParam 0.005 499) mwc
  cirRun <- runRVar (cir 0.14 cirParam 0.005 499) mwc
  chenRun <- runRVar (chen 0.14 0.05 0.03 0.5 chenMeanParam chenVarianceParam 0.005 499) mwc
  toFile def "example.png" $ do
    layout_title .= "Interest Rate Models"
    plot (line "vasicek" [zip ([1 ..] :: [Float]) vasicekRun])
    plot (line "cir" [zip ([1 ..] :: [Float]) cirRun])
    plot (line "chen" [zip ([1 ..] :: [Float]) chenRun])
