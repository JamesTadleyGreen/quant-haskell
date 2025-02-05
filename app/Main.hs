module Main where

import Data.Random (runRVar)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import MyLib (OrnstienUhlenbeck (..), cir, vasicek)
import System.Random.MWC (create)

main :: IO ()
main = do
  mwc <- create
  let vasSetup = OrnstienUhlenbeck {theta = 1, mu = 0.04, sigma = 0.03}
  let cirSetup = OrnstienUhlenbeck {theta = 0.5, mu = 0.06, sigma = 0.3}
  vasicekRun <- runRVar (vasicek 0.04 vasSetup 0.005 499) mwc
  cirRun <- runRVar (cir 0.04 cirSetup 0.005 499) mwc
  toFile def "example.png" $ do
    layout_title .= "Interest Rate Models"
    plot (line "vasicek" [zip ([1 ..] :: [Float]) vasicekRun])
    plot (line "cir" [zip ([1 ..] :: [Float]) cirRun])
