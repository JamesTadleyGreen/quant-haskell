module Main where

import Data.Random (runRVar)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import qualified MyLib (cir, vasicek)
import System.Random.MWC (create)

main :: IO ()
main = do
  mwc <- create
  vasicek <- runRVar (MyLib.vasicek 0.04 1 0.03 0.03 0.005 499) mwc
  cir <- runRVar (MyLib.cir 0.04 0.5 0.06 0.3 0.005 499) mwc
  toFile def "example.png" $ do
    layout_title .= "Vasicek model"
    plot (line "vasicek" [zip ([1 ..] :: [Float]) vasicek])
    plot (line "cir" [zip ([1 ..] :: [Float]) cir])
