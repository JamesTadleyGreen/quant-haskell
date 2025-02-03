module Main where

import Data.Random (runRVar)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import qualified MyLib (vasicek)
import System.Random.MWC (create)

main :: IO ()
main = do
  mwc <- create
  path <- runRVar (MyLib.vasicek 0.03 0.05 0.05 0.02 1 365) mwc
  toFile def "example.png" $ do
    layout_title .= "Vasicek model"
    plot (line "am" [zip ([1 ..] :: [Float]) path])
