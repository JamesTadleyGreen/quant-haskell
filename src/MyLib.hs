module MyLib (vasicek) where

import Control.Monad (replicateM)
import Data.Random (RVar, normal)

type Mean = Float

type MeanReversion = Float

type Variance = Float

type TimeStep = Float

ornsteinUhlenbeckStep :: MeanReversion -> Mean -> Variance -> TimeStep -> Float -> Float -> Float
ornsteinUhlenbeckStep theta mu sigma dt x_prev z = x_prev + drift + noise
  where
    drift = theta * (mu - x_prev) * dt
    noise = sigma * z

-- https://en.wikipedia.org/wiki/Ornstein%E2%80%93Uhlenbeck_process
ornsteinUhlenbeck :: Float -> MeanReversion -> Mean -> Variance -> TimeStep -> Int -> RVar [Float]
ornsteinUhlenbeck x theta mu sigma dt number_of_steps = do
  zs <- replicateM number_of_steps (normal 0 1)
  pure $ scanl (ornsteinUhlenbeckStep theta mu sigma dt) x zs

-- https://en.wikipedia.org/wiki/Vasicek_model
vasicek :: Float -> MeanReversion -> Mean -> Variance -> TimeStep -> Int -> RVar [Float]
vasicek x theta mu sigma dt = ornsteinUhlenbeck x theta mu (sigma * sqrt dt) dt
