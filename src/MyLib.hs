module MyLib (vasicek) where

import Control.Monad (replicateM)
import Data.Random (RVar, normal)

ornsteinUhlenbeckStep :: Float -> Float -> Float -> Float -> Float -> Float -> Float
ornsteinUhlenbeckStep theta mu sigma dt z x_prev = x_prev + drift + noise
  where
    drift = theta * (mu - x_prev) * dt
    noise = sigma * z

-- https://en.wikipedia.org/wiki/Ornstein%E2%80%93Uhlenbeck_process
ornsteinUhlenbeck :: Float -> Float -> Float -> Float -> Float -> Int -> RVar [Float]
ornsteinUhlenbeck x theta mu sigma dt number_of_steps = do
  zs <- replicateM number_of_steps (normal 0 1)
  pure $ scanl (ornsteinUhlenbeckStep theta mu sigma dt) x zs

-- https://en.wikipedia.org/wiki/Vasicek_model
vasicek :: Float -> Float -> Float -> Float -> Float -> Int -> RVar [Float]
vasicek = ornsteinUhlenbeck
