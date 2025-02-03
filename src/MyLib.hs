module MyLib (vasicek, cir) where

import Control.Monad (replicateM)
import Data.Random (RVar, normal)

type Mean = Float

type MeanReversion = Float

type Variance = Float

type TimeStep = Float

-- https://en.wikipedia.org/wiki/Ornstein%E2%80%93Uhlenbeck_process
ornsteinUhlenbeck :: Float -> (Float -> Float -> Float) -> Int -> RVar [Float]
ornsteinUhlenbeck x f number_of_steps = do
  zs <- replicateM number_of_steps (normal 0 1)
  pure $ scanl f x zs

vasicekStep :: MeanReversion -> Mean -> Variance -> TimeStep -> Float -> Float -> Float
vasicekStep theta mu sigma dt x_prev z = x_prev + drift + noise
  where
    drift = theta * (mu - x_prev) * dt
    noise = sigma * sqrt dt * z

-- https://en.wikipedia.org/wiki/Vasicek_model
vasicek :: Float -> MeanReversion -> Mean -> Variance -> TimeStep -> Int -> RVar [Float]
vasicek r b a sigma dt = ornsteinUhlenbeck r (vasicekStep b a sigma dt)

cirStep :: MeanReversion -> Mean -> Variance -> TimeStep -> Float -> Float -> Float
cirStep b a sigma dt r_prev z = r_prev + drift + noise
  where
    drift = b * (a - r_prev) * dt
    noise = sigma * sqrt (dt * r_prev) * z

-- https://en.wikipedia.org/wiki/Vasicek_model
cir :: Float -> MeanReversion -> Mean -> Variance -> TimeStep -> Int -> RVar [Float]
cir r b a sigma dt = ornsteinUhlenbeck r (cirStep b a sigma dt)
