module MyLib (vasicek, chen, cir, OrnstienUhlenbeck (..)) where

import Control.Monad (replicateM)
import Data.Random (RVar, normal)

type Initial = Float

type TimeStep = Float

data OrnstienUhlenbeck = OrnstienUhlenbeck
  { theta :: Float,
    mu :: Float,
    sigma :: Float
  }

-- https://en.wikipedia.org/wiki/Ornstein%E2%80%93Uhlenbeck_process
ornsteinUhlenbeck :: Float -> (Float -> Float -> Float) -> Int -> RVar [Float]
ornsteinUhlenbeck x f number_of_steps = do
  zs <- replicateM number_of_steps (normal 0 1)
  pure $ scanl f x zs

vasicekStep :: OrnstienUhlenbeck -> TimeStep -> Float -> Float -> Float
vasicekStep ou dt x_prev z = x_prev + drift + noise
  where
    drift = theta ou * (mu ou - x_prev) * dt
    noise = sigma ou * sqrt dt * z

-- https://en.wikipedia.org/wiki/Vasicek_model
vasicek :: Initial -> OrnstienUhlenbeck -> TimeStep -> Int -> RVar [Float]
vasicek r ou dt = ornsteinUhlenbeck r (vasicekStep ou dt)

cirStep :: OrnstienUhlenbeck -> TimeStep -> Float -> Float -> Float
cirStep ou dt r_prev z = r_prev + drift + noise
  where
    drift = theta ou * (mu ou - r_prev) * dt
    noise = sigma ou * sqrt (dt * r_prev) * z

-- https://en.wikipedia.org/wiki/Cox%E2%80%93Ingersoll%E2%80%93Ross_model
cir :: Initial -> OrnstienUhlenbeck -> TimeStep -> Int -> RVar [Float]
cir r ou dt = ornsteinUhlenbeck r (cirStep ou dt)

chenStep :: Float -> Float -> Float -> (Float, Float, Float) -> Float
chenStep k dt r (t, s, z) = r + drift + noise
  where
    drift = k * (t - r) * dt
    noise = sqrt r * sqrt s * sqrt dt * z

-- https://en.wikipedia.org/wiki/Chen_model
chen :: Initial -> Initial -> Initial -> Float -> OrnstienUhlenbeck -> OrnstienUhlenbeck -> TimeStep -> Int -> RVar [Float]
chen r t s k t_ou s_ou dt n = do
  thetas <- cir t t_ou dt n
  sigmas <- cir s s_ou dt n
  zs <- replicateM n (normal 0 1) :: RVar [Float]
  return $ scanl (chenStep k dt) r $ zip3 thetas sigmas zs
