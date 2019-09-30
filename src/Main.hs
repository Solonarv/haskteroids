{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Word

import Apecs
import Apecs.Physics
import Apecs.Gloss
import Apecs.Physics.Gloss
import Linear.Vector
import System.Random

data Player = Player { playerShootCooldown :: Double, playerForwardAccel :: Double }
instance Component Player where
  type Storage Player = Unique Player

newtype BodyColor = BodyColor { getBodyColor :: Color }
  deriving (Eq)
instance Component BodyColor where
  type Storage BodyColor = Map BodyColor

newtype Size = Size { getSize :: Word }
  deriving newtype (Eq, Ord, Num, Real, Integral, Bounded, Enum)
instance Component Size where
  type Storage Size = Map Size

newtype Score = Score { getScore :: Int }
  deriving newtype (Eq, Ord, Num, Real, Integral, Bounded, Enum)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where
  type Storage Score = Global Score

makeWorld "World" [''Physics, ''BodyColor, ''Camera, ''Player, ''Size, ''Score]

mkAsteroid :: (Has w IO Physics, Has w IO EntityCounter, Has w IO BodyColor, Has w IO Size) => Word -> System w Entity
mkAsteroid size = do
  asteroid <- newEntity (asteroidTemplate size)
  newEntity (Shape asteroid (cCircle (fromIntegral size)))
  where
    asteroidTemplate size =
      ( DynamicBody
      , Density 1000
      , Elasticity 0.9
      , BodyColor red
      , defaultFilter{ filterCategories = maskList [2] }
      , CollisionType 2
      , Size size
      )

mkPlayer :: (Has w IO Physics, Has w IO EntityCounter, Has w IO BodyColor, Has w IO Player) => System w Entity
mkPlayer = do
  player <- newEntity playerTemplate
  newEntity (Shape player playerShape)
  where
    playerTemplate =
      ( KinematicBody
      -- , BodyMass 1e3
      , Elasticity 1
      , BodyColor white
      , defaultFilter{ filterCategories = maskList [0], filterMask = maskList [2]}
      , Player 0 0
      )
    playerShape = Convex [V2 0 1, V2 0.6 0.8, V2 (-0.6) 0.8] 0.2

mkBullet :: (Has w IO Physics, Has w IO EntityCounter, Has w IO BodyColor, Has w IO Size, Has w IO Score) => (Position, Angle, Velocity) -> System w Entity
mkBullet posData = do
  killAsteroidCB <- mkBeginCB killAsteroid
  let collider = CollisionHandler (Between 1 2) (Just killAsteroidCB) Nothing Nothing Nothing
  bullet <- newEntity (bulletTemplate, posData, collider)
  newEntity (Shape bullet (cCircle 1))
  where
    bulletTemplate = 
      ( DynamicBody
      , BodyMass 10
      , Elasticity 0.1
      , BodyColor cyan
      , defaultFilter{ filterCategories = maskList [1], filterMask = maskList [2]}
      , CollisionType 1
      )
    killAsteroid (Collision _ bodyA bodyB _ _) = False <$ addPostStepCallback 1 do
      destroy bodyA (Proxy @(Body, BodyColor))
      (BodyMass mass, Size size) <- get bodyB
      destroy bodyB (Proxy @(Body, BodyColor, Size))
      modify global (<> Score (fromIntegral size))
      when (size >= 20) do
        let bounds = (1, size `div` 4 - 2)
        childSize <- liftIO $ sum <$> replicateM 4 (randomRIO bounds)
        mkAsteroid childSize
        mkAsteroid (size - childSize)
        pure ()
      

foldMapM :: (Monoid e, Applicative f, Foldable t) => (a -> f e) -> (t a -> f e)
foldMapM k= getAp . foldMap (Ap . k)

drawSys :: (Has w IO Physics, Has w IO BodyColor) => System w Picture
drawSys = foldDrawM \(transform, ShapeList shapes) -> worldTransform transform <$> foldMapM drawSh shapes
  where
    drawSh ety = do
      (Shape _ convex, BodyColor eColor) <- get ety
      pure . color eColor . convexToPicture $ convex

main :: IO ()
main = initWorld >>= runSystem do
  initialize
  play
    (InWindow "Haskteroids" (100,100) (640,480))
    black
    60
    drawSys
    handleEvt
    (\(realToFrac -> dT) -> tickPlayer dT >> stepPhysics dT)
  Score score <- get global
  liftIO . putStrLn $ "Score: " <> show score

initialize :: System World ()
initialize = do
  mkPlayer
  nEnemies <- liftIO (randomRIO (3,8))
  replicateM_ nEnemies (liftIO (randomRIO (80,200)) >>= mkAsteroid)
  set global (Camera (V2 0 0) 800)

handleEvt :: Event -> System World ()
handleEvt EventMotion{} = pure ()
handleEvt EventResize{} = pure ()
handleEvt (EventKey key keyState _ _) = case (key, keyState) of
  (Char 'w', Down) -> addForward 1
  (Char 'w', Up) -> addForward (-1)
  (Char 's', Down) -> addForward (-0.5)
  (Char 's', Up) -> addForward 0.5
  (Char 'a', Down) -> addTurning 1
  (Char 'a', Up) -> addTurning (-1)
  (Char 'd', Down) -> addTurning (-1)
  (Char 'd', Up) -> addTurning 1
  (SpecialKey KeySpace, Down) -> shoot
  _ -> pure ()

addForward :: (Has w IO Player, Has w IO Physics) => Double -> System w ()
addForward dA = cmap \player -> player{ playerForwardAccel = playerForwardAccel player + dA }

addTurning :: (Has w IO Player, Has w IO Physics) => Double -> System w ()
addTurning dTheta = cmap \(Player{}, AngularVelocity avOld) -> AngularVelocity (avOld + dTheta)

shoot :: System World ()
shoot = cmapM \(player, pos, Velocity vel, Angle phi) ->
  if playerShootCooldown player > 0
  then pure player
  else do
    let muzzleVel = 300 * angle phi
    mkBullet (pos, Angle phi, Velocity (vel + muzzleVel))
    pure player{ playerShootCooldown = 0.7 }

tickPlayer :: (Has w IO Player, Has w IO Physics) => Double -> System w ()
tickPlayer dT = cmap \(Player shootCD fwdAccel, Angle phi, Velocity vel) ->
  let shootCD' = max 0 (shootCD - dT)
      vel' = vel + 20 * dT *^ angle phi
  in (Player shootCD' fwdAccel, Velocity vel')