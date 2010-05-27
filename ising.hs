module Main where

import System.Random.Mersenne as M
import Data.Vector.Unboxed.Mutable as U
import Control.Monad.State as S
import Control.Monad.Primitive
import Control.Applicative
import Control.Concurrent
import Graphics.UI.SDL as SDL

newtype Grid = Grid { unG :: (Int, MVector (PrimState IO) Int) }
newtype Ising = Ising { unI :: (M.MTGen, Grid) }

type Pos = (Int, Int)

(|>) :: Grid -> Pos -> IO Int
(Grid (n,g)) |> (x,y) | x == n     = U.unsafeRead g (y*n)
                      | x == -1    = U.unsafeRead g (y*n + n - 1)
                      | y == n     = U.unsafeRead g x
                      | y == -1    = U.unsafeRead g ((n - 1)*n + x)
                      | otherwise  = U.unsafeRead g (y*n + x)

(<==) :: Pos -> Int -> (Pos, Int)
pos <== val = (pos,val)

(<|) :: Grid -> (Pos, Int) -> IO ()
Grid (n,g) <| ((x,y), val) = U.unsafeWrite g (y*n + x) val

infixr 1 <==
infixr 0 <|

-- Just a shortcut

l = lift

-- Initialize a ising system

initIsing :: M.MTGen -> Int -> IO Ising
initIsing rg n = do
            g <- U.new $ n*n
            grid <- return $ Grid (n, g)
            let spin x = x + (x - 1)
            sequence_ [ do rnumber <- random rg 
                           grid <| (i,j) <== spin (rnumber `mod` 2) | 
                                        i <- [0..n - 1], j <- [0..n - 1] ]
            return $ Ising (rg, grid)

-- Update the grid of Ising Model 'ns' steps in temperatur 't'

updateIsing :: Int -> Double -> StateT Ising IO ()
updateIsing ns t = up 0 ns
    where 
          up i ns | i == ns = return ()
                  | otherwise = do 
                        Ising (rg, grid@(Grid (n,_))) <- S.get
                        x <- l $ (`mod` n) <$> random rg 
                        y <- l $ (`mod` n) <$> random rg 
                        (e,s) <- l $ calc_energy grid (x,y)
                        if e < 0
                            then do l $ grid <| (x,y) <== negate s
                            else do p <- l $ (random rg :: IO Double)
                                    when (p < exp (-2*(fromIntegral e)/t)) $
                                        l $ grid <| (x,y) <== negate s
                        up (i + 1) ns

          calc_energy g (x,y) = do
              s <- g |> (x,y)
              e <- (s*).(sum) <$> (mapM (g |>) [ (x, y - 1)
                                               , (x, y + 1)
                                               , (x - 1, y)
                                               , (x + 1, y) ])
              return (e, s)

-- Draw ising

drawIsing screen (Ising (_, grid@(Grid (n,_)))) = do    
    sequence [ drawSquare screen i j | i <- [0..n - 1], j <- [0..n - 1] ] 
    SDL.flip screen
        where drawSquare s i j = do
                s <- grid |> (i,j)
                let color 1 = 0xFF0000
                    color _ = 0x00FF00
                    sSize = screen_size `div` n
                    rect i j = Just $ Rect (i*sSize) (j*sSize) sSize sSize
                SDL.fillRect screen (rect i j) (SDL.Pixel $ color s)
                return ()

-- Update ising and draw

sdlUpIsing  i t c s
   | t < 0     = return ()
   | otherwise = do
    evalStateT (updateIsing 1000 t) i
    drawIsing s i
    threadDelay 1
    sdlUpIsing  i (t - 0.001) (c + 1) s

screen_size = 400

main = do
    SDL.init [InitEverything]
    setVideoMode screen_size screen_size 32 []
    screen <- SDL.getVideoSurface
    rg <- M.newMTGen Nothing
    i <- initIsing rg 200
    forkIO . forever $ waitEvent >>= \e -> when (e == Quit) quit
    sdlUpIsing i 3 0 screen

