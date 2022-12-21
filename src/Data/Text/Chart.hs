{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Chart
-- License     :  MIT
-- Maintainer  :  Fabian Beuke <mail@beuke.org>
--
-- This module contains 4 functions. The plot function provides a very simple
-- interface for plotting. It takes a List of Integers and prints out a
-- corresponding chart with a default terminal height of 14 blocks.
-- The 'plot' function is therefore equivalent to @'plotWith'
-- options {height = 14}@. You can find some examples
-- <https://github.com/madnight/asciichart/tree/master/examples here>.
-----------------------------------------------------------------------------

module Data.Text.Chart
    ( -- * Plot
      plot
    , plotWith
    , plotWith'
      -- * Options
    -- , options
    -- , height
    , plotWithString
    , Options (..)
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative     ((<$>))
import Control.Monad.ST.Safe   (ST, runST)
#else
import Control.Monad.ST        (ST, runST)
#endif
import Control.Monad           (forM_)
import Data.Array.ST.Safe      (STArray, getElems, writeArray, newArray)
import Data.Char               (isSpace)
import Data.List               (unfoldr, dropWhileEnd)
import Text.Printf             (printf)
import Data.Bool               (bool)

data Options =
  Options { height :: Int  -- ^ Allows to set the height of the chart.
          }

-- | Provides default options: @Options { 'height' = 14 }@.
options :: Options
options =
  Options { height = 14 }

newArray2D :: Integer -> Integer ->
              ST s (STArray s (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) " "

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

pad :: Integral a => [a] -> Int
pad series =
  let floats = fromIntegral <$> series
      toStr :: [Float] -> [String]
      toStr = fmap (printf "%0.2f")
  in  maximum $ length <$> toStr floats

plotWith' :: Options -> [Integer] -> [String]
plotWith' opts series =

    -- variables and functions
    let min' = minimum series
        max' = maximum series
        range = abs $ max' - min'
        offset = 3
        ratio = if range == 0 then 1
                else fromIntegral (height opts) / fromIntegral range :: Float
        min2 = fromIntegral min' * ratio
        max2 = fromIntegral max' * ratio
        rows = round $ abs $ max2 - min2
        width = toInteger $ length series + 3

    in runST $ do

    -- array creation
    arr <- newArray2D rows width
    let result x y = writeArray arr (head x, head y)

    -- axis and labels
    forM_ [min2..max2] $ \y -> do
            let label = if rows == 0 then y
                        else fromInteger max' - (y - min2) *
                             fromInteger range / fromIntegral rows
            result [round $ y - min2] [max 0 $ offset - 5] $
                   printf ("%" ++ show (pad series) ++ ".2f") label
            result [round $ y - min2] [offset - 1] . bool "┤" "┼" $ y == 0

    -- initial value]
    let first = fromInteger (head series) * ratio - min2
    result [round $ fromInteger rows - first] [offset - 1] "┼"

    -- plot the line
    forM_ [0..length series - 2] $ \x -> do
        let offset' = toInteger x + offset
        let y' i = round (fromInteger (series !! i) * ratio) - round min2
        let (y0, y1) = (y' x, y' $ x + 1)
        if y0 == y1 then
            result [rows - y0] [offset'] "─"
        else do
            result [rows - y1] [offset'] . bool "╭" "╰" $ y0 > y1
            result [rows - y0] [offset'] . bool "╯" "╮" $ y0 > y1

            forM_ [min y0 y1 + 1..max y0 y1 - 1] $ \y ->
                result [rows - y] [offset'] "│"

    getElems arr

-- | Takes a List of Integers and prints out a
--   corresponding chart with a default terminal height of 14 blocks.
plot :: [Integer] -> IO ()
plot x =  if length x < 1 then return () else plotWith options x

-- | Same as plot but it's possible to define custom options.
--   Example: @'plotWith' options { 'height' = 20 }@
plotWith :: Options -> [Integer] -> IO ()
plotWith options' series = forM_ result $
      putStrLn . dropWhileEnd isSpace . concat
    where result = splitEvery (length series + 4) $ plotWith' options' series

plotWithString :: Options -> [Integer] -> [String]
plotWithString options' series =
  fmap (dropWhileEnd isSpace . concat) result
  where result = splitEvery (length series + 4) $ plotWith' options' series          

-- s = "58880.1\n58880.1\n58880.11\n58880.11\n58880.04\n58880.02\n58880.04\n58800.0\n58800.0\n58880.11\n58880.1\n58880.09\n58880.08\n58880.07\n58880.06\n58880.05\n58880.04\n58879.95\n58879.96\n58879.97\n58879.98\n58879.99\n58880.0\n58880.01\n58880.02\n58852.13\n58852.13\n58852.12\n58852.12\n58852.11\n58852.11\n58852.1\n58852.1\n58852.09\n58852.09\n58852.08\n58852.08\n58852.07\n58852.07\n58852.06\n58852.06\n58852.15\n58852.15\n58852.16\n58852.16\n58852.17\n58852.17\n58852.18\n58852.18\n58852.19\n58852.19\n58852.2\n58852.2\n58852.21\n58852.21\n58852.22\n58852.22\n58852.15\n58852.22\n58852.21\n58852.2\n58852.19\n58852.18\n58852.17\n58852.16\n58852.15\n58852.06\n58852.07\n58852.08\n58852.09\n58852.1\n58852.11\n58852.12\n58852.13\n58843.06\n58843.06\n58843.05\n58843.05\n58843.04\n58843.04\n58843.03\n58843.03\n58843.02\n58843.02\n58843.01\n58843.01\n58843.0\n58843.0\n58842.99\n58842.99\n58843.08\n58843.08\n58843.09\n58843.09\n58843.1\n58843.1\n58843.11\n58843.11\n58843.12\n58843.12\n58843.13\n58843.13\n58843.14\n58843.14\n58843.15\n58843.15\n" :: String
-- d = fmap round (fmap read $ lines s :: [Double]) :: [Integer]
