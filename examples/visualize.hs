{-# LANGUAGE
    DeriveDataTypeable
  , RecordWildCards #-}
module Main(main) where

-- Visualize results of Lin-Kernighan on random points
--
-- deps :  cabal install diagrams cmdargs
-- build:  ghc -O --make visualize.hs
-- usage:  ./visualize -?


import qualified Algorithms.Concorde.LinKern as T

-- package 'diagrams'
import qualified Diagrams.Prelude       as D
import qualified Diagrams.Backend.Cairo as D

-- package 'cmdargs'
import qualified System.Console.CmdArgs as Arg
import           System.Console.CmdArgs ( (&=), Typeable, Data )

import Control.Monad
import Data.List
import Data.Monoid
import System.IO
import System.Random
import System.Exit


data Visualize = Visualize
    { out       :: String
    , numPoints :: Int
    , linkern   :: FilePath
    , verbose   :: Bool
    , timeBound :: Maybe Double
    , steps     :: Maybe Int
    , runs      :: Int }
    deriving (Show, Typeable, Data)

argSpec :: Visualize
argSpec = Visualize
    { out       = "out.pdf" &= Arg.help "Name of output PDF file [out.pdf]"
    , numPoints = 1000      &= Arg.help "Number of points [1000]"
    , linkern   = "linkern" &= Arg.help "Path to linkern executable [search $PATH]"
    , verbose   = False     &= Arg.help "Write progress information to standard output [no]"
    , timeBound = Nothing   &= Arg.help "Stop looking for better solutions after this many seconds [no]"
    , steps     = Nothing   &= Arg.help "Run this many optimization steps [# points]"
    , runs      = 1         &= Arg.help "Run this many separate optimizations [1]" }

    &= Arg.summary "Visualize results of Lin-Kernighan on random points"


type Diagram = D.Diagram D.Cairo D.R2

diaPoints :: [T.R2] -> Diagram
diaPoints = mconcat . map circ where
    circ p = D.translate p . D.fc D.red $ D.circle 20

diaTour :: [T.R2] -> Diagram
diaTour [] = mempty
diaTour xs@(x:_) = D.lw 10 . D.fromVertices $ map D.P (xs ++ [x])

writePDF :: FilePath -> Diagram -> IO ()
writePDF pdfName dia = fst $ D.renderDia D.Cairo opts dia where
    opts = D.CairoOptions pdfName (D.PDF (400,400))


main :: IO ()
main = do
    Visualize{..} <- Arg.cmdArgs argSpec
    let cfg = T.Config
            { T.executable = linkern
            , T.verbose    = verbose
            , T.timeBound  = timeBound
            , T.steps      = steps
            , T.runs       = runs
            , T.otherArgs  = [] }

    let rnd = randomRIO (0,10000)
    points <- replicateM numPoints (liftM2 (,) rnd rnd)
    tour   <- T.tsp cfg id points

    -- Sanity check
    when (sort tour /= sort points) $ do
        hPrint    stderr (sort tour, sort points)
        hPutStrLn stderr "ERROR: tour is not a permutation"
        exitFailure

    writePDF out (diaPoints points D.<> diaTour tour)
