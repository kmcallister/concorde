-- | Approximate a solution to 2D Euclidean TSP using the Lin-Kernighan
-- heuristic.
module Algorithms.Concorde.LinKern
    ( -- * The heuristic
      tsp, R2
      -- * Configuration
    , Config(..), defConfig
    ) where

import Control.Monad
import Control.Exception
import Data.Maybe
import System.Exit
import System.IO
import System.IO.Temp
import Text.Printf
import Safe

import qualified Data.IntMap    as IM
import qualified System.Process as P

errStr :: String -> String
errStr = ("Algorithms.Concorde.LinKern: " ++)

-- | Configuration for @'tsp'@.
data Config = Config
    {  -- | Path to the @linkern@ executable.  Searches @$PATH@ by default.
      executable :: FilePath
      -- | If set, write progress information to standard output.
    , verbose    :: Bool
      -- | Stop looking for better solutions after this many seconds.
    , timeBound  :: Maybe Double
      -- | Run this many optimization steps.  Default is the number of points.
    , steps      :: Maybe Int
      -- | Run this many separate optimizations.  Default is 1.
    , runs       :: Int
      -- | Other command-line arguments to the @linkern@ executable.
    , otherArgs  :: [String]
    } deriving (Eq, Ord, Read, Show)

-- | Default configuration.
defConfig :: Config
defConfig = Config
    { executable = "linkern"
    , verbose    = False
    , timeBound  = Nothing
    , steps      = Nothing
    , runs       = 1
    , otherArgs  = [] }

-- | A point in Euclidean two-dimensional space.
type R2 = (Double, Double)

-- | Approximate a solution to the two-dimensional Euclidean Traveling
-- Salesperson Problem, using the Lin-Kernighan heuristic.
--
-- Invokes Concorde's @linkern@ executable as an external process.
--
-- Note: @linkern@ uses Euclidean distance rounded to the nearest integer.
-- You may need to scale up coordinates in the function passed to @'tsp'@.
tsp
    :: Config     -- ^ Configuration.
    -> (a -> R2)  -- ^ Gives the rectangular coordinates of each point; see below.
    -> [a]        -- ^ List of points to visit.
    -> IO [a]     -- ^ Produces points permuted in tour order.
tsp cfg getCoord xs =
    -- Log to a temp file if not verbose.
    -- On Unix we could open /dev/null, but this is not portable.
    withSystemTempFile "log"    $ \_          logHdl    ->
    withSystemTempFile "coords" $ \coordsPath coordsHdl ->
    withSystemTempFile "tour"   $ \tourPath   tourHdl   -> do

        -- Output coordinates in TSPLIB format
        let pts = IM.fromList $ zip [0..] xs
        mapM_ (hPutStrLn coordsHdl)
            [ "TYPE:TSP"
            , "DIMENSION:" ++ show (IM.size pts)
            , "EDGE_WEIGHT_TYPE:EUC_2D"
            , "NODE_COORD_SECTION" ]
        forM_ (IM.toList pts) $ \(i,p) -> do
            let (x,y) = getCoord p
            hPrintf coordsHdl "%d %f %f\n" i x y
        hPutStrLn coordsHdl "EOF"
        hClose coordsHdl

        -- Invoke linkern
        let optArg flag fmt proj = case proj cfg of
                Nothing -> []
                Just x  -> [flag, printf fmt x]

            allArgs = concat [ ["-o", tourPath]
                             , ["-r", show (runs cfg)]
                             , optArg "-t" "%f" timeBound
                             , optArg "-R" "%d" steps
                             , otherArgs cfg
                             , [coordsPath] ]

            subOut  = if verbose cfg then P.Inherit else P.UseHandle logHdl
            procCfg = (P.proc (executable cfg) allArgs) { P.std_out = subOut }

        (Nothing, Nothing, Nothing, procHdl) <- P.createProcess procCfg
        ec <- P.waitForProcess procHdl
        case ec of
            ExitSuccess   -> return ()
            ExitFailure n -> throwIO . ErrorCall . errStr $
                ("process exited with code " ++ show n ++ extra) where
                    extra | n == 127  = "; program not installed or not in path?"
                          | otherwise = ""

        -- Skip the first line, then read the first int of each remaining
        -- line as an index into the original list of points.
        lns <- lines `fmap` hGetContents tourHdl
        _   <- evaluate (length lns)
        let get = headMay >=> readMay >=> flip IM.lookup pts
            fj  = fromMaybe (error (errStr "internal error in lookup"))
        return $ map (fj . get . words) (drop 1 lns)
