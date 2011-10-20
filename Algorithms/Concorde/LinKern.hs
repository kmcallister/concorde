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
    , runs       = 1
    , otherArgs  = [] }

-- | A point in Euclidean two-dimensional space.
type R2 = (Double, Double)

-- | Approximate a solution to the two-dimensional Euclidean Traveling
-- Salesperson Problem, using the Lin-Kernighan heuristic.
--
-- Invokes Concorde's @linkern@ executable as an external process.
tsp
    :: Config     -- ^ Configuration.
    -> (a -> R2)  -- ^ Gives the Euclidean coordinates of each point.
    -> [a]        -- ^ List of points to visit.
    -> IO [a]     -- ^ Produces points permuted in tour order.
tsp cfg getCoord xs =
    -- Log to a temp file if not verbose.
    -- On Unix we could open /dev/null, but this is not portable.
    withSystemTempFile "log"    $ \_          logHdl    ->
    withSystemTempFile "coords" $ \coordsPath coordsHdl ->
    withSystemTempFile "tour"   $ \tourPath   tourHdl   -> do

        let pts = IM.fromList $ zip [0..] xs
        _ <- hPrintf coordsHdl "TYPE : TSP\nDIMENSION : %d\n" (IM.size pts)
        _ <- hPrintf coordsHdl "EDGE_WEIGHT_TYPE : EUC_2D\nNODE_COORD_SECTION\n"
        forM_ (IM.toList pts) $ \(i,p) -> do
            let (x,y) = getCoord p
            hPrintf coordsHdl "%d %f %f\n" i x y
        hClose coordsHdl

        let timeArgs = case timeBound cfg of
                Nothing -> []
                Just n  -> ["-t", printf "%f" n]

            allArgs = concat [ timeArgs
                             , ["-r", show (runs cfg)]
                             , ["-o", tourPath]
                             , otherArgs cfg, [coordsPath] ]

            subOut  = if verbose cfg then P.Inherit else P.UseHandle logHdl
            procCfg = (P.proc (executable cfg) allArgs) { P.std_out = subOut }

        (Nothing, Nothing, Nothing, procHdl) <- P.createProcess procCfg
        ec <- P.waitForProcess procHdl
        case ec of
            ExitSuccess   -> return ()
            ExitFailure n -> throwIO . ErrorCall . errStr $
                ("linkern exited with code " ++ show n)

        lns <- lines `fmap` hGetContents tourHdl
        _   <- evaluate (length lns)
        let get = headMay >=> readMay >=> flip IM.lookup pts
            fj  = fromMaybe (error (errStr "internal error in lookup"))
        return $ map (fj . get . words) (drop 1 lns)
