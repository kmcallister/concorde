module Algorithms.Concorde.LinKern
    ( tsp
    , Config(..), defConfig
    , R2
    ) where

import Control.Monad
import Control.Exception
import Data.Maybe
import System.Exit
import System.IO
import System.IO.Temp
import Text.Printf

import qualified Data.IntMap    as IM
import qualified System.Process as P

errStr :: String -> String
errStr = ("Algorithms.Concorde.LinKern: " ++)

data Config = Config
    { executable :: FilePath
    , verbose    :: Bool
    , timeBound  :: Maybe Double
    , runs       :: Int
    , otherArgs  :: [String]
    } deriving (Eq, Ord, Read, Show)

defConfig :: Config
defConfig = Config
    { executable = "linkern"
    , verbose    = False
    , timeBound  = Nothing
    , runs       = 1
    , otherArgs  = [] }

type R2 = (Double, Double)

tsp :: Config -> (a -> R2) -> [a] -> IO [a]
tsp cfg getCoord xs =
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

        let get (x:_) = fromMaybe err $ IM.lookup (read x) pts
            get []    = err
            err = error (errStr "internal error in lookup")

        lns <- lines `fmap` hGetContents tourHdl
        _   <- evaluate (length lns)
        return $ map (get . words) (drop 1 lns)
