module CompileDriver where

import GHC.IO.Exception
import System.Cmd

compile :: [FilePath] -> [String] -> IO ExitCode
compile files ccopts = rawSystem "gcc" (ccopts ++ ["dsimp/null.c"] ++ files)
