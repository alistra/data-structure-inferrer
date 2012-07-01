module CompileDriver where

import GHC.IO.Exception
import System.Cmd

--TODO CC env
compile :: [FilePath] -> [String] -> IO ExitCode
compile files ("--":ccopts) = rawSystem "gcc" (ccopts ++ ["dsimp/null.c"] ++ files)
compile files [] = rawSystem "gcc" (["dsimp/null.c"] ++ files)
