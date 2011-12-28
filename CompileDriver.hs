module CompileDriver where

import System.Cmd

compile files = rawSystem "gcc" (["dsimp/null.c"] ++ files)
