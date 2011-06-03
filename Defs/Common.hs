module Defs.Common where

import System.Console.ANSI

greenColor = setSGR [SetColor Foreground Vivid Green]
yellowColor = setSGR [SetColor Foreground Dull Yellow]
redColor = setSGR [SetColor Foreground Vivid Red]
cyanColor = setSGR [SetColor Foreground Vivid Cyan]
resetColor = setSGR [Reset]

type Name =  String
type Context = [Name]
