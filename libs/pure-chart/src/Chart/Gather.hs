module Chart.Gather where

import Chart.Commands
import Chart.Path
import Data.SVG as SVG

type Gather = [Path]

paths :: Gather -> [View]
paths ps = [ SVG.Path <| SVG.D (draw (fromPath p)) | p <- ps ]

merge :: Gather -> Path
merge = mconcat