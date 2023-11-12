module Main where
import Control.Concurrent
import Sim
import Chain
import Patricia
import Msg
import Graphics.Gnuplot.Simple
import Control.Parallel

title t = defaultStyle{lineSpec = CustomStyle [LineTitle t]}

main = do
  setNumCapabilities 7
  (times, avgs) <- simulate (noMsgs :: Chain') 60 10
  (times2, avgs2) <- avgs `par` simulate (noMsgs :: Patricia) 60 10
  plotPathsStyle [] [(title "Chain", zip times avgs), (title "Patricia", zip times2 avgs2)]

-- You could test on shuffled events to ensure that the time is helping as you think it should.
-- Then make a model in which gossiping peers go offline for exponential amounts of time. 