module SampleTest where
import Sim
import Test.QuickCheck

prop_a = sampleSum 1 (-1) [(0.1, 1), (1.1, 2), (1.3, 3), (2.0, 4), (5.1, 5)] ===
    [1, 2, 4, 5,5,5]

return []
runTests = $quickCheckAll