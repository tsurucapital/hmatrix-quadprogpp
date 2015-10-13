import Control.Monad
import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra
import Numeric.Minimization.QuadProgPP

main = do
  test 1e-12 answer0 expected0
  test 1e-5 answer1 expected1
  where
    test tol answer expected =
      when (either (const True) (bad . fst) answer) $ print (answer, expected)
      where
        bad x = VS.length x /= VS.length expected
          || VS.any ((>tol) . abs) (VS.zipWith (-) x expected)

    -- Problem 0
    -- minimize: 2(x-1)^2 + (y-4)^2 
    -- s.t.: x >= 0, y >= 0, x + 2y <= 2
    answer0 = solveQuadProg
      ( (2><2)
        [ 4, 0
        , 0, 2
        ]
        , VS.fromList [-4, -8])
      Nothing
      (Just
        ((3><2)
          [ 1, 0
          , 0, 1
          ,-1,-2
          ]
        , VS.fromList [0, 0, 2]))

    expected0 = VS.fromList [2/9, 8/9]

    -- Problem 1
    -- Same as Problem 0, but substitute:
    --  s = x
    --  t = x + y
    --  u = x - y
    -- and add a small positive constant to the diagonal elements to keep
    -- the problem positive definite.
    answer1 = solveQuadProg
      ( ((3><3)
        [   1, 2/3, 1/3
        , 2/3, 2/3,   0
        , 1/3,   0, 1/3
        ] `add` (1e-12 `scale` ident 3))
        , VS.fromList [-2, -4, 2]
        )
      (Just
        ((1><3) [-3, 2, 1]
        , VS.fromList [0]))
      (Just
        ((3><3)
          [ 1,    0,    0
          , 0,  1/3, -1/3
          , 0, -4/3,  1/3
          ]
        , VS.fromList [0, 0, 2]))
    expected1 = VS.fromList [2/9, 10/9, -14/9]
