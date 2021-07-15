import Control.Monad
import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra
    ( (><), ident, Additive(add), Linear(scale) )
import Numeric.Minimization.QuadProgPP ( solveQuadProg )
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, assertEqual, assertFailure )
import Test.HUnit.Approx ( assertApproxEqual )
import Debug.Trace
import Numeric.LinearAlgebra.HMatrix (tr)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "small"
    [ problem0
    , problem1
    , problemQuadprogppDemo
    ]

-- Problem 0
-- minimize: 2(x-1)^2 + (y-4)^2
-- s.t.: x >= 0, y >= 0, x + 2y <= 2
problem0 :: TestTree
problem0 = testCase "Problem 0" $ do
    let answer = solveQuadProg
            ( (2><2)
                [ 4, 0
                , 0, 2
                ]
            , VS.fromList [-4, -8]
            )
            Nothing
            ( Just
                ( (3><2)
                    [ 1, 0
                    , 0, 1
                    ,-1,-2
                    ]
                , VS.fromList [0, 0, 2]
                )
            )
    (actual, best) <- case answer of
        Left err -> assertFailure $ show err
        Right x -> return x
    let expected = VS.fromList [2/9, 8/9] :: VS.Vector Double
    assertEqual "The answer should have the expected length"
        (VS.length expected)
        (VS.length actual)
    VS.zipWithM_ (assertApproxEqual "" 1e-12) (traceShowId expected) (traceShowId actual)


-- Problem 1
-- Same as Problem 0, but substitute:
--  s = x
--  t = x + y
--  u = x - y
-- and add a small positive constant to the diagonal elements to keep
-- the problem positive definite.
problem1 :: TestTree
problem1 = testCase "Problem 1" $ do
    let answer = solveQuadProg
            ((3><3)
                [   1, 2/3, 1/3
                , 2/3, 2/3,   0
                , 1/3,   0, 1/3
                ] `add` (1e-12 `scale` ident 3)
            , VS.fromList [-2, -4, 2]
            )
            (Just
                ( (1><3) [-3, 2, 1]
                , VS.fromList [0]
                )
            )
            (Just
                ((3><3)
                    [ 1,    0,    0
                    , 0,  1/3, -1/3
                    , 0, -4/3,  1/3
                    ]
                , VS.fromList [0, 0, 2]
                )
            )
    (actual, best) <- case answer of
        Left err -> assertFailure $ show err
        Right x -> return x
    let expected = VS.fromList [2/9, 10/9, -14/9] :: VS.Vector Double
    assertEqual "The answer should have the expected length"
        (traceShowId $ VS.length expected)
        (traceShowId $ VS.length actual)
    VS.zipWithM_ (assertApproxEqual "" 1e-5)
        (traceShowId expected)
        (traceShowId actual)

problemQuadprogppDemo :: TestTree
problemQuadprogppDemo = testCase "Problem quadpropp demo" $ do
    let answer = solveQuadProg
            ((2><2)
                [  4, -2
                , -2,  4 ]
            , VS.fromList [6, 0]
            )
            (Just
                ( (1><2) [1, 1]
                , VS.fromList [-3]
                )
            )
            (Just
                ((3><2)
                    [ 1, 0
                    , 0, 1
                    , 1, 1 ]
                , VS.fromList [0, -2, 0]
                )
            )
    (actual, best) <- case answer of
        Left err -> assertFailure $ show err
        Right x -> return x
    let epsilon = 1e-5
    assertApproxEqual "best" epsilon 12 best
    let expected = VS.fromList [1, 2] :: VS.Vector Double
    assertEqual "The answer should have the expected length"
        (traceShowId $ VS.length expected)
        (traceShowId $ VS.length actual)
    VS.zipWithM_ (assertApproxEqual "" epsilon)
        (traceShowId expected)
        (traceShowId actual)
