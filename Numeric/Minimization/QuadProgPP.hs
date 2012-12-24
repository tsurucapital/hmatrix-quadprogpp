{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Numeric.Minimization.QuadProgPP (solveQuadProg) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Packed
import Data.Packed.Development
import qualified Data.Vector.Storable as VS
import Foreign.C.String
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

-- | Solve a strictly convex quadratic program with optional linear
-- constraints. It returns a pair of the optimal solution and the
-- value of the objective function at that point. On error it returns
-- Left.
solveQuadProg
    :: (Matrix Double, Vector Double)
        -- ^ The function to minimize. It should be of the form
        -- @(A, B)@, which represents a quadratic function
        -- @x -> (1/2)x'Ax + x'B@ where t' denotes the transpose
        -- of t. @A@ must be positive definite.
    -> Maybe (Matrix Double, Vector Double)
        -- ^ Optional equality constraints. When given, this
        -- argument should be of the form @Just (C, D)@, which
        -- represents a linear equality @x -> x'C + D = 0@.
    -> Maybe (Matrix Double, Vector Double)
        -- ^ Optional inequality constraints. When given, this
        -- argument should be of the form @Just (E, F)@, which
        -- represents linear inequalities @x -> x'E + F >= 0@.
    -> Either String (Vector Double, Double)
solveQuadProg (g, g0) (split -> (ce, ce0)) (split -> (ci, ci0))
        = unsafePerformIO $
    mat' (Just g) $ \gRow gCol gPtr ->
    vec' (Just g0) $ \g0Size g0Ptr ->
    mat' (trans <$> ce) $ \ceRow ceCol cePtr ->
    vec' ce0 $ \ce0Size ce0Ptr ->
    mat' (trans <$> ci) $ \ciRow ciCol ciPtr ->
    vec' ci0 $ \ci0Size ci0Ptr ->
    fromMaybe (return $ Left sizeMismatchError) $ do
        let !nVar = gRow
        guard $ gCol == nVar
        guard $ g0Size == nVar
        guard $ ceRow == nVar || ceRow == 0
        let !nCE = ceCol
        guard $ ce0Size == nCE
        guard $ ciRow == nVar || ciRow == 0
        let !nCI = ciCol
        guard $ ci0Size == nCI
        return $ alloca $ \ptrErrorStr -> do
            fpSolution <- mallocForeignPtrArray (fromIntegral nVar)
            best <- withForeignPtr fpSolution $ \ptrSolution ->
                c_hs_solve_quadprog nVar nCE nCI
                    gPtr g0Ptr
                    cePtr ce0Ptr
                    ciPtr ci0Ptr
                    ptrSolution
                    ptrErrorStr
            errorCStr <- peek ptrErrorStr
            if errorCStr == nullPtr -- success
                then let
                    !solutionVec = VS.unsafeFromForeignPtr0 fpSolution
                        (fromIntegral nVar)
                    in return $ Right (solutionVec, best)
                else do
                    errorStr <- peekCAString errorCStr
                    free errorCStr
                    return $ Left errorStr
    where
        mat' (Just m) f = mat (cmat m) $ \church -> church $ \nrow ncol ptr -> f nrow ncol ptr
        mat' Nothing f = f 0 0 nullPtr
        vec' (Just v) f = vec v $ \church -> church $ \size ptr -> f size ptr
        vec' Nothing f = f 0 nullPtr
        sizeMismatchError =
            "Numeric.Minimization.QuadProgPP.solveQuadProg: size mismatch"

split :: Maybe (a, b) -> (Maybe a, Maybe b)
split x = (fst <$> x, snd <$> x)

foreign import ccall "hs_solve_quadprog"
    c_hs_solve_quadprog
        :: CInt -> CInt -> CInt
        -> Ptr Double
        -> Ptr Double
        -> Ptr Double
        -> Ptr Double
        -> Ptr Double
        -> Ptr Double
        -> Ptr Double
        -> Ptr CString
        -> IO Double
