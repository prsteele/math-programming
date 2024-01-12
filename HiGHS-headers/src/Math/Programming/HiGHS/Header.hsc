{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Math.Programming.HiGHS.Header where

import Data.Typeable
import GHC.Generics (Generic)
import GHC.Int
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic

#include <interfaces/highs_c_api.h>

-- | A phantom type representing a problem in HiGHS.
data Problem

-- | Phantom type used to denote data as being a column.
data HighsColumn

-- | Phantom type used to denote data as being a row.
data HighsRow

-- | Phantom type used to denote data as being a count
data HighsCount

newtype HighsArray a
  = HighsArray { fromHighsArray :: Ptr a }
  deriving
    ( Eq
    , Ord
    , Show
    , Storable
    )

-- | Wrapper around 'CInt' values, tagged with a phantom type to help
-- track what it refers to.
newtype HighsInt a
  = HighsInt { fromHighsInt :: CInt }
  deriving
    ( Enum
    , Eq
    , Integral
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

type Row = HighsInt HighsRow
type Column = HighsInt HighsColumn
type Count = HighsInt HighsCount

newtype HighsStatus
  = HighsStatus { fromHighsStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
    HighsStatus
  , HighsStatus
  , highsStatusError = kHighsStatusError
  , highsStatusOk = kHighsStatusOk
  , highsStatusWarning = kHighsStatusWarning
  }

newtype HighsVarType
  = HighsVarType { fromHighsVarType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
    HighsVarType
  , HighsVarType
  , highsVarTypeContinuous = kHighsVarTypeContinuous
  , highsVarTypeInteger = kHighsVarTypeInteger
  , highsVarTypeSemiContinuous = kHighsVarTypeSemiContinuous
  , highsVarTypeSemiInteger = kHighsVarTypeSemiInteger
  , highsVarTypeImplicitInteger = kHighsVarTypeImplicitInteger
 }

newtype HighsOptionType
  = HighsOptionType { fromHighsOptionType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
    HighsOptionType
  , HighsOptionType
  , highsOptionTypeBool = kHighsOptionTypeBool
  , highsOptionTypeInt = kHighsOptionTypeInt
  , highsOptionTypeDouble = kHighsOptionTypeDouble
  , highsOptionTypeString = kHighsOptionTypeString
  }

newtype HighsInfoType
  = HighsInfoType { fromHighsInfoType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsInfoType
 , HighsInfoType
 , highsInfoTypeInt64 = kHighsInfoTypeInt64
 , highsInfoTypeInt = kHighsInfoTypeInt
 , highsInfoTypeDouble = kHighsInfoTypeDouble
 }

newtype HighsObjSense
  = HighsObjSense { fromHighsObjSense :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsObjSense
 , HighsObjSense
 , highsObjSenseMinimize = kHighsObjSenseMinimize
 , highsObjSenseMaximize = kHighsObjSenseMaximize
 }

newtype HighsMatrixFormat
  = HighsMatrixFormat { fromHighsMatrixFormat :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsMatrixFormat
 , HighsMatrixFormat
 , highsMatrixFormatColwise = kHighsMatrixFormatColwise
 , highsMatrixFormatRowwise = kHighsMatrixFormatRowwise
 }

newtype HighsHessianFormat
  = HighsHessianFormat { fromHighsHessianFormat :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsHessianFormat
 , HighsHessianFormat
 , highsHessianFormatTriangular = kHighsHessianFormatTriangular
 , highsHessianFormatSquare = kHighsHessianFormatSquare
 }

newtype HighsSolutionStatus
  = HighsSolutionStatus { fromHighsSolutionStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsSolutionStatus
 , HighsSolutionStatus
 , highsSolutionStatusNone = kHighsSolutionStatusNone
 , highsSolutionStatusFeasible = kHighsSolutionStatusFeasible
 , highsSolutionStatusInfeasible = kHighsSolutionStatusInfeasible
 }

newtype HighsBasisValidity
  = HighsBasisValidity { fromHighsBasisValidity :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsBasisValidity
 , HighsBasisValidity
 , highsBasisValidityInvalid = kHighsBasisValidityInvalid
 , highsBasisValidityValid = kHighsBasisValidityValid
 }

newtype HighsPresolveStatus
  = HighsPresolveStatus { fromHighsPresolveStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsPresolveStatus
 , HighsPresolveStatus
 , highsPresolveStatusNotPresolved = kHighsPresolveStatusNotPresolved
 , highsPresolveStatusNotReduced = kHighsPresolveStatusNotReduced
 , highsPresolveStatusInfeasible = kHighsPresolveStatusInfeasible
 , highsPresolveStatusUnboundedOrInfeasible = kHighsPresolveStatusUnboundedOrInfeasible
 , highsPresolveStatusReduced = kHighsPresolveStatusReduced
 , highsPresolveStatusReducedToEmpty = kHighsPresolveStatusReducedToEmpty
 , highsPresolveStatusTimeout = kHighsPresolveStatusTimeout
 , highsPresolveStatusNullError = kHighsPresolveStatusNullError
 , highsPresolveStatusOptionsError = kHighsPresolveStatusOptionsError
 }
newtype HighsModelStatus
  = HighsModelStatus { fromHighsModelStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsModelStatus
 , HighsModelStatus
 , highsModelStatusNotset = kHighsModelStatusNotset
 , highsModelStatusLoadError = kHighsModelStatusLoadError
 , highsModelStatusModelError = kHighsModelStatusModelError
 , highsModelStatusPresolveError = kHighsModelStatusPresolveError
 , highsModelStatusSolveError = kHighsModelStatusSolveError
 , highsModelStatusPostsolveError = kHighsModelStatusPostsolveError
 , highsModelStatusModelEmpty = kHighsModelStatusModelEmpty
 , highsModelStatusOptimal = kHighsModelStatusOptimal
 , highsModelStatusInfeasible = kHighsModelStatusInfeasible
 , highsModelStatusUnboundedOrInfeasible = kHighsModelStatusUnboundedOrInfeasible
 , highsModelStatusUnbounded = kHighsModelStatusUnbounded
 , highsModelStatusObjectiveBound = kHighsModelStatusObjectiveBound
 , highsModelStatusObjectiveTarget = kHighsModelStatusObjectiveTarget
 , highsModelStatusTimeLimit = kHighsModelStatusTimeLimit
 , highsModelStatusIterationLimit = kHighsModelStatusIterationLimit
 , highsModelStatusUnknown = kHighsModelStatusUnknown
 , highsModelStatusSolutionLimit = kHighsModelStatusSolutionLimit
 , highsModelStatusInterrupt = kHighsModelStatusInterrupt
 }

newtype HighsBasisStatus
  = HighsBasisStatus { fromHighsBasisStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsBasisStatus
 , HighsBasisStatus
 , highsBasisStatusLower = kHighsBasisStatusLower
 , highsBasisStatusBasic = kHighsBasisStatusBasic
 , highsBasisStatusUpper = kHighsBasisStatusUpper
 , highsBasisStatusZero = kHighsBasisStatusZero
 , highsBasisStatusNonbasic = kHighsBasisStatusNonbasic
 }
newtype HighsCallback
  = HighsCallback { fromHighsCallback :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   HighsCallback
 , HighsCallback
 , highsCallbackLogging = kHighsCallbackLogging
 , highsCallbackSimplexInterrupt = kHighsCallbackSimplexInterrupt
 , highsCallbackIpmInterrupt = kHighsCallbackIpmInterrupt
 , highsCallbackMipImprovingSolution = kHighsCallbackMipImprovingSolution
 , highsCallbackMipLogging = kHighsCallbackMipLogging
 , highsCallbackMipInterrupt = kHighsCallbackMipInterrupt
 }

data HighsCallbackDataOut
  = HighsCallbackDataOut
    { hcdoLogType :: CInt
    , hcdoRunningTime :: CDouble
    , hcdoSimplexIterationCount :: CInt
    , hcdoIPMIterationCount :: CInt
    , hcdoMipNodeCount :: CInt
    , hcdoMipPrimalBound :: CDouble
    , hcdoMipDualBound :: CDouble
    , hcdoMipGap :: CDouble
    , hcdoMipSolution :: HighsArray CDouble
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable HighsCallbackDataOut

data HighsCallbackDataIn
  = HighsCallbackDataIn
    { hcdiUserInterrupt :: CInt
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable HighsCallbackDataIn

foreign import ccall "Highs_create" highs_create
  :: IO (Ptr Problem)
  -- ^ The allocated problem instance

foreign import ccall "Highs_destroy" highs_destroy
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "Highs_version" highs_version
  :: IO CString
  -- ^ The version string

foreign import ccall "Highs_versionMajor" highs_versionMajor
  :: IO CInt
  -- ^ The major version

foreign import ccall "Highs_versionMinor" highs_versionMinor
  :: IO CInt
  -- ^ The minor version

foreign import ccall "Highs_versionPatch" highs_versionPatch
  :: IO CInt
  -- ^ The patch version

foreign import ccall "Highs_githash" highs_githash
  :: IO CString
  -- ^ The Git hash

foreign import ccall "Highs_compilationDate" highs_compilationDate
  :: IO CString
  -- ^ The compilation date of HiGHS

foreign import ccall "Highs_readModel" highs_readModel
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to read
  -> IO HighsStatus

foreign import ccall "Highs_writeModel" highs_writeModel
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to write
  -> IO HighsStatus

foreign import ccall "Highs_clear" highs_clear
  :: Ptr Problem
  -- ^ The problem instance
  -> IO HighsStatus

foreign import ccall "Highs_clearModel" highs_clearModel
  :: Ptr Problem
  -- ^ The problem instance
  -> IO HighsStatus

foreign import ccall "Highs_clearSolver" highs_clearSolver
  :: Ptr Problem
  -- ^ The problem instance
  -> IO HighsStatus

foreign import ccall "Highs_run" highs_run
  :: Ptr Problem
  -- ^ The problem instance
  -> IO HighsStatus

foreign import ccall "Highs_writeSolution" highs_writeSolution
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to write
  -> IO HighsStatus

foreign import ccall "Highs_writeSolutionPretty" highs_writeSolutionPretty
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to write
  -> IO HighsStatus

foreign import ccall "Highs_passLp" highs_passLp
  :: Ptr Problem
  -> Column
  -> Row
  -> Count
  -> HighsMatrixFormat
  -> HighsObjSense
  -> CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray (HighsInt CDouble)
  -> IO HighsStatus

foreign import ccall "Highs_passMip" highs_passMip
  :: Ptr Problem
  -> Column
  -> Row
  -> Count
  -> HighsMatrixFormat
  -> HighsObjSense
  -> CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray (HighsInt CDouble)
  -> HighsArray HighsVarType
  -> IO HighsStatus

foreign import ccall "Highs_passModel" highs_passModel
  :: Ptr Problem
  -> Column
  -> Row
  -> Count
  -> HighsMatrixFormat
  -> HighsHessianFormat
  -> HighsObjSense
  -> CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray (HighsInt CDouble)
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray (HighsInt CDouble)
  -> HighsArray HighsVarType
  -> IO HighsStatus

foreign import ccall "Highs_passHessian" highs_passHessian
  :: Ptr Problem
  -> Count
  -> Count
  -> HighsHessianFormat
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_passRowName" highs_passRowName
  :: Ptr Problem
  -> Row
  -> CString
  -> IO HighsStatus

foreign import ccall "Highs_passColName" highs_passColName
  :: Ptr Problem
  -> Column
  -> CString
  -> IO HighsStatus

foreign import ccall "Highs_readOptions" highs_readOptions
  :: Ptr Problem
  -> CString
  -> IO HighsStatus

foreign import ccall "Highs_setBoolOptionValue" highs_setBoolOptionValue
  :: Ptr Problem
  -> CString
  -> CInt
  -> IO HighsStatus

foreign import ccall "Highs_setIntOptionValue" highs_setIntOptionValue
  :: Ptr Problem
  -> CString
  -> CInt
  -> IO HighsStatus

foreign import ccall "Highs_setDoubleOptionValue" highs_setDoubleOptionValue
  :: Ptr Problem
  -> CString
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_setStringOptionValue" highs_setStringOptionValue
  :: Ptr Problem
  -> CString
  -> CString
  -> IO HighsStatus

foreign import ccall "Highs_getBoolOptionValue" highs_getBoolOptionValue
  :: Ptr Problem
  -> CString
  -> Ptr CInt
  -> IO HighsStatus

foreign import ccall "Highs_getIntOptionValue" highs_getIntOptionValue
  :: Ptr Problem
  -> CString
  -> Ptr CInt
  -> IO HighsStatus

foreign import ccall "Highs_getDoubleOptionValue" highs_getDoubleOptionValue
  :: Ptr Problem
  -> CString
  -> Ptr CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getStringOptionValue" highs_getStringOptionValue
  :: Ptr Problem
  -> CString
  -> Ptr CString
  -> IO HighsStatus

foreign import ccall "Highs_getOptionType" highs_getOptionType
  :: Ptr Problem
  -> CString
  -> Ptr HighsOptionType
  -> IO HighsStatus

foreign import ccall "Highs_resetOptions" highs_resetOptions
  :: Ptr Problem
  -> IO HighsStatus

foreign import ccall "Highs_writeOptions" highs_writeOptions
  :: Ptr Problem
  -> CString
  -> IO HighsStatus

foreign import ccall "Highs_writeOptionDeviations" highs_writeOptionDeviations
  :: Ptr Problem
  -> CString
  -> IO HighsStatus

foreign import ccall "Highs_getNumOptions" highs_getNumOptions
  :: Ptr Problem
  -> IO CInt

foreign import ccall "Highs_getOptionName" highs_getOptionName
  :: Ptr Problem
  -> CInt
  -> Ptr CString
  -> IO HighsStatus

foreign import ccall "Highs_getBoolOptionValues" highs_getBoolOptionValues
  :: Ptr Problem
  -> CString
  -> Ptr CInt
  -> Ptr CInt
  -> IO HighsStatus

foreign import ccall "Highs_getIntOptionValues" highs_getIntOptionValues
  :: Ptr Problem
  -> CString
  -> Ptr CInt
  -> Ptr CInt
  -> IO HighsStatus

foreign import ccall "Highs_getDoubleOptionValues" highs_getDoubleOptionValues
  :: Ptr Problem
  -> CString
  -> Ptr CDouble
  -> Ptr CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getStringOptionValues" highs_getStringOptionValues
  :: Ptr Problem
  -> CString
  -> Ptr CString
  -> Ptr CString
  -> IO HighsStatus

foreign import ccall "Highs_getIntInfoValue" highs_getIntInfoValue
  :: Ptr Problem
  -> CString
  -> Ptr CInt
  -> IO HighsStatus

foreign import ccall "Highs_getDoubleInfoValue" highs_getDoubleInfoValue
  :: Ptr Problem
  -> CString
  -> Ptr CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getInt64InfoValue" highs_getInt64InfoValue
  :: Ptr Problem
  -> CString
  -> Ptr Int64
  -> IO HighsStatus

foreign import ccall "Highs_getInfoType" highs_getInfoType
  :: Ptr Problem
  -> CString
  -> Ptr HighsInfoType
  -> IO HighsStatus

foreign import ccall "Highs_getSolution" highs_getSolution
  :: Ptr Problem
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getBasis" highs_getBasis
  :: Ptr Problem
  -> HighsArray HighsBasisStatus
  -> HighsArray HighsBasisStatus
  -> IO HighsStatus

foreign import ccall "Highs_getModelStatus" highs_getModelStatus
  :: Ptr Problem
  -> IO HighsModelStatus

foreign import ccall "Highs_getDualRay" highs_getDualRay
  :: Ptr Problem
  -> Ptr CInt
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getPrimalRay" highs_getPrimalRay
  :: Ptr Problem
  -> Ptr CInt
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getObjectiveValue" highs_getObjectiveValue
  :: Ptr Problem
  -> IO CDouble

foreign import ccall "Highs_getBasicVariables" highs_getBasicVariables
  :: Ptr Problem
  -> HighsArray Row
  -> IO HighsStatus

foreign import ccall "Highs_getBasisInverseRow" highs_getBasisInverseRow
  :: Ptr Problem
  -> Ptr Row
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Row
  -> IO HighsStatus

foreign import ccall "Highs_getBasisInverseCol" highs_getBasisInverseCol
  :: Ptr Problem
  -> Ptr Column
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Column
  -> IO HighsStatus

foreign import ccall "Highs_getBasisSolve" highs_getBasisSolve
  :: Ptr Problem
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Row
  -> IO HighsStatus

foreign import ccall "Highs_getBasisTransposeSolve" highs_getBasisTransposeSolve
  :: Ptr Problem
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Row
  -> IO HighsStatus

foreign import ccall "Highs_getReducedRow" highs_getReducedRow
  :: Ptr Problem
  -> Row
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Row
  -> IO HighsStatus

foreign import ccall "Highs_getReducedColumn" highs_getReducedColumn
  :: Ptr Problem
  -> Column
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Column
  -> IO HighsStatus

foreign import ccall "Highs_setBasis" highs_setBasis
  :: Ptr Problem
  -> HighsArray HighsBasisStatus
  -> HighsArray HighsBasisStatus
  -> IO HighsStatus

foreign import ccall "Highs_setLogicalBasis" highs_setLogicalBasis
  :: Ptr Problem
  -> IO HighsStatus

foreign import ccall "Highs_setSolution" highs_setSolution
  :: Ptr Problem
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_setCallback" highs_setCallback
  :: Ptr Problem
  -> FunPtr (HighsCallback -> CString -> Ptr HighsCallbackDataOut -> Ptr HighsCallbackDataIn, Ptr a -> IO ())
  -> Ptr a
  -> IO HighsStatus

foreign import ccall "Highs_startCallback" highs_startCallback
  :: Ptr Problem
  -> HighsCallback
  -> IO HighsStatus

foreign import ccall "Highs_stopCallback" highs_stopCallback
  :: Ptr Problem
  -> HighsCallback
  -> IO HighsStatus

foreign import ccall "Highs_getRunTime" highs_getRunTime
  :: Ptr Problem
  -> IO CDouble

foreign import ccall "Highs_zeroAllClocks" highs_zeroAllClocks
  :: Ptr Problem
  -> IO HighsStatus

foreign import ccall "Highs_addCol" highs_addCol
  :: Ptr Problem
  -> CDouble
  -> CDouble
  -> CDouble
  -> Count
  -> IO HighsStatus

foreign import ccall "Highs_addCols" highs_addCols
  :: Ptr Problem
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Count
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_addVar" highs_addVar
  :: Ptr Problem
  -> CDouble
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_addVars" highs_addVars
  :: Ptr Problem
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_addRow" highs_addRow
  :: Ptr Problem
  -> CDouble
  -> CDouble
  -> CDouble
  -> Count
  -> IO HighsStatus

foreign import ccall "Highs_addRows" highs_addRows
  :: Ptr Problem
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Count
  -> HighsArray Row
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeObjectiveSense" highs_changeObjectiveSense
  :: Ptr Problem
  -> HighsObjSense
  -> IO HighsStatus

foreign import ccall "Highs_changeObjectiveOffset" highs_changeObjectiveOffset
  :: Ptr Problem
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeColIntegrality" highs_changeColIntegrality
  :: Ptr Problem
  -> Column
  -> HighsVarType
  -> IO HighsStatus

foreign import ccall "Highs_changeColIntegralityByRange" highs_changeColIntegralityByRange
  :: Ptr Problem
  -> Column
  -> Column
  -> HighsArray HighsVarType
  -> IO HighsStatus

foreign import ccall "Highs_changeColIntegralityBySet" highs_changeColIntegralityBySet
  :: Ptr Problem
  -> Count
  -> HighsArray Column
  -> HighsArray HighsVarType
  -> IO HighsStatus

foreign import ccall "Highs_changeColIntegralityByMask" highs_changeColIntegralityByMask
  :: Ptr Problem
  -> HighsArray (HighsInt CInt)
  -> HighsArray HighsVarType
  -> IO HighsStatus

foreign import ccall "Highs_changeColCost" highs_changeColCost
  :: Ptr Problem
  -> Column
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeColCostByRange" highs_changeColCostByRange
  :: Ptr Problem
  -> Column
  -> Column
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeColCostBySet" highs_changeColCostBySet
  :: Ptr Problem
  -> Count
  -> HighsArray Column
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeColCostByMask" highs_changeColCostByMask
  :: Ptr Problem
  -> HighsArray (HighsInt CInt)
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeColBounds" highs_changeColBounds
  :: Ptr Problem
  -> Column
  -> CDouble
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeColBoundsByRange" highs_changeColBoundsByRange
  :: Ptr Problem
  -> Column
  -> Column
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeColBoundsBySet" highs_changeColBoundsBySet
  :: Ptr Problem
  -> Count
  -> HighsArray Column
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeColBoundsByMask" highs_changeColBoundsByMask
  :: Ptr Problem
  -> HighsArray (HighsInt CInt)
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeRowBounds" highs_changeRowBounds
  :: Ptr Problem
  -> Row
  -> CDouble
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeRowBoundsByRange" highs_changeRowBoundsByRange
  :: Ptr Problem
  -> Row
  -> Row
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeRowBoundsBySet" highs_changeRowBoundsBySet
  :: Ptr Problem
  -> Count
  -> HighsArray Row
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeRowBoundsByMask" highs_changeRowBoundsByMask
  :: Ptr Problem
  -> HighsArray (HighsInt CInt)
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_changeCoef" highs_changeCoef
  :: Ptr Problem
  -> Row
  -> Column
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getObjectiveSense" highs_getObjectiveSense
  :: Ptr Problem
  -> Ptr HighsObjSense
  -> IO HighsStatus

foreign import ccall "Highs_getObjectiveOffset" highs_getObjectiveOffset
  :: Ptr Problem
  -> Ptr CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getColsByRange" highs_getColsByRange
  :: Ptr Problem
  -> Column
  -> Column
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getColsBySet" highs_getColsBySet
  :: Ptr Problem
  -> Count
  -> HighsArray Column
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getColsByMask" highs_getColsByMask
  :: Ptr Problem
  -> HighsArray CInt
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Column
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getRowsByRange" highs_getRowsByRange
  :: Ptr Problem
  -> Row
  -> Row
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Row
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getRowsBySet" highs_getRowsBySet
  :: Ptr Problem
  -> Count
  -> HighsArray Row
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Row
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getRowsByMask" highs_getRowsByMask
  :: Ptr Problem
  -> HighsArray CInt
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> Ptr Count
  -> HighsArray Row
  -> HighsArray Row
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getRowName" highs_getRowName
  :: Ptr Problem
  -> Row
  -> Ptr CString
  -> IO HighsStatus

foreign import ccall "Highs_getRowByName" highs_getRowByName
  :: Ptr Problem
  -> CString
  -> Ptr Row
  -> IO HighsStatus

foreign import ccall "Highs_getColName" highs_getColName
  :: Ptr Problem
  -> Column
  -> Ptr CString
  -> IO HighsStatus

foreign import ccall "Highs_getColByName" highs_getColByName
  :: Ptr Problem
  -> CString
  -> Ptr Column
  -> IO HighsStatus

foreign import ccall "Highs_getColIntegrality" highs_getColIntegrality
  :: Ptr Problem
  -> Column
  -> Ptr HighsVarType
  -> IO HighsStatus

foreign import ccall "Highs_deleteColsByRange" highs_deleteColsByRange
  :: Ptr Problem
  -> Column
  -> Column
  -> IO HighsStatus

foreign import ccall "Highs_deleteColsBySet" highs_deleteColsBySet
  :: Ptr Problem
  -> Count
  -> Column
  -> IO HighsStatus

foreign import ccall "Highs_deleteColsByMask" highs_deleteColsByMask
  :: Ptr Problem
  -> Column
  -> IO HighsStatus

foreign import ccall "Highs_deleteRowsByRange" highs_deleteRowsByRange
  :: Ptr Problem
  -> Row
  -> Row
  -> IO HighsStatus

foreign import ccall "Highs_deleteRowsBySet" highs_deleteRowsBySet
  :: Ptr Problem
  -> Count
  -> Row
  -> IO HighsStatus

foreign import ccall "Highs_deleteRowsByMask" highs_deleteRowsByMask
  :: Ptr Problem
  -> Row
  -> IO HighsStatus

foreign import ccall "Highs_scaleCol" highs_scaleCol
  :: Ptr Problem
  -> Column
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_scaleRow" highs_scaleRow
  :: Ptr Problem
  -> Row
  -> CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getInfinity" highs_getInfinity
  :: Ptr Problem
  -> IO CDouble

foreign import ccall "Highs_getNumCol" highs_getNumCol
  :: Ptr Problem
  -> IO Count

foreign import ccall "Highs_getNumRow" highs_getNumRow
  :: Ptr Problem
  -> IO Count

foreign import ccall "Highs_getNumNz" highs_getNumNz
  :: Ptr Problem
  -> IO Count

foreign import ccall "Highs_getHessianNumNz" highs_getHessianNumNz
  :: Ptr Problem
  -> IO Count

foreign import ccall "Highs_getModel" highs_getModel
  :: Ptr Problem
  -> HighsMatrixFormat
  -> HighsHessianFormat
  -> Ptr Count
  -> Ptr Count
  -> Ptr Count
  -> Ptr Count
  -> Ptr HighsObjSense
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CInt
  -> HighsArray CInt
  -> HighsArray CDouble
  -> HighsArray CInt
  -> HighsArray CInt
  -> HighsArray CDouble
  -> HighsArray HighsVarType
  -> IO HighsStatus

foreign import ccall "Highs_crossover" highs_crossover
  :: Ptr Problem
  -> Count
  -> Count
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> IO HighsStatus

foreign import ccall "Highs_getRanging" highs_getRanging
  :: Ptr Problem
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CInt
  -> HighsArray CInt
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CInt
  -> HighsArray CInt
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CInt
  -> HighsArray CInt
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CInt
  -> HighsArray CInt
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CInt
  -> HighsArray CInt
  -> HighsArray CDouble
  -> HighsArray CDouble
  -> HighsArray CInt
  -> HighsArray CInt
  -> IO HighsStatus

foreign import ccall "Highs_resetGlobalScheduler" highs_resetGlobalScheduler
  :: CInt
  -> IO ()
