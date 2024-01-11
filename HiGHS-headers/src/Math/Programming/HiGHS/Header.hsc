{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Math.Programming.HiGHS.Header where

import Data.Typeable
import GHC.Generics (Generic)
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic

#include <interfaces/highs_c_api.h>

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
