-- | See "Test.Inspection".
--
{-# LANGUAGE CPP #-}
module Test.Inspection.TcPlugin (inspectionTcPlugin) where

-- For the TC plugin
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import TcEvidence
import TcPluginM
import TcRnTypes
import Class
#if MIN_VERSION_GLASGOW_HASKELL(8,5,0,0)
import MkCore
import TyCon
#endif
import Type

inspectionTcPlugin :: TcPlugin
inspectionTcPlugin =
  TcPlugin { tcPluginInit  = lookupPNLTyCon
           , tcPluginSolve = solvePNL
           , tcPluginStop  = const (return ())
           }

lookupPNLTyCon :: TcPluginM Class
lookupPNLTyCon = do
    Found _ md   <- findImportedModule testInspectionModule Nothing
    pnlNm <- lookupOrig md (mkTcOcc "PluginNotLoaded")
    tcLookupClass pnlNm
  where
    testInspectionModule  = mkModuleName "Test.Inspection"

#if MIN_VERSION_GLASGOW_HASKELL(8,5,0,0)
mkNullaryEv :: Class -> EvTerm
mkNullaryEv cls = EvExpr appDc
  where
    tyCon = classTyCon cls
    dc = tyConSingleDataCon tyCon
    appDc = mkCoreConApps dc []
# else
mkNullaryEv :: Class -> EvTerm
mkNullaryEv _ = error "Test.Inspection.TcPlugin needs GHC 8.6 or later"
#endif

findClassConstraint :: Class -> Ct -> Bool
findClassConstraint cls ct
    | Just (cls', []) <- getClassPredTys_maybe (ctPred ct)
    , cls' == cls
    = True
    | otherwise
    = False

solvePNL :: Class -- ^ PNL's TyCon
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solvePNL inspectionTcCls _ _ wanteds =
    return $ TcPluginOk [(mkNullaryEv inspectionTcCls, x)| x <- our_wanteds ] []
  where
    our_wanteds = filter (findClassConstraint inspectionTcCls) wanteds
