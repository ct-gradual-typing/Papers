-- --Original working code:

-- --import GHC
-- import GHC.Paths
-- --import DynFlags
-- import Unsafe.Coerce

-- main :: IO ()
-- main =
    -- defaultErrorHandler defaultDynFlags $ do
      -- func <- runGhc (Just libdir) $ do
        -- dflags <- getSessionDynFlags
        -- setSessionDynFlags dflags
        -- target <- guessTarget "Test.hs" Nothing
        -- addTarget target
        -- r <- load LoadAllTargets
        -- case r of
          -- Failed -> error "Compilation failed"
          -- Succeeded -> do
            -- m <- findModule (mkModuleName "Test") Nothing
            -- setContext [] [m]
            -- value <- compileExpr ("Test.print")
            -- do let value' = (unsafeCoerce value) :: String -> IO ()
               -- return value'
      -- func "Hello"
      -- return ()
      
-- -- FIST UPDATE:
-- GHC.setContext []
  -- -- import qualified Module
  -- [ (GHC.simpleImportDecl . GHC.mkModuleName $ moduleName)
    -- { GHC.ideclQualified = True
    -- }
  -- -- -- import qualified Data.Dynamic
  -- -- , (GHC.simpleImportDecl . GHC.mkModuleName $ "Data.Dynamic")
  -- --   { GHC.ideclQualified = True
  -- --   }
  -- ]
-- fetched <- GHC.compileExpr $ moduleName ++ "." ++ externalFuncName
-- return . unsafeCoerce $ fetched
-- -- or:
-- -- fetched <- GHC.dynCompileExpr $ moduleName ++ "." ++ externalFuncName
-- -- return . fromDynamic (error "Illegal type cast") $ fetched

-- -- SECOND UPDATE:
-- GHC.setContext  
  -- -- import qualified Module
  -- [ GHC.IIDecl $ 
    -- (GHC.simpleImportDecl . GHC.mkModuleName $ moduleName)
    -- {GHC.ideclQualified = True}
  -- ]
  
  
------------------------------------------------------------------------------------------
--EXAMPLE FROM HASKELL WIKI  
--A.hs
--invoke: ghci -package ghc A.hs
 
{-# LANGUAGE CPP #-}
--import GHC
--import Outputable
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
 
--import DynFlags
targetFile = "LoadingTest.hs"
 
main :: IO ()
main = do
   res <- example
#if __GLASGOW_HASKELL__ > 704
   str <- runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      return $ showSDoc dflags $ ppr res
   putStrLn str
#else
   putStrLn $ showSDoc ( ppr res )
#endif
 
example = 
#if __GLASGOW_HASKELL__ > 704
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
#else
    defaultErrorHandler defaultLogAction $ do
#endif
runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = foldl xopt_set dflags
                        [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
    setSessionDynFlags dflags'
    target <- guessTarget targetFile Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName "LoadingTest"
    p <- parseModule modSum
    t <- typecheckModule p
    d <- desugarModule t
    l <- loadModule d
    n <- getNamesInScope
    c <- return $ coreModule d

    g <- getModuleGraph
    mapM showModule g     
    return $ (parsedSource d,"/n-----/n",  typecheckedSource d)
--B.hs

  