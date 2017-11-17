module Test.Properties.Frontend 
  (module Test.Properties.Frontend
  ,module Test.Properties.Runtime
  ) where

import Test.Properties.Runtime
import System.Process
import System.FilePath


binFile :: FilePath -> FilePath
binFile p = p </> "dist" </> "build" </> "property" </> "property" ++ [c|c<-".exe",pathSeparator == '\\']


query :: FilePath -> IO PropData
query p = do 
    s <- readProcess (binFile p) [] ""
    return (read s)    

executeSingle :: FilePath -> Setup -> [Int] -> IO Res
executeSingle p s i = do
    s <- readProcess (binFile p) ["single"] (show (s,i)) 
    return (read s)
    

runAll :: FilePath -> Setup -> IO (PropTree Res)
runAll p s = do
  s <- query p
  go s 
  where
    go (Property i) = fmap Property $ executeSingle p s i
    go (PropertySet ts ps) = do
      ps' <- mapM go ps
      return $ PropertySet ts ps'