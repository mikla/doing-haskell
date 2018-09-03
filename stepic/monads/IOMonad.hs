module IOMonad where

import System.Directory (getDirectoryContents, removeFile)
import Data.List(isInfixOf, filter)
import Control.Monad(liftM)

main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if (length name == 0) then main' else putStrLn $ "Hi, " ++ name ++ "!"

{-
  mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
  mapM_ f = sequence_ . map f
-}

main'' = do
  putStr "Substring: "
  token <- getLine
  if (length token == 0) then putStrLn "Cancelled"
  else getFiles token >>= mapM_ deleteFile


-- liftM :: Monad m => (a -> b) -> m a -> m b (promoting function to a monad)
getFiles :: String -> IO [FilePath]
getFiles token =
  liftM (filter (isInfixOf token)) $ getDirectoryContents "."

deleteFile :: FilePath -> IO()
deleteFile path = do
  putStrLn $ "Removing file: " ++ path
  removeFile path
