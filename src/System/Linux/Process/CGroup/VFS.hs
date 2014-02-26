{-# LANGUAGE OverloadedStrings #-}
-- | Interfaces with the linux cgroup virtual filesystem to fetch various basic data. Note that this is an early iteration of the API, and subject to change.
module System.Linux.Process.CGroup.VFS(CGroup(..), allCGroups, listTasks, addTask) where

   import Control.Monad(guard)
   import System.IO (Handle, IOMode(..), hGetContents, openFile, withFile, hPutStr)
   import System.FilePath.Posix
   import Data.Monoid (mempty, Monoid(..))
   import Data.Maybe(catMaybes)
   import System.Posix.Types(ProcessID)

   data CGroup = CheckedCGroup FilePath -- checked for existance by `checked` (came from user)
               | SystemCGroup  FilePath -- not checked since we trust ourselves
         deriving (Eq, Show)
   type CGroupType = String

   -- | basic helper to fetch the path out of the CGroup type.
   filePathForCGroup :: CGroup -> FilePath
   filePathForCGroup (CheckedCGroup g) = g
   filePathForCGroup (SystemCGroup  g) = g

   -- | Writes a string to a file, and appends a newline
   writeLine :: FilePath -> String -> IO ()
   writeLine name line = withFile name WriteMode (\h -> hPutStr h line >> hPutStr h "\n")

   -- | Produces a list of cgroups active on the system. Assumes location of mounts
   allCGroups :: IO [CGroup]
   allCGroups = fmap (map SystemCGroup . catMaybes . map decodeLine . lines) (readFile "/proc/mounts")
       where decodeLine l = case words l of
                              ("cgroup":b:_) -> Just b
                              _              -> Nothing

   -- | Given a CGroup and two monads, will determine whether or not the cgroup exists IFF the cgroup originates from user code, and resolve to one of the two monads.
   checked :: Monoid a => CGroup -> (FilePath -> IO a) -> IO a
   checked   (SystemCGroup  p) m = (m p) -- Trusted
   checked g@(CheckedCGroup p) m = do allG <- allCGroups -- double-check no funny-business
                                      if g `elem` allG then (m p) else (return mempty)

   -- | List all PIDs associated with a cgroup. If the cgroup does not exist, an empty list is produced.
   listTasks :: CGroup -> IO [ProcessID]
   listTasks g = checked g (\p -> fmap (map read . lines) (readFile (p </> "tasks")))

   -- | Adds a PID to a cgroup. If the PID or cgroup does not exist, no action is taken.
   addTask :: CGroup -> ProcessID -> IO ()
   addTask g p = checked g (\z -> writeLine (z </> "tasks") (show p))
