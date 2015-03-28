{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
module PullRequestFiles (filesByPullRequest, prsTouchingFile) where

import           BasicPrelude
import           Data.Text           (unpack)
import           Github.PullRequests

fromEither :: String -> Either Error a -> IO a
fromEither msgPrefix (Left e)  = fail $ msgPrefix ++ ": " ++ unpack (show e)
fromEither _         (Right a) = return a

filesByPullRequest :: String -> String -> IO [(Int, [File])]
filesByPullRequest user repo = do
  pullReqs <- fromEither "pull requests" =<< pullRequestsFor user repo
  mapM (combine . pullRequestNumber) pullReqs
  where
        combine prNum = (prNum, ) <$> (fromEither ("files for " ++ unpack (show prNum)) =<< pullRequestFiles user repo prNum)

prsTouchingFile :: String -> String -> String -> IO [Int]
prsTouchingFile file user repo = map fst . filter (any ((== file) . fileFilename) . snd) <$> filesByPullRequest user repo
