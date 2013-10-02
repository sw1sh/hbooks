{-# LANGUAGE NoImplicitPrelude, ViewPatterns #-}

module Lib.S3 where

import ClassyPrelude
import Network.AWS.S3Bucket
import Network.AWS.S3Object
import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import System.Environment

uploadToS3 :: FilePath -> Text -> [(String, Text)] -> ByteString -> IO ()
uploadToS3 (fpToString -> name) (unpack -> contentType) 
           (map (second unpack) -> headers) (repack -> blob) = do
  bucket <- getEnv "S3_BUCKET_NAME"
  let obj = S3Object bucket name contentType headers blob
  amazonS3ConnectionFromEnv >>= maybe (error "Connection to S3 failed.") 
    (\con -> do
      res <- sendObject con obj
      either (error . prettyReqError) return res
    )
