{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Test.Framework (defaultMain)
import RuleInfo.ExecutionRoot (fixFilePathFlags, fakeExecutionRoot)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

main :: IO ()
main =
  defaultMain
    [ testCase "fixFilePathFlags" $ do
        let flags =
              [ "-package-db=foo/bar",
                "-pgmP",
                "some/path",
                "-optP-isystem",
                "-optPinclude/dir",
                "-optP-isystem",
                "without-optP", -- not prefixed with -optP; should not be modified
                "-some-flag=some-value",
                "-another-flag",
                "another value"
              ]
        fixFilePathFlags (fakeExecutionRoot "/root/path") flags
          @?= [ "-package-db=/root/path/foo/bar",
                "-pgmP",
                "/root/path/some/path",
                "-optP-isystem",
                "-optP/root/path/include/dir",
                "-optP-isystem",
                "without-optP",
                "-some-flag=some-value",
                "-another-flag",
                "another value"
              ]
    ]
