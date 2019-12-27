-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{- |
A script for running the Haskell interpreter on Haskell code in bazel project.

For more information, see ...
-}
module Main (main)  where

import Data.List (isPrefixOf, partition)
import Bazel (defBazelOpts, BazelOpts(..))
import Repl (runWith, ReplOptions(..))
import Options.Applicative

-- TODO(jinwoo): Consider merging this with ReplOptions once the test
-- interacts directly with the binary.
data MainOptions = MainOptions
  { replOptions :: ReplOptions
  }

parseOptions :: Parser MainOptions
parseOptions =
    MainOptions
        <$> parseGhciOptions

parseGhciOptions :: Parser ReplOptions
parseGhciOptions =
    liftA2 uncurry
        (ReplOptions <$> bazelOpts <*> packageIds <*> rtsOpts)
        ghcFlagsAndTargets

ghcFlagsAndTargets :: Parser ([String], [String])
ghcFlagsAndTargets = fmap (partition isFlag) $ many
                          $ strArgument
                              (metavar "ARGUMENTS"
                                <> help "A Bazel target or a GHC flag")
                            <|> failOnPackageFlag
  where
    isFlag ('-':_) = True
    isFlag _ = False

-- | Blacklist the specific flag "-package", to avoid confusion with
-- the "--package" flag which could lead to
-- However, allow any other "-p..." flags to pass through to GHC.
-- (This is a bit of a hack because optparse-applicative always parses
-- flags like "-pabcdefg" as "-p=abcdefg", unlike GHC.)
failOnPackageFlag :: Parser String
failOnPackageFlag =
    ("-p" ++) <$> option (eitherReader checkIsNotPackage) (short 'p')
  where
    checkIsNotPackage s
        | "ackage" `isPrefixOf` s =
            Left $ "The \"-package\" flag is not supported; did you mean "
                    ++ "\"--package\"?"
        | otherwise = Right s

bazelOpts :: Parser BazelOpts
bazelOpts = (\bazel preArgs args cmds ->
        defBazelOpts
          { bazelBin = bazel
          , bazelPre = bazelPre defBazelOpts ++ preArgs
          , bazelPost = bazelPost defBazelOpts ++ args
          , bazelShowCommands = cmds
          })
    <$> bazelBinPath
    <*> manySpaced bazelPreArgs
    <*> manySpaced bazelArgs
    <*> showCommands
  where
    manySpaced = fmap (concatMap words) . many
    bazelBinPath = strOption
        $ long "bazel"
       <> value (bazelBin defBazelOpts)
       <> metavar "PATH"
       <> help "Path to the bazel executable"
    bazelPreArgs = strOption
        $ long "bazel-pre-args"
       <> metavar "ARGS"
       <> help ("Space-separated arguments to pass to Bazel before "
                ++ "the main command")
    bazelArgs =
      strOption (
        long "bazel-args"
        <> metavar "ARGS"
        <> help "Space-separated arguments to pass to Bazel"
      ) <|>
      ((\mode -> "-c " <> mode) <$>
       strOption (
          short 'c'
          <> long "compilation_mode"
          <> metavar "MODE"
          <> help "Compilation mode passed to Bazel"
       )
      )
    showCommands = switch $
                      long "show-commands"
                      <> help "Print the calls to Bazel"

packageIds :: Parser [String]
packageIds = many $ strOption
                  $ long "package"
                  <> metavar "TARGET"
                  <> help ("Allows a given library to be imported "
                           ++ "in the interpreter session.")

rtsOpts :: Parser String
rtsOpts = strOption
              $ long "with-rtsopts"
              <> value ""
              <> metavar "OPTS"
              <> help ("RTS options to be used for running the target. "
                       ++ "Space-separated.")

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> parseOptions) forwardOptions
    runWith $ replOptions opts
