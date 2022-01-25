--------------------------------------------------------------------------------
-- Functional Programming - Super Functor Factory Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module Config (
    Config(..),
    parseCmdLineArgs
) where

--------------------------------------------------------------------------------

import Options.Applicative

--------------------------------------------------------------------------------

-- | Represents program configurations.
data Config = Cfg {
    -- | A value indicating whether we should not use colour codes in
    -- the program output.
    cfgNoColour  :: Bool,
    -- | A value indicating whether we should not use unicode characters
    -- in the program output.
    cfgNoUnicode :: Bool
} deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | `runP` is a parser for command-line configurations.
runP :: Parser Config
runP = Cfg <$> switch (long "no-colour")
           <*> switch (long "no-unicode")

-- | `opts` represents the configuration for the command-line parser.
opts :: ParserInfo Config
opts = info (runP <**> helper) idm

-- | `parseCmdLineArgs` is a computation which consumes the command-line
-- arguments for the program, parses them, and returns them as a
-- `Config` value.
parseCmdLineArgs :: IO Config
parseCmdLineArgs = execParser opts

--------------------------------------------------------------------------------