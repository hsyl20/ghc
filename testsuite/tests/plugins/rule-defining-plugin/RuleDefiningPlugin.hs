module RuleDefiningPlugin where

import GHC.Plugin

{-# RULES "unsound" forall x. show x = "SHOWED" #-}

plugin :: Plugin
plugin = defaultPlugin
