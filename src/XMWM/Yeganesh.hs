module XMWM.Yeganesh (yeganesh) where

import Relude

import XMonad.Core (X)

-- TODO: Implement a wrapper around dmenu that provides yeganesh functionality,
-- but gets rid of annoying extraneous binaries that I will never want to invoke
-- via dmenu.
yeganesh :: X ()
yeganesh = pass
  where
    -- These applications are always listed before any others.
    priority = []
    -- These applications are never listed.
    --
    -- TODO: Maybe all I need is an ignore list, since the things here will
    -- mostly be CLI tools that nobody else will ever want to invoke either.
    ignore = []

    -- On each run, populate the cache of binaries in $PATH. This way we won't
    -- need to load it on-demand on every run.
    populateCache = undefined

    readCache = undefined
    writeCache = undefined
