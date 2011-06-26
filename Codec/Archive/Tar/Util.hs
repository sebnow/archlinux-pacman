-- |
-- Module:      Codec.Archive.Tar
-- Stability:   Experimental
-- Portability: Portable
--
-- Internal module with conveniance functions for Codec.Archive.Tar
module Codec.Archive.Tar.Util (filterEntries) where
import Codec.Archive.Tar

-- | Like the standard 'filter' but for 'Entries'.
filterEntries :: (Entry -> Bool) -> Entries -> Entries
filterEntries _ Done = Done
filterEntries p (Fail s) = Fail s
filterEntries p (Next e es)
    | p e       = Next e (filterEntries p es)
    | otherwise = filterEntries p es

