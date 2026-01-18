-- | Air Canada Sentinel: guards tool calls and manages fact resolution.
--
-- This module provides the Air Canada sentinel built from the toolkit.
-- All tool definitions, guards, and execution logic are in Tools.hs.
module Examples.AirCanada.Sentinel
  ( -- * Sentinel
    airCanadaSentinel,

    -- * Type aliases
    AirCanadaSentinelM,
  )
where

import Examples.AirCanada.Facts qualified as Facts
import Examples.AirCanada.Tools (airCanadaToolkit)
import Examples.AirCanada.Types (AirlineDB)
import Sentinel.Sentinel (Sentinel, SentinelM)
import Sentinel.Toolkit (toolkitSentinel)

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

-- | Air Canada Sentinel monad.
type AirCanadaSentinelM = SentinelM AirlineDB Facts.Fact

--------------------------------------------------------------------------------
-- Sentinel Implementation
--------------------------------------------------------------------------------

-- | The Air Canada Sentinel.
--
-- Built from the Air Canada toolkit using the generic toolkitSentinel.
airCanadaSentinel :: Sentinel AirlineDB Facts.Fact
airCanadaSentinel = toolkitSentinel airCanadaToolkit
