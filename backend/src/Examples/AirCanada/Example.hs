-- | Air Canada example configuration.
module Examples.AirCanada.Example
  ( airCanadaExample,
  )
where

import Examples.AirCanada.MockDB (initialDB)
import Examples.AirCanada.Tools (airCanadaToolkit)
import Examples.AirCanada.Types (AirlineDB)
import Sentinel.Example (Example (..))
import Sentinel.Toolkit (withVerification)

-- | The Air Canada example.
airCanadaExample :: Example AirlineDB
airCanadaExample =
  Example
    { name = "aircanada",
      description = "Air Canada Customer Service",
      sampleQueries =
        [ "What's the status of my booking REF123?",
          "Is flight AC102 delayed?",
          "I need a refund for booking REF789",
          "Find my bookings, my name is Alice Smith"
        ],
      goodbyeMessage = "Thank you for using Air Canada. Goodbye!",
      toolkit = withVerification airCanadaToolkit,
      initialDB = initialDB
    }
