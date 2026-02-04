-- | AirLogic Airlines example configuration.
module Examples.AirLogic.Example
  ( airLogicExample,
  )
where

import Examples.AirLogic.MockDB (initialDB)
import Examples.AirLogic.Tools (airLogicToolkit)
import Examples.AirLogic.Types (AirLogicDB)
import Sentinel.Example (Example (..))
-- | The AirLogic Airlines example.
airLogicExample :: Example AirLogicDB
airLogicExample =
  Example
    { name = "airlogic",
      description = "AirLogic Airlines Customer Service",
      sampleQueries =
        [ "What's the status of my booking BK-2847?",
          "Is flight AL-445 delayed?",
          "I need a refund for booking BK-2847",
          "Find my bookings, my user ID is usr_sarah_chen"
        ],
      goodbyeMessage = "Thank you for using AirLogic Airlines. Goodbye!",
      toolkit = airLogicToolkit,
      initialDB = initialDB
    }
