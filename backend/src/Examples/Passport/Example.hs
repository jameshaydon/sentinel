-- | UK Passport eligibility example configuration.
module Examples.Passport.Example
  ( passportExample,
  )
where

import Examples.Passport.Tools (passportToolkit)
import Examples.Passport.Types (PassportDB (..))
import Sentinel.Example (Example (..))
passportExample :: Example PassportDB
passportExample =
  Example
    { name = "passport",
      description = "UK Passport Eligibility",
      sampleQueries =
        [ "Am I eligible for a British passport?",
          "Can I get British citizenship through my mother?",
          "I was born abroad but my father is British"
        ],
      goodbyeMessage = "Good luck with your application!",
      toolkit = passportToolkit,
      initialDB = PassportDB
    }
