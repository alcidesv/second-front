module SecondFront.Workers(
    -- | Submodules here and handy `CoherentWorker`s

    -- * File-serving
    module SecondFront.Workers.FileServer,

    -- * Utility
    module SecondFront.Workers.W404
    ) where

import SecondFront.Workers.FileServer
import SecondFront.Workers.W404