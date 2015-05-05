module SecondFront.Workers.W404(
    -- | A coherent worker that always returns 404 errors

    w404
    ) where 

import SecondTransfer 
import SecondFront.Workers.Data

import Data.Conduit 

-- | Very simple handler
w404 :: CoherentWorker 
w404 _ = return (
    bad404ResponseHeaders,
    [],
    ( do 
        yield bad404ResponseData
        return []
    )
  )