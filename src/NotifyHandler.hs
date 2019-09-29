-- file: NotifyHandler.hs

module NotifyHandler where

import Notifier


-- With the use of this message handler caller
-- should be make sure about the message list
-- is in a form (Subject, Body)
emailNotify :: MessageHandler
emailNotify unit = do
  let subject = head . snd $ unit
      body = last . snd $ unit

