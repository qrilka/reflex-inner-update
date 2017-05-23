{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Reflex.Dom

main :: IO ()
main =
  mainWidget $ do
    (btn, _) <- el' "button" $ text "Click Me!"
    el "div" $ text "Boo"
    elWithChildren (["a", "b"] <$ domEvent Click btn)
    blank

elWithChildren
  :: MonadWidget t m
  => Event t [Text] -> m ()
elWithChildren ev = do
  ev' <- delay 0 ev
  _ <- performEventAsync $ (\_ -> liftIO js_logSpans) <$ ev'
  void . el "div" $
    widgetHold (text "Waiting") $
    ffor ev $ \xs -> forM_ xs $ \x -> el "span" $ text x

foreign import javascript unsafe
  "console.log('Number of SPANs: ' + document.getElementsByTagName('span').length);"
  js_logSpans :: IO ()
