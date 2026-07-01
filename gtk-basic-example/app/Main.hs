
{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
module Main (main) where
{- cabal:
build-depends: base >= 4.16, haskell-gi-base, gi-gtk4
-}
import Control.Monad (void)

import qualified GI.Gtk as Gtk
import Data.GI.Base

activate :: Gtk.Application -> IO ()
activate app = do
  button <- new Gtk.Button [#label := "Click me",
                            On #clicked (?self `set` [#sensitive := False,
                                                      #label := "Thanks for clicking me"])]

  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Hi there",
                                       #child := button]
  window.show  -- or window.showAll if using gi-gtk3 instead of gi-gtk4

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing
