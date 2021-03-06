module Main
( main
) where

import Cathode.UI.CategoryList as CategoryList
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Types (Element)
import Prelude
import React (createClass, createFactory, ReactClass, spec)
import React.DOM as D
import ReactDOM (render)

app :: forall props. ReactClass {| props}
app = createClass (spec initial render)
  where initial = unit
        render _ = pure $ D.div [] [createFactory CategoryList.ui {}]

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = container >>= render (createFactory app {}) >>> void

foreign import container :: forall eff. Eff (dom :: DOM | eff) Element
