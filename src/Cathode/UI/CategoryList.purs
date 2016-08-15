module Cathode.UI.CategoryList
( Props
, State
, Derivative(..)
, ui
) where

import Prelude
import React (createClass, ReactClass, ReactElement, readState, spec, transformState)
import React.DOM as D
import React.DOM.Props as P

type Props p = {| p}

type State = { derivative :: Derivative
             }

data Derivative = Original | Arrow | Opposite
derive instance eqDerivative :: Eq Derivative

ui :: forall props. ReactClass {| props}
ui = createClass $ spec {derivative: Original} \this -> do
  derivative <- _.derivative <$> readState this

  let tab next =
        D.button [ P.onClick \_ -> transformState this (_ {derivative = next})
                 , P.className (if next == derivative then "-current" else "")
                 ]
                 ([D.text "ℂ"] <> derivativeSuffix next)
  let tabs = D.nav [P.className "-tabs"]
                   [tab Original, tab Arrow, tab Opposite]

  let listItem name = D.li [] ([D.text name] <> derivativeSuffix derivative)
  let list = D.ul [P.className "-list"]
                  [ listItem "Poset"
                  , listItem "Purs"
                  , listItem "Set"
                  ]

  pure $ D.div [P.className "cathode--category-list"]
               [tabs, list]

derivativeSuffix :: Derivative -> Array ReactElement
derivativeSuffix Original = []
derivativeSuffix Arrow    = [D.sup [] [D.text "→"]]
derivativeSuffix Opposite = [D.sup [] [D.text "op"]]
