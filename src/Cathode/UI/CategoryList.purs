module Cathode.UI.CategoryList
( Props
, State
, Derivative(..)
, ui
) where

import Prelude
import React (createClass, ReactClass, readState, spec, transformState)
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

  let tab next text =
        D.button [ P.onClick \_ -> transformState this (_ {derivative = next})
                 , P.className (if next == derivative then "-current" else "")
                 ]
                 text
  let tabs = D.nav [P.className "-tabs"]
                   [ tab Original [D.text "ℂ"]
                   , tab Arrow    [D.text "ℂ", D.sup [] [D.text "→"]]
                   , tab Opposite [D.text "ℂ", D.sup [] [D.text "op"]]
                   ]

  let listItemSuffix = case derivative of
                         Original -> []
                         Arrow    -> [D.sup [] [D.text "→"]]
                         Opposite -> [D.sup [] [D.text "op"]]
  let listItem name = D.li [] ([D.text name] <> listItemSuffix)
  let list = D.ul [P.className "-list"]
                  [ listItem "Poset"
                  , listItem "Purs"
                  , listItem "Set"
                  ]

  pure $ D.div [P.className "cathode--category-list"]
               [tabs, list]
