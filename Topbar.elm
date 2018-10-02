module Main exposing (..)

import Bindata
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HSA
import Task


stickySection : String -> Int -> Html msg
stickySection headerText numEntries =
    Html.div [ HSA.class stickyHeaderConfig.sectionClass ]
        [ stickyHeader headerText
        , Html.ul
            [ HSA.class stickyHeaderConfig.sectionBodyClass
            ]
            (List.map (\i -> Html.li [] [ Html.text (toString i) ]) (List.range 0 numEntries))
        ]


stickyHeader : String -> Html msg
stickyHeader headerText =
    Html.div
        [ HSA.css
            [ Css.backgroundColor (Css.hex "3d3c3c")
            , Css.color (Css.hex "868585")
            , Css.fontSize (Css.px 18)
            , Css.lineHeight (Css.num 1.4)
            , Css.width (Css.pct 100)
            , Css.opacity (Css.num 0.9)
            , Css.zIndex (Css.int 1)
            , Css.paddingTop (Css.px 17.5)
            , Css.paddingBottom (Css.px 2.5)
            , Css.paddingLeft (Css.px 37.5)
            ]
        , HSA.class stickyHeaderConfig.sectionHeaderClass
        ]
        [ Html.text headerText ]
