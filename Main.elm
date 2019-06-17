module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


bulma =
    div []
        [ node "meta" [ name "viewport", content "width=device-width, initial-scale=1" ] []
        , node "link" [ rel "stylesheet", href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.6.2/css/bulma.min.css" ] []
        ]


type alias Model =
    { livre : Livre }


type Msg
    = SetElem BookField String


type alias Livre =
    { prenom : String, nom : String, titre : String, lieuPub : String, editeur : String, anneePub : String, nbPages : String }


dummyLivre =
    { prenom = "Martin", nom = "Chabot", titre = "La vie devant soi", lieuPub = "Montreal", editeur = "Hachette", anneePub = "2019", nbPages = "23" }


model : Model
model =
    { livre = dummyLivre }


update msg model =
    case msg of
        SetElem bookInput value ->
            { model | livre = model.livre |> updateInput bookInput value }


type BookField
    = Prenom
    | Nom
    | Title
    | LieuPub
    | Editeur
    | AnneePub
    | NbPages


updateInput : BookField -> String -> Livre -> Livre
updateInput field value livre =
    case field of
        Prenom ->
            { livre | prenom = value }

        Nom ->
            { livre | nom = value }

        Title ->
            { livre | titre = value }

        LieuPub ->
            { livre | lieuPub = value }

        Editeur ->
            { livre | editeur = value }

        AnneePub ->
            { livre | anneePub = value }

        NbPages ->
            { livre | nbPages = value }


main =
    Html.beginnerProgram { model = model, update = update, view = view }


view : Model -> Html Msg
view model =
    div []
        [ bulma
        , section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        [ livreForm model.livre
                        ]
                    ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ bioText model.livre |> text ]
                    ]
                ]
            ]
        ]


livreForm livre =
    Html.form [ class "form" ]
        [ textField_ Prenom "Prénom" livre.prenom
        , textField_ Nom "Nom" livre.nom
        , textField_ Title "Titre" livre.titre
        , textField_ LieuPub "Lieu de publication" livre.lieuPub
        , textField_ Editeur "Éditeur" livre.editeur
        , textField_ AnneePub "Année de publication" livre.anneePub
        , textField_ NbPages "Nombre de pages" livre.nbPages
        ]


textField_ fieldTag labelStr val =
    div [ class "field" ]
        [ label [ class "label" ] [ text labelStr ]
        , div [ class "control" ]
            [ input [ class "input", type_ "text", value val, onInput (SetElem fieldTag) ] [ text labelStr ] ]
        ]


bioText : Livre -> String
bioText livre =
    (livre.nom |> String.toUpper)
        ++ ", "
        ++ livre.prenom
        ++ ". "
        ++ livre.titre
        ++ ", "
        ++ viewLieu livre.lieuPub
        ++ ", "
        ++ livre.editeur
        ++ ", "
        ++ livre.anneePub
        ++ ", "
        ++ livre.nbPages
        ++ " p."


viewLieu lieu =
    if lieu |> String.isEmpty then
        "s.l."
    else
        lieu
