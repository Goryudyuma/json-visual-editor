module Main exposing (main)

import Browser
import DnDList
import Element
import Html
import Html.Attributes



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- DATA


type alias Task =
    { title : String
    , status : TaskStatus
    }


type TaskStatus
    = Todo
    | Doing
    | Done


initialTaskList : List Task
initialTaskList =
    [ Task "a" Todo, Task "b" Todo, Task "c" Doing, Task "d" Done ]



-- SYSTEM


config : DnDList.Config Task
config =
    { beforeUpdate = beforeUpdate
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


beforeUpdate : Int -> Int -> List Task -> List Task
beforeUpdate a b list =
    list
        |> List.indexedMap Tuple.pair
        |> List.map
            (\( id, item ) ->
                if id == a then
                    { item
                        | status =
                            case b of
                                0 ->
                                    Todo

                                1 ->
                                    Doing

                                _ ->
                                    Done
                    }

                else
                    item
            )


system : DnDList.System Task Msg
system =
    DnDList.create config MyMsg



-- MODEL


type alias Model =
    { dnd : DnDList.Model
    , items : List Task
    }


initialModel : Model
initialModel =
    { dnd = system.model
    , items = initialTaskList
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd



-- UPDATE


type Msg
    = MyMsg DnDList.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MyMsg msg ->
            let
                ( dnd, items ) =
                    system.update msg model.dnd model.items
            in
            ( { model | dnd = dnd, items = items }
            , system.commands model.dnd
            )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.section
        [ Html.Attributes.style "text-align" "center" ]
        [ Element.layout [] <|
            Element.row []
                [ model.items
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\( _, item ) -> item.status == Todo)
                    |> List.map (itemView model.dnd 0)
                    |> List.map Element.html
                    |> Element.column []
                , model.items
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\( _, item ) -> item.status == Doing)
                    |> List.map (itemView model.dnd 1)
                    |> List.map Element.html
                    |> Element.column []
                , model.items
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\( _, item ) -> item.status == Done)
                    |> List.map (itemView model.dnd 2)
                    |> List.map Element.html
                    |> Element.column []
                ]
        , ghostView model.dnd model.items
        ]


itemView : DnDList.Model -> Int -> ( Int, Task ) -> Html.Html Msg
itemView dnd place ( index, item ) =
    let
        itemId : String
        itemId =
            "id-" ++ item.title
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                Html.p
                    (Html.Attributes.id itemId :: system.dropEvents index itemId)
                    [ Html.text item.title ]

            else
                Html.text ""

        Nothing ->
            Html.p
                (Html.Attributes.id itemId :: system.dragEvents index itemId)
                [ Html.text item.title ]


ghostView : DnDList.Model -> List Task -> Html.Html Msg
ghostView dnd items =
    let
        maybeDragItem : Maybe Task
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            Html.div
                (system.ghostStyles dnd)
                [ Html.text item.title ]

        Nothing ->
            Html.text ""
