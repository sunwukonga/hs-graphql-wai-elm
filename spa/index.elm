module Main exposing (..)

import Html exposing (Html, div, text)
import GraphQL.Request.Builder as Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)

--type Hello {
--  greeting(who: String!): String!
--}
type alias Hello =
  { greeting : String
  }

helloQuery : Document Query Hello vars
--{ v | who : String }
helloQuery =
  let
--    whoVar =
--      Var.required "who" .who Var.string

    hello =
      object Hello
        |> with
          (field "greeting"
--            [( "who", Arg.variable whoVar )]
            [( "who", (Arg.string "Tim") )]
            string
          )
  in
     queryDocument hello
{-
     hello
       |> queryDocument
       |> request
            { who = "Tim" }
-}

helloRequest : Request Query Hello
helloRequest =
  helloQuery
    |> request vars
   -- { who = "John" }
--    |> request { who = "Tim" }

{-| A function that helps you extract node objects from paginated Relay connections.
-}
connectionNodes :
    ValueSpec NonNull ObjectType result vars
    -> ValueSpec NonNull ObjectType (List result) vars
connectionNodes spec =
    extract
        (field "edges"
            []
            (list
                (extract
                    (field "node" [] spec)
                )
            )
        )


type alias HelloResponse =
    Result GraphQLClient.Error Hello


type alias Model =
  { queryResponse : Maybe HelloResponse
  , originalRequest : String
  }

type Msg
    = ReceiveQueryResponse HelloResponse


sendQueryRequest : Request Query a -> Task GraphQLClient.Error a
sendQueryRequest request =
    GraphQLClient.sendQuery "http://localhost:8000/graphql" request


sendHelloQuery : Request Query Hello -> Cmd Msg
sendHelloQuery request =
--    sendQueryRequest helloRequest
    sendQueryRequest request
        |> Task.attempt ReceiveQueryResponse


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
  let
    staticHelloRequest = helloRequest
  in
    ( Model Nothing (Builder.requestBody staticHelloRequest), sendHelloQuery staticHelloRequest )


view : Model -> Html Msg
view model =
    div []
        [ div [] [model.queryResponse |> toString |> text]
        , div [] [model.originalRequest |> text]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update (ReceiveQueryResponse response) model =
    ( Model (Just response) model.originalRequest, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
