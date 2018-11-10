module UrlParser exposing
    ( Context
    , QueryParser
    , Url
    , UrlParser(..)
    , andIgnore
    , andMap
    , fail
    , int
    , parse
    , query
    , queryCustom
    , queryInt
    , queryString
    , s
    , string
    , succeed
    )

import Dict exposing (Dict)
import Url


type alias Url =
    { path : String
    , query : Maybe String
    }


type alias Context =
    { path : List String
    , query : Dict String (List String)
    }


type UrlParser a
    = UrlParser (Context -> Result String ( a, Context ))


type QueryParser a
    = QueryParser (Dict String (List String) -> a)


succeed : a -> UrlParser a
succeed a =
    UrlParser (\context -> Ok ( a, context ))


fail : String -> UrlParser a
fail message =
    UrlParser (\_ -> Err message)


andMap : UrlParser a -> UrlParser (a -> b) -> UrlParser b
andMap (UrlParser pa) (UrlParser pf) =
    UrlParser
        (\context ->
            case pf context of
                Ok ( f, context1 ) ->
                    case pa context1 of
                        Ok ( a, context2 ) ->
                            Ok ( f a, context2 )

                        Err message ->
                            Err message

                Err message ->
                    Err message
        )


andIgnore : UrlParser ignore -> UrlParser a -> UrlParser a
andIgnore (UrlParser pig) (UrlParser pa) =
    UrlParser
        (\context ->
            case pa context of
                Ok ( a, context1 ) ->
                    case pig context1 of
                        Ok ( _, context2 ) ->
                            Ok ( a, context2 )

                        Err message ->
                            Err message

                Err message ->
                    Err message
        )


s : String -> UrlParser ()
s str =
    UrlParser
        (\context ->
            case context.path of
                head :: tail ->
                    if head == str then
                        Ok
                            ( ()
                            , { context
                                | path = tail
                              }
                            )

                    else
                        Err ("Expected " ++ str ++ ", but " ++ head ++ " found!")

                [] ->
                    Err ("Expected " ++ str ++ ", but there is no path left!")
        )


string : UrlParser String
string =
    UrlParser
        (\context ->
            case context.path of
                head :: tail ->
                    Ok
                        ( head
                        , { context
                            | path = tail
                          }
                        )

                [] ->
                    Err "Expected a string, but there is no path left!"
        )


int : UrlParser Int
int =
    UrlParser
        (\context ->
            case context.path of
                head :: tail ->
                    case String.toInt head of
                        Just i ->
                            Ok
                                ( i
                                , { context
                                    | path = tail
                                  }
                                )

                        Nothing ->
                            Err ("Expected an int, but " ++ head ++ " found !")

                [] ->
                    Err "Expected an int, but there is no path left!"
        )


query : QueryParser a -> UrlParser a
query queryParser =
    UrlParser
        (\context ->
            Ok
                ( parseQuery queryParser context.query
                , context
                )
        )


parseQuery : QueryParser a -> Dict String (List String) -> a
parseQuery (QueryParser p) q =
    p q


queryInt : String -> QueryParser (Maybe Int)
queryInt key =
    queryCustom String.toInt key


queryString : String -> QueryParser (Maybe String)
queryString key =
    queryCustom Just key


queryCustom : (String -> Maybe a) -> String -> QueryParser (Maybe a)
queryCustom f key =
    QueryParser
        (\q ->
            case Dict.get key q of
                Just [ value ] ->
                    f value

                _ ->
                    Nothing
        )


parse : UrlParser a -> Url -> Maybe a
parse (UrlParser p) url =
    case
        p
            { path = preparePath url.path
            , query = prepareQuery url.query
            }
    of
        Ok ( a, context ) ->
            let
                _ =
                    Debug.log "context" context
            in
            Just a

        Err message ->
            let
                _ =
                    Debug.log "error message" message
            in
            Nothing



-- PREPARE PATH (from elm/url)


preparePath : String -> List String
preparePath path =
    case String.split "/" path of
        "" :: segments ->
            removeFinalEmpty segments

        segments ->
            removeFinalEmpty segments


removeFinalEmpty : List String -> List String
removeFinalEmpty segments =
    case segments of
        [] ->
            []

        "" :: [] ->
            []

        segment :: rest ->
            segment :: removeFinalEmpty rest



-- PREPARE QUERY (from elm/url)


prepareQuery : Maybe String -> Dict String (List String)
prepareQuery maybeQuery =
    case maybeQuery of
        Nothing ->
            Dict.empty

        Just qry ->
            List.foldr addParam Dict.empty (String.split "&" qry)


addParam : String -> Dict String (List String) -> Dict String (List String)
addParam segment dict =
    case String.split "=" segment of
        [ rawKey, rawValue ] ->
            case Url.percentDecode rawKey of
                Nothing ->
                    dict

                Just key ->
                    case Url.percentDecode rawValue of
                        Nothing ->
                            dict

                        Just value ->
                            Dict.update key (addToParametersHelp value) dict

        _ ->
            dict


addToParametersHelp : a -> Maybe (List a) -> Maybe (List a)
addToParametersHelp value maybeList =
    case maybeList of
        Nothing ->
            Just [ value ]

        Just list ->
            Just (value :: list)
