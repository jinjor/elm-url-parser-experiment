module UrlParser exposing
    ( Context
    , Url
    , UrlParser(..)
    , andIgnore
    , andMap
    , fail
    , int
    , parse
    , query
    , s
    , string
    , succeed
    )

import Dict exposing (Dict)


type alias Url =
    { path : List String
    , query : Dict String String
    }


type alias Context =
    { path : List String
    , query : Dict String String
    }


type UrlParser a
    = UrlParser (Context -> Result String ( a, Context ))


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


query : String -> (String -> a) -> UrlParser (Maybe a)
query key f =
    UrlParser
        (\context ->
            case Dict.get key context.query of
                Just value ->
                    Ok
                        ( Just (f value)
                        , context
                        )

                Nothing ->
                    Ok ( Nothing, context )
        )


parse : UrlParser a -> Url -> Maybe a
parse (UrlParser p) url =
    case p url of
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
