module RemoteResource exposing
    ( RemoteDataResource, RemoteResource
    , init, loading, reloading
    , setResource, setOnlyResource, setBackground, setOnlyBackground, replaceResourceByBackground
    , resource, backgroundResource, resourceAndBackgroundWithDefault
    , map, mapBackground, hasNewResource
    )

{-| Remote Resource alllows you to handle foreground and background resources, using [https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/](RemoteData).

For instance, let's say you want to retrieve a list of posts. You are using the RemoteData package, thus you can handle the state of your request (Loading, Success, etc...).
You will probably display a loading icon while your request is in progress, and once it has been fully retrieved you will display your posts.
But now, let's say you know your posts have changed and you would like to update them. You will probably want to send another request to your webservice with the same Elm RemoteData you
used when you first retrieved your posts. That means your users will see your posts disapear for a few milliseconds (depending on the latency and network speed),
replaced by a loading icon, to be finally updated and displayed again to your users.

Now, with `RemoteResource` you can refresh your posts in the background. Than means that you keep displaying your "old" posts to your users,
and when the new data has been retrieved you can replace your old posts by the new ones without your users seeing some loading icon!


# Definitions

@docs RemoteDataResource, RemoteResource


# Initialization

@docs init, loading, reloading


# Updating state

@docs setResource, setOnlyResource, setBackground, setOnlyBackground, replaceResourceByBackground


# Getters

@docs resource, backgroundResource, resourceAndBackgroundWithDefault


# Advanced

@docs map, mapBackground, hasNewResource

-}

import RemoteData


{-| Definition of the `RemoteResource`.

  - `error` is the error type, usually a `Http.Error`
  - `res` is your final resource, the one you are trying to retrieve

-}
type RemoteResource error res
    = RemoteResource (RemoteResource_ error res)


{-| This is just an alias for `RemoteData error res`
-}
type alias RemoteDataResource error res =
    RemoteData.RemoteData error res


type alias RemoteResource_ error res =
    { res : RemoteDataResource error res
    , background : RemoteDataResource error res
    }


{-| Initializes the remote resource. This is the first thing to do to get a brand new `RemoteResource`.
-}
init : RemoteResource error res
init =
    RemoteResource
        { res = RemoteData.NotAsked
        , background = RemoteData.NotAsked
        }


{-| To mark you foreground resource as `Loading`.
Your background resource will be reset to `NotAsked`.

        RemoteResource.init |> RemoteResource.loading

-}
loading : RemoteResource error res -> RemoteResource error res
loading (RemoteResource { res, background }) =
    RemoteResource
        { res = RemoteData.Loading
        , background = RemoteData.NotAsked
        }


{-| To mark your background resource as `Loading`
This does not change your foreground resource.

        RefreshPosts ->
            ( posts |> RemoteResource.reloading, Cmd.none )

-}
reloading : RemoteResource error res -> RemoteResource error res
reloading (RemoteResource rr) =
    RemoteResource { rr | background = RemoteData.Loading }


{-| Get the current foreground resource
-}
resource : RemoteResource error res -> RemoteDataResource error res
resource (RemoteResource { res }) =
    res


{-| Get the current background resource
-}
backgroundResource : RemoteResource error res -> RemoteDataResource error res
backgroundResource (RemoteResource { background }) =
    background


{-| Get a tuple with the foreground resource and the background resource.
You will retrieve the final resource, thus, if the resource is not in `Success` state, the default resource will be returned.
-}
resourceAndBackgroundWithDefault : res -> RemoteResource error res -> ( res, res )
resourceAndBackgroundWithDefault default (RemoteResource { res, background }) =
    ( RemoteData.withDefault default res, RemoteData.withDefault default background )


{-| Set the foreground resource. This will reset the background resource to `NotAsked`.
-}
setResource : RemoteDataResource error res -> RemoteResource error res -> RemoteResource error res
setResource res (RemoteResource rr) =
    RemoteResource { rr | res = res, background = RemoteData.NotAsked }


{-| Set the foreground resource without resetting the background resource.
-}
setOnlyResource : RemoteDataResource error res -> RemoteResource error res -> RemoteResource error res
setOnlyResource res (RemoteResource rr) =
    RemoteResource { rr | res = res }


{-| If the foreground resource is in `Loading` state, this function will update the foreground resource.
Otherwise, this will update the background resource without changing the foreground resource.

That means that you can use this function both when this is the first time you retrieve your resources (in this case you want to set the foreground resource),
and when you want to refresh your resources and you already have foreground resources (in this case, maybe you want to store your background resources and tell your
users there are new resources awailable before replacing the foreground resources))

-}
setBackground : RemoteDataResource error res -> RemoteResource error res -> RemoteResource error res
setBackground background (RemoteResource rr) =
    if rr.res == RemoteData.Loading then
        RemoteResource { rr | res = background, background = RemoteData.NotAsked }

    else
        RemoteResource { rr | background = background }


{-| Directly update the background resource without changing the foreground resource
-}
setOnlyBackground : RemoteDataResource error res -> RemoteResource error res -> RemoteResource error res
setOnlyBackground background (RemoteResource rr) =
    RemoteResource { rr | background = background }


{-| Map your foreground resource (`RemoteData.map` is applied to the foreground resource)
-}
map : (res -> res) -> RemoteResource error res -> RemoteResource error res
map mapper (RemoteResource rr) =
    RemoteResource { rr | res = RemoteData.map mapper rr.res }


{-| Map your background resource (`RemoteData.map` is applied to the background resource)
-}
mapBackground : (res -> res) -> RemoteResource error res -> RemoteResource error res
mapBackground mapper (RemoteResource rr) =
    RemoteResource { rr | background = RemoteData.map mapper rr.background }


{-| Compare the foreground and the background resources to check if there are different (In this case you could tell your users new resources have arrived).

  - If background resource is not in `Success` state this will return false.
  - If background resource is in `Success` state but foreground resource is not in `Success` state this will return true
  - If both foreground and background resources are in `Success` state the compare function you provide will be used to determine if the resources are different

-}
hasNewResource : (res -> res -> Bool) -> RemoteResource error res -> Bool
hasNewResource compareFunc (RemoteResource { res, background }) =
    background
        |> RemoteData.map
            (\back ->
                res
                    |> RemoteData.map (compareFunc back)
                    |> RemoteData.withDefault True
            )
        |> RemoteData.withDefault False


{-| If the current background resource is in `Success` state, then we replace the foreground resource by the background resource and
we set the background resource to `NotAsked`
-}
replaceResourceByBackground : RemoteResource error res -> RemoteResource error res
replaceResourceByBackground (RemoteResource { res, background }) =
    RemoteResource
        { res =
            case background of
                RemoteData.Success _ ->
                    background

                _ ->
                    res
        , background = RemoteData.NotAsked
        }
