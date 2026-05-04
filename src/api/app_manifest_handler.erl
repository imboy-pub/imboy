-module(app_manifest_handler).

-behavior(cowboy_rest).

-export([init/2]).
-export([build_manifest/0, compute_etag/1]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        manifest -> manifest(Method, Req0);
        _ -> Req0
    end,
    {ok, Req1, State}.

-spec manifest(binary(), cowboy_req:req()) -> cowboy_req:req().
manifest(<<"GET">>, Req0) ->
    Manifest = build_manifest(),
    Etag = compute_etag(Manifest),
    case cowboy_req:header(<<"if-none-match">>, Req0) of
        Etag when Etag =/= undefined ->
            cowboy_req:reply(304, #{<<"etag">> => Etag}, <<>>, Req0);
        _ ->
            JsonBody = jsone:encode(Manifest, [native_utf8]),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json; charset=utf-8">>,
                <<"etag">> => Etag,
                <<"cache-control">> => <<"max-age=30">>
            }, JsonBody, Req0)
    end;
manifest(_, Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec build_manifest() -> map().
build_manifest() ->
    Features = imboy_feature:all(),
    Policy = imboy_policy:effective_view(),
    AppEntries = imboy_plugin_registry:enabled_app_entries(Features),
    AdminEntries = imboy_plugin_registry:enabled_admin_entries(Features),
    PluginList = build_plugin_list(Features),
    #{
        <<"features">> => Features,
        <<"policy">> => Policy,
        <<"app_entries">> => [atom_to_binary(E, utf8) || E <- AppEntries],
        <<"admin_entries">> => [atom_to_binary(E, utf8) || E <- AdminEntries],
        <<"plugins">> => PluginList,
        <<"generated_at">> => erlang:system_time(millisecond)
    }.

-spec build_plugin_list(map()) -> [map()].
build_plugin_list(EnabledFeatures) ->
    Names = imboy_plugin_registry:plugin_names(),
    lists:filtermap(fun(Name) ->
        MF = imboy_plugin_registry:manifest(Name),
        FeatureKeys = maps:get(feature_keys, MF, []),
        HasEnabled = lists:any(fun(K) ->
            maps:get(K, EnabledFeatures, false) =:= true
        end, FeatureKeys),
        case HasEnabled of
            false -> false;
            true ->
                AppE = [atom_to_binary(E, utf8) || E <- maps:get(app_entries, MF, [])],
                AdminE = [atom_to_binary(E, utf8) || E <- maps:get(admin_entries, MF, [])],
                {true, #{
                    <<"name">> => atom_to_binary(Name, utf8),
                    <<"app_entries">> => AppE,
                    <<"admin_entries">> => AdminE
                }}
        end
    end, Names).

-spec compute_etag(map()) -> binary().
compute_etag(Manifest) ->
    StableContent = maps:without([<<"generated_at">>], Manifest),
    Bin = term_to_binary(StableContent),
    <<Hash:128>> = erlang:md5(Bin),
    list_to_binary(io_lib:format("\"~32.16.0b\"", [Hash])).
