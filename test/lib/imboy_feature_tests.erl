-module(imboy_feature_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

defaults_to_enabled_when_features_missing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun(features, undefined) -> undefined end}
        ]}
    ], fun() ->
        ?assertEqual(true, imboy_feature:enabled(moment))
    end).

reads_disabled_switch_from_config_map_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun(features, undefined) ->
                #{moment => #{enabled => false}}
            end}
        ]}
    ], fun() ->
        ?assertEqual(false, imboy_feature:enabled(moment))
    end).

channel_subfeature_depends_on_channel_switch_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun(features, undefined) ->
                #{
                    channel => #{enabled => false},
                    channel_discover => #{enabled => true}
                }
            end}
        ]}
    ], fun() ->
        ?assertEqual(false, imboy_feature:enabled(channel_discover))
    end).

ensure_enabled_returns_uniform_feature_disabled_response_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun(features, undefined) ->
                #{moment => #{enabled => false}}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, Msg, Code) ->
                {error_resp, Msg, Code}
            end}
        ]}
    ], fun() ->
        Result = imboy_feature:ensure_enabled(#{}, moment),
        ?assertEqual({error, {error_resp, <<"功能未启用"/utf8>>, ?ERR_FEATURE_DISABLED}}, Result)
    end).


all_returns_canonical_feature_map_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun(features, undefined) ->
                #{
                    core => #{enabled => true},
                    e2ee => #{enabled => false},
                    channel => #{enabled => true},
                    location => #{enabled => false},
                    moment => #{enabled => true},
                    channel_discover => #{enabled => true},
                    channel_invitation => #{enabled => true},
                    channel_order => #{enabled => false},
                    group_vote => #{enabled => false},
                    group_schedule => #{enabled => true},
                    group_task => #{enabled => false}
                }
            end}
        ]}
    ], fun() ->
        Payload = imboy_feature:all(),
        ?assertEqual(true, maps:get(<<"core">>, Payload)),
        ?assertEqual(false, maps:get(<<"e2ee">>, Payload)),
        ?assertEqual(true, maps:get(<<"moment">>, Payload)),
        ?assertEqual(false, maps:get(<<"channel_order">>, Payload)),
        ?assertEqual(true, maps:get(<<"group_schedule">>, Payload))
    end).
