-module(feature_gate_middleware_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

base_route_passes_without_feature_test() ->
    Req = #{request => base},
    Env = #{handler_opts => #{action => show}},
    ?assertEqual({ok, Req, Env}, feature_gate_middleware:execute(Req, Env)).

enabled_feature_passes_test_() ->
    ?WITH_MECKS(
        [
            {imboy_feature, [
                {ensure_enabled, 2, fun(_Req, channel) -> ok end}
            ]}
        ],
        fun() ->
            Req = #{request => channel},
            Env = #{handler_opts => #{required_feature => channel}},
            ?assertEqual({ok, Req, Env}, feature_gate_middleware:execute(Req, Env))
        end
    ).

disabled_feature_stops_test_() ->
    ?WITH_MECKS(
        [
            {imboy_feature, [
                {ensure_enabled, 2, fun(_Req, channel) -> {error, blocked_req} end}
            ]}
        ],
        fun() ->
            Env = #{handler_opts => #{required_feature => channel}},
            ?assertEqual({stop, blocked_req}, feature_gate_middleware:execute(#{}, Env))
        end
    ).
