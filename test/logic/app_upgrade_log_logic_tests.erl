-module(app_upgrade_log_logic_tests).

%%%===================================================================
%%% @doc F16：入库前词表校验。
%%%
%%% 词表必须覆盖客户端实际上报的全部漏斗事件，upgrade_type 必须允许空串
%%% （除 check/prompted 外的事件不携带该字段，列 DEFAULT ''）。这份词表与
%%% priv/migrations/00000207 的 CHECK 一一对应——任何一侧改动都要同步。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

client_funnel_events_accepted_test() ->
    Events = [
        <<"check">>,
        <<"prompted">>,
        <<"download_start">>,
        <<"download_done">>,
        <<"verify_ok">>,
        <<"verify_fail">>,
        <<"verify_exception">>,
        <<"install">>,
        <<"cancel">>,
        <<"error">>,
        <<"downgrade">>
    ],
    lists:foreach(
        fun(Event) ->
            ?assertEqual(ok, app_upgrade_log_logic:validate(data(Event, <<>>)))
        end,
        Events
    ).

upgrade_types_accepted_including_empty_test() ->
    lists:foreach(
        fun(Type) ->
            ?assertEqual(ok, app_upgrade_log_logic:validate(data(<<"prompted">>, Type)))
        end,
        [<<"force">>, <<"recommend">>, <<"silent">>, <<"none">>, <<>>]
    ).

unknown_values_rejected_with_field_name_test() ->
    ?assertEqual(
        {error, <<"invalid event">>},
        app_upgrade_log_logic:validate(data(<<"rebooted">>, <<>>))
    ),
    ?assertEqual(
        {error, <<"invalid upgrade_type">>},
        app_upgrade_log_logic:validate(data(<<"check">>, <<"optional">>))
    ),
    ?assertEqual(
        {error, <<"invalid cos">>},
        app_upgrade_log_logic:validate((data(<<"check">>, <<>>))#{<<"cos">> => <<"windows">>})
    ),
    %% 缺字段按空串处理：event 为空本来就该在 handler 层被挡下，这里兜底
    ?assertEqual({error, <<"invalid event">>}, app_upgrade_log_logic:validate(#{})).

%% 非 binary 取值（客户端传 null）不能让校验崩溃，必须作为非法值拒绝。
non_binary_values_rejected_test() ->
    ?assertEqual(
        {error, <<"invalid upgrade_type">>},
        app_upgrade_log_logic:validate(data(<<"check">>, null))
    ).

data(Event, Type) ->
    #{<<"event">> => Event, <<"upgrade_type">> => Type, <<"cos">> => <<"android">>}.
