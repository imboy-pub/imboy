%%% @doc CS-ADM-02（CS-GOV-03B）平台运营面按需统计 handler 合同：真 cowboy
%%% 监听器 + 真 HTTP + meck facade（零 DB；与 cs_handler_tests 同款纪律）。
%%%
%%% 增量合同（租户面 CS-BE-07 十二例之外的**平台面**接线事实）：
%%%   * 平台 stats 复用租户面同一 session_stats facade 用例（A02：不复制
%%%     业务逻辑）；Org 来自 :org_id path（org_source=path）；
%%%   * workspace optional：缺失不 422（键不存在——org-wide 语义由
%%%     application 裁决）；给出则照常注入 workspace_id；
%%%   * date / tz_offset 显式透传（服务端事实窗口，客户端不重算）；
%%%   * customer_service:read 即满足只读 GET（read/write 权限分面）；
%%%   * facade {error, invalid_date} 经 cs_http 映射 422（非法窗口前置拒绝）。
-module(cs_platform_stats_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(ADM, 78).
-define(ORG, 7001044).
-define(WS, 90044).

platform_stats_test_() ->
    {foreach,
        fun() ->
            {ok, _} = application:ensure_all_started(cowboy),
            meck:new(customer_service_facade, [passthrough]),
            ok
        end,
        fun(_) ->
            meck:unload(customer_service_facade),
            cs_fake_facts:clear(),
            ok
        end,
        [fun platform_stats_tests/1]}.

platform_stats_tests(_) ->
    Inject = #{auth_facts => cs_fake_facts, adm_user_id => ?ADM},
    [
        {"platform stats hits tenant facade use case, workspace omitted is org-wide (200)", fun() ->
                cs_fake_facts:set(#{
                    adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
                }),
                Stats = stats_view(<<"2026-09-25">>, 480),
                meck:expect(customer_service_facade, session_stats, fun(Org, Params) ->
                    ?assertEqual(?ORG, Org),
                    ?assertEqual(<<"2026-09-25">>, maps:get(date, Params)),
                    ?assertEqual(480, maps:get(tz_offset, Params)),
                    ?assert(is_integer(maps:get(at, Params))),
                    %% workspace optional：缺失时键不存在（CSB-02R 同款门）。
                    ?assertNot(maps:is_key(workspace_id, Params)),
                    {ok, Stats}
                end),
                ?S:with_listener(platform, p_session_stats, Inject, fun(Port) ->
                    Path = ?S:path(platform, p_session_stats, #{org_id => ?ORG}),
                    Q = <<"?date=2026-09-25&tz_offset=480">>,
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        <<Path/binary, Q/binary>>,
                        <<>>,
                        #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    Payload = ?S:payload(Resp),
                    ?assertEqual(7, maps:get(<<"new_sessions">>, Payload)),
                    ?assertEqual(5, maps:get(<<"closed_sessions">>, Payload)),
                    Current = maps:get(<<"current">>, Payload),
                    ?assertEqual(4, maps:get(<<"queued">>, Current))
                end)
            end},

        {"platform stats with workspace_id injects the scope (200)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            meck:expect(customer_service_facade, session_stats, fun(_Org, Params) ->
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                {ok, stats_view(undefined, 0)}
            end),
            ?S:with_listener(platform, p_session_stats, Inject, fun(Port) ->
                Path = ?S:path(platform, p_session_stats, #{org_id => ?ORG}),
                Q = <<"?workspace_id=", (int_bin(?WS))/binary>>,
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<Path/binary, Q/binary>>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp))
            end)
        end},

        {"platform stats without customer_service:read is 403", fun() ->
            cs_fake_facts:set(#{adm_user_id => ?ADM, permissions => []}),
            ?S:with_listener(platform, p_session_stats, Inject, fun(Port) ->
                Path = ?S:path(platform, p_session_stats, #{org_id => ?ORG}),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    Path,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp))
            end)
        end},

        {"platform stats invalid date maps to 422", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            meck:expect(customer_service_facade, session_stats, fun(_Org, _Params) ->
                {error, {invalid_date, <<"2026-13-99">>}}
            end),
            ?S:with_listener(platform, p_session_stats, Inject, fun(Port) ->
                Path = ?S:path(platform, p_session_stats, #{org_id => ?ORG}),
                Q = <<"?date=2026-13-99">>,
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<Path/binary, Q/binary>>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(422, ?S:status(Resp))
            end)
        end}
    ].

%% 五指标出站视图（CS-GOV-03A 冻结键序的 meck 投影）。
stats_view(Date, Tz) ->
    #{
        organization_id => ?ORG,
        date => Date,
        tz_offset => Tz,
        window_start => 1790313600,
        window_end => 1790399999,
        new_sessions => 7,
        first_response => #{count => 6, avg_seconds => 42},
        closed_sessions => 5,
        rating => #{count => 4, avg => 4.5},
        current => #{queued => 4, active => 2}
    }.

int_bin(N) ->
    integer_to_binary(N).
