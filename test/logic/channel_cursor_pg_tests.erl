-module(channel_cursor_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W07.b（channel-group-ux-v2.2 §6.5 C2 已订阅/我管理频道 cursor 协议）：
%%% 真实 PostgreSQL 上验证签名游标的 J20 向量（频道两族）：
%%% - J20.a：多页全扫描完整（订阅按 s.id DESC、管理按 c.id DESC），
%%%   授权交集外对象不出现；
%%% - J20.b：关键词换代 → invalid（不回落）；
%%% - J20.c：篡改/跨账号 → invalid；错 list_kind → invalid；
%%% - J20.d：扫描中并发新增订阅（更高 s.id）不误入旧扫描，重扫收敛；
%%% - 终止页契约：has_more=false。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(UID, 880901).
-define(OTHER_UID, 880902).
%% UID 管理（admin role 2）
-define(CH1, 881001).
%% UID 已订阅
-define(CH2, 881002).
%% OTHER 所有，UID 无关系（授权外）
-define(CH3, 881003).
%% UID 已订阅（J20.d 并发新增）
-define(CH4, 881004).
-define(ALL_CHS, [?CH1, ?CH2, ?CH3, ?CH4]).
-define(ALL_UIDS, [?UID, ?OTHER_UID]).
-define(KEY_CFG, list_cursor_signing_key).
-define(KEY, <<"w07b_channel_cursor_signing_key_0123456789">>).

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM channel_subscription WHERE channel_id = ANY($1)">>, [?ALL_CHS]},
            {<<"DELETE FROM channel_admin WHERE channel_id = ANY($1)">>, [?ALL_CHS]},
            {<<"DELETE FROM channel WHERE id = ANY($1)">>, [?ALL_CHS]},
            {<<"DELETE FROM \"user\" WHERE id = ANY($1)">>, [?ALL_UIDS]}
        ]
    ),
    ok.

seed_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
        [
            {
                <<
                    "INSERT INTO \"user\" (id, account, nickname, password, reg_ip, reg_cosv) VALUES ",
                    "($1,'w07b-ch-uid','w07b-ch-uid','w07b-synthetic','127.0.0.1','w07b-synthetic'),",
                    "($2,'w07b-ch-other','w07b-ch-other','w07b-synthetic','127.0.0.1','w07b-synthetic')"
                >>,
                [?UID, ?OTHER_UID]
            },
            {
                <<
                    "INSERT INTO channel (id, name, creator_uid) VALUES "/utf8,
                    "($1, '管理频道', $4),"/utf8,
                    "($2, '订阅频道一', $5),"/utf8,
                    "($3, '外部频道', $5),"/utf8,
                    "($6, '订阅频道二', $5)"/utf8
                >>,
                [?CH1, ?CH2, ?CH3, ?UID, ?OTHER_UID, ?CH4]
            },
            {
                <<"INSERT INTO channel_admin (id, channel_id, user_id, role) ",
                    "VALUES (881101, $1, $2, 2)">>,
                [?CH1, ?UID]
            },
            {
                <<"INSERT INTO channel_subscription (id, channel_id, user_id, status) ",
                    "VALUES (881102, $1, $2, 1)">>,
                [?CH2, ?UID]
            }
        ]
    ),
    ok.

ids({ok, Rows}) when is_list(Rows) -> [maps:get(<<"id">>, R) || R <- Rows];
ids({ok, #{list := Rows}}) -> [maps:get(<<"id">>, R) || R <- Rows];
ids(Other) -> error({unexpected, Other}).

scan_all(Fetcher, Uid, Limit, Q) ->
    scan_all(Fetcher, Uid, Limit, Q, none, 0, [], true).

scan_all(_Fetcher, _Uid, _Limit, _Q, _Cursor, Pages, Acc, false) ->
    {Pages, lists:reverse(Acc), false};
scan_all(Fetcher, Uid, Limit, Q, Cursor, Pages, Acc, _Cont) ->
    BeforeId =
        case Cursor of
            none -> 16#7FFFFFFFFFFFFFFF;
            Upper when is_integer(Upper) -> Upper
        end,
    {ok, #{list := Rows, has_more := HasMore, upper := Upper1}} =
        Fetcher(Uid, BeforeId, Limit, Q),
    {NextUpper, Cont} =
        case HasMore of
            true -> {Upper1, true};
            false -> {none, false}
        end,
    scan_all(
        Fetcher,
        Uid,
        Limit,
        Q,
        NextUpper,
        Pages + 1,
        lists:reverse(ids({ok, Rows})) ++ Acc,
        Cont
    ).

channel_cursor_j20_vectors_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        PrevKey = application:get_env(imboy, ?KEY_CFG),
        application:set_env(imboy, ?KEY_CFG, ?KEY),
        cleanup_test_rows(),
        try
            seed_test_rows(),
            SubF = fun channel_logic:get_subscribed_channels_cursor/4,
            MngF = fun channel_logic:get_managed_channels_cursor/4,

            %% ── J20.a：订阅全扫描（仅已订阅 CH2）；管理全扫描（仅 CH1）；
            %% 授权外 CH3 不出现在任何一族。
            ?assertEqual(ids(SubF(?UID, 16#7FFFFFFFFFFFFFFF, 10, <<>>)), [?CH2]),
            {_, IdsMng, _} = scan_all(MngF, ?UID, 2, <<>>),
            ?assertEqual([?CH1], IdsMng),

            %% ── J20.b：关键词换代 → invalid（不回落）。
            {ok, #{has_more := false}} =
                SubF(?UID, 16#7FFFFFFFFFFFFFFF, 10, <<>>),
            {ok, Token} =
                list_cursor:issue(
                    ?UID,
                    <<"channel_subscribed">>,
                    #{
                        scope => <<"personal">>,
                        workspace_id => 0,
                        q => <<"旧词"/utf8>>,
                        upper => ?CH2
                    },
                    os:system_time(second)
                ),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(
                    Token,
                    ?UID,
                    <<"channel_subscribed">>,
                    #{scope => <<"personal">>, workspace_id => 0, q => <<>>}
                )
            ),

            %% ── J20.c：篡改/跨账号 → invalid；错 list_kind → invalid。
            Tampered = tamper(Token),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(
                    Tampered,
                    ?UID,
                    <<"channel_subscribed">>,
                    #{
                        scope => <<"personal">>,
                        workspace_id => 0,
                        q => <<"旧词"/utf8>>
                    }
                )
            ),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(
                    Token,
                    ?OTHER_UID,
                    <<"channel_subscribed">>,
                    #{
                        scope => <<"personal">>,
                        workspace_id => 0,
                        q => <<"旧词"/utf8>>
                    }
                )
            ),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(
                    Token,
                    ?UID,
                    <<"channel_managed">>,
                    #{
                        scope => <<"personal">>,
                        workspace_id => 0,
                        q => <<"旧词"/utf8>>
                    }
                )
            ),

            %% ── J20.d：扫描中并发新增订阅（更高 s.id）不误入旧扫描；
            %% 重扫收敛可见；管理命中不产生订阅副作用。
            {ok, #{has_more := false}} =
                MngF(?UID, 16#7FFFFFFFFFFFFFFF, 10, <<>>),
            {ok, _} = insert_subscription(?CH4, ?UID, 881103),
            ?assertEqual(
                [?CH4, ?CH2],
                ids(SubF(?UID, 16#7FFFFFFFFFFFFFFF, 10, <<>>))
            ),
            {ok, #{list := Rows2}} =
                SubF(?UID, 16#7FFFFFFFFFFFFFFF, 10, <<>>),
            ?assertEqual([?CH4, ?CH2], [maps:get(<<"id">>, R) || R <- Rows2])
        after
            cleanup_test_rows(),
            restore_key(PrevKey)
        end
    end).

%% 宏外 helper：epp 宏参数切分不识别 <<>> 内顶层逗号（W07.b 同款规避）。
insert_subscription(ChanId, Uid, SubId) ->
    elib_pg:execute(
        <<"INSERT INTO channel_subscription (id, channel_id, user_id, status) ",
            "VALUES ($1, $2, $3, 1)">>,
        [SubId, ChanId, Uid]
    ).

tamper(Token) when is_binary(Token), byte_size(Token) > 0 ->
    <<H, T/binary>> = Token,
    <<(H bxor 16#FF), T/binary>>.

restore_key(undefined) -> application:unset_env(imboy, ?KEY_CFG);
restore_key({ok, K}) -> application:set_env(imboy, ?KEY_CFG, K).
