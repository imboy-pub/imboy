-module(msg_history_context_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TARGET, <<"w11_target">>).
-define(KEY, <<"c2c:100:200">>).

handler_requires_authentication_test() ->
    with_handler(fun() ->
        ?assertMatch({error, _, 401}, msg_handler:history(request(<<>>), #{})),
        ?assertEqual([], meck:history(messaging_logic))
    end).

handler_target_dispatch_ignores_incremental_cursor_test() ->
    with_handler(fun() ->
        Payload = context_payload([]),
        meck:expect(
            messaging_logic,
            history_context,
            fun(100, <<"c2c">>, <<"200">>, ?TARGET, 50, <<"device">>) -> {ok, Payload} end
        ),
        ?assertEqual(
            {success, Payload},
            handle(
                <<"chat_type=c2c&peer_id=200&target_msg_id=w11_target&did=device&after_seq=invalid">>
            )
        )
    end).

handler_duplicate_target_test() ->
    with_handler(fun() ->
        ?assertMatch(
            {error, _, 400},
            handle(
                <<"target_msg_id=w11_target&target_msg_id=w11_other">>
            )
        ),
        ?assertEqual([], meck:history(messaging_logic))
    end).

handler_empty_or_bare_target_test_() ->
    [
        {binary_to_list(Query), fun() ->
            with_handler(fun() ->
                ?assertMatch({error, _, 400}, handle(Query))
            end)
        end}
     || Query <- [<<"target_msg_id=">>, <<"target_msg_id">>]
    ].

handler_invalid_limit_test_() ->
    [
        {binary_to_list(Query), fun() ->
            with_handler(fun() ->
                ?assertMatch(
                    {error, _, 400}, handle(<<"target_msg_id=w11_target&", Query/binary>>)
                ),
                ?assertEqual([], meck:history(messaging_logic))
            end)
        end}
     || Query <- [
            <<"limit">>,
            <<"limit=">>,
            <<"limit=x">>,
            <<"limit=1.5">>,
            <<"limit=1tail">>,
            <<"limit=1&limit=2">>
        ]
    ].

handler_propagates_context_errors_test_() ->
    [
        {integer_to_list(Code), fun() ->
            with_handler(fun() ->
                Error = {error, <<"synthetic_failure">>, Code},
                meck:expect(
                    messaging_logic,
                    history_context,
                    fun(_, _, _, _, _, _) -> Error end
                ),
                ?assertEqual(Error, handle(<<"target_msg_id=w11_target">>))
            end)
        end}
     || Code <- [400, 401, 403, 404, 500]
    ].

handler_legacy_request_preserves_arguments_and_payload_test() ->
    with_handler(fun() ->
        Payload = #{
            <<"messages">> => [],
            <<"next_seq">> => 42,
            <<"safe_head">> => 42,
            <<"blocked">> => false,
            <<"gaps">> => [],
            <<"has_more">> => false,
            <<"conv_key">> => ?KEY
        },
        meck:expect(
            messaging_logic,
            history,
            fun(100, <<"c2c">>, <<"200">>, 41, 100, <<"device">>) -> {ok, Payload} end
        ),
        ?assertEqual(
            {success, Payload},
            handle(
                <<"chat_type=c2c&peer_id=200&after_seq=41&limit=999&did=device">>
            )
        ),
        ?assertNot(meck:called(messaging_logic, history_context, '_'))
    end).

invalid_target_test_() ->
    [
        {lists:flatten(io_lib:format("~p", [Target])), fun() ->
            ?assertMatch(
                {error, _, 400},
                messaging_logic:history_context(
                    100, <<"c2c">>, <<"200">>, Target, 50, <<>>
                )
            )
        end}
     || Target <- [
            <<>>,
            true,
            123,
            <<"with space">>,
            <<"../other">>,
            <<"非Xid"/utf8>>,
            binary:copy(<<"x">>, 51)
        ]
    ].

invalid_scope_test_() ->
    [
        {lists:flatten(io_lib:format("~p/~p", [ChatType, Peer])), fun() ->
            ?assertMatch(
                {error, _, 400},
                messaging_logic:history_context(
                    100, ChatType, Peer, ?TARGET, 50, <<>>
                )
            )
        end}
     || {ChatType, Peer} <- [
            {<<>>, <<"200">>},
            {<<"s2c">>, <<"200">>},
            {<<"c2c">>, <<>>},
            {<<"c2c">>, <<"1.5">>},
            {<<"c2c">>, <<"9223372036854775808">>},
            {<<"c2c">>, true}
        ]
    ].

bare_device_parameter_test() ->
    ?assertMatch(
        {error, _, 400},
        messaging_logic:history_context(
            100, <<"c2c">>, <<"200">>, ?TARGET, 50, true
        )
    ).

context_limit_clamping_test_() ->
    [
        {integer_to_list(Limit), fun() ->
            with_logic({ok, [row(?TARGET, 7)]}, fun() ->
                ?assertMatch({ok, _}, context(Limit, <<>>)),
                ?assert(
                    meck:called(
                        msg_archive_ds,
                        history_context,
                        [?KEY, ?TARGET, Expected, <<"c2c">>, 100, 100, 0]
                    )
                )
            end)
        end}
     || {Limit, Expected} <- [{-9, 1}, {0, 1}, {1, 1}, {1000, 100}]
    ].

context_encoding_and_no_sync_watermarks_test() ->
    with_logic({ok, [row(?TARGET, 7)]}, fun() ->
        {ok, Payload} = context(50, <<>>),
        ?assertEqual(
            lists:sort([<<"conv_key">>, <<"context_msg_id">>, <<"messages">>]),
            lists:sort(maps:keys(Payload))
        ),
        ?assertEqual(?TARGET, maps:get(<<"context_msg_id">>, Payload)),
        [Message] = maps:get(<<"messages">>, Payload),
        ?assertEqual(<<"200">>, maps:get(<<"from">>, Message)),
        ?assertEqual(<<"100">>, maps:get(<<"to">>, Message)),
        ?assertEqual(#{<<"ciphertext">> => <<"synthetic-only">>}, maps:get(<<"payload">>, Message)),
        ?assertNot(maps:is_key(<<"from_id">>, Message)),
        ?assertNot(meck:called(msg_archive_ds, safe_history, '_'))
    end).

missing_target_does_not_return_neighbors_test() ->
    with_logic({ok, [row(<<"w11_neighbor">>, 8)]}, fun() ->
        ?assertEqual({error, <<"history_context_unavailable">>, 404}, context(50, <<>>))
    end).

undeliverable_target_does_not_return_neighbors_test() ->
    with_logic(
        {ok, [device_row(?TARGET, 7, <<"other-device">>), row(<<"w11_neighbor">>, 8)]},
        fun() ->
            ?assertEqual({error, <<"history_context_unavailable">>, 404}, context(50, <<"device">>))
        end
    ).

context_reuses_device_filter_test() ->
    with_logic(
        {ok, [
            device_row(<<"w11_before">>, 6, <<"other-device">>),
            device_row(?TARGET, 7, <<"device">>)
        ]},
        fun() ->
            {ok, #{<<"messages">> := [Message]}} = context(50, <<"device">>),
            ?assertEqual(?TARGET, maps:get(<<"msg_id">>, Message))
        end
    ).

group_scope_denial_prevents_archive_read_test() ->
    with_logic({ok, []}, fun() ->
        meck:expect(group_ds, authorize_group_history, fun(100, 300) -> {error, denied} end),
        ?assertMatch({error, _, 403}, group_context()),
        ?assertNot(meck:called(msg_archive_ds, history_context, '_'))
    end).

group_context_passes_generation_floor_test() ->
    with_logic({ok, [row(?TARGET, 7)]}, fun() ->
        ?assertMatch({ok, _}, group_context()),
        ?assert(
            meck:called(
                msg_archive_ds,
                history_context,
                [<<"c2g:300">>, ?TARGET, 50, <<"c2g">>, 100, 300, 5]
            )
        )
    end).

context_storage_failure_test_() ->
    [
        {atom_to_list(Reason), fun() ->
            with_logic({error, Reason}, fun() ->
                ?assertMatch({error, _, Code}, context(50, <<>>))
            end)
        end}
     || {Reason, Code} <- [
            {permission_denied, 403},
            {synthetic_db_error, 500},
            {synthetic_transaction_error, 500}
        ]
    ].

disabled_archive_is_not_successful_empty_test() ->
    with_logic({ok, [row(?TARGET, 7)]}, fun() ->
        meck:expect(config_ds, env, fun(msg_archive_enabled, false) -> false end),
        ?assertMatch({error, _, 500}, context(50, <<>>)),
        ?assertNot(meck:called(msg_archive_ds, history_context, '_'))
    end).

legacy_history_keeps_safe_head_and_cursor_semantics_test() ->
    with_logic({error, unexpected_context_read}, fun() ->
        meck:expect(
            msg_archive_ds,
            safe_history,
            fun(?KEY, 0, 3, <<"c2c">>, 100, 200) ->
                {ok, [row(<<"w11_first">>, 1), row(<<"w11_second">>, 2), row(?TARGET, 7)], 1, []}
            end
        ),
        {ok, Payload} = messaging_logic:history(100, <<"c2c">>, <<"200">>, 0, 2, <<>>),
        ?assertEqual(1, maps:get(<<"next_seq">>, Payload)),
        ?assertEqual(1, maps:get(<<"safe_head">>, Payload)),
        ?assertEqual(true, maps:get(<<"blocked">>, Payload)),
        ?assertEqual(true, maps:get(<<"has_more">>, Payload)),
        ?assertNot(maps:is_key(<<"context_msg_id">>, Payload)),
        ?assertNot(meck:called(msg_archive_ds, history_context, '_'))
    end).

repo_bounded_single_statement_test_() ->
    [
        {integer_to_list(Limit), fun() ->
            with_repo(
                fun(_Conn, Sql, Params) ->
                    ?assertEqual([?KEY, ?TARGET, 0, Before, After], Params),
                    lists:foreach(
                        fun(Fragment) ->
                            ?assertNotEqual(nomatch, binary:match(Sql, Fragment))
                        end,
                        [
                            <<"target AS MATERIALIZED">>,
                            <<"msg_id=$2">>,
                            <<"conv_key=$1">>,
                            <<"conv_seq >= $3">>,
                            <<"LIMIT 1">>,
                            <<"DESC LIMIT $4">>,
                            <<"ASC LIMIT $5">>,
                            <<"expire_at > statement_timestamp()">>,
                            <<"FROM context WHERE">>
                        ]
                    ),
                    {ok, [row(?TARGET, 7)]}
                end,
                fun() ->
                    ?assertMatch({ok, [_]}, repo_context(<<"c2c">>, Limit, 0)),
                    ?assertEqual(1, meck:num_calls(elib_pg, query, '_'))
                end
            )
        end}
     || {Limit, Before, After} <- [{1, 0, 0}, {50, 24, 25}, {1000, 49, 50}]
    ].

repo_rechecks_current_generation_and_uses_fresh_floor_test() ->
    with_repo(
        fun(_Conn, Sql, Params) ->
            case Params of
                [300, 100] ->
                    lists:foreach(
                        fun(Fragment) ->
                            ?assertNotEqual(nomatch, binary:match(Sql, Fragment))
                        end,
                        [
                            <<"gm.status=1">>,
                            <<"grp.status=1">>,
                            <<"gmg.end_seq IS NULL">>,
                            <<"FOR SHARE OF gmg, gm, grp">>
                        ]
                    ),
                    {ok, [generation(2, 5)]};
                [<<"c2g:300">>, ?TARGET, 5, 24, 25] ->
                    {ok, [row(?TARGET, 7)]}
            end
        end,
        fun() ->
            ?assertMatch({ok, [_]}, repo_context(<<"c2g">>, 50, 1)),
            ?assertEqual(2, meck:num_calls(elib_pg, query, [context_connection, '_', [300, 100]]))
        end
    ).

%% 初始拒绝不读内容：用"内容行查询"子句内的进程字典计数器代替全局
%% meck:num_calls 计数。meck v1.2.0 的 history 为跨进程全局记录且在
%% fake 返回后才入史，本套件运行期间遗留的后台杂散进程对 elib_pg 的
%% 调用会污染 '_' 计数（opt-run8 实测 expected=1 value=5：本进程仅
%% 1 次 access，其余为杂散调用）；进程字典按进程隔离，天然免疫。
repo_initial_denial_never_reads_content_test() ->
    with_repo(
        fun
            (_Conn, _Sql, [300, 100]) ->
                {ok, []};
            (_Conn, _Sql, [<<"c2g:300">>, ?TARGET, _Floor, _Before, _After]) ->
                %% 只有"读内容"的 rows 查询计数；access/re-check 的 [Gid,Uid]
                %% 形态走上一子句，不计数。
                put(content_reads, pdict(content_reads) + 1),
                {ok, []}
        end,
        fun() ->
            put(content_reads, 0),
            ?assertEqual({error, permission_denied}, repo_context(<<"c2g">>, 50, 1)),
            %% 预期常数静态推导 = 0：产品 tx 内序为 access → rows → re-check
            %% （src/repo/msg_archive_repo.erl:61-69）；c2g 首查 access 空集即
            %% throw({rollback, permission_denied})（msg_archive_repo.erl:94-98），
            %% rows 永不执行。候选值 1 仅在"拒绝仍读内容"的回归形态下出现，
            %% 不取；与原断言 num_calls==1（无污染时 = 仅 access、rows 为 0）
            %% 的语义等价且更精确。
            ?assertEqual(0, pdict(content_reads)),
            ?assertNot(
                content_read([Entry || {Pid, _, _} = Entry <- meck:history(elib_pg), Pid =:= self()])
            )
        end
    ).

content_read_detection_test() ->
    Sql = <<"SELECT * FROM public.msg_store">>,
    ?assert(content_read([{self(), {elib_pg, query, [other_connection, Sql, []]}, ok}])),
    ?assert(content_read([{self(), {elib_pg, query, [Sql, []]}, ok}])),
    ?assertNot(
        content_read([{self(), {elib_pg, query, [other_connection, <<"SELECT 1">>, []]}, ok}])
    ).

content_read(History) ->
    lists:any(
        fun
            ({_, {elib_pg, query, [_, Sql, _]}, _}) when is_binary(Sql) ->
                binary:match(Sql, <<"msg_store">>) =/= nomatch;
            ({_, {elib_pg, query, [Sql, _]}, _}) when is_binary(Sql) ->
                binary:match(Sql, <<"msg_store">>) =/= nomatch;
            (_) ->
                false
        end,
        History
    ).

%% re-check 授权失败丢弃内容：access/re-check 的区分用本测试进程的字典序数
%% （fake 与断言同进程），不依赖全局 meck:num_calls——后者跨进程计数且杂散
%% 后台调用会污染（opt-run8 rejoined 例首查即误入 Final 分支 → Floor=5 →
%% rows 参数偏离预置子句 → case_clause）。首查固定返回旧 generation(1,1)，
%% re-check 返回 Final；两代际不同即触发产品的 permission_denied，与例意
%% "rejoined 后旧授权读到的内容被丢弃"一致。revoked / db-error 两例的
%% Final 子句行为不变（re-check 时才消费 Final，最终错误值不变）。
repo_final_authorization_failure_discards_content_test_() ->
    [
        {Name, fun() ->
            with_repo(
                fun
                    (_Conn, _Sql, [300, 100]) ->
                        N = pdict(c2g_access_reads),
                        put(c2g_access_reads, N + 1),
                        case N of
                            0 -> {ok, [generation(1, 1)]};
                            _ -> Final
                        end;
                    (_Conn, _Sql, [<<"c2g:300">>, ?TARGET, 1, 24, 25]) ->
                        {ok, [row(?TARGET, 7)]};
                    (_Conn, _Sql, _Other) ->
                        %% 非预置参数形态返回空内容集：tx 继续到 re-check，
                        %% 由授权比对给出 permission_denied（授权失败丢弃内容），
                        %% 而不是以 case_clause 中断。
                        {ok, []}
                end,
                fun() ->
                    put(c2g_access_reads, 0),
                    ?assertEqual({error, Expected}, repo_context(<<"c2g">>, 50, 1))
                end
            )
        end}
     || {Name, Final, Expected} <- [
            {"revoked", {ok, []}, permission_denied},
            {"rejoined", {ok, [generation(2, 5)]}, permission_denied},
            {"authorization_query_failed", {error, synthetic_db_error}, synthetic_db_error}
        ]
    ].

repo_transaction_failure_propagates_test_() ->
    [
        {Name, fun() ->
            with_repo(fun(_, _, _) -> {error, synthetic_db_error} end, fun() ->
                case Injected of
                    query -> ok;
                    _ -> meck:expect(elib_pg, with_tx, fun(_, _) -> Injected end)
                end,
                ?assertEqual({error, Expected}, repo_context(<<"c2c">>, 50, 0))
            end)
        end}
     || {Name, Injected, Expected} <- [
            {"query_failed", query, synthetic_db_error},
            {"rollback", {rollback, synthetic_rollback}, synthetic_rollback},
            {"pool_failed", {error, synthetic_pool_error}, synthetic_pool_error}
        ]
    ].

ds_forwards_context_result_test() ->
    mocked([msg_archive_repo], fun() ->
        meck:expect(
            msg_archive_repo,
            history_context,
            fun(?KEY, ?TARGET, 50, <<"c2c">>, 100, 100, 0) -> {error, synthetic_db_error} end
        ),
        ?assertEqual(
            {error, synthetic_db_error},
            msg_archive_ds:history_context(
                ?KEY, ?TARGET, 50, <<"c2c">>, 100, 100, 0
            )
        )
    end).

request(Query) -> #{method => <<"GET">>, qs => Query}.
handle(Query) -> msg_handler:history(request(Query), #{current_uid => 100}).
context(Limit, DID) ->
    messaging_logic:history_context(100, <<"c2c">>, <<"200">>, ?TARGET, Limit, DID).
group_context() -> messaging_logic:history_context(100, <<"c2g">>, <<"300">>, ?TARGET, 50, <<>>).

repo_context(<<"c2c">>, Limit, MinSeq) ->
    msg_archive_repo:history_context(?KEY, ?TARGET, Limit, <<"c2c">>, 100, 100, MinSeq);
repo_context(<<"c2g">>, Limit, MinSeq) ->
    msg_archive_repo:history_context(<<"c2g:300">>, ?TARGET, Limit, <<"c2g">>, 100, 300, MinSeq).

context_payload(Messages) ->
    #{<<"conv_key">> => ?KEY, <<"context_msg_id">> => ?TARGET, <<"messages">> => Messages}.
%% 进程字典带默认值读取（stdlib 无 get/2）：stub 可能被杂散进程在 put
%% 初始化之前触达，未初始化时按 0 计。
pdict(Key) ->
    case get(Key) of
        undefined -> 0;
        V -> V
    end.

generation(Generation, StartSeq) ->
    #{<<"generation_no">> => Generation, <<"start_seq">> => StartSeq}.

row(MsgId, Seq) ->
    #{
        <<"msg_id">> => MsgId,
        <<"chat_type">> => <<"c2c">>,
        <<"conv_seq">> => Seq,
        <<"msg_type">> => <<"e2ee">>,
        <<"from_id">> => 200,
        <<"to_id">> => 100,
        <<"e2ee">> => null,
        <<"payload">> => <<"{\"ciphertext\":\"synthetic-only\"}">>
    }.

device_row(MsgId, Seq, Device) ->
    (row(MsgId, Seq))#{
        <<"e2ee">> => jsone:encode(#{
            <<"fan_out">> => <<"per_device">>,
            <<"devices">> => #{Device => #{<<"ciphertext">> => <<"synthetic-only">>}}
        })
    }.

with_handler(Check) ->
    CachedQuery = erase({elib_param, qs_vals}),
    try
        mocked([messaging_logic, elib_response], fun() ->
            meck:expect(elib_response, success_rfc3339, fun(_Req, Payload) -> {success, Payload} end),
            meck:expect(elib_response, error, fun(_Req, Reason, Code) -> {error, Reason, Code} end),
            Check()
        end)
    after
        case CachedQuery of
            undefined -> erase({elib_param, qs_vals});
            _ -> put({elib_param, qs_vals}, CachedQuery)
        end
    end.

with_logic(Result, Check) ->
    mocked([config_ds, msg_archive_ds, group_ds], fun() ->
        meck:expect(config_ds, env, fun(msg_archive_enabled, false) -> true end),
        meck:expect(msg_archive_ds, history_context, fun(_, _, _, _, _, _, _) -> Result end),
        meck:expect(
            group_ds,
            authorize_group_history,
            fun(100, 300) -> {ok, #{generation_no => 1, start_seq => 5}} end
        ),
        Check()
    end).

with_repo(Query, Check) ->
    mocked([elib_pg], fun() ->
        meck:expect(elib_pg, with_tx, fun(Callback, Options) ->
            ?assertEqual(false, proplists:get_value(reraise, Options)),
            ?assertEqual(
                <<"ISOLATION LEVEL READ COMMITTED">>, proplists:get_value(begin_opts, Options)
            ),
            try
                Callback(context_connection)
            catch
                _:Reason -> {rollback, Reason}
            end
        end),
        meck:expect(elib_pg, query, Query),
        Check()
    end).

mocked(Modules, Check) ->
    lists:foreach(fun(Module) -> meck:new(Module, [passthrough, no_link]) end, Modules),
    try
        Check()
    after
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.
