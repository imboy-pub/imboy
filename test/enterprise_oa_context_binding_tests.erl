-module(enterprise_oa_context_binding_tests).
-include_lib("eunit/include/eunit.hrl").

%% 2026-10-12 定位修订（organization 是业务对象，非租户）：application_key
%% 实例内全局唯一、按 key 直取；org 成员收敛与 organization_id 请求字段
%% 校验已移除。本测试用 meck 钉住签发链的应用解析语义：
%%   - active 单行命中 → 直入 allowlist/签发（INSERT 落 code 行）
%%   - 多行（数据异常，违反全局唯一）→ 404 fail-closed
%%   - disabled → 404
%%   - organization_id 请求字段容忍并忽略：垃圾值不改变结果
context_binding_test() ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        Params = #{
            <<"application_key">> => <<"shared-oa-key">>,
            <<"redirect_uri">> => <<"https://oa.example.com/sso">>,
            <<"nonce">> => <<"nonce_0123456789abcdef">>
        },
        %% ① active 命中：application 查询返回行，INSERT 返回新行 → 签发成功
        mock_query([active_app()]),
        {ok, Issued} = issue(Params),
        ?assertMatch(#{<<"code">> := <<"oa_sso_", _/binary>>}, Issued),
        ?assertEqual(60, maps:get(<<"expires_in">>, Issued)),
        %% ② organization_id 垃圾值一律忽略（不再 400/403/404 分支）
        lists:foreach(
            fun(Value) ->
                {ok, Issued2} = issue(Params#{<<"organization_id">> => Value}),
                ?assertMatch(#{<<"code">> := _}, Issued2)
            end,
            [0, -1, 101, null, <<"101">>, 1.5, 9223372036854775808]
        ),
        %% ③ 多行（违反全局唯一的数据异常）→ 404 fail-closed
        mock_query([active_app(), active_app()]),
        ?assertMatch({error, {404, _}}, issue(Params)),
        %% ④ disabled → 404
        mock_query([disabled_app()]),
        ?assertMatch({error, {404, _}}, issue(Params)),
        %% 已认证正 Uid 仍须查询当前账号；停用/非 Human/缺失均无 INSERT。
        Inserts = code_insert_calls(),
        mock_query([active_app()], {ok, []}),
        ?assertMatch({error, {403, _}}, issue(Params)),
        ?assertEqual(Inserts, code_insert_calls()),
        %% 当前账号事实不可确认时 fail-closed，不返回 code 或新增映射/会话。
        mock_query([active_app()], {error, synthetic_account_db_error}),
        ?assertMatch({error, {500, _}}, issue(Params)),
        ?assertEqual(Inserts, code_insert_calls()),
        ?assert(meck:validate(elib_pg))
    after
        meck:unload(elib_pg)
    end.

issue(Params) -> enterprise_oa_sso_logic:issue_code_tx(conn, 301, Params).

mock_query(AppRows) ->
    mock_query(AppRows, {ok, [#{<<"active">> => 1}]}).

mock_query(AppRows, AccountResult) ->
    meck:expect(elib_pg, query, fun(conn, Sql, _Args) ->
        IsCodeInsert =
            binary:match(Sql, <<"INSERT INTO">>) =/= nomatch andalso
                binary:match(Sql, <<"enterprise_oa_sso_code">>) =/= nomatch,
        IsAccountCheck = binary:match(Sql, <<"SELECT 1 FROM \"user\"">>) =/= nomatch,
        case {IsCodeInsert, IsAccountCheck} of
            {true, false} ->
                {ok, [#{<<"id">> => 1}]};
            {false, true} ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"account_type = 0">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"status = 1">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"FOR SHARE OF u">>)),
                AccountResult;
            {false, false} ->
                {ok, AppRows}
        end
    end).

active_app() ->
    #{
        <<"organization_id">> => 101,
        <<"id">> => 201,
        <<"status">> => <<"active">>,
        <<"allowed_redirect_uris">> => [<<"https://oa.example.com/sso">>]
    }.

disabled_app() ->
    (active_app())#{<<"status">> => <<"disabled">>}.

code_insert_calls() ->
    length([
        ok
     || {_, {elib_pg, query, [conn, Sql, _]}, _} <- meck:history(elib_pg),
        binary:match(Sql, <<"INSERT INTO">>) =/= nomatch,
        binary:match(Sql, <<"enterprise_oa_sso_code">>) =/= nomatch
    ]).
