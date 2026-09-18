%% moya_unread_since_cast_tests
%% D02-LOW（A1-D02）— history unread-count 的 since 裸 ::timestamptz cast 退化。
%%
%% 缺陷链（真库实测口径见 moya_submission_repo:queue_cond_sql/2 注释）：
%%   handler ?TIME_PARAM_RE 白名单放行无时区形态（2026-09-15 / THH:MM 等），
%%   而 repo 的 since 用裸 `$2::timestamptz` —— epgsql_codec_rfc3339_bin 对
%%   无时区串 encode 失败退化为 <<0:64>>（PG 纪元 2000-01-01）→
%%   `published_at > 2000-01-01` 恒真 → 家长未读角标恒计全部。
%%
%% 修复口径：与 queue_cond_sql/2 同款 `($2::text)::timestamptz` 双转换
%% （先钉 text 绕开 codec，PG 按 psql 字面量语义解析，无时区按会话时区）。
%%
%% 覆盖（SQL 形态断言，meck elib_pg:query 捕获；语义正确性由真库
%% 集成测试 Wave3 验证）：
%%   无 since    —— SQL 不含 published_at 边界、参数只 learner
%%   带时区     —— 含双转换边界（裸 cast 回归守卫）
%%   不带时区   —— 同一双转换边界（缺陷路径：裸 cast 即退化纪元）
%% （非法 since → handler 422 的用例已在 moya_assignment_handler_tests
%%   unread_since_invalid_422_test_ 覆盖，此处不重复。）

-module(moya_unread_since_cast_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(LEARNER_ID, 974001).

%%%===================================================================
%%% 三种 since 形态的 SQL 边界形态
%%%===================================================================

%% 无 since：不拼 published_at 边界，参数只 learner_id
unread_without_since_no_boundary_test_() ->
    with_captured_query(
        fun() ->
            moya_submission_repo:history_unread_count(?LEARNER_ID, undefined)
        end,
        fun(Sql, Args) ->
            ?assertEqual(nomatch, binary:match(Sql, <<"published_at >">>)),
            ?assertEqual([?LEARNER_ID], Args)
        end
    ).

%% 带时区（服务端 codec 产出形态的回传）：双转换边界
unread_since_with_tz_double_cast_test_() ->
    with_captured_query(
        fun() ->
            moya_submission_repo:history_unread_count(
                ?LEARNER_ID, <<"2026-09-13T09:28:23.467976+08:00">>
            )
        end,
        fun(Sql, Args) ->
            ?assertMatch({_, _}, binary:match(Sql, <<"($2::text)::timestamptz">>)),
            %% 裸 cast（无 ::text 钉型）必须不存在——那是 rfc3339 codec 纪元退化入口
            ?assertEqual(nomatch, binary:match(Sql, <<"$2::timestamptz">>)),
            ?assertEqual([?LEARNER_ID, <<"2026-09-13T09:28:23.467976+08:00">>], Args)
        end
    ).

%% 不带时区（handler 白名单放行的客户端自拼形态）：缺陷路径，必须同样双转换
unread_since_no_tz_double_cast_test_() ->
    with_captured_query(
        fun() ->
            moya_submission_repo:history_unread_count(?LEARNER_ID, <<"2026-09-15">>)
        end,
        fun(Sql, _Args) ->
            ?assertMatch({_, _}, binary:match(Sql, <<"($2::text)::timestamptz">>)),
            ?assertEqual(nomatch, binary:match(Sql, <<"$2::timestamptz">>))
        end
    ).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% meck elib_pg:query 捕获 (Sql, Args)，测试体对形态断言。
with_captured_query(CallFun, AssertFun) ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, Args) ->
                    self() ! {captured_query, Sql, Args},
                    {ok, [#{<<"cnt">> => 0}]}
                end}
            ]}
        ],
        fun() ->
            _ = CallFun(),
            receive
                {captured_query, Sql, Args} ->
                    AssertFun(iolist_to_binary(Sql), Args)
            after 1000 ->
                ?assert(false, "elib_pg:query not called")
            end
        end
    ).
