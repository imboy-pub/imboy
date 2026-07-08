%% @doc msg_read_repo:save_read/5 真库集成测试。
%%
%% 背景（真机实测 2026-07-07，commit 6c2d508c）：$1/$4 同时出现在
%% SELECT 列表（无类型上下文推断为 text）与 WHERE 比较（列类型 varchar），
%% PostgreSQL 报 42P08 ambiguous_parameter，导致 save_read 对真实数据库
%% 从未成功写入过一条记录——现有 test/repo/msg_read_repo_tests.erl 全部
%% 经 meck 拦截 elib_pg:query，只验证调用参数形状，从未真正让这条 SQL 打到
%% PostgreSQL，故该 bug 长期潜伏、真机验收才暴露。
%%
%% 这里补真库集成测试钉住修复：不仅验证不再报错，还验证 MSG-P2-6
%% 三列应用层去重（msg_id, to_uid, to_did）语义，以及批量已读回执场景。
-module(msg_read_repo_integration_tests).

-include_lib("eunit/include/eunit.hrl").

msg_read_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {foreach, fun setup/0, fun cleanup/1, [
                {"save_read 真库写入成功且可读回（回归 42P08）", fun test_save_read_and_read_back/0},
                {"save_read 三列去重：不同 read_at 仍视为重复", fun test_save_read_dedup_by_three_cols/0},
                {"save_read 批量已读回执（多条消息）", fun test_save_read_batch_messages/0}
            ]};
        {error, _Reason} ->
            {"Database not available", fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    FromUid = elib_tsid:generate(),
    ToUid = elib_tsid:generate(),
    Context = #{from_uid => FromUid, to_uid => ToUid, to_did => <<"integration_test_device">>},
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(Context) ->
    #{from_uid := FromUid, to_uid := ToUid} = Context,
    %% 清理本测试写入的 msg_read 行，避免污染共享库
    {ok, _} = elib_pg:execute(
        <<"DELETE FROM public.msg_read WHERE from_uid = $1 AND to_uid = $2">>, [FromUid, ToUid]
    ),
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_save_read_and_read_back() ->
    #{from_uid := FromUid, to_uid := ToUid, to_did := ToDid} = get_context(),
    MsgId = unique_msg_id(),
    ReadAt = elib_dt:now(),

    %% 不应再报 42P08 ambiguous_parameter
    ?assertEqual(ok, msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, ReadAt)),

    {ok, Rows} = msg_read_repo:get_read_status(MsgId, FromUid),
    ?assertEqual(1, length(Rows)),
    [Row] = Rows,
    ?assertEqual(ToUid, maps:get(<<"to_uid">>, Row)),
    ?assertEqual(ToDid, maps:get(<<"to_did">>, Row)).

test_save_read_dedup_by_three_cols() ->
    #{from_uid := FromUid, to_uid := ToUid, to_did := ToDid} = get_context(),
    MsgId = unique_msg_id(),

    %% 两次独立上报，read_at 故意不同——MSG-P2-6 要求仍按 (msg_id, to_uid,
    %% to_did) 三列去重，不能绕过产生重复已读行
    ?assertEqual(ok, msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, elib_dt:now())),
    timer:sleep(10),
    ?assertEqual(ok, msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, elib_dt:now())),

    {ok, Rows} = msg_read_repo:get_read_status(MsgId, FromUid),
    ?assertEqual(1, length(Rows)).

test_save_read_batch_messages() ->
    #{from_uid := FromUid, to_uid := ToUid, to_did := ToDid} = get_context(),
    MsgIds = [unique_msg_id() || _ <- lists:seq(1, 3)],
    ReadAt = elib_dt:now(),

    lists:foreach(
        fun(MsgId) ->
            ?assertEqual(ok, msg_read_repo:save_read(MsgId, FromUid, ToUid, ToDid, ReadAt))
        end,
        MsgIds
    ),

    lists:foreach(
        fun(MsgId) ->
            {ok, Rows} = msg_read_repo:get_read_status(MsgId, FromUid),
            ?assertEqual(1, length(Rows))
        end,
        MsgIds
    ).

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}, undefined).

unique_msg_id() ->
    Id = integer_to_binary(elib_tsid:generate()),
    <<"integration_test_msg_", Id/binary>>.
