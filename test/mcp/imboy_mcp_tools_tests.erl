-module(imboy_mcp_tools_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc MCP tool wrapper 测试（Phase 3 T3.4）
%%% 首批 4 个只读 tool，全部经 mcp_handler:process/2 端到端驱动：
%%%   get_user_profile / get_contacts / search_messages / list_group_members
%%% 验收要点：
%%%   ① 越权判定一律以 Ctx.auth_info（调用者 uid）为准，不信 Args 自报 uid；
%%%   ② 非授权/未认证返回 isError=true 的 tool_error；
%%%   ③ 正常路径返回 structuredContent。
%%% 底层 logic/ds 用 meck mock，无需真实 DB。
%%%===================================================================

tools_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"get_user_profile: 查自己成功", fun t_profile_self/0},
        {"get_user_profile: 查好友成功", fun t_profile_friend/0},
        {"get_user_profile: 查陌生人被拒", fun t_profile_stranger/0},
        {"get_user_profile: 未认证被拒", fun t_profile_unauth/0},
        {"get_contacts: 强制用调用者uid(忽略Args自报)", fun t_contacts_forces_caller/0},
        {"search_messages: 强制调用者uid做权限过滤", fun t_search_forces_caller/0},
        {"search_messages: 空keyword被拒", fun t_search_empty_kw/0},
        {"list_group_members: 群成员成功", fun t_members_ok/0},
        {"list_group_members: 非成员被拒", fun t_members_denied/0}
    ]}.

setup() ->
    _ = start_srv(barrel_mcp_registry),
    _ = start_srv(barrel_mcp_session),
    ok = barrel_mcp_registry:wait_for_ready(),
    ok = imboy_mcp_tools:reg_all(),
    meck:new(user_logic, [no_link, passthrough]),
    meck:new(friend_ds, [no_link, passthrough]),
    meck:new(fts_logic, [no_link, passthrough]),
    meck:new(group_member_ds, [no_link, passthrough]),
    ok.

cleanup(_) ->
    meck:unload(),
    ok.

start_srv(Mod) ->
    case Mod:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

%%%===================================================================
%%% helper：经 process/2 端到端调用一个 tool，返回 result map
%%%===================================================================
call(Name, Args, AuthInfo) ->
    Req = iolist_to_binary(
        json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"tools/call">>,
            <<"params">> => #{<<"name">> => Name, <<"arguments">> => Args}
        })
    ),
    {200, Body} = mcp_handler:process(Req, AuthInfo),
    Resp = json:decode(Body),
    maps:get(<<"result">>, Resp).

is_error(Result) -> maps:get(<<"isError">>, Result, false).

%%%===================================================================
%%% get_user_profile
%%%===================================================================
t_profile_self() ->
    meck:expect(user_logic, find_by_id, fun(42) -> #{<<"id">> => 42, <<"nickname">> => <<"me">>} end),
    R = call(<<"get_user_profile">>, #{<<"uid">> => 42}, 42),
    ?assertNot(is_error(R)),
    ?assertMatch(#{<<"id">> := 42}, maps:get(<<"structuredContent">>, R)).

t_profile_friend() ->
    meck:expect(friend_ds, is_friend, fun(42, 7) -> true end),
    meck:expect(user_logic, find_by_id, fun(7) -> #{<<"id">> => 7, <<"nickname">> => <<"pal">>} end),
    R = call(<<"get_user_profile">>, #{<<"uid">> => 7}, 42),
    ?assertNot(is_error(R)),
    ?assertMatch(#{<<"id">> := 7}, maps:get(<<"structuredContent">>, R)).

t_profile_stranger() ->
    meck:expect(friend_ds, is_friend, fun(42, 999) -> false end),
    R = call(<<"get_user_profile">>, #{<<"uid">> => 999}, 42),
    ?assertEqual(true, is_error(R)).

t_profile_unauth() ->
    R = call(<<"get_user_profile">>, #{<<"uid">> => 7}, 0),
    ?assertEqual(true, is_error(R)).

%%%===================================================================
%%% get_contacts：不信 Args 自报 uid，一律查调用者好友
%%%===================================================================
t_contacts_forces_caller() ->
    meck:expect(friend_ds, page_by_uid, fun
        (42, _L, _O) -> [#{<<"to_user_id">> => 7}];
        (_, _, _) -> []
    end),
    %% Args 恶意自报 uid=999，调用者是 42 → 结果应来自 42 的好友
    R = call(<<"get_contacts">>, #{<<"uid">> => 999}, 42),
    ?assertNot(is_error(R)),
    #{<<"list">> := List} = maps:get(<<"structuredContent">>, R),
    ?assertEqual([#{<<"to_user_id">> => 7}], List).

%%%===================================================================
%%% search_messages：强制调用者 uid 做权限过滤
%%%===================================================================
t_search_forces_caller() ->
    meck:expect(fts_logic, search_msg, fun
        (42, _P, _S, <<"hi">>, _T) -> #{<<"total">> => 1, <<"list">> => [#{<<"id">> => 1}]};
        (_, _, _, _, _) -> #{<<"total">> => 0, <<"list">> => []}
    end),
    R = call(<<"search_messages">>, #{<<"keyword">> => <<"hi">>, <<"uid">> => 999}, 42),
    ?assertNot(is_error(R)),
    ?assertMatch(#{<<"total">> := 1}, maps:get(<<"structuredContent">>, R)).

t_search_empty_kw() ->
    R = call(<<"search_messages">>, #{<<"keyword">> => <<>>}, 42),
    ?assertEqual(true, is_error(R)).

%%%===================================================================
%%% list_group_members：仅群成员可列
%%%===================================================================
t_members_ok() ->
    meck:expect(group_member_ds, is_member, fun(100, 42) -> true end),
    meck:expect(group_member_ds, list_members, fun(100) -> {ok, [#{<<"user_id">> => 42}]} end),
    R = call(<<"list_group_members">>, #{<<"group_id">> => 100}, 42),
    ?assertNot(is_error(R)),
    #{<<"list">> := List} = maps:get(<<"structuredContent">>, R),
    ?assertEqual([#{<<"user_id">> => 42}], List).

t_members_denied() ->
    meck:expect(group_member_ds, is_member, fun(100, 42) -> false end),
    R = call(<<"list_group_members">>, #{<<"group_id">> => 100}, 42),
    ?assertEqual(true, is_error(R)).
