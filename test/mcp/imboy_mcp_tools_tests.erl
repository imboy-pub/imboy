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
        {"list_group_members: 非成员被拒", fun t_members_denied/0},
        {"list_conversations: 强制调用者uid", fun t_conversations_ok/0},
        {"list_conversations: 未认证被拒", fun t_conversations_unauth/0},
        {"create_group: 建群成功(建群者=调用者)", fun t_create_group_ok/0},
        {"create_group: 配额超限被拒", fun t_create_group_quota/0},
        {"send_message: 发送成功(from强制为调用者)", fun t_send_ok/0},
        {"send_message: 非好友被拒", fun t_send_rejected/0},
        {"send_message: 未认证被拒", fun t_send_unauth/0},
        {"send_message: 空body被拒", fun t_send_empty_body/0},
        {"send_message: 非text类型被拒", fun t_send_bad_type/0},
        {"send_message: 限流被拒", fun t_send_throttled/0}
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
    meck:new(conversation_logic, [no_link, passthrough]),
    meck:new(group_logic, [no_link, passthrough]),
    meck:new(msg_c2c_logic, [no_link, passthrough]),
    meck:new(elib_tsid, [no_link, passthrough]),
    meck:new(throttle, [no_link, passthrough]),
    %% 默认放行限流 + 固定 MsgId，个别用例覆盖
    meck:expect(throttle, check, fun(_, _) -> ok end),
    meck:expect(elib_tsid, generate, fun() -> 123456789 end),
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

%%%===================================================================
%%% list_conversations：uid 强制为调用者
%%%===================================================================
t_conversations_ok() ->
    meck:expect(conversation_logic, list, fun
        (42, _Opts) -> {ok, [#{<<"conversation_id">> => 7, <<"type">> => <<"c2c">>}]};
        (_, _) -> {ok, []}
    end),
    R = call(<<"list_conversations">>, #{<<"uid">> => 999}, 42),
    ?assertNot(is_error(R)),
    #{<<"list">> := List} = maps:get(<<"structuredContent">>, R),
    ?assertEqual([#{<<"conversation_id">> => 7, <<"type">> => <<"c2c">>}], List).

t_conversations_unauth() ->
    R = call(<<"list_conversations">>, #{}, 0),
    ?assertEqual(true, is_error(R)).

%%%===================================================================
%%% create_group：建群者一律为调用者，配额 count_by_owner 内建
%%%===================================================================
t_create_group_ok() ->
    meck:expect(group_logic, count_by_owner, fun(42) -> 3 end),
    %% 断言建群者是调用者 42（非 Args 自报），Type 默认 2
    meck:expect(group_logic, add, fun(3, 42, 2, [7, 8]) -> {ok, 100} end),
    R = call(<<"create_group">>, #{<<"member_uids">> => [7, 8]}, 42),
    ?assertNot(is_error(R)),
    ?assertMatch(#{<<"group_id">> := 100}, maps:get(<<"structuredContent">>, R)).

t_create_group_quota() ->
    meck:expect(group_logic, count_by_owner, fun(42) -> 200 end),
    meck:expect(group_logic, add, fun(200, 42, 2, _) -> {error, <<"群数量已达上限"/utf8>>} end),
    R = call(<<"create_group">>, #{<<"member_uids">> => [7]}, 42),
    ?assertEqual(true, is_error(R)).

%%%===================================================================
%%% send_message：from 强制为调用者；只发明文 text；关系门控内建
%%%===================================================================
t_send_ok() ->
    %% 只定义 from=42 子句：若被自报 from=999 影响，meck 无匹配子句 → 崩溃 → isError
    meck:expect(msg_c2c_logic, c2c, fun(_MsgId, 42, <<"7">>, _Payload) -> ok end),
    R = call(<<"send_message">>, #{<<"to">> => 7, <<"body">> => <<"hi">>, <<"from">> => 999}, 42),
    ?assertNot(is_error(R)),
    ?assertMatch(#{<<"status">> := <<"sent">>}, maps:get(<<"structuredContent">>, R)).

t_send_rejected() ->
    meck:expect(msg_c2c_logic, c2c, fun(_M, 42, <<"7">>, _P) ->
        {reply, #{<<"action">> => <<"not_a_friend">>}}
    end),
    R = call(<<"send_message">>, #{<<"to">> => 7, <<"body">> => <<"hi">>}, 42),
    ?assertEqual(true, is_error(R)).

t_send_unauth() ->
    R = call(<<"send_message">>, #{<<"to">> => 7, <<"body">> => <<"hi">>}, 0),
    ?assertEqual(true, is_error(R)).

t_send_empty_body() ->
    R = call(<<"send_message">>, #{<<"to">> => 7, <<"body">> => <<>>}, 42),
    ?assertEqual(true, is_error(R)).

t_send_bad_type() ->
    R = call(
        <<"send_message">>,
        #{<<"to">> => 7, <<"body">> => <<"hi">>, <<"msg_type">> => <<"image">>},
        42
    ),
    ?assertEqual(true, is_error(R)).

t_send_throttled() ->
    meck:expect(throttle, check, fun(_, _) -> {limit_exceeded, 0, 0} end),
    R = call(<<"send_message">>, #{<<"to">> => 7, <<"body">> => <<"hi">>}, 42),
    ?assertEqual(true, is_error(R)).
