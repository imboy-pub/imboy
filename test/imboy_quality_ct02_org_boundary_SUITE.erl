-module(imboy_quality_ct02_org_boundary_SUITE).

%%% IMBOY_ERLANG_QUALITY P2-09 CT02：跨组织/工作区身份与资源绑定。
%%% 真实产品合同（冻结自 src/lib/workspace_resolver.erl 的
%%% ensure_channel_member_access/2 + workspace_logic:my_role/2）：
%%%   workspace-scope channel 要求请求者是该 workspace 的 active 成员；
%%%   非成员 {error, {403, _}}；channel 不存在按 not_found 放行（合同原样）；
%%%   personal scope 放行。拒绝路径不得产生副作用（成员/资源行不变）。
%%% 断言全部基于真实 PG 行级状态，不是只测返回码。

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    org_boundary/1
]).

all() ->
    [org_boundary].

init_per_suite(Config) ->
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    eunit_runner:ct_suite_cleanup(Config).

org_boundary(_Config) ->
    WsId = 931100001,
    Member = 931100002,
    Outsider = 931100003,
    ChId = 931100004,
    cleanup(WsId, ChId, Member, Outsider),

    %% 夹具：user → workspace → workspace-scope channel + active 成员
    %% （outsider 无成员行；user 形状同 talk_record_parent_fixture:user/3）
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv)"
            " VALUES ($1, 'synthetic-unusable-password', $2, '127.0.0.1', '{}')"
        >>,
        [Member, <<"ct02-fixture-", (integer_to_binary(Member))/binary>>]
    ),
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv)"
            " VALUES ($1, 'synthetic-unusable-password', $2, '127.0.0.1', '{}')"
        >>,
        [Outsider, <<"ct02-fixture-", (integer_to_binary(Outsider))/binary>>]
    ),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO workspace (id, name, owner_id, status) VALUES ($1, $2, $3, 'active')">>,
        [WsId, <<"ct02-boundary-ws">>, Member]
    ),
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id)"
            " VALUES ($1, $2, $3, 1, 'workspace', $4)"
        >>,
        [ChId, <<"ct02-boundary-ch">>, Member, WsId]
    ),
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO workspace_member (workspace_id, user_id, role, status)"
            " VALUES ($1, $2, 'member', 'active')"
        >>,
        [WsId, Member]
    ),

    %% 正控：active 成员放行
    ok = workspace_resolver:ensure_channel_member_access(Member, ChId),

    %% 拒绝：非成员 403
    ?assertMatch(
        {error, {403, _}}, workspace_resolver:ensure_channel_member_access(Outsider, ChId)
    ),

    %% 无副作用：拒绝调用前后成员表/资源行不变（outsider 无行、channel 原样）
    {ok, [#{<<"c">> := OutsiderRows}]} = elib_pg:query(
        <<"SELECT count(*) AS c FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [WsId, Outsider]
    ),
    ?assertEqual(0, OutsiderRows),
    {ok, [#{<<"scope">> := <<"workspace">>, <<"workspace_id">> := WsId}]} = elib_pg:query(
        <<"SELECT scope, workspace_id FROM channel WHERE id = $1">>, [ChId]
    ),

    %% 合同原样：channel 不存在 → not_found → 守卫按 ok 放行（不越权数据）
    ok = workspace_resolver:ensure_channel_member_access(Outsider, 931199999),

    %% 失效成员（status='removed'，约束合法值集 active/removed）同样拒绝
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO workspace_member (workspace_id, user_id, role, status)"
            " VALUES ($1, $2, 'member', 'removed')"
        >>,
        [WsId, Outsider]
    ),
    ?assertMatch(
        {error, {403, _}}, workspace_resolver:ensure_channel_member_access(Outsider, ChId)
    ),

    cleanup(WsId, ChId, Member, Outsider),
    {comment, "跨工作区边界：成员放行/非成员拒绝/无副作用/失效成员拒绝"}.

cleanup(WsId, ChId, Member, Outsider) ->
    elib_pg:execute(<<"DELETE FROM channel WHERE id = $1">>, [ChId]),
    elib_pg:execute(<<"DELETE FROM workspace WHERE id = $1">>, [WsId]),
    elib_pg:execute(<<"DELETE FROM \"user\" WHERE id IN ($1, $2)">>, [Member, Outsider]),
    elib_pg:execute(
        <<"DELETE FROM workspace_member WHERE workspace_id = $1 AND user_id IN ($2, $3)">>,
        [WsId, Member, Outsider]
    ),
    ok.
