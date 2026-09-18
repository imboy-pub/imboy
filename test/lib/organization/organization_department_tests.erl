%%% @doc ORG-04 Department 纯域逻辑套件（无 DB；C10 域规则的可独立验证部分）。
-module(organization_department_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 名称校验
%% ===================================================================

valid_name_test_() ->
    [
        {"非空二进制合法", ?_assertEqual(ok, organization_department:valid_name(<<"研发部"/utf8>>))},
        {"空二进制拒绝",
            ?_assertMatch(
                {error, {invalid_name, <<>>}},
                organization_department:valid_name(<<>>)
            )},
        {"纯空白拒绝",
            ?_assertMatch(
                {error, {invalid_name, _}},
                organization_department:valid_name(<<"   ">>)
            )},
        {"非二进制拒绝",
            ?_assertMatch(
                {error, {invalid_name, 42}},
                organization_department:valid_name(42)
            )},
        {"201 字节超限拒绝",
            ?_assertMatch(
                {error, {invalid_name, _}},
                organization_department:valid_name(binary:copy(<<"a">>, 201))
            )},
        {"200 字节恰好在界内",
            ?_assertEqual(
                ok,
                organization_department:valid_name(binary:copy(<<"a">>, 200))
            )}
    ].

%% ===================================================================
%% 状态机（C10：V1 只有 active -> archived）
%% ===================================================================

transition_test_() ->
    [
        {"active->archived 允许",
            ?_assertEqual(
                {ok, archived},
                organization_department:transition(active, archived)
            )},
        {"archived->active（restore）V1 拒绝",
            ?_assertMatch(
                {error, {invalid_transition, archived, active}},
                organization_department:transition(archived, active)
            )},
        {"active->active 非法迁移",
            ?_assertMatch(
                {error, {invalid_transition, active, active}},
                organization_department:transition(active, active)
            )},
        {"状态全集冻结为 active|archived",
            ?_assertEqual([active, archived], organization_department:statuses())}
    ].

archive_decision_test_() ->
    [
        {"active 进流程",
            ?_assertEqual({ok, proceed}, organization_department:archive_decision(active))},
        {"archived 幂等",
            ?_assertEqual(
                {ok, idempotent},
                organization_department:archive_decision(archived)
            )},
        {"未知状态拒绝",
            ?_assertMatch(
                {error, _},
                organization_department:archive_decision(bogus)
            )}
    ].

%% ===================================================================
%% 环检测（纯函数；DB 触发器是权威，这里是应用层快速失败）
%% ===================================================================

cycle_test_() ->
    [
        {"self-parent 拒绝",
            ?_assertMatch(
                {error, {self_parent, 7}},
                organization_department:ensure_not_self_parent(7, 7)
            )},
        {"父不同则放行", ?_assertEqual(ok, organization_department:ensure_not_self_parent(7, 8))},
        {"自身在新父祖先链中 => 环拒绝",
            ?_assertMatch(
                {error, {cycle, 10, _}},
                organization_department:ensure_not_in_ancestors(10, [5, 10, 3])
            )},
        {"自身不在祖先链中放行",
            ?_assertEqual(ok, organization_department:ensure_not_in_ancestors(10, [5, 4, 3]))},
        {"祖先链非列表拒绝",
            ?_assertMatch(
                {error, {cycle, _, _}},
                organization_department:ensure_not_in_ancestors(10, bad)
            )}
    ].

%% ===================================================================
%% 出站投影白名单（白名单外一个不出站）
%% ===================================================================

project_test_() ->
    [
        {"白名单键保留",
            ?_assertEqual(
                #{id => 1, name => <<"d">>},
                organization_department:project([id, name], #{id => 1, name => <<"d">>})
            )},
        {"白名单外键剔除",
            ?_assertEqual(
                #{id => 1},
                organization_department:project([id], #{id => 1, secret => <<"x">>})
            )},
        {"白名单内缺失键出站 null",
            ?_assertEqual(
                #{id => 1, parent_id => null},
                organization_department:project([id, parent_id], #{id => 1})
            )}
    ].
