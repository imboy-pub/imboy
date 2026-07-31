-module(adm_plugin_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%% 合法且原子已存在的插件名 → {ok, Atom}
%% 用 'undefined'：该原子在 VM 中必然已存在。
existing_atom_name_test() ->
    ?assertEqual({ok, undefined}, adm_plugin_handler:safe_plugin_name(<<"undefined">>)).

%% 攻击者提交的、原子从未存在过的随机名 → {error, _}
%% 修复前用 binary_to_atom 会新建原子并返回 {ok, _}（原子表 DoS）；
%% 修复后用 binary_to_existing_atom 应拒绝。
unknown_name_rejected_test() ->
    ?assertMatch(
        {error, _}, adm_plugin_handler:safe_plugin_name(<<"attacker_atom_dos_probe_9c3f">>)
    ).

%% 格式非法（首字符为数字）→ {error, _}
bad_format_leading_digit_test() ->
    ?assertMatch({error, _}, adm_plugin_handler:safe_plugin_name(<<"1plugin">>)).

%% 格式非法（含连字符）→ {error, _}
bad_format_dash_test() ->
    ?assertMatch({error, _}, adm_plugin_handler:safe_plugin_name(<<"a-b">>)).

%% 超长（>64）→ {error, _}
too_long_test() ->
    Long = list_to_binary(lists:duplicate(65, $a)),
    ?assertMatch({error, _}, adm_plugin_handler:safe_plugin_name(Long)).

%% 空 → {error, _}
empty_test() ->
    ?assertMatch({error, _}, adm_plugin_handler:safe_plugin_name(<<>>)).

%% ===== A-28: is_lifecycle_mutation/1 门控分类 =====

%% 7 个写 action 受 IMBOY_PLUGIN_LIFECYCLE_ENABLED 门控
lifecycle_mutation_writes_test() ->
    Writes = [install, enable, disable, upgrade, uninstall, reset, force_uninstall],
    [
        ?assert(adm_plugin_handler:is_lifecycle_mutation(A))
     || A <- Writes
    ].

%% 5 个只读 action + 未知/缺省（false）不被门控
lifecycle_mutation_reads_test() ->
    Reads = [list, detail, state_query, health, logs],
    [
        ?assertNot(adm_plugin_handler:is_lifecycle_mutation(A))
     || A <- Reads
    ],
    ?assertNot(adm_plugin_handler:is_lifecycle_mutation(unknown_action)),
    ?assertNot(adm_plugin_handler:is_lifecycle_mutation(false)).
