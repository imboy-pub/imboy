%%% @doc W4-03 Storage Boundary 验收（disk 纯逻辑 + registry 语义 + switch）。
%%%
%%% 覆盖：
%%%   * namespace-alias-integrity：别名归 canonical、locator digest 同 root
%%%     同值（不含凭据）、identity 四元组含完整 I；
%%%   * driver-content-immutable（disk）：put → read 内容一致 → 同 key 幂等
%%%     覆盖同内容 ok；head size 忠实；
%%%   * disk 路径校验：.. / 绝对段 / 控制字符 → invalid_path（禁逃逸）；
%%%   * storage-switch：app env 切 driver → dispatch 换模块（零代码变更）；
%%%   * late-put（disk 形状）：put 后 head 可见（tmp+rename 原子写）。
%%%
%%% Garage 行为等价（storage-garage）与 66 旧 Disk 协议叶
%%%（disk-predecessor-protocols）由带 env 的 gate 执行器单独跑。
-module(attachment_lifecycle_v5_w403_storage_tests).

-include_lib("eunit/include/eunit.hrl").

w403_storage_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    Prior = application:get_env(imboy, attachment_storage_registry),
    Root = filename:join(temp_dir(), "w403-disk-" ++
        binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))),
    ok = file:make_dir(Root),
    try
        ok = file:change_mode(Root, 8#700),
        setup_registry(Root),
        #{root => Root, prior => Prior}
    catch
        Class:Reason:Stack ->
            restore_registry(Prior),
            file:del_dir_r(Root),
            erlang:raise(Class, Reason, Stack)
    end.

setup_registry(Root) ->
    Registry = #{
        version => 1,
        roots => #{
            <<"root-garage-1">> => #{
                driver => <<"garage">>,
                namespace => <<"imboy-ns1">>,
                aliases => [<<"imboy-alias-1">>, <<"imboy-alias-2">>],
                endpoint => <<"http://127.0.0.1:3900">>,
                container => <<"imboy">>
            },
            <<"root-disk-1">> => #{
                driver => <<"disk">>,
                namespace => <<"disk-ns1">>,
                aliases => [<<"disk-alias-1">>],
                endpoint => list_to_binary("file://" ++ Root),
                container => <<"c">>
            }
        }
    },
    application:set_env(imboy, attachment_storage_registry, Registry).

cleanup(#{root := Root, prior := Prior}) ->
    restore_registry(Prior),
    ok = file:del_dir_r(Root).

restore_registry({ok, Value}) -> application:set_env(imboy, attachment_storage_registry, Value);
restore_registry(undefined) -> application:unset_env(imboy, attachment_storage_registry).

temp_dir() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.

cases(_Ok) ->
    [
        fun namespace_alias_integrity/0,
        fun driver_content_immutable_disk/0,
        fun disk_path_validation/0,
        fun storage_switch/0,
        fun late_put_atomic_visible/0,
        fun unknown_namespace_fail_closed/0
    ].

%% 别名归 canonical；同 root 别名 locator digest 相同；identity 含完整 I
namespace_alias_integrity() ->
    {ok, #{namespace := <<"imboy-ns1">>, root_id := <<"root-garage-1">>} = Id1} =
        attachment_storage:identity(<<"imboy-ns1">>, <<"c1">>, <<"k1">>),
    <<"imboy-ns1">> = attachment_storage:canonical_namespace(<<"imboy-alias-1">>),
    <<"imboy-ns1">> = attachment_storage:canonical_namespace(<<"imboy-alias-2">>),
    {ok, Id2} = attachment_storage:identity(<<"imboy-alias-1">>, <<"c1">>, <<"k1">>),
    ?assertEqual(maps:get(locator_digest, Id1), maps:get(locator_digest, Id2)),
    ?assertEqual(maps:get(root_id, Id1), maps:get(root_id, Id2)),
    ?assertEqual(64, byte_size(maps:get(locator_digest, Id1))),
    {ok, IdDisk} = attachment_storage:identity(<<"disk-ns1">>, <<"c1">>, <<"k1">>),
    ?assertNotEqual(maps:get(locator_digest, Id1), maps:get(locator_digest, IdDisk)).

%% driver 内容一致 + 幂等覆盖同内容（内容不可变语义：driver 不改写字节）
driver_content_immutable_disk() ->
    Body = crypto:strong_rand_bytes(1024),
    ok = attachment_storage_disk:put(<<"disk-ns1">>, <<"c">>, <<"k-immutable">>, Body),
    {ok, Body} = attachment_storage_disk:read(<<"disk-ns1">>, <<"c">>, <<"k-immutable">>),
    {ok, #{size := 1024}} = attachment_storage_disk:head(
        <<"disk-ns1">>, <<"c">>, <<"k-immutable">>
    ),
    ok = attachment_storage_disk:put(<<"disk-ns1">>, <<"c">>, <<"k-immutable">>, Body),
    {ok, Body} = attachment_storage_disk:read(<<"disk-ns1">>, <<"c">>, <<"k-immutable">>),
    ok.

%% 路径校验：逃逸与非法段 fail-closed
disk_path_validation() ->
    %% with_path/4 rejects unsafe segments before touching the filesystem.
    {error, invalid_path} =
        attachment_storage_disk:put(<<"disk-ns1">>, <<"c">>, <<"../escape">>, <<"x">>),
    {error, invalid_path} =
        attachment_storage_disk:read(<<"disk-ns1">>, <<"c">>, <<"a/../..//b">>),
    {error, invalid_path} =
        attachment_storage_disk:head(<<"disk-ns1">>, <<"c">>, <<"/abs/path">>),
    {error, invalid_path} =
        attachment_storage_disk:put(<<"disk-ns1">>, <<"c">>, <<"bad", 1, "key">>, <<"x">>),
    ok.

%% storage-switch：同一入口按 registry driver 派发到不同模块（零代码变更）
storage_switch() ->
    {ok, #{driver := <<"disk">>}} = attachment_storage:resolve(<<"disk-ns1">>),
    attachment_storage_disk = attachment_storage:driver_module(<<"disk">>),
    {ok, #{driver := <<"garage">>}} = attachment_storage:resolve(<<"imboy-ns1">>),
    attachment_storage_garage = attachment_storage:driver_module(<<"garage">>),
    {error, unsupported_driver} = attachment_storage:driver_module(<<"cos">>),
    ok.

%% late-put 原子可见：put（tmp+rename）后立即可读，无半成品窗口
late_put_atomic_visible() ->
    Body = <<"late-put-body">>,
    ok = attachment_storage_disk:put(<<"disk-ns1">>, <<"c">>, <<"k-late">>, Body),
    {ok, Body} = attachment_storage_disk:read(<<"disk-ns1">>, <<"c">>, <<"k-late">>),
    ok = attachment_storage_disk:delete(<<"disk-ns1">>, <<"c">>, <<"k-late">>),
    {error, not_found} = attachment_storage_disk:read(
        <<"disk-ns1">>, <<"c">>, <<"k-late">>
    ),
    ok.

%% 未知 namespace fail-closed（保守：不落入任何默认桶）
unknown_namespace_fail_closed() ->
    {error, unknown} = attachment_storage:resolve(<<"never-registered-ns">>),
    {error, unknown} = attachment_storage:identity(
        <<"never-registered-ns">>, <<"c">>, <<"k">>
    ),
    {error, unknown} = attachment_storage:canonical_namespace(
        <<"never-registered-ns">>
    ),
    ok.
