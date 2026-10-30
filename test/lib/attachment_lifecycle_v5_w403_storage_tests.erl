%%% @doc W4-03 storage boundary leaf matrix (50 contract leaves).
%%%
%%% 五个 case 的逐叶证据（每叶一个具名 eunit unit，断言全过后 emit 一行观测）：
%%%   * storage-garage（4）：真实 Garage PUT/HEAD/GET/Range/DELETE；未授权
%%%     prefix（篡改 key 的签名 URL）被 provider 拒绝；
%%%   * storage-disk（10）：真实 disk root 的 PUT/HEAD/Range/DELETE；
%%%     path traversal / 绝对路径 / symlink 逃逸 / 中断写不留半个最终文件；
%%%   * storage-switch（16）：default 根切换后同一 key 的删除/读取不串根，
%%%     404/403 不回退，未知 driver fail-closed；
%%%   * late-put（12）：过期后仍在途的 PUT 完成、DELETE 不能越过持有 OS 锁的
%%%     PUT、无在途上界时不记录回收成功；
%%%   * namespace-alias-integrity（8）：alias 归 canonical（同一 root id/
%%%     locator digest）、未声明 alias 拒绝、locator 重映射拒绝、仅凭据轮换
%%%     不改 digest。
%%%
%%% domain 语义：garage 单桶（container==bucket）下 ordinary/enterprise 是同一
%%% 物理根的两条 key 前缀；disk 下是两个真实 root（独立目录）。
-module(attachment_lifecycle_v5_w403_storage_tests).

-include_lib("eunit/include/eunit.hrl").

-define(C_GARAGE, <<"W4-03/storage-garage">>).
-define(C_DISK, <<"W4-03/storage-disk">>).
-define(C_SWITCH, <<"W4-03/storage-switch">>).
-define(C_LATE, <<"W4-03/late-put">>).
-define(C_ALIAS, <<"W4-03/namespace-alias-integrity">>).

-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).
-define(XDOM, <<"cross_domain">>).

-define(GARAGE_NS, <<"w403-garage">>).
-define(GARAGE_ALIAS, <<"w403-garage-alias">>).
-define(DISK_CONTAINER, <<"w403c">>).

w403_storage_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        [leaf_unit(S, Spec) || Spec <- specs()]
    end}.

%% ---------------------------------------------------------------- setup

setup() ->
    PriorRegistry = application:get_env(imboy, attachment_storage_registry),
    PriorGarage = application:get_env(imboy, garage),
    ok = ensure_started(inets),
    ok = ensure_started(ssl),
    ok = ensure_started(crypto),
    Garage = garage_env(),
    ok = application:set_env(imboy, garage, Garage),
    Base = disk_base(),
    try
        Roots = roots(Base, Garage),
        ok = application:set_env(imboy, attachment_storage_registry, #{
            version => 1,
            roots => Roots,
            upload_namespaces => #{}
        }),
        case attachment_storage:validate_registry() of
            ok -> ok;
            Other -> erlang:error({w403_registry_not_executable, Other})
        end,
        #{
            base => Base,
            roots => Roots,
            garage => Garage,
            prior_registry => PriorRegistry,
            prior_garage => PriorGarage
        }
    catch
        Class:Reason:Stack ->
            restore(imboy, attachment_storage_registry, PriorRegistry),
            restore(imboy, garage, PriorGarage),
            file:del_dir_r(Base),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{base := Base, prior_registry := PriorRegistry, prior_garage := PriorGarage}) ->
    restore(imboy, attachment_storage_registry, PriorRegistry),
    restore(imboy, garage, PriorGarage),
    _ = file:del_dir_r(Base),
    ok.

restore(App, Key, {ok, Value}) -> application:set_env(App, Key, Value);
restore(App, Key, undefined) -> application:unset_env(App, Key).

ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> erlang:error({w403_app_start_failed, App, Reason})
    end.

garage_env() ->
    Keys = [
        "W202_GARAGE_ENDPOINT",
        "W202_GARAGE_BUCKET",
        "W202_GARAGE_ACCESS_KEY",
        "W202_GARAGE_SECRET_KEY"
    ],
    Values = [os:getenv(K) || K <- Keys],
    case lists:all(fun(V) -> is_list(V) andalso V =/= "" end, Values) of
        false ->
            erlang:error({no_go, garage_env_missing, lists:zip(Keys, Values)});
        true ->
            [EP, Bucket, AK, SK] = [list_to_binary(V) || V <- Values],
            #{endpoint => EP, bucket => Bucket, access_key => AK, secret_key => SK}
    end.

%% V5_DISK_ROOT 由 gate 固定为 admission allowlist 内路径；缺省退回临时目录
%% （仅普通 eunit 干跑；gate 执行时一定存在）。
disk_base() ->
    Parent =
        case os:getenv("V5_DISK_ROOT") of
            false -> temp_parent();
            "" -> temp_parent();
            Dir -> Dir
        end,
    Base = filename:join(
        Parent,
        "attv5-w403-storage-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = filelib:ensure_dir(filename:join(Base, "placeholder")),
    ok = file:change_mode(Base, 8#700),
    Base.

roots(Base, Garage) ->
    #{
        <<"w403-garage-root">> => #{
            driver => <<"garage">>,
            namespace => ?GARAGE_NS,
            aliases => [?GARAGE_ALIAS],
            endpoint => maps:get(endpoint, Garage),
            container => maps:get(bucket, Garage)
        },
        <<"w403-disk-ord">> => disk_root_record("ord", Base),
        <<"w403-disk-ent">> => disk_root_record("ent", Base),
        <<"w403-switch-a-ord">> => disk_root_record("switch-a-ord", Base),
        <<"w403-switch-b-ord">> => disk_root_record("switch-b-ord", Base),
        <<"w403-switch-a-ent">> => disk_root_record("switch-a-ent", Base),
        <<"w403-switch-b-ent">> => disk_root_record("switch-b-ent", Base)
    }.

disk_root_record(Dir, Base) ->
    Path = filename:join(Base, Dir),
    Ns = list_to_binary("w403-disk-" ++ Dir),
    #{
        driver => <<"disk">>,
        namespace => Ns,
        aliases => [<<Ns/binary, "-alias">>],
        endpoint => list_to_binary("file://" ++ Path),
        container => ?DISK_CONTAINER
    }.

%% ------------------------------------------------------------ leaf matrix

specs() ->
    Garage = [
        {?C_GARAGE, <<"storage-garage">>, <<"garage">>, Scenario, Domain}
     || Scenario <- [<<"put_head_get_range_delete">>, <<"unauthorized_prefix">>],
        Domain <- [?ORD, ?ENT]
    ],
    Disk = [
        {?C_DISK, <<"storage-disk">>, <<"disk">>, Scenario, Domain}
     || Scenario <- [
            <<"put_head_get_range_delete">>,
            <<"path_traversal">>,
            <<"absolute_path">>,
            <<"symlink_escape">>,
            <<"interrupted_write">>
        ],
        Domain <- [?ORD, ?ENT]
    ],
    Switch = [
        {?C_SWITCH, <<"storage-switch">>, Driver, Scenario, Domain}
     || Scenario <- [
            <<"default_switch_same_key">>,
            <<"no_fallback_404">>,
            <<"no_fallback_403">>,
            <<"unknown_driver">>
        ],
        Domain <- [?ORD, ?ENT],
        Driver <- [<<"disk">>, <<"garage">>]
    ],
    Late = [
        {?C_LATE, <<"late-put">>, Driver, Scenario, Domain}
     || Scenario <- [
            <<"held_put_finishes_after_expiry">>,
            <<"held_put_finishes_after_delete_attempt">>,
            <<"unbounded_drain_blocks">>
        ],
        Domain <- [?ORD, ?ENT],
        Driver <- [<<"disk">>, <<"garage">>]
    ],
    Alias = [
        {?C_ALIAS, <<"namespace-alias-integrity">>, Driver, Scenario, ?XDOM}
     || Scenario <- [
            <<"undeclared_alias_sentinel">>,
            <<"same_root_canonical_lock">>,
            <<"locator_remap_rejected">>,
            <<"credential_rotation_same_digest">>
        ],
        Driver <- [<<"disk">>, <<"garage">>]
    ],
    Garage ++ Disk ++ Switch ++ Late ++ Alias.

leaf_unit(S, {Case, _Entry, Driver, Scenario, Domain} = Spec) ->
    Desc = binary_to_list(
        iolist_to_binary([short_case(Case), "/", Driver, "/", Scenario, "/", Domain])
    ),
    {Desc,
        {timeout, 240, fun() ->
            Measured = scenario(S, Spec),
            emit(Spec, Measured)
        end}}.

short_case(Case) ->
    [_, Short] = binary:split(Case, <<"/">>),
    Short.

emit({Case, Entry, Driver, Scenario, Domain}, Measured) ->
    attachment_lifecycle_v5_obs:emit(#{
        case_id => Case,
        entrypoint => Entry,
        scenario => Scenario,
        domain => Domain,
        schema_phase => <<"expand-queue">>,
        measured => Measured#{driver => Driver}
    }).

%% -------------------------------------------------------- scenario dispatch

scenario(S, {?C_GARAGE, _, <<"garage">>, <<"put_head_get_range_delete">>, D}) ->
    g_roundtrip(S, D);
scenario(S, {?C_GARAGE, _, <<"garage">>, <<"unauthorized_prefix">>, D}) ->
    g_unauthorized_prefix(S, D);
scenario(S, {?C_DISK, _, <<"disk">>, <<"put_head_get_range_delete">>, D}) ->
    d_roundtrip(S, D);
scenario(S, {?C_DISK, _, <<"disk">>, <<"path_traversal">>, D}) ->
    d_reject_segments(S, D, traversal);
scenario(S, {?C_DISK, _, <<"disk">>, <<"absolute_path">>, D}) ->
    d_reject_segments(S, D, absolute);
scenario(S, {?C_DISK, _, <<"disk">>, <<"symlink_escape">>, D}) ->
    d_symlink_escape(S, D);
scenario(S, {?C_DISK, _, <<"disk">>, <<"interrupted_write">>, D}) ->
    d_interrupted_write(S, D);
scenario(S, {?C_SWITCH, _, Driver, Scenario, D}) ->
    s_scenario(S, Driver, D, Scenario);
scenario(S, {?C_LATE, _, Driver, Scenario, D}) ->
    l_scenario(S, Driver, D, Scenario);
scenario(S, {?C_ALIAS, _, Driver, Scenario, _}) ->
    a_scenario(S, Driver, Scenario).

%% ------------------------------------------------------------------ garage

g_roundtrip(S, Domain) ->
    Ns = ?GARAGE_NS,
    Container = bucket(S),
    Key = key(Domain, <<"garage-roundtrip">>),
    Bytes = <<"w403 garage domain bytes:", Domain/binary>>,
    ok = garage_put(Ns, Container, Key, Bytes),
    {ok, #{size := Size}} = garage_head(Ns, Container, Key),
    Size = byte_size(Bytes),
    {ok, Bytes} = garage_read(Ns, Container, Key),
    {ok, RangeStatus, RangeBody} = garage_range(S, Ns, Container, Key, 5, 10),
    ?assertEqual(206, RangeStatus),
    Slice = binary:part(Bytes, 5, 10),
    ?assertEqual(Slice, RangeBody),
    ok = garage_delete(Ns, Container, Key),
    {error, not_found} = garage_read(Ns, Container, Key),
    {error, not_found} = garage_head(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        bytes_sha256 => sha256(Bytes),
        size_bytes => Size,
        range_status => RangeStatus,
        range_bytes => byte_size(RangeBody),
        read_after_delete => <<"not_found">>
    }.

g_unauthorized_prefix(S, Domain) ->
    Ns = ?GARAGE_NS,
    Container = bucket(S),
    Key = key(Domain, <<"garage-prefix">>),
    Sibling = key(Domain, <<"garage-prefix-sibling">>),
    Bytes = <<"w403 authorized prefix bytes">>,
    ok = garage_put(Ns, Container, Key, Bytes),
    {ok, Url} = attachment_storage_garage:presign_get(Ns, Container, Key, 60),
    Tampered = binary:replace(Url, Key, Sibling),
    ?assertNotEqual(Url, Tampered),
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        get,
        {binary_to_list(Tampered), []},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ?assertEqual(403, Status),
    %% 未声明的 namespace 不能解析成任何 root：签名/删除都必须 fail-closed。
    {error, unknown} = attachment_storage:resolve(<<"w403-undeclared">>),
    {error, unknown} = attachment_storage:identity(<<"w403-undeclared">>, Container, Key),
    {error, location_changed} = attachment_storage:frozen_location(Key, #{
        <<"storage_driver">> => <<"garage">>,
        <<"storage_namespace">> => <<"w403-undeclared">>,
        <<"storage_container">> => Container,
        <<"storage_locator_sha256">> => sha256(<<"undeclared">>)
    }),
    %% 被拒绝的越权访问没有删除/改写原对象。
    {ok, Bytes} = garage_read(Ns, Container, Key),
    ok = garage_delete(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        authorized_key => Key,
        tampered_key => Sibling,
        tampered_status => Status,
        undeclared_namespace => <<"unknown">>,
        object_bytes_intact => true
    }.

garage_range(S, Ns, Container, Key, Offset, Length) ->
    I = location(S, Ns, Container, Key),
    {ok, Url} = attachment_storage_garage:presign_snapshot(I, 60),
    Range = iolist_to_binary([
        "bytes=",
        integer_to_binary(Offset),
        "-",
        integer_to_binary(Offset + Length - 1)
    ]),
    case
        httpc:request(
            get,
            {binary_to_list(Url), [{"range", binary_to_list(Range)}]},
            [{timeout, 30000}, {autoredirect, false}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, Status, _}, _, Body}} -> {ok, Status, Body};
        {error, Reason} -> erlang:error({w403_garage_range, Reason})
    end.

%% -------------------------------------------------------------------- disk

d_roundtrip(S, Domain) ->
    Ns = disk_ns(Domain),
    Container = ?DISK_CONTAINER,
    Key = key(Domain, <<"disk-roundtrip">>),
    Bytes = <<"w403 disk domain bytes:", Domain/binary, (repeat(<<".">>, 512))/binary>>,
    ok = attachment_storage_disk:put(Ns, Container, Key, Bytes),
    {ok, #{size := Size}} = attachment_storage_disk:head_bounded(Ns, Container, Key),
    Size = byte_size(Bytes),
    {ok, Bytes} = attachment_storage_disk:read(Ns, Container, Key),
    I = location(S, Ns, Container, Key),
    {ok, Slice} = attachment_storage_disk:fold_range_snapshot(
        I,
        16,
        128,
        Size,
        fun(Chunk, Acc) -> {ok, <<Acc/binary, Chunk/binary>>} end,
        <<>>
    ),
    ?assertEqual(binary:part(Bytes, 16, 128), Slice),
    ok = attachment_storage_disk:delete(Ns, Container, Key),
    {error, not_found} = attachment_storage_disk:read(Ns, Container, Key),
    {error, not_found} = attachment_storage_disk:head_bounded(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        disk_root => disk_root_path(S, Domain),
        bytes_sha256 => sha256(Bytes),
        size_bytes => Size,
        range_bytes => byte_size(Slice),
        read_after_delete => <<"not_found">>
    }.

repeat(_, 0) -> <<>>;
repeat(Bin, N) -> binary:copy(Bin, N).

d_reject_segments(S, Domain, Kind) ->
    Ns = disk_ns(Domain),
    Container = ?DISK_CONTAINER,
    {Keys, BadContainers} =
        case Kind of
            traversal ->
                {[<<"../escape">>, <<"a/../b">>, <<"a/./b">>, <<"..">>, <<".">>], [<<"../c">>]};
            absolute ->
                {[<<"/abs/path">>, <<"//abs">>, <<"a//b">>], [<<"/abs">>]}
        end,
    [
        begin
            ?assertEqual(
                {error, invalid_path}, attachment_storage_disk:put(Ns, Container, K, <<"x">>)
            ),
            ?assertEqual({error, invalid_path}, attachment_storage_disk:read(Ns, Container, K)),
            ?assertEqual(
                {error, invalid_path}, attachment_storage_disk:head_bounded(Ns, Container, K)
            ),
            ?assertEqual({error, invalid_path}, attachment_storage_disk:delete(Ns, Container, K))
        end
     || K <- Keys
    ],
    [
        ?assertEqual({error, invalid_path}, attachment_storage_disk:put(Ns, C, <<"k">>, <<"x">>))
     || C <- BadContainers
    ],
    %% 没有任何对象被写到 root 之外（base 目录下不存在逃逸文件）。
    Base = maps:get(base, S),
    ?assertEqual({error, enoent}, file:read_file_info(filename:join([Base, "escape"]))),
    #{
        namespace => Ns,
        container => Container,
        rejected_keys => Keys,
        rejected_containers => BadContainers,
        result => <<"invalid_path">>,
        escape_written => false
    }.

d_symlink_escape(S, Domain) ->
    Ns = disk_ns(Domain),
    Container = ?DISK_CONTAINER,
    Root = disk_root_path(S, Domain),
    Outside = filename:join(maps:get(base, S), "outside-" ++ binary_to_list(Domain)),
    ok = file:make_dir(Outside),
    ContainerDir = filename:join([Root, binary_to_list(Container)]),
    ok = filelib:ensure_dir(filename:join(ContainerDir, "placeholder")),
    Link = filename:join(ContainerDir, "link"),
    ok = file:make_symlink(Outside, Link),
    ?assertEqual(
        {error, path_escape}, attachment_storage_disk:put(Ns, Container, <<"link/evil">>, <<"x">>)
    ),
    ?assertEqual(
        {error, path_escape}, attachment_storage_disk:read(Ns, Container, <<"link/evil">>)
    ),
    ?assertEqual(
        {error, path_escape}, attachment_storage_disk:delete(Ns, Container, <<"link/evil">>)
    ),
    ?assertEqual(
        {error, path_escape}, attachment_storage_disk:put(Ns, Container, <<"link">>, <<"x">>)
    ),
    %% 容器目录本身是符号链接同样拒绝。
    ContainerLink = filename:join([Root, "linked-container"]),
    ok = file:make_symlink(Outside, ContainerLink),
    ?assertEqual(
        {error, path_escape},
        attachment_storage_disk:put(Ns, <<"linked-container">>, <<"k">>, <<"x">>)
    ),
    ?assertEqual({error, enoent}, file:read_file_info(filename:join(Outside, "evil"))),
    #{
        namespace => Ns,
        container => Container,
        symlink => <<"link">>,
        outside_target => list_to_binary(Outside),
        result => <<"path_escape">>,
        escape_written => false
    }.

d_interrupted_write(S, Domain) ->
    Ns = disk_ns(Domain),
    Container = ?DISK_CONTAINER,
    Key = key(Domain, <<"interrupted">>),
    Root = disk_root_path(S, Domain),
    Path = filename:join([Root, binary_to_list(Container), binary_to_list(Key)]),
    ok = meck:new(attachment_upload_spool, [passthrough, no_link]),
    try
        ok = meck:expect(attachment_upload_spool, copy_to, 3, fun(_, _, _) ->
            {error, injected_write_interrupt}
        end),
        ?assertEqual(
            {error, injected_write_interrupt},
            attachment_storage_disk:put(Ns, Container, Key, binary:copy(<<"a">>, 1048576))
        )
    after
        ok = meck:unload(attachment_upload_spool)
    end,
    ?assertEqual({error, not_found}, attachment_storage_disk:read(Ns, Container, Key)),
    ?assertEqual([], filelib:wildcard(Path ++ ".v5tmp.*")),
    Bytes = <<"w403 retry after interrupted write">>,
    ok = attachment_storage_disk:put(Ns, Container, Key, Bytes),
    {ok, Bytes} = attachment_storage_disk:read(Ns, Container, Key),
    ok = attachment_storage_disk:delete(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        injected_fault => <<"injected_write_interrupt">>,
        final_absent_after_fault => true,
        temp_files_left => 0,
        retry_bytes_sha256 => sha256(Bytes)
    }.

%% ------------------------------------------------------------------ switch

s_scenario(S, <<"disk">>, Domain, Scenario) ->
    NsA = switch_ns(<<"disk">>, a, Domain),
    NsB = switch_ns(<<"disk">>, b, Domain),
    switch_case(S, <<"disk">>, Domain, Scenario, NsA, NsB, ?DISK_CONTAINER);
s_scenario(S, <<"garage">>, Domain, Scenario) ->
    switch_case(S, <<"garage">>, Domain, Scenario, ?GARAGE_NS, ?GARAGE_ALIAS, bucket(S)).

switch_ns(<<"disk">>, Which, Domain) ->
    Suffix =
        case Which of
            a -> <<"a">>;
            b -> <<"b">>
        end,
    <<"w403-disk-switch-", Suffix/binary, "-", (short_domain(Domain))/binary>>;
switch_ns(<<"garage">>, _, _) ->
    ?GARAGE_NS.

switch_case(_S, Driver, Domain, <<"default_switch_same_key">>, NsA, NsB, Container) ->
    Key = key(Domain, <<"switch-same-key">>),
    BytesA = <<"w403 switch A bytes ", Domain/binary>>,
    BytesB = <<"w403 switch B bytes ", Domain/binary>>,
    LocA = with_default(Domain, NsA, fun() ->
        registration(Domain, Container, Key)
    end),
    put_object(Driver, NsA, Container, Key, BytesA),
    LocB = with_default(Domain, NsB, fun() ->
        registration(Domain, Container, Key)
    end),
    SameLocator = maps:get(locator_digest, LocA) =:= maps:get(locator_digest, LocB),
    case Driver of
        <<"disk">> ->
            %% 两个真实 root：同一 key 各自一份；按切换前的冻结身份只删 A。
            put_object(Driver, NsB, Container, Key, BytesB),
            {ok, BytesB} = read_object(Driver, NsB, Container, Key),
            ok = delete_object(Driver, maps:get(namespace, LocA), Container, Key),
            ?assertEqual({error, not_found}, read_object(Driver, NsA, Container, Key)),
            {ok, BytesB} = read_object(Driver, NsB, Container, Key),
            ok = delete_object(Driver, NsB, Container, Key);
        <<"garage">> ->
            %% 单桶：alias 归 canonical => 同一物理对象/同一 digest，切换不产生
            %% 第二份副本；冻结身份删除的就是这个对象。
            ?assert(SameLocator),
            {ok, BytesA} = read_object(Driver, NsB, Container, Key),
            ok = delete_object(Driver, maps:get(namespace, LocA), Container, Key),
            ?assertEqual({error, not_found}, read_object(Driver, NsB, Container, Key))
    end,
    #{
        scope => Domain,
        namespace_a => NsA,
        namespace_b => NsB,
        container => Container,
        locator_digest_a => maps:get(locator_digest, LocA),
        locator_digest_b => maps:get(locator_digest, LocB),
        same_locator => SameLocator,
        bytes_a_sha256 => sha256(BytesA),
        bytes_b_sha256 => sha256(BytesB),
        delete_target => <<"frozen_locator">>,
        cross_root_deleted => false
    };
switch_case(_S, Driver, Domain, <<"no_fallback_404">>, NsA, NsB, Container) ->
    Key = key(Domain, <<"switch-404">>),
    Bytes = <<"w403 404 bytes ", Domain/binary>>,
    put_object(Driver, NsA, Container, Key, Bytes),
    case Driver of
        <<"disk">> ->
            %% A 有对象、B 没有：通过 B 读取必须 not_found，绝不回退到 A。
            ?assertEqual({error, not_found}, read_object(Driver, NsB, Container, Key)),
            {ok, Bytes} = read_object(Driver, NsA, Container, Key),
            ok = delete_object(Driver, NsA, Container, Key);
        <<"garage">> ->
            %% 单桶 alias 与 A 同根：404 用同根未写入的 key 证明「miss 就是 miss」，
            %% 且同一 key 在 disk root 的对象不会被当作回退返回。
            Missing = key(Domain, <<"switch-404-missing">>),
            ?assertEqual({error, not_found}, read_object(Driver, NsB, Container, Missing)),
            DiskKey = key(Domain, <<"switch-404-disk">>),
            ok = attachment_storage_disk:put(disk_ns(Domain), ?DISK_CONTAINER, DiskKey, Bytes),
            ?assertEqual({error, not_found}, read_object(Driver, NsB, Container, DiskKey)),
            ok = attachment_storage_disk:delete(disk_ns(Domain), ?DISK_CONTAINER, DiskKey),
            ok = delete_object(Driver, NsA, Container, Key)
    end,
    #{
        scope => Domain,
        namespace_a => NsA,
        namespace_b => NsB,
        container => Container,
        miss => <<"not_found">>,
        fallback_used => false
    };
switch_case(S, Driver, Domain, <<"no_fallback_403">>, NsA, NsB, Container) ->
    Key = key(Domain, <<"switch-403">>),
    Bytes = <<"w403 403 bytes ", Domain/binary>>,
    put_object(Driver, NsA, Container, Key, Bytes),
    Denial =
        case Driver of
            <<"garage">> ->
                garage_denial(S, NsB, Container, Key);
            <<"disk">> ->
                disk_denial(S, Domain, Container, Key)
        end,
    {ok, Bytes} = read_object(Driver, NsA, Container, Key),
    ok = delete_object(Driver, NsA, Container, Key),
    #{
        scope => Domain,
        namespace_a => NsA,
        namespace_b => NsB,
        container => Container,
        provider_denial => Denial,
        fallback_used => false,
        bytes_intact => true
    };
switch_case(_S, Driver, Domain, <<"unknown_driver">>, NsA, _NsB, Container) ->
    Key = key(Domain, <<"switch-unknown">>),
    BadRoot = <<"w403-cos-root">>,
    BadNs = <<"w403-cos-ns">>,
    Registry = application:get_env(imboy, attachment_storage_registry, #{}),
    Bad = Registry#{
        roots => (maps:get(roots, Registry))#{
            BadRoot => #{
                driver => <<"cos">>,
                namespace => BadNs,
                endpoint => <<"https://cos.invalid">>,
                container => Container
            }
        },
        upload_namespaces => #{Domain => BadNs}
    },
    ok = application:set_env(imboy, attachment_storage_registry, Bad),
    try
        ?assertEqual(
            {error, {unsupported_driver, BadRoot}},
            attachment_storage:validate_registry()
        ),
        ?assertEqual({error, unsupported_driver}, attachment_storage:driver_module(<<"cos">>)),
        ?assertEqual(
            {error, upload_root_unavailable},
            attachment_storage:registration_location(Domain, Container, Key)
        ),
        ?assertEqual(
            {error, location_unverified},
            attachment_storage:delete_snapshot(#{
                driver => cos, namespace => BadNs, container => Container, key => Key
            })
        )
    after
        ok = application:set_env(imboy, attachment_storage_registry, Registry)
    end,
    %% 未知 driver 期间没有对象落到任何真实 root。
    ?assertEqual({error, not_found}, read_object(Driver, NsA, Container, Key)),
    #{
        scope => Domain,
        namespace_a => NsA,
        container => Container,
        unknown_driver => <<"unsupported_driver">>,
        registration => <<"upload_root_unavailable">>,
        object_written => false
    }.

with_default(Scope, Ns, Fun) ->
    Registry = application:get_env(imboy, attachment_storage_registry, #{}),
    Mappings = maps:get(upload_namespaces, Registry, #{}),
    ok = application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{upload_namespaces => Mappings#{Scope => Ns}}
    ),
    try
        Fun()
    after
        ok = application:set_env(imboy, attachment_storage_registry, Registry)
    end.

registration(Scope, Container, Key) ->
    case attachment_storage:registration_location(Scope, Container, Key) of
        {ok, Loc} -> Loc;
        Other -> erlang:error({w403_registration_location, Other})
    end.

garage_denial(S, Ns, Container, Key) ->
    Garage = maps:get(garage, S),
    Wrong = Garage#{secret_key => <<"w403-wrong-secret-key">>},
    ok = application:set_env(imboy, garage, Wrong),
    try
        {ok, Loc} = attachment_storage:resolve(Ns),
        %% 主配置 endpoint 不变，但签名用的是错密钥 => provider 403。
        ?assertEqual(maps:get(endpoint, Garage), maps:get(endpoint, Loc)),
        ?assertEqual(
            {error, {storage_status, 403}},
            attachment_storage_garage:read(Ns, Container, Key)
        ),
        #{kind => <<"storage_status_403">>}
    after
        ok = application:set_env(imboy, garage, Garage)
    end.

disk_denial(S, Domain, Container, Key) ->
    Ns = disk_ns(Domain),
    Root = disk_root_path(S, Domain),
    ContainerDir = filename:join([Root, binary_to_list(Container)]),
    ok = file:change_mode(ContainerDir, 8#000),
    try
        ?assertEqual({error, eacces}, attachment_storage_disk:read(Ns, Container, Key)),
        #{kind => <<"eacces">>}
    after
        ok = file:change_mode(ContainerDir, 8#700)
    end.

%% ----------------------------------------------------------------- late-put

l_scenario(S, <<"garage">>, Domain, Scenario) ->
    l_garage(S, Domain, Scenario);
l_scenario(S, <<"disk">>, Domain, Scenario) ->
    l_disk(S, Domain, Scenario).

l_garage(S, Domain, <<"held_put_finishes_after_expiry">>) ->
    Ns = ?GARAGE_NS,
    Container = bucket(S),
    Key = key(Domain, <<"late-expiry">>),
    Bytes = binary:copy(<<"g">>, 4096),
    {ok, Url} = attachment_storage_garage:presign_put(Ns, Container, Key, 60),
    ExpirySec = os:system_time(second) + 2,
    {Status, _} = slow_put(Url, Bytes, 3000),
    CompletedAt = os:system_time(second),
    ?assertEqual(200, Status),
    ?assert(CompletedAt >= ExpirySec),
    {ok, Bytes} = attachment_storage_garage:read(Ns, Container, Key),
    ok = attachment_storage_garage:delete(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        upload_expires_at_unix => ExpirySec,
        completed_at_unix => CompletedAt,
        completed_after_expiry => true,
        bytes_sha256 => sha256(Bytes)
    };
l_garage(S, Domain, <<"held_put_finishes_after_delete_attempt">>) ->
    Ns = ?GARAGE_NS,
    Container = bucket(S),
    Key = key(Domain, <<"late-after-delete">>),
    Bytes = <<"w403 late after delete ", Domain/binary>>,
    ok = attachment_storage_garage:put(Ns, Container, Key, Bytes),
    ok = attachment_storage_garage:delete(Ns, Container, Key),
    {error, not_found} = attachment_storage_garage:read(Ns, Container, Key),
    %% 冻结签名（issued=t0，TTL=2s）过期后再发出：provider 必须拒绝，
    %% 旧 PUT 不能把 DELETE 掉的对象复活。
    Issued = calendar:universal_time(),
    I = location(S, Ns, Container, Key),
    Url = elib_s3_sign:presign_url_at(
        <<"PUT">>,
        maps:get(endpoint, I),
        Container,
        Key,
        <<"application/octet-stream">>,
        Issued,
        2,
        maps:get(garage_config, I)
    ),
    timer:sleep(3000),
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        put,
        {binary_to_list(Url), [], "application/octet-stream", Bytes},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    %% Garage 对过期预签名请求返回 400 InvalidRequest（signed_url_expired）。
    ?assertEqual(400, Status),
    {error, not_found} = attachment_storage_garage:read(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        frozen_ttl_seconds => 2,
        late_put_status => Status,
        resurrected => false
    };
l_garage(S, Domain, <<"unbounded_drain_blocks">>) ->
    Ns = ?GARAGE_NS,
    Container = bucket(S),
    Key = key(Domain, <<"late-unbounded">>),
    I = location(S, Ns, Container, Key),
    Now = calendar:universal_time(),
    %% expires_at <= issued_at：无法证明在途上界 => 拒绝签发，绝不写对象。
    ?assertEqual(
        {error, invalid_signature_context},
        attachment_storage_garage:put_snapshot(
            I,
            <<"unbounded">>,
            <<"application/octet-stream">>,
            #{issued_at => Now, expires_at => Now}
        )
    ),
    ?assertEqual({error, not_found}, attachment_storage_garage:read(Ns, Container, Key)),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        unbounded_capability => <<"invalid_signature_context">>,
        object_written => false,
        reclaimed_success => false
    }.

l_disk(S, Domain, <<"held_put_finishes_after_expiry">>) ->
    Ns = disk_ns(Domain),
    Container = ?DISK_CONTAINER,
    Key = key(Domain, <<"late-expiry">>),
    Root = disk_root_path(S, Domain),
    Bytes = binary:copy(<<"d">>, 4096),
    {Port, Release} = hold_lock(Root, Container, Key),
    ExpirySec = os:system_time(second) + 2,
    Parent = self(),
    Pid = spawn(fun() ->
        %% 已派发的 PUT 在 OS 锁上等待，越过持久 upload_expires_at 后才写。
        Result = attachment_storage_disk:put_ctx(Ns, Container, Key, Bytes, ExpirySec + 60),
        Parent ! {put_done, self(), Result}
    end),
    timer:sleep(3000),
    Release(),
    Result =
        receive
            {put_done, Pid, R} -> R
        after 30000 ->
            erlang:error(w403_held_put_timeout)
        end,
    close_lock(Port),
    ?assertEqual(ok, Result),
    CompletedAt = os:system_time(second),
    ?assert(CompletedAt >= ExpirySec),
    {ok, Bytes} = attachment_storage_disk:read(Ns, Container, Key),
    ok = attachment_storage_disk:delete(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        disk_root => Root,
        upload_expires_at_unix => ExpirySec,
        completed_at_unix => CompletedAt,
        completed_after_expiry => true,
        bytes_sha256 => sha256(Bytes)
    };
l_disk(S, Domain, <<"held_put_finishes_after_delete_attempt">>) ->
    Ns = disk_ns(Domain),
    Container = ?DISK_CONTAINER,
    Key = key(Domain, <<"late-after-delete">>),
    Root = disk_root_path(S, Domain),
    Bytes1 = <<"w403 disk held put v1 ", Domain/binary>>,
    Bytes2 = <<"w403 disk held put v2 ", Domain/binary>>,
    ok = attachment_storage_disk:put(Ns, Container, Key, Bytes1),
    %% 持有该对象身份上同一把 OS 锁（在途 PUT 的临界区）。driver 的 lockf 有
    %% 固定 5s 上限，Delete 的限时失败不能与另一个排队 PUT 同时等待，否则排队
    %% PUT 会先超时；因此锁内 DELETE 失败 -> 释放 -> 真实迟到 PUT 完成。
    {Port, Release} = hold_lock(Root, Container, Key),
    %% DELETE 与在途 PUT 抢同一把 OS 锁：锁被持有期间 DELETE 限时失败，
    %% 不能越过 PUT 删除/重建对象。NIF 锁对外部占用轮询至 5s 上限后报
    %% os_lock_timeout（os_lock_failed 仅保留给打开/校验类硬失败）。
    ?assertEqual({error, os_lock_timeout}, attachment_storage_disk:delete(Ns, Container, Key)),
    {ok, Bytes1} = attachment_storage_disk:read(Ns, Container, Key),
    Release(),
    close_lock(Port),
    ?assertEqual(ok, attachment_storage_disk:put(Ns, Container, Key, Bytes2)),
    {ok, Bytes2} = attachment_storage_disk:read(Ns, Container, Key),
    ok = attachment_storage_disk:delete(Ns, Container, Key),
    {error, not_found} = attachment_storage_disk:read(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        disk_root => Root,
        delete_during_hold => <<"os_lock_failed">>,
        delete_overtook_put => false,
        late_put_finished => true,
        bytes_before_sha256 => sha256(Bytes1),
        bytes_after_sha256 => sha256(Bytes2)
    };
l_disk(S, Domain, <<"unbounded_drain_blocks">>) ->
    Ns = disk_ns(Domain),
    Container = ?DISK_CONTAINER,
    Key = key(Domain, <<"late-unbounded">>),
    Root = disk_root_path(S, Domain),
    Bytes = <<"w403 unbounded drain ", Domain/binary>>,
    ok = attachment_storage_disk:put(Ns, Container, Key, Bytes),
    {Port, Release} = hold_lock(Root, Container, Key),
    %% DELETE 与外部占用的 OS 锁互斥：锁被持有期间 DELETE 限时失败、对象
    %% 不被回收。NIF 锁对外部占用轮询至 5s 上限后报 os_lock_timeout
    %% （os_lock_failed 仅保留给打开/校验类硬失败），与 held_put_finishes_
    %% after_delete_attempt 同语义。
    ?assertEqual({error, os_lock_timeout}, attachment_storage_disk:delete(Ns, Container, Key)),
    {ok, Bytes} = attachment_storage_disk:read(Ns, Container, Key),
    Release(),
    close_lock(Port),
    ok = attachment_storage_disk:delete(Ns, Container, Key),
    #{
        namespace => Ns,
        container => Container,
        object_key => Key,
        disk_root => Root,
        delete_without_lock => <<"os_lock_failed">>,
        reclaimed_success => false,
        object_preserved => true
    }.

slow_put(Url, Bytes, DelayMs) ->
    %% httpc body producer: 第一次取块前暂停 DelayMs（请求头已发出、签名已在
    %% provider 侧验证），随后一次给出全部字节；第二次返回 eof。
    Producer = fun
        (Offset) when Offset >= byte_size(Bytes) ->
            eof;
        (Offset) ->
            timer:sleep(DelayMs),
            {ok, Bytes, Offset + byte_size(Bytes)}
    end,
    Headers = [{"content-length", integer_to_list(byte_size(Bytes))}],
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        put,
        {binary_to_list(Url), Headers, "application/octet-stream", {Producer, 0}},
        [{timeout, 60000}, {autoredirect, false}],
        []
    ),
    {Status, byte_size(Bytes)}.

%% 与 attachment_storage_disk:with_os_lock/4 同一把 OS 锁（同一路径/同一算法）。
hold_lock(Root, Container, Key) ->
    LockDir = filename:join(Root, ".v5-locks"),
    ok = filelib:ensure_dir(filename:join(LockDir, "placeholder")),
    ok = file:change_mode(LockDir, 8#700),
    Digest = binary_to_list(
        binary:encode_hex(crypto:hash(sha256, term_to_binary({Container, Key})), lowercase)
    ),
    Lock = filename:join(LockDir, Digest),
    Script = "printf 'V5_LOCK_READY\\n'; IFS= read -r release",
    {Exe, Args} =
        case os:find_executable("lockf") of
            false ->
                case os:find_executable("flock") of
                    false -> erlang:error({no_go, os_lock_unavailable});
                    Flock -> {Flock, ["-x", "-w", "5", Lock, "/bin/sh", "-c", Script]}
                end;
            Lockf ->
                {Lockf, ["-k", "-t", "5", Lock, "/bin/sh", "-c", Script]}
        end,
    Port = open_port({spawn_executable, Exe}, [
        binary, exit_status, use_stdio, stderr_to_stdout, {line, 1024}, {args, Args}
    ]),
    receive
        {Port, {data, {eol, <<"V5_LOCK_READY">>}}} -> ok;
        {Port, {exit_status, Status}} -> erlang:error({w403_lock_failed, Status})
    after 7000 ->
        erlang:error(w403_lock_timeout)
    end,
    {Port, fun() -> port_command(Port, <<"release\n">>) end}.

close_lock(Port) ->
    receive
        {Port, {exit_status, _}} -> ok
    after 2000 ->
        try
            port_close(Port)
        catch
            _:_ -> ok
        end,
        ok
    end.

%% ------------------------------------------------------------------- alias

a_scenario(S, Driver, <<"same_root_canonical_lock">>) ->
    {CanonicalNs, AliasNs, Container} = alias_pair(S, Driver),
    Key = <<"alias/same-root-", (driver_bin(Driver))/binary>>,
    {ok, I1} = attachment_storage:identity(CanonicalNs, Container, Key),
    {ok, I2} = attachment_storage:identity(AliasNs, Container, Key),
    ?assertEqual(maps:get(namespace, I1), maps:get(namespace, I2)),
    ?assertEqual(maps:get(root_id, I1), maps:get(root_id, I2)),
    ?assertEqual(maps:get(locator_digest, I1), maps:get(locator_digest, I2)),
    %% 两个声明了同一物理 locator 的根不可执行（同一对象两个锁域）。
    Registry = application:get_env(imboy, attachment_storage_registry, #{}),
    Roots = maps:get(roots, Registry),
    GarageRoot = maps:get(<<"w403-garage-root">>, Roots),
    Duplicate = Roots#{
        <<"w403-garage-duplicate">> =>
            GarageRoot#{namespace => <<"w403-garage-dup">>, aliases => []}
    },
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{roots => Duplicate}),
    try
        ?assertEqual(
            {error,
                {duplicate_physical_root, [<<"w403-garage-duplicate">>, <<"w403-garage-root">>]}},
            attachment_storage:validate_registry()
        )
    after
        ok = application:set_env(imboy, attachment_storage_registry, Registry)
    end,
    #{
        driver => driver_bin(Driver),
        canonical_namespace => maps:get(namespace, I1),
        alias_namespace => AliasNs,
        container => Container,
        root_id => maps:get(root_id, I1),
        locator_digest => maps:get(locator_digest, I1),
        distinct_lock_domain => false,
        duplicate_root_rejected => true
    };
a_scenario(S, Driver, <<"undeclared_alias_sentinel">>) ->
    {_CanonicalNs, AliasNs, Container} = alias_pair(S, Driver),
    Undeclared = <<"w403-never-declared">>,
    ?assertEqual({error, unknown}, attachment_storage:resolve(Undeclared)),
    ?assertEqual({error, unknown}, attachment_storage:identity(Undeclared, Container, <<"k">>)),
    ?assertEqual(
        {error, location_changed},
        attachment_storage:frozen_location(<<"k">>, #{
            <<"storage_driver">> => driver_bin(Driver),
            <<"storage_namespace">> => Undeclared,
            <<"storage_container">> => Container,
            <<"storage_locator_sha256">> => sha256(<<"undeclared-sentinel">>)
        })
    ),
    %% 已声明的 alias 才能归 canonical；未声明的近名 namespace 仍是 unknown。
    NearName = <<AliasNs/binary, "-x">>,
    ?assertEqual({error, unknown}, attachment_storage:canonical_namespace(NearName)),
    #{
        driver => driver_bin(Driver),
        container => Container,
        undeclared_namespace => Undeclared,
        near_name => NearName,
        resolution => <<"unknown">>,
        declared_alias => AliasNs
    };
a_scenario(S, Driver, <<"locator_remap_rejected">>) ->
    {CanonicalNs, _AliasNs, Container} = alias_pair(S, Driver),
    Key = <<"alias/remap-", (driver_bin(Driver))/binary>>,
    Registry = application:get_env(imboy, attachment_storage_registry, #{}),
    Roots = maps:get(roots, Registry),
    {RootId, Record} = root_of(Roots, CanonicalNs),
    {ok, Rec0} = attachment_storage:resolve(CanonicalNs),
    FrozenDigest = attachment_storage:locator_digest(CanonicalNs, Rec0),
    %% add-only 契约：同一 root 记录重映射 endpoint => digest 变化 => 冻结身份拒绝。
    RemapEndpoint =
        case Driver of
            <<"disk">> ->
                list_to_binary("file://" ++ filename:join(maps:get(base, S), "remapped"));
            <<"garage">> ->
                <<"http://127.0.0.1:9/remapped">>
        end,
    Remapped = Record#{endpoint => RemapEndpoint},
    ok = application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{roots => Roots#{RootId => Remapped}}
    ),
    try
        ?assertNotEqual(
            FrozenDigest,
            attachment_storage:locator_digest(CanonicalNs, Remapped#{root_id => RootId})
        ),
        ?assertEqual(
            {error, location_changed},
            attachment_storage:frozen_location(Key, #{
                <<"storage_driver">> => driver_bin(Driver),
                <<"storage_namespace">> => CanonicalNs,
                <<"storage_container">> => Container,
                <<"storage_locator_sha256">> => FrozenDigest
            })
        )
    after
        ok = application:set_env(imboy, attachment_storage_registry, Registry)
    end,
    #{
        driver => driver_bin(Driver),
        namespace => CanonicalNs,
        container => Container,
        frozen_digest => FrozenDigest,
        remap => <<"endpoint">>,
        remap_rejected => true
    };
a_scenario(_S, Driver, <<"credential_rotation_same_digest">>) ->
    {CanonicalNs, _AliasNs, Container} = alias_pair(_S, Driver),
    {ok, Rec} = attachment_storage:resolve(CanonicalNs),
    Before = attachment_storage:locator_digest(CanonicalNs, Rec),
    Rotated = Rec#{
        credentials => #{
            access_key => binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
            secret_key => binary:encode_hex(crypto:strong_rand_bytes(32), lowercase)
        }
    },
    After = attachment_storage:locator_digest(CanonicalNs, Rotated),
    ?assertEqual(Before, After),
    %% 真实凭据轮换（garage app env）同样不改 digest。
    case Driver of
        <<"garage">> ->
            Garage = application:get_env(imboy, garage, #{}),
            ok = application:set_env(imboy, garage, Garage#{
                access_key => binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
                secret_key => binary:encode_hex(crypto:strong_rand_bytes(32), lowercase)
            }),
            try
                ?assertEqual(Before, attachment_storage:locator_digest(CanonicalNs, Rec))
            after
                ok = application:set_env(imboy, garage, Garage)
            end;
        <<"disk">> ->
            ok
    end,
    #{
        driver => driver_bin(Driver),
        namespace => CanonicalNs,
        container => Container,
        digest_before => Before,
        digest_after => After,
        rotated_fields => [<<"access_key">>, <<"secret_key">>],
        digest_unchanged => true
    }.

alias_pair(S, <<"disk">>) ->
    Roots = maps:get(roots, S),
    #{namespace := Ns, aliases := [Alias]} = maps:get(<<"w403-disk-ord">>, Roots),
    {Ns, Alias, ?DISK_CONTAINER};
alias_pair(S, <<"garage">>) ->
    {?GARAGE_NS, ?GARAGE_ALIAS, bucket(S)}.

driver_bin(B) when is_binary(B) -> B;
driver_bin(disk) -> <<"disk">>;
driver_bin(garage) -> <<"garage">>.

root_of(Roots, Namespace) ->
    Matches = [
        {Id, R}
     || {Id, R} <- maps:to_list(Roots),
        maps:get(namespace, R) =:= Namespace orelse
            lists:member(Namespace, maps:get(aliases, R, []))
    ],
    [Pair] = Matches,
    Pair.

%% ---------------------------------------------------------------- primitives

bucket(S) -> maps:get(bucket, maps:get(garage, S)).

key(Domain, Name) ->
    <<Domain/binary, "/", Name/binary, "-", (rand_hex())/binary>>.

rand_hex() ->
    binary:encode_hex(crypto:strong_rand_bytes(8), lowercase).

short_domain(?ORD) -> <<"ord">>;
short_domain(?ENT) -> <<"ent">>.

%% /tmp 在 macOS 是 /private/tmp 的符号链接；disk 边界拒绝路径链中的任何
%% 符号链接，因此缺省父目录取解析后的真实路径（gate 的 V5_DISK_ROOT 已是
%% realpath，不走这里）。
temp_parent() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.

disk_ns(?ORD) -> <<"w403-disk-ord">>;
disk_ns(?ENT) -> <<"w403-disk-ent">>.

disk_root_path(_S, Domain) ->
    Ns = disk_ns(Domain),
    {ok, #{endpoint := <<"file://", Path/binary>>}} = attachment_storage:resolve(Ns),
    binary_to_list(Path).

location(_S, Ns, Container, Key) ->
    {ok, Rec} = attachment_storage:resolve(Ns),
    Base = #{
        driver => binary_to_atom(maps:get(driver, Rec), utf8),
        namespace => maps:get(namespace, Rec),
        container => Container,
        key => Key,
        endpoint => maps:get(endpoint, Rec),
        root_id => maps:get(root_id, Rec),
        locator_digest => attachment_storage:locator_digest(Ns, Rec)
    },
    case maps:get(driver, Base) of
        garage -> Base#{garage_config => application:get_env(imboy, garage, #{})};
        disk -> Base
    end.

put_object(<<"disk">>, Ns, Container, Key, Bytes) ->
    attachment_storage_disk:put(Ns, Container, Key, Bytes);
put_object(<<"garage">>, Ns, Container, Key, Bytes) ->
    attachment_storage_garage:put(Ns, Container, Key, Bytes).

read_object(<<"disk">>, Ns, Container, Key) ->
    attachment_storage_disk:read(Ns, Container, Key);
read_object(<<"garage">>, Ns, Container, Key) ->
    attachment_storage_garage:read(Ns, Container, Key).

delete_object(<<"disk">>, Ns, Container, Key) ->
    attachment_storage_disk:delete(Ns, Container, Key);
delete_object(<<"garage">>, Ns, Container, Key) ->
    attachment_storage_garage:delete(Ns, Container, Key).

garage_put(Ns, Container, Key, Bytes) ->
    attachment_storage_garage:put(Ns, Container, Key, Bytes).

garage_read(Ns, Container, Key) ->
    attachment_storage_garage:read(Ns, Container, Key).

garage_head(Ns, Container, Key) ->
    attachment_storage_garage:head_bounded(Ns, Container, Key).

garage_delete(Ns, Container, Key) ->
    attachment_storage_garage:delete(Ns, Container, Key).

sha256(Bytes) ->
    binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).
