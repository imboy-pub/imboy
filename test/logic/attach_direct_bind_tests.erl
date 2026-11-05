-module(attach_direct_bind_tests).
-moduledoc "step-11：direct 确认引用绑定（群文件 bind_file / 相册 bind_photo、批量逐项，T27）".
%%%
% 每次生成器拥有 fresh peer 和全迁移临时库；每例创建独立的真实父级关系。
% 用例：
%   * 未 confirm（attachment 无行）→ not_confirmed（T27 核心拒绝）；
%   * 跨主体（creator≠调用者）→ cross_subject；
%   * 群文件 bind 成功：group_file 行 + attachment 引用（group_file_id 回填）；
%   * 重复 bind → 同一 file_id（不重复引用）；
%   * 相册 bind 成功 + 重复 → duplicate 标记同 id；
%   * 批量逐项：ok 与 not_confirmed 逐项明确，部分失败继续。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% 钉名 gate env 在场判定：与 inttest_marker_db:bound_v5_database/1 的
%% 第一步输入对齐——只检查 gate 导出的 <PREFIX>_DB 是否设置且非空。
%% 专项 gate executor 一定成套导出 _DB；只有 _DB 而缺其余凭证的非法半配置
%% 不在此静默让位，继续走 provision 由 fail-closed 契约显式报 contract_conflict。
v5_gate_pinned(Prefix) when is_binary(Prefix) ->
    Env = binary_to_list(<<Prefix/binary, "_DB">>),
    case os:getenv(Env) of
        false -> false;
        "" -> false;
        _ -> true
    end.

bind_test_() ->
    %% 受控让位（仓库先例：attachment_lifecycle_v5_pg_tests 的
    %% v5_gate_pinned，生成器返回空清单）。本套件虽已包 isolated_pg_fixture，
    %% 但该夹具只负责子 VM 隔离，不做 env 让位：无钉名 env 裸跑时
    %% setup_database 内 inttest_marker_db:provision 由 fail-closed 契约
    %% 抛 contract_conflict，且返回空清单也会被其 Passed>0 收据断言判
    %% 失败——让位必须落在本生成器入口。nightly 全量
    %% `make eunit-local-legacy` 不经专项 gate 裸跑，此时生成器返回空清单
    %% 静默让位（eunit 无 {skip, ...} 顶层描述符，返回空清单才是等价
    %% 机制）；gate 路径（ATTACH_BIND_INTTEST_DB 在场）全语义执行，
    %% 断言零删减。
    case v5_gate_pinned(<<"ATTACH_BIND_INTTEST">>) of
        true ->
            isolated_pg_fixture:test(fun bind_fixture/0);
        false ->
            io:format(user, "ATTACH_BIND_EUNIT_YIELD missing_env=~p~n", [
                "ATTACH_BIND_INTTEST_DB"
            ]),
            []
    end.

bind_fixture() ->
    {setup, fun setup_database/0, fun cleanup_database/1, fun(Database) ->
        {foreach, fun() -> setup(Database) end, fun cleanup/1, [
            fun(S) -> fun() -> channel_bind_and_gc_guard(S) end end,
            fun(S) -> fun() -> channel_bind_delete_race(S) end end,
            fun(S) -> fun() -> bind_file_without_confirmation_rejected(S) end end,
            fun(S) -> fun() -> bind_existing_unconfirmed_rejected(S) end end,
            fun(S) -> fun() -> bind_mismatched_winner_rejected(S) end end,
            fun(S) -> fun() -> restricted_role_can_check_winner(S) end end,
            fun(S) -> fun() -> archived_workspace_rejects_bind(S) end end,
            fun(S) -> fun() -> bind_file_cross_subject_rejected(S) end end,
            fun(S) -> fun() -> bind_file_success_and_idempotent(S) end end,
            fun(S) -> fun() -> group_file_audit_failure_rolls_back(S) end end,
            fun(S) -> fun() -> group_file_concurrent_bind_once(S) end end,
            fun(S) -> fun() -> bind_photo_success_and_idempotent(S) end end,
            fun(S) -> fun() -> bind_photo_concurrent_idempotent(S) end end,
            fun(S) -> fun() -> bind_photo_delete_race(S) end end,
            fun(S) -> fun() -> bind_photo_after_delete_rejected(S) end end,
            fun(S) -> fun() -> bind_photo_string_album_id(S) end end,
            fun(S) -> fun() -> bind_photo_album_not_in_group_rejected(S) end end,
            fun(S) -> fun() -> bind_photo_album_missing_rejected(S) end end,
            fun(S) -> fun() -> bind_photos_batch_per_item(S) end end
        ]}
    end}.

setup_database() ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    Original = #{
        apps => [A || {A, _, _} <- application:which_applications()],
        pooler_env => application:get_env(pooler, pools)
    },
    try
        Marker = inttest_marker_db:provision(#{env_prefix => <<"ATTACH_BIND_INTTEST">>}),
        S = Marker#{original_runtime => Original},
        try
            application:set_env(pooler, pools, []),
            {ok, _} = application:ensure_all_started(pooler),
            {ok, _} = application:ensure_all_started(crypto),
            Opts = (maps:with([host, port, username, password], maps:get(server, S)))#{
                database => binary_to_list(maps:get(db, S)),
                codecs => [{epgsql_codec_rfc3339_bin, []}]
            },
            {ok, _} = pooler:new_pool(#{
                name => pgsql,
                init_count => 2,
                max_count => 4,
                start_mfa => {epgsql, connect, [Opts]}
            }),
            ok = elib_tsid:init(#{dc_id => 0, node_id => 1}),
            S
        catch
            Class0:Reason0:Stack0 ->
                CleanupResult =
                    try
                        cleanup_database(S)
                    catch
                        CleanupClass:CleanupReason -> {error, {CleanupClass, CleanupReason}}
                    end,
                case CleanupResult of
                    ok -> erlang:raise(Class0, Reason0, Stack0);
                    _ -> error({fixture_setup_and_cleanup_failed, {Class0, Reason0}, CleanupResult})
                end
        end
    catch
        Class:Reason:Stack ->
            restore_runtime(Original),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup_database(S) ->
    %% Always attempt DB disposal, even when partial setup never made a pool.
    try
        PoolResult =
            try
                pooler:rm_pool(pgsql)
            catch
                PoolClass:PoolReason -> {error, {PoolClass, PoolReason}}
            end,
        DbResult =
            try
                inttest_marker_db:release(S)
            catch
                DbClass:DbReason -> {error, {DbClass, DbReason}}
            end,
        case {PoolResult, DbResult} of
            {ok, ok} -> ok;
            Other -> error({owned_fixture_cleanup_failed, Other})
        end
    after
        restore_runtime(maps:get(original_runtime, S))
    end.

restore_runtime(#{apps := OriginalApps, pooler_env := OriginalEnv}) ->
    NewApps = [
        A
     || {A, _, _} <- application:which_applications(),
        not lists:member(A, OriginalApps)
    ],
    [application:stop(A) || A <- NewApps],
    case OriginalEnv of
        undefined -> application:unset_env(pooler, pools);
        {ok, Value} -> application:set_env(pooler, pools, Value)
    end,
    ok.

setup(_Database) ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Base = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    EP = <<"http://127.0.0.1:39999">>,
    Ns = <<"dv1bind-", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    S = #{
        before_env => Before,
        ns => Ns,
        ep => EP,
        uid => Base,
        org => Base + 1,
        ws => Base + 2,
        gid => Base + 3,
        other_uid => Base + 4,
        other_gid => Base + 5,
        album => Base + 6,
        other_album => Base + 7,
        missing_album => Base + 8,
        attachment => Base + 9
    },
    try
        ok = application:set_env(
            imboy,
            garage,
            #{endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>}
        ),
        Registry = attachment_storage:registry(),
        Roots = maps:get(roots, Registry, #{}),
        ok = application:set_env(imboy, attachment_storage_registry, Registry#{
            roots => Roots#{
                Ns => #{
                    driver => <<"garage">>,
                    namespace => Ns,
                    endpoint => EP,
                    container => <<"b">>,
                    root_id => Ns
                }
            }
        }),
        ok = elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<"INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES($1,$2,'','127.0.0.1','owned-bind-fixture')">>,
                [Base, <<"bind-", (integer_to_binary(Base))/binary>>]
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"INSERT INTO organization(id,name,owner_id) VALUES($1,'owned-bind-fixture',$2)">>,
                [maps:get(org, S), Base]
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"INSERT INTO workspace(id,organization_id,name,owner_id) VALUES($1,$2,'owned-bind-fixture',$3)">>,
                [maps:get(ws, S), maps:get(org, S), Base]
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES($1,$2,'','127.0.0.1','owned-bind-fixture')">>,
                [maps:get(other_uid, S), <<"bind-other-", (integer_to_binary(Base))/binary>>]
            ),
            %% The production organization INSERT trigger creates its owner membership.
            {ok, _} = elib_pg:query(
                Conn,
                <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
                [maps:get(org, S), maps:get(other_uid, S)]
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'owner','active'),($1,$3,'member','active')">>,
                [maps:get(ws, S), Base, maps:get(other_uid, S)]
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,title,status,scope,workspace_id) VALUES($1,$2,$2,'owned-bind-fixture',1,'workspace',$3),($4,$5,$5,'owned-bind-other',1,'workspace',$3)">>,
                [
                    maps:get(gid, S),
                    Base,
                    maps:get(ws, S),
                    maps:get(other_gid, S),
                    maps:get(other_uid, S)
                ]
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"INSERT INTO group_member(id,group_id,user_id,role,is_join,status) VALUES($1,$2,$3,5,true,1),($4,$5,$6,5,true,1)">>,
                [
                    Base + 10,
                    maps:get(gid, S),
                    Base,
                    Base + 11,
                    maps:get(other_gid, S),
                    maps:get(other_uid, S)
                ]
            ),
            ok
        end),
        S
    catch
        Class:Reason:Stack ->
            cleanup(S),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{before_env := Before}) ->
    %% Rows live only until the enclosing marker DB is dropped. Never bypass
    %% bounded-purge authorization or suppress referential-integrity checks.
    [
        case Value of
            undefined -> application:unset_env(imboy, K);
            {ok, V} -> application:set_env(imboy, K, V)
        end
     || {K, Value} <- Before
    ],
    ok.

%% ---------------------------------------------------------------- helpers

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).

locator(S) ->
    attachment_storage:locator_digest(maps:get(ns, S), #{
        driver => <<"garage">>,
        root_id => maps:get(ns, S),
        endpoint => <<"http://127.0.0.1:39999">>,
        container => <<"b">>
    }).

%% 插入 direct confirm 后的 group 附件行（confirmed 引用资格的唯一来源）
insert_unconfirmed_attachment(S, Path, Creator) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url, size, info,"
            " referer_time, creator_user_id, scope, scope_ref, cipher, status, updated_at, created_at,"
            " storage_driver, storage_namespace, storage_container, storage_locator_sha256,"
            " storage_physical_key)"
            " VALUES ($1,$2,'image/png','.png','dv1-x.png',$3,$3,10,'{}',1,$4,'group',$5,null,1,"
            " NOW(),NOW(),'garage',$8,'b',$6,$7)"
        >>,
        [
            maps:get(attachment, S),
            hex64(maps:get(attachment, S)),
            Path,
            Creator,
            integer_to_binary(maps:get(gid, S)),
            locator(S),
            physical_of(Path),
            maps:get(ns, S)
        ]
    ),
    ok.

insert_direct_attachment(S, Path, Creator) ->
    ok = insert_unconfirmed_attachment(S, Path, Creator),
    %% This is an isolated marker DB; real HTTP tests separately exercise winner creation.
    elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            Conn,
            <<
                "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,scope_ref,"
                "storage_driver,storage_namespace,storage_container,storage_locator_sha256,"
                "session_state,direct_state,confirmed_attachment_id,upload_registration,upload_expires_at,direct_expected)"
                " VALUES($1,'b','group',$2,$3,'garage',$4,'b',$5,'confirmed','CONFIRMED',$6,"
                " '{\"protocol\":\"direct-v1\",\"subject\":{},\"declaration\":{}}'::jsonb,now()-interval '1 day',"
                " jsonb_build_object('version','1','sha256',$7::text,'size_bytes','10','mime','image/png'))"
            >>,
            [
                Path,
                Creator,
                integer_to_binary(maps:get(gid, S)),
                maps:get(ns, S),
                locator(S),
                maps:get(attachment, S),
                hex64(maps:get(attachment, S))
            ]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<
                "INSERT INTO attach_direct_attempt(object_key,attempt_no,attempt_state,source_key,target_key,"
                "target_driver,target_namespace,target_container,profile_digest,dispatch_cas_uuid,dispatched_at,"
                "proof_sha256,proof_size,proof_received_at) VALUES($1,1,'WINNER','staging/test',$2,"
                "'garage',$3,'b',$4,gen_random_uuid(),now(),$5,10,now())"
            >>,
            [Path, physical_of(Path), maps:get(ns, S), locator(S), hex64(maps:get(attachment, S))]
        ),
        ok
    end).

physical_of(Path) ->
    <<"direct-c/", Path/binary, "/p1">>.

%% album_id is varchar; bind_photo accepts the corresponding integer ID.
seed_albums(S) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO group_album(id,group_id,album_id,album_name,creator_id,status)"
            " VALUES($1,$2,$3,'dv1-own',$4,1),($5,$6,$7,'dv1-other',$8,1)"
        >>,
        [
            maps:get(album, S),
            maps:get(gid, S),
            integer_to_binary(maps:get(album, S)),
            maps:get(uid, S),
            maps:get(other_album, S),
            maps:get(other_gid, S),
            integer_to_binary(maps:get(other_album, S)),
            maps:get(other_uid, S)
        ]
    ),
    ok.

object_key(S, Suffix) ->
    <<"u", (integer_to_binary(maps:get(uid, S)))/binary, "/dv1/", Suffix/binary>>.

%% ---------------------------------------------------------------- cases

seed_channel(S, Path) ->
    Channel = maps:get(gid, S) + 100,
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO channel(id,name,creator_uid,scope,workspace_id)"
            " VALUES($1,'owned-channel-fixture',$2,'workspace',$3)"
        >>,
        [Channel, maps:get(uid, S), maps:get(ws, S)]
    ),
    ok = insert_direct_attachment(S, Path, maps:get(uid, S)),
    ok = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE attachment SET scope='channel',scope_ref=$2 WHERE path=$1">>,
            [Path, integer_to_binary(Channel)]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE attach_pending SET scope='channel',scope_ref=$2 WHERE object_key=$1">>,
            [Path, integer_to_binary(Channel)]
        ),
        {ok, _} = elib_pg:query(
            C,
            <<"DELETE FROM workspace_attachment_retention WHERE attachment_id=$1">>,
            [maps:get(attachment, S)]
        ),
        ok
    end),
    Channel.

publish_channel_image(Channel, Uid, Path) ->
    channel_ds:publish_message(
        Channel,
        Uid,
        <<"fixture">>,
        <<"image">>,
        #{<<"images">> => [#{<<"uri">> => Path}]}
    ).

channel_bind_and_gc_guard(S) ->
    Path = object_key(S, <<"channel.png">>),
    Channel = seed_channel(S, Path),
    ?assertMatch({error, forbidden}, publish_channel_image(Channel, maps:get(other_uid, S), Path)),
    ok = set_channel_attempt_hash(Path, hex64(123)),
    ?assertMatch({error, _}, publish_channel_image(Channel, maps:get(uid, S), Path)),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM channel_message WHERE channel_id=$1">>, [Channel]
    ),
    ok = set_channel_attempt_hash(Path, hex64(maps:get(attachment, S))),
    {ok, MessageId} = publish_channel_image(Channel, maps:get(uid, S), Path),
    {ok, [#{<<"blocked">> := true}]} = elib_pg:query(
        <<"SELECT fn_v5_channel_reference_blocks($1) AS blocked">>, [Path]
    ),
    ?assertEqual(
        {ok, [#{<<"blocked">> => true}]},
        elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            elib_pg:query(C, <<"SELECT fn_v5_channel_reference_blocks($1) AS blocked">>, [Path])
        end)
    ),
    ?assertEqual(
        {ok, false},
        elib_pg:with_tx(fun(C) ->
            attachment_repo:refresh_gc_verified_tx(C, Path)
        end)
    ),
    {ok, _} = elib_pg:query(<<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [Path]),
    ?assertMatch(
        {error, #error{code = <<"23510">>}},
        elib_pg:query(<<"SELECT fn_v5_intent_ordinary_delete($1,'channel fixture')">>, [Path])
    ),
    ?assertMatch({ok, 1}, channel_message_ds:revoke(MessageId, maps:get(uid, S), elib_dt:now())),
    {ok, [#{<<"blocked">> := true}]} = elib_pg:query(
        <<"SELECT fn_v5_channel_reference_blocks($1) AS blocked">>, [Path]
    ),
    ?assertMatch({ok, 1}, channel_message_ds:delete(MessageId)),
    {ok, [#{<<"blocked">> := false}]} = elib_pg:query(
        <<"SELECT fn_v5_channel_reference_blocks($1) AS blocked">>, [Path]
    ),
    ?assertEqual(
        {error, channel_reference_update_unsupported},
        channel_message_ds:update(MessageId, #{status => 1})
    ).

set_channel_attempt_hash(Path, Sha) ->
    elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            C,
            <<"UPDATE attach_direct_attempt SET proof_sha256=$2 WHERE object_key=$1">>,
            [Path, Sha]
        ),
        ok
    end).

channel_bind_delete_race(S) ->
    Path = object_key(S, <<"channel-race.png">>),
    Channel = seed_channel(S, Path),
    {ok, _} = elib_pg:query(<<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [Path]),
    Parent = self(),
    Start = fun(Tag, F) ->
        spawn(fun() ->
            Parent ! {ready, self()},
            receive
                go -> Parent ! {result, Tag, F()}
            end
        end)
    end,
    Binder = Start(bind, fun() -> publish_channel_image(Channel, maps:get(uid, S), Path) end),
    Deleter = Start(delete, fun() ->
        elib_pg:query(<<"SELECT fn_v5_intent_ordinary_delete($1,'channel race')">>, [Path])
    end),
    receive
        {ready, Binder} -> ok
    after 10000 -> error(bind_not_ready)
    end,
    receive
        {ready, Deleter} -> ok
    after 10000 -> error(delete_not_ready)
    end,
    Binder ! go,
    Deleter ! go,
    BindResult =
        receive
            {result, bind, BR} -> BR
        after 20000 -> timeout
        end,
    DeleteResult =
        receive
            {result, delete, DR} -> DR
        after 20000 -> timeout
        end,
    {ok, [Row]} = elib_pg:query(
        <<
            "SELECT lifecycle_state,fn_v5_channel_reference_blocks(path) AS ref"
            " FROM attachment WHERE path=$1"
        >>,
        [Path]
    ),
    case
        {BindResult, DeleteResult, maps:get(<<"lifecycle_state">>, Row), maps:get(<<"ref">>, Row)}
    of
        {{ok, _}, {error, #error{code = <<"23510">>}}, <<"live">>, true} -> ok;
        {{error, _}, {ok, _}, <<"delete_intent">>, false} -> ok;
        Other -> error({unsafe_channel_delete_race, Other})
    end.

bind_file_without_confirmation_rejected(S) ->
    %% 未 confirm：attachment 无行 → 拒绝（T27）
    ?assertEqual(
        {error, not_confirmed},
        group_file_ds:bind_file(
            maps:get(gid, S),
            maps:get(uid, S),
            <<"dv1-no.bin">>,
            <<"application/octet-stream">>,
            object_key(S, <<"nope">>),
            10,
            hex64(1)
        )
    ),
    ok.

bind_existing_unconfirmed_rejected(S) ->
    Path = object_key(S, <<"unconfirmed-row.png">>),
    ok = insert_unconfirmed_attachment(S, Path, maps:get(uid, S)),
    ?assertEqual(
        {error, not_confirmed},
        group_file_ds:bind_file(
            maps:get(gid, S),
            maps:get(uid, S),
            <<"unconfirmed.png">>,
            <<"image/png">>,
            Path,
            10,
            <<>>
        )
    ),
    ok = seed_albums(S),
    ?assertEqual(
        {error, not_confirmed},
        group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(album, S),
            <<"unconfirmed.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(maps:get(attachment, S))
        )
    ).

bind_mismatched_winner_rejected(S) ->
    Path = object_key(S, <<"wrong-winner.png">>),
    ok = insert_direct_attachment(S, Path, maps:get(uid, S)),
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            Conn,
            <<"UPDATE attach_direct_attempt SET proof_sha256=$2 WHERE object_key=$1">>,
            [Path, hex64(123)]
        ),
        ok
    end),
    ?assertEqual(
        {error, not_confirmed},
        group_file_ds:bind_file(
            maps:get(gid, S),
            maps:get(uid, S),
            <<"wrong.png">>,
            <<"image/png">>,
            Path,
            10,
            <<>>
        )
    ),
    ok = seed_albums(S),
    ?assertEqual(
        {error, not_confirmed},
        group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(album, S),
            <<"wrong.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(maps:get(attachment, S))
        )
    ).

restricted_role_can_check_winner(S) ->
    Path = object_key(S, <<"restricted-role.png">>),
    ok = insert_direct_attachment(S, Path, maps:get(uid, S)),
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL ROLE v5_app_runtime">>, []),
        ?assertEqual(ok, attachment_repo:verify_direct_winner_tx(Conn, Path)),
        ?assertEqual(
            {error, not_confirmed},
            attachment_repo:verify_direct_winner_tx(Conn, <<"missing">>)
        ),
        ok
    end).

archived_workspace_rejects_bind(S) ->
    ok = seed_albums(S),
    Path = object_key(S, <<"archived.png">>),
    ok = insert_direct_attachment(S, Path, maps:get(uid, S)),
    {ok, _} = elib_pg:query(
        <<"UPDATE workspace SET status='archived' WHERE id=$1">>,
        [maps:get(ws, S)]
    ),
    FileResult = group_file_ds:bind_file(
        maps:get(gid, S),
        maps:get(uid, S),
        <<"archived.png">>,
        <<"image/png">>,
        Path,
        10,
        <<>>
    ),
    PhotoResult = group_album_ds:bind_photo(
        maps:get(gid, S),
        maps:get(uid, S),
        maps:get(album, S),
        <<"archived.png">>,
        Path,
        10,
        <<"image/png">>,
        hex64(maps:get(attachment, S))
    ),
    ?assert(workspace_guard:is_archived_error(FileResult)),
    ?assert(workspace_guard:is_archived_error(PhotoResult)),
    {ok, Row} = elib_pg:one(
        <<
            "SELECT (SELECT count(*) FROM group_file WHERE file_url=$1) AS files,"
            " (SELECT count(*) FROM group_album_photo WHERE photo_url=$1) AS photos"
        >>,
        [Path]
    ),
    ?assertEqual(0, maps:get(<<"files">>, Row)),
    ?assertEqual(0, maps:get(<<"photos">>, Row)).

bind_file_cross_subject_rejected(S) ->
    Path = object_key(S, <<"cross.png">>),
    %% creator 是别人
    insert_direct_attachment(S, Path, maps:get(other_uid, S)),
    ?assertEqual(
        {error, cross_subject},
        group_file_ds:bind_file(
            maps:get(gid, S),
            maps:get(uid, S),
            <<"dv1-cross.png">>,
            <<"image/png">>,
            Path,
            10,
            hex64(2)
        )
    ),
    ok.

bind_file_success_and_idempotent(S) ->
    Path = object_key(S, <<"ok.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    %% bind_file 第 7 参的运行时契约是 legacy file_md5（生产 handler 透传
    %% body 的 "file_md5"，默认 <<>>）：V5 confirm 后 attachment 只存
    %% SHA-256，提交 MD5 无法从该摘要认证，group_file_ds 冻结契约对非空
    %% md5 fail-closed 拒绝（unverifiable_file_md5，group_file_ds_tests
    %% 已锁定该拒绝语义）。成功路径按生产契约传 <<>>；原断言零删减。
    {ok, FileId1} = group_file_ds:bind_file(
        maps:get(gid, S),
        maps:get(uid, S),
        <<"dv1-ok.png">>,
        <<"image/png">>,
        Path,
        10,
        <<>>
    ),
    %% group_file 行 + attachment 引用（group_file_id 回填 DB 主键）均落库
    {ok, RefRow} = elib_pg:one(
        <<
            "SELECT a.group_file_id, a.storage_physical_key, gf.id AS gf_pk"
            " FROM attachment a JOIN group_file gf ON gf.id = a.group_file_id"
            " WHERE a.path=$1"
        >>,
        [Path]
    ),
    ?assertEqual(maps:get(<<"gf_pk">>, RefRow), maps:get(<<"group_file_id">>, RefRow)),
    ?assertEqual(physical_of(Path), maps:get(<<"storage_physical_key">>, RefRow)),
    {ok, GfRow} = elib_pg:one(
        <<"SELECT count(*) AS n FROM group_file WHERE group_id=$1 AND file_url=$2">>,
        [maps:get(gid, S), Path]
    ),
    ?assertEqual(1, maps:get(<<"n">>, GfRow)),
    %% 业务 file_id 一致（file_url 语义 = object_key）
    {ok, BizRow} = elib_pg:one(
        <<"SELECT file_id FROM group_file WHERE group_id=$1 AND file_url=$2">>,
        [maps:get(gid, S), Path]
    ),
    ?assertEqual(FileId1, maps:get(<<"file_id">>, BizRow)),
    %% 重复绑定：同一 file_id，group_file 行不再增加（幂等不重复引用）
    {ok, FileId2} = group_file_ds:bind_file(
        maps:get(gid, S),
        maps:get(uid, S),
        <<"dv1-ok.png">>,
        <<"image/png">>,
        Path,
        10,
        <<>>
    ),
    ?assertEqual(FileId1, FileId2),
    {ok, GfRow2} = elib_pg:one(
        <<"SELECT count(*) AS n FROM group_file WHERE group_id=$1 AND file_url=$2">>,
        [maps:get(gid, S), Path]
    ),
    ?assertEqual(1, maps:get(<<"n">>, GfRow2)),
    {ok, _} = elib_pg:query(
        <<"UPDATE group_file SET status=0 WHERE id=$1">>,
        [maps:get(<<"gf_pk">>, RefRow)]
    ),
    ?assertEqual(
        {error, group_file_binding_missing},
        group_file_ds:bind_file(
            maps:get(gid, S),
            maps:get(uid, S),
            <<"dv1-ok.png">>,
            <<"image/png">>,
            Path,
            10,
            <<>>
        )
    ),
    ok.

group_file_audit_failure_rolls_back(S) ->
    Path = object_key(S, <<"audit-failure.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    Bind = fun() ->
        group_file_ds:bind_file(
            maps:get(gid, S),
            maps:get(uid, S),
            <<"audit.png">>,
            <<"image/png">>,
            Path,
            10,
            <<>>
        )
    end,
    ok = meck:new(enterprise_audit_event_repo, [passthrough, no_link]),
    try
        ok = meck:expect(
            enterprise_audit_event_repo,
            append_scope_file_tx,
            fun(_, _, _, _) -> {error, audit_injected} end
        ),
        ?assertEqual({error, audit_injected}, Bind()),
        {ok, Before} = elib_pg:one(<<"SELECT group_file_id FROM attachment WHERE path=$1">>, [Path]),
        ?assertEqual(null, maps:get(<<"group_file_id">>, Before)),
        {ok, None} = elib_pg:one(<<"SELECT count(*) AS n FROM group_file WHERE file_url=$1">>, [
            Path
        ]),
        ?assertEqual(0, maps:get(<<"n">>, None)),
        ok = meck:unload(enterprise_audit_event_repo),
        {ok, _} = Bind(),
        {ok, Bound} = elib_pg:one(<<"SELECT group_file_id FROM attachment WHERE path=$1">>, [Path]),
        Id = maps:get(<<"group_file_id">>, Bound),
        ok = meck:new(enterprise_audit_event_repo, [passthrough, no_link]),
        ok = meck:expect(
            enterprise_audit_event_repo,
            append_scope_file_tx,
            fun(_, _, _, _) -> {error, audit_injected} end
        ),
        ?assertEqual({error, audit_injected}, group_file_ds:delete_file(Id, maps:get(uid, S))),
        {ok, After} = elib_pg:one(<<"SELECT status FROM group_file WHERE id=$1">>, [Id]),
        ?assertEqual(1, maps:get(<<"status">>, After))
    after
        catch meck:unload(enterprise_audit_event_repo)
    end.

group_file_concurrent_bind_once(S) ->
    Path = object_key(S, <<"file-concurrent.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    Parent = self(),
    Bind = fun() ->
        Result = group_file_ds:bind_file(
            maps:get(gid, S),
            maps:get(uid, S),
            <<"concurrent.png">>,
            <<"image/png">>,
            Path,
            10,
            <<>>
        ),
        Parent ! {file_bind_result, Result}
    end,
    spawn(Bind),
    spawn(Bind),
    Results = [
        receive
            {file_bind_result, R} -> R
        after 20000 -> timeout
        end
     || _ <- [1, 2]
    ],
    [{ok, First}, {ok, Second}] = Results,
    ?assertEqual(First, Second),
    {ok, Row} = elib_pg:one(<<"SELECT count(*) AS n FROM group_file WHERE file_url=$1">>, [Path]),
    ?assertEqual(1, maps:get(<<"n">>, Row)).

bind_photo_success_and_idempotent(S) ->
    ok = seed_albums(S),
    Path = object_key(S, <<"photo.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO group_member_generation(group_id,user_id,start_seq) VALUES($1,$2,1)"
            " ON CONFLICT(group_id,user_id,generation_no) DO NOTHING"
        >>,
        [maps:get(gid, S), maps:get(uid, S)]
    ),
    ?assertNot(attachment_repo:authorize_group_access(Path, maps:get(uid, S))),
    ?assertEqual(
        {error, attachment_metadata_mismatch},
        group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(album, S),
            <<"dv1-photo.png">>,
            Path,
            11,
            <<"image/png">>,
            hex64(maps:get(attachment, S))
        )
    ),
    {ok, P1} = group_album_ds:bind_photo(
        maps:get(gid, S),
        maps:get(uid, S),
        maps:get(album, S),
        <<"dv1-photo.png">>,
        Path,
        10,
        <<"image/png">>,
        hex64(maps:get(attachment, S))
    ),
    ?assertEqual(false, maps:get(<<"duplicate">>, P1, false)),
    {ok, P2} = group_album_ds:bind_photo(
        maps:get(gid, S),
        maps:get(uid, S),
        maps:get(album, S),
        <<"dv1-photo.png">>,
        Path,
        10,
        <<"image/png">>,
        hex64(maps:get(attachment, S))
    ),
    ?assertEqual(true, maps:get(<<"duplicate">>, P2)),
    ?assertEqual(maps:get(<<"id">>, P1), maps:get(<<"id">>, P2)),
    ?assert(attachment_repo:authorize_group_access(Path, maps:get(uid, S))),
    ?assertNot(attachment_repo:authorize_group_access(Path, maps:get(other_uid, S))),
    {ok, _} = elib_pg:query(
        <<"UPDATE group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>,
        [maps:get(gid, S), maps:get(uid, S)]
    ),
    ?assertNot(attachment_repo:authorize_group_access(Path, maps:get(uid, S))),
    {ok, _} = elib_pg:query(
        <<"UPDATE group_member SET status=1 WHERE group_id=$1 AND user_id=$2">>,
        [maps:get(gid, S), maps:get(uid, S)]
    ),
    {ok, _} = elib_pg:query(<<"UPDATE group_album_photo SET status=0 WHERE photo_url=$1">>, [Path]),
    ?assertNot(attachment_repo:authorize_group_access(Path, maps:get(uid, S))),
    {ok, _} = elib_pg:query(<<"UPDATE group_album_photo SET status=1 WHERE photo_url=$1">>, [Path]),
    %% Isolate the album guard from the fixture's automatic workspace hold.
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM workspace_attachment_retention WHERE attachment_id=$1">>,
            [maps:get(attachment, S)]
        ),
        ok
    end),
    {ok, false} = elib_pg:with_tx(fun(Conn) ->
        attachment_repo:refresh_gc_verified_tx(Conn, Path)
    end),
    {ok, _} = elib_pg:query(<<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [Path]),
    {error, #error{code = <<"23510">>, message = Message}} =
        elib_pg:query(<<"SELECT fn_v5_intent_ordinary_delete($1, 'test')">>, [Path]),
    ?assertEqual(<<"v5 intent blocked by group album reference">>, Message),
    {ok, State} = elib_pg:one(<<"SELECT lifecycle_state FROM attachment WHERE path=$1">>, [Path]),
    ?assertEqual(<<"live">>, maps:get(<<"lifecycle_state">>, State)),
    {ok, _} = elib_pg:query(<<"UPDATE group_album_photo SET status=0 WHERE photo_url=$1">>, [Path]),
    ?assertEqual(
        {error, photo_binding_deleted},
        group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(album, S),
            <<"dv1-photo.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(maps:get(attachment, S))
        )
    ),
    ok.

bind_photo_concurrent_idempotent(S) ->
    ok = seed_albums(S),
    Path = object_key(S, <<"concurrent.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    Parent = self(),
    Bind = fun() ->
        Result = group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(album, S),
            <<"concurrent.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(maps:get(attachment, S))
        ),
        Parent ! {binding_result, Result}
    end,
    spawn(Bind),
    spawn(Bind),
    Results = [
        receive
            {binding_result, R} -> R
        after 10000 -> timeout
        end
     || _ <- [1, 2]
    ],
    [{ok, First}, {ok, Second}] = Results,
    ?assertEqual(maps:get(<<"id">>, First), maps:get(<<"id">>, Second)),
    {ok, Row} = elib_pg:one(
        <<"SELECT count(*) AS n FROM group_album_photo WHERE photo_url=$1 AND status=1">>, [Path]
    ),
    ?assertEqual(1, maps:get(<<"n">>, Row)).

bind_photo_delete_race(S) ->
    ok = seed_albums(S),
    Path = object_key(S, <<"delete-race.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    %% Remove only this disposable fixture's automatic hold to isolate reference/GC exclusion.
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM workspace_attachment_retention WHERE attachment_id=$1">>,
            [maps:get(attachment, S)]
        ),
        ok
    end),
    {ok, _} = elib_pg:query(<<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [Path]),
    Parent = self(),
    Start = fun(Tag, Fun) ->
        spawn(fun() ->
            Parent ! {ready, self()},
            receive
                go -> Parent ! {result, Tag, Fun()}
            end
        end)
    end,
    Binder = Start(bind, fun() ->
        group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(album, S),
            <<"race.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(maps:get(attachment, S))
        )
    end),
    Deleter = Start(delete, fun() ->
        elib_pg:query(
            <<"SELECT fn_v5_intent_ordinary_delete($1, 'race')">>, [Path]
        )
    end),
    receive
        {ready, Binder} -> ok
    after 10000 -> error(bind_not_ready)
    end,
    receive
        {ready, Deleter} -> ok
    after 10000 -> error(delete_not_ready)
    end,
    Binder ! go,
    Deleter ! go,
    BindResult =
        receive
            {result, bind, BR} -> BR
        after 20000 -> timeout
        end,
    DeleteResult =
        receive
            {result, delete, DR} -> DR
        after 20000 -> timeout
        end,
    {ok, Row} = elib_pg:one(
        <<
            "SELECT lifecycle_state, (SELECT count(*) FROM group_album_photo"
            " WHERE photo_url=$1 AND status=1) AS refs FROM attachment WHERE path=$1"
        >>,
        [Path]
    ),
    case
        {BindResult, DeleteResult, maps:get(<<"lifecycle_state">>, Row), maps:get(<<"refs">>, Row)}
    of
        {{ok, _}, {error, #error{code = <<"23510">>}}, <<"live">>, 1} -> ok;
        {{error, _}, {ok, _}, <<"delete_intent">>, 0} -> ok;
        Other -> error({unsafe_bind_delete_race, Other})
    end.

bind_photo_after_delete_rejected(S) ->
    ok = seed_albums(S),
    Path = object_key(S, <<"deleted-first.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM workspace_attachment_retention WHERE attachment_id=$1">>,
            [maps:get(attachment, S)]
        ),
        ok
    end),
    {ok, _} = elib_pg:query(<<"UPDATE attachment SET gc_verified=true WHERE path=$1">>, [Path]),
    {ok, _} = elib_pg:query(<<"SELECT fn_v5_intent_ordinary_delete($1, 'first')">>, [Path]),
    ?assertMatch(
        {error, _},
        group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(album, S),
            <<"deleted.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(maps:get(attachment, S))
        )
    ),
    {ok, Row} = elib_pg:one(
        <<
            "SELECT count(*) AS n FROM group_album_photo"
            " WHERE photo_url=$1 AND status=1"
        >>,
        [Path]
    ),
    ?assertEqual(0, maps:get(<<"n">>, Row)).

bind_photo_string_album_id(S) ->
    AlbumKey = <<"album_1793775600000_123456">>,
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO group_album(id,group_id,album_id,album_name,creator_id,status)"
            " VALUES($1,$2,$3,'string-id',$4,1)"
        >>,
        [maps:get(album, S), maps:get(gid, S), AlbumKey, maps:get(uid, S)]
    ),
    Path = object_key(S, <<"string-id.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    {ok, _} = group_album_ds:bind_photo(
        maps:get(gid, S),
        maps:get(uid, S),
        AlbumKey,
        <<"string-id.png">>,
        Path,
        10,
        <<"image/png">>,
        hex64(maps:get(attachment, S))
    ),
    {ok, Row} = elib_pg:one(<<"SELECT album_id FROM group_album_photo WHERE photo_url=$1">>, [Path]),
    ?assertEqual(AlbumKey, maps:get(<<"album_id">>, Row)).

bind_photo_album_not_in_group_rejected(S) ->
    %% review P2 修复验证：相册属于其他群 → 拒绝（不再跨群塞照片）
    ok = seed_albums(S),
    Path = object_key(S, <<"photo2.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    ?assertEqual(
        {error, album_not_in_group},
        group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(other_album, S),
            <<"dv1-photo2.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(7)
        )
    ),
    %% 无行写入（他群相册零污染）
    {ok, Row} = elib_pg:one(
        <<"SELECT count(*) AS n FROM group_album_photo WHERE album_id=$1">>,
        [integer_to_binary(maps:get(other_album, S))]
    ),
    ?assertEqual(0, maps:get(<<"n">>, Row)),
    ok.

bind_photo_album_missing_rejected(S) ->
    %% 相册不存在（此前相册不存在也能绑成功——同缺陷的另一面）
    Path = object_key(S, <<"photo3.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    ?assertEqual(
        {error, album_not_found},
        group_album_ds:bind_photo(
            maps:get(gid, S),
            maps:get(uid, S),
            maps:get(missing_album, S),
            <<"dv1-photo3.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(8)
        )
    ),
    ok.

bind_photos_batch_per_item(S) ->
    ok = seed_albums(S),
    OkPath = object_key(S, <<"batch-ok.png">>),
    insert_direct_attachment(S, OkPath, maps:get(uid, S)),
    Items = [
        #{
            <<"object_key">> => OkPath,
            <<"album_id">> => maps:get(album, S),
            <<"photo_name">> => <<"dv1-b1.png">>,
            <<"photo_size">> => 10,
            <<"mime_type">> => <<"image/png">>,
            <<"sha256">> => hex64(maps:get(attachment, S))
        },
        %% 未 confirm：逐项 error，不影响第一项
        #{
            <<"object_key">> => object_key(S, <<"batch-missing.png">>),
            <<"album_id">> => maps:get(album, S),
            <<"photo_name">> => <<"dv1-b2.png">>,
            <<"photo_size">> => 10,
            <<"mime_type">> => <<"image/png">>,
            <<"sha256">> => hex64(6)
        }
    ],
    {ok, Results} = group_album_ds:bind_photos(maps:get(gid, S), maps:get(uid, S), Items, 50),
    ?assertEqual(2, length(Results)),
    [R1, R2] = Results,
    ?assertEqual(<<"ok">>, maps:get(<<"status">>, R1)),
    ?assertEqual(<<"error">>, maps:get(<<"status">>, R2)),
    ?assertEqual(not_confirmed, maps:get(<<"reason">>, R2)),
    ok.
