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

bind_test_() ->
    isolated_pg_fixture:test(fun bind_fixture/0).

bind_fixture() ->
    {setup, fun setup_database/0, fun cleanup_database/1, fun(Database) ->
        {foreach, fun() -> setup(Database) end, fun cleanup/1, [
            fun(S) -> fun() -> bind_file_without_confirmation_rejected(S) end end,
            fun(S) -> fun() -> bind_file_cross_subject_rejected(S) end end,
            fun(S) -> fun() -> bind_file_success_and_idempotent(S) end end,
            fun(S) -> fun() -> bind_photo_success_and_idempotent(S) end end,
            fun(S) -> fun() -> bind_photo_album_not_in_group_rejected(S) end end,
            fun(S) -> fun() -> bind_photo_album_missing_rejected(S) end end,
            fun(S) -> fun() -> bind_photos_batch_per_item(S) end end
        ]}
    end}.

setup_database() ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    Original = #{apps => [A || {A, _, _} <- application:which_applications()],
        pooler_env => application:get_env(pooler, pools)},
    try
        Marker = inttest_marker_db:provision(#{env_prefix => <<"ATTACH_BIND_INTTEST">>}),
        S = Marker#{original_runtime => Original},
        try
            application:set_env(pooler, pools, []),
            {ok, _} = application:ensure_all_started(pooler),
            {ok, _} = application:ensure_all_started(crypto),
            Opts = (maps:with([host, port, username, password], maps:get(server, S)))#{
                database => binary_to_list(maps:get(db, S)),
                codecs => [{epgsql_codec_rfc3339_bin, []}]},
            {ok, _} = pooler:new_pool(#{name => pgsql, init_count => 2, max_count => 4,
                start_mfa => {epgsql, connect, [Opts]}}),
            ok = elib_tsid:init(#{dc_id => 0, node_id => 1}),
            S
        catch Class0:Reason0:Stack0 ->
            CleanupResult = try cleanup_database(S)
                catch CleanupClass:CleanupReason -> {error, {CleanupClass, CleanupReason}} end,
            case CleanupResult of
                ok -> erlang:raise(Class0, Reason0, Stack0);
                _ -> error({fixture_setup_and_cleanup_failed,
                    {Class0, Reason0}, CleanupResult})
            end
        end
    catch Class:Reason:Stack ->
        restore_runtime(Original), erlang:raise(Class, Reason, Stack)
    end.

cleanup_database(S) ->
    %% Always attempt DB disposal, even when partial setup never made a pool.
    try
        PoolResult = try pooler:rm_pool(pgsql)
            catch PoolClass:PoolReason -> {error, {PoolClass, PoolReason}} end,
        DbResult = try inttest_marker_db:release(S)
            catch DbClass:DbReason -> {error, {DbClass, DbReason}} end,
        case {PoolResult, DbResult} of
            {ok, ok} -> ok;
            Other -> error({owned_fixture_cleanup_failed, Other})
        end
    after restore_runtime(maps:get(original_runtime, S))
    end.

restore_runtime(#{apps := OriginalApps, pooler_env := OriginalEnv}) ->
    NewApps = [A || {A, _, _} <- application:which_applications(),
        not lists:member(A, OriginalApps)],
    [application:stop(A) || A <- NewApps],
    case OriginalEnv of
        undefined -> application:unset_env(pooler, pools);
        {ok, Value} -> application:set_env(pooler, pools, Value)
    end,
    ok.

setup(_Database) ->
    Before = [{K, application:get_env(imboy, K)}
        || K <- [garage, attachment_storage_registry]],
    Base = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    EP = <<"http://127.0.0.1:39999">>,
    Ns = <<"dv1bind-", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    S = #{before_env => Before, ns => Ns, ep => EP,
        uid => Base, org => Base + 1, ws => Base + 2, gid => Base + 3,
        other_uid => Base + 4, other_gid => Base + 5, album => Base + 6,
        other_album => Base + 7, missing_album => Base + 8, attachment => Base + 9},
    try
        ok = application:set_env(imboy, garage,
            #{endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>}),
        Registry = attachment_storage:registry(),
        Roots = maps:get(roots, Registry, #{}),
        ok = application:set_env(imboy, attachment_storage_registry, Registry#{
            roots => Roots#{Ns => #{driver => <<"garage">>, namespace => Ns,
                endpoint => EP, container => <<"b">>, root_id => Ns}}}),
        ok = elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES($1,$2,'','127.0.0.1','owned-bind-fixture')">>,
                [Base, <<"bind-", (integer_to_binary(Base))/binary>>]),
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO organization(id,name,owner_id) VALUES($1,'owned-bind-fixture',$2)">>,
                [maps:get(org, S), Base]),
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO workspace(id,organization_id,name,owner_id) VALUES($1,$2,'owned-bind-fixture',$3)">>,
                [maps:get(ws, S), maps:get(org, S), Base]),
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES($1,$2,'','127.0.0.1','owned-bind-fixture')">>,
                [maps:get(other_uid, S), <<"bind-other-", (integer_to_binary(Base))/binary>>]),
            %% The production organization INSERT trigger creates its owner membership.
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
                [maps:get(org, S), maps:get(other_uid, S)]),
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'owner','active'),($1,$3,'member','active')">>,
                [maps:get(ws, S), Base, maps:get(other_uid, S)]),
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,title,status,scope,workspace_id) VALUES($1,$2,$2,'owned-bind-fixture',1,'workspace',$3),($4,$5,$5,'owned-bind-other',1,'workspace',$3)">>,
                [maps:get(gid, S), Base, maps:get(ws, S), maps:get(other_gid, S), maps:get(other_uid, S)]),
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO group_member(id,group_id,user_id,role,is_join,status) VALUES($1,$2,$3,5,true,1),($4,$5,$6,5,true,1)">>,
                [Base + 10, maps:get(gid, S), Base, Base + 11, maps:get(other_gid, S), maps:get(other_uid, S)]),
            ok
        end),
        S
    catch Class:Reason:Stack ->
        cleanup(S), erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{before_env := Before}) ->
    %% Rows live only until the enclosing marker DB is dropped. Never bypass
    %% bounded-purge authorization or suppress referential-integrity checks.
    [case Value of
        undefined -> application:unset_env(imboy, K);
        {ok, V} -> application:set_env(imboy, K, V)
     end || {K, Value} <- Before],
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
insert_direct_attachment(S, Path, Creator) ->
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

physical_of(Path) ->
    <<"direct-c/", Path/binary, "/p1">>.

%% album_id is varchar; bind_photo accepts the corresponding integer ID.
seed_albums(S) ->
    {ok, _} = elib_pg:query(
        <<"INSERT INTO group_album(id,group_id,album_id,album_name,creator_id,status)"
          " VALUES($1,$2,$3,'dv1-own',$4,1),($5,$6,$7,'dv1-other',$8,1)">>,
        [maps:get(album, S), maps:get(gid, S), integer_to_binary(maps:get(album, S)), maps:get(uid, S),
         maps:get(other_album, S), maps:get(other_gid, S), integer_to_binary(maps:get(other_album, S)), maps:get(other_uid, S)]),
    ok.

object_key(S, Suffix) ->
    <<"u", (integer_to_binary(maps:get(uid, S)))/binary, "/dv1/", Suffix/binary>>.

%% ---------------------------------------------------------------- cases

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
    {ok, FileId1} = group_file_ds:bind_file(
        maps:get(gid, S),
        maps:get(uid, S),
        <<"dv1-ok.png">>,
        <<"image/png">>,
        Path,
        10,
        hex64(3)
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
        hex64(3)
    ),
    ?assertEqual(FileId1, FileId2),
    {ok, GfRow2} = elib_pg:one(
        <<"SELECT count(*) AS n FROM group_file WHERE group_id=$1 AND file_url=$2">>,
        [maps:get(gid, S), Path]
    ),
    ?assertEqual(1, maps:get(<<"n">>, GfRow2)),
    ok.

bind_photo_success_and_idempotent(S) ->
    ok = seed_albums(S),
    Path = object_key(S, <<"photo.png">>),
    insert_direct_attachment(S, Path, maps:get(uid, S)),
    {ok, P1} = group_album_ds:bind_photo(
        maps:get(gid, S),
        maps:get(uid, S),
        maps:get(album, S),
        <<"dv1-photo.png">>,
        Path,
        10,
        <<"image/png">>,
        hex64(4)
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
        hex64(4)
    ),
    ?assertEqual(true, maps:get(<<"duplicate">>, P2)),
    ?assertEqual(maps:get(<<"id">>, P1), maps:get(<<"id">>, P2)),
    ok.

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
            <<"sha256">> => hex64(5)
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
