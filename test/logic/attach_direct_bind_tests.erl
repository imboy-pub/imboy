-module(attach_direct_bind_tests).
-moduledoc "step-11：direct 确认引用绑定（群文件 bind_file / 相册 bind_photo、批量逐项，T27）".
%%%
% 前置 fixture（持久测试基础设施，见 resources.json）：
%   workspace 990901 / "group" 990901（scope=workspace, ws=990901）/
%   group_member(990901, 990901)。
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

-define(GID, 990901).
-define(UID, 990901).

bind_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun bind_file_without_confirmation_rejected/0,
        fun bind_file_cross_subject_rejected/0,
        fun bind_file_success_and_idempotent/0,
        fun bind_photo_success_and_idempotent/0,
        fun bind_photo_album_not_in_group_rejected/0,
        fun bind_photo_album_missing_rejected/0,
        fun bind_photos_batch_per_item/0
    ]}.

setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Booted = eunit_runner:eunit_setup(),
    EP = <<"http://127.0.0.1:39999">>,
    ok = application:set_env(
        imboy,
        garage,
        #{endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>}
    ),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots => Roots#{
            <<"g">> => #{
                driver => <<"garage">>,
                namespace => <<"g">>,
                endpoint => EP,
                container => <<"b">>,
                root_id => <<"g">>
            }
        }
    }),
    #{booted => Booted, before_env => Before}.

cleanup(#{booted := Booted, before_env := Before}) ->
    %% 只清本套件 uid 的 attachment/group_file/group_album_photo 行；
    %% group/workspace/member fixture 持久保留（resources.json 登记）。
    try
        elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<"SET LOCAL session_replication_role = replica">>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"DELETE FROM attachment WHERE path LIKE 'u990901/dv1/%'">>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"DELETE FROM group_file WHERE group_id=$1 AND file_name LIKE 'dv1-%'">>,
                [?GID]
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"DELETE FROM group_album_photo WHERE group_id=$1 AND photo_name LIKE 'dv1-%'">>,
                [?GID]
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"DELETE FROM group_album_photo WHERE group_id=990903">>,
                []
            ),
            %% album fixture（990902 本群 / 990903 他群）
            {ok, _} = elib_pg:query(
                Conn,
                <<"DELETE FROM group_album WHERE album_id IN ('990902','990903')">>,
                []
            ),
            ok
        end),
        eunit_runner:eunit_cleanup(Booted)
    catch
        _:_ -> eunit_runner:eunit_cleanup(Booted)
    after
        [
            case Value of
                undefined -> application:unset_env(imboy, K);
                {ok, V} -> application:set_env(imboy, K, V)
            end
         || {K, Value} <- Before
        ]
    end.

%% ---------------------------------------------------------------- helpers

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).

locator() ->
    attachment_storage:locator_digest(<<"g">>, #{
        driver => <<"garage">>,
        root_id => <<"g">>,
        endpoint => <<"http://127.0.0.1:39999">>,
        container => <<"b">>
    }).

%% 插入 direct confirm 后的 group 附件行（confirmed 引用资格的唯一来源）
insert_direct_attachment(Path, Creator) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url, size, info,"
            " referer_time, creator_user_id, scope, scope_ref, cipher, status, updated_at, created_at,"
            " storage_driver, storage_namespace, storage_container, storage_locator_sha256,"
            " storage_physical_key)"
            " VALUES ($1,$2,'image/png','.png','dv1-x.png',$3,$3,10,'{}',1,$4,'group',$5,null,1,"
            " NOW(),NOW(),'garage','g','b',$6,$7)"
        >>,
        [
            creator_path_id(Path),
            hex64(creator_path_id(Path)),
            Path,
            Creator,
            integer_to_binary(?GID),
            locator(),
            physical_of(Path)
        ]
    ),
    ok.

physical_of(Path) ->
    <<"direct-c/", Path/binary, "/p1">>.

creator_path_id(Path) ->
    erlang:phash2(Path, 16#FFFFFFFF).

%% 相册 fixture：AlbumId=990902 属于本群 990901；990903 属于他群 990903
%% （album_id 列为 varchar，参数按字符串化匹配）。
seed_albums() ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO group_album (id, group_id, album_id, album_name, creator_id, status)"
            " VALUES (990992, 990901, '990902', 'dv1-own', 990901, 1),"
            " (990993, 990903, '990903', 'dv1-other', 990903, 1)"
        >>,
        []
    ),
    ok.

%% ---------------------------------------------------------------- cases

bind_file_without_confirmation_rejected() ->
    %% 未 confirm：attachment 无行 → 拒绝（T27）
    ?assertEqual(
        {error, not_confirmed},
        group_file_ds:bind_file(
            ?GID,
            ?UID,
            <<"dv1-no.bin">>,
            <<"application/octet-stream">>,
            <<"u990901/dv1/nope">>,
            10,
            hex64(1)
        )
    ),
    ok.

bind_file_cross_subject_rejected() ->
    Path = <<"u990901/dv1/cross.png">>,
    %% creator 是别人
    insert_direct_attachment(Path, 990802),
    ?assertEqual(
        {error, cross_subject},
        group_file_ds:bind_file(
            ?GID,
            ?UID,
            <<"dv1-cross.png">>,
            <<"image/png">>,
            Path,
            10,
            hex64(2)
        )
    ),
    ok.

bind_file_success_and_idempotent() ->
    Path = <<"u990901/dv1/ok.png">>,
    insert_direct_attachment(Path, ?UID),
    {ok, FileId1} = group_file_ds:bind_file(
        ?GID,
        ?UID,
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
        [?GID, Path]
    ),
    ?assertEqual(1, maps:get(<<"n">>, GfRow)),
    %% 业务 file_id 一致（file_url 语义 = object_key）
    {ok, BizRow} = elib_pg:one(
        <<"SELECT file_id FROM group_file WHERE group_id=$1 AND file_url=$2">>,
        [?GID, Path]
    ),
    ?assertEqual(FileId1, maps:get(<<"file_id">>, BizRow)),
    %% 重复绑定：同一 file_id，group_file 行不再增加（幂等不重复引用）
    {ok, FileId2} = group_file_ds:bind_file(
        ?GID,
        ?UID,
        <<"dv1-ok.png">>,
        <<"image/png">>,
        Path,
        10,
        hex64(3)
    ),
    ?assertEqual(FileId1, FileId2),
    {ok, GfRow2} = elib_pg:one(
        <<"SELECT count(*) AS n FROM group_file WHERE group_id=$1 AND file_url=$2">>,
        [?GID, Path]
    ),
    ?assertEqual(1, maps:get(<<"n">>, GfRow2)),
    ok.

bind_photo_success_and_idempotent() ->
    ok = seed_albums(),
    Path = <<"u990901/dv1/photo.png">>,
    insert_direct_attachment(Path, ?UID),
    {ok, P1} = group_album_ds:bind_photo(
        ?GID,
        ?UID,
        990902,
        <<"dv1-photo.png">>,
        Path,
        10,
        <<"image/png">>,
        hex64(4)
    ),
    ?assertEqual(false, maps:get(<<"duplicate">>, P1, false)),
    {ok, P2} = group_album_ds:bind_photo(
        ?GID,
        ?UID,
        990902,
        <<"dv1-photo.png">>,
        Path,
        10,
        <<"image/png">>,
        hex64(4)
    ),
    ?assertEqual(true, maps:get(<<"duplicate">>, P2)),
    ?assertEqual(maps:get(<<"id">>, P1), maps:get(<<"id">>, P2)),
    ok.

bind_photo_album_not_in_group_rejected() ->
    %% review P2 修复验证：相册属于其他群 → 拒绝（不再跨群塞照片）
    ok = seed_albums(),
    Path = <<"u990901/dv1/photo2.png">>,
    insert_direct_attachment(Path, ?UID),
    ?assertEqual(
        {error, album_not_in_group},
        group_album_ds:bind_photo(
            ?GID,
            ?UID,
            990903,
            <<"dv1-photo2.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(7)
        )
    ),
    %% 无行写入（他群相册零污染）
    {ok, Row} = elib_pg:one(
        <<"SELECT count(*) AS n FROM group_album_photo WHERE album_id='990903'">>, []
    ),
    ?assertEqual(0, maps:get(<<"n">>, Row)),
    ok.

bind_photo_album_missing_rejected() ->
    %% 相册不存在（此前相册不存在也能绑成功——同缺陷的另一面）
    Path = <<"u990901/dv1/photo3.png">>,
    insert_direct_attachment(Path, ?UID),
    ?assertEqual(
        {error, album_not_found},
        group_album_ds:bind_photo(
            ?GID,
            ?UID,
            999909,
            <<"dv1-photo3.png">>,
            Path,
            10,
            <<"image/png">>,
            hex64(8)
        )
    ),
    ok.

bind_photos_batch_per_item() ->
    ok = seed_albums(),
    OkPath = <<"u990901/dv1/batch-ok.png">>,
    insert_direct_attachment(OkPath, ?UID),
    Items = [
        #{
            <<"object_key">> => OkPath,
            <<"album_id">> => 990902,
            <<"photo_name">> => <<"dv1-b1.png">>,
            <<"photo_size">> => 10,
            <<"mime_type">> => <<"image/png">>,
            <<"sha256">> => hex64(5)
        },
        %% 未 confirm：逐项 error，不影响第一项
        #{
            <<"object_key">> => <<"u990901/dv1/batch-missing.png">>,
            <<"album_id">> => 990902,
            <<"photo_name">> => <<"dv1-b2.png">>,
            <<"photo_size">> => 10,
            <<"mime_type">> => <<"image/png">>,
            <<"sha256">> => hex64(6)
        }
    ],
    {ok, Results} = group_album_ds:bind_photos(?GID, ?UID, Items, 50),
    ?assertEqual(2, length(Results)),
    [R1, R2] = Results,
    ?assertEqual(<<"ok">>, maps:get(<<"status">>, R1)),
    ?assertEqual(<<"error">>, maps:get(<<"status">>, R2)),
    ?assertEqual(not_confirmed, maps:get(<<"reason">>, R2)),
    ok.
