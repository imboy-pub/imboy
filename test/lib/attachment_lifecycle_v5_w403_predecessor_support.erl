%%% @doc W4-03 disk-predecessor-protocols 共享 fixture/断言 helper。
%%%
%%% 66 叶全部落在同一真实 disk root（registry 由套件 setup 注册）与同一
%%% marker DB 上；本模块只提供可复用的 SQL/文件原语，不含任何叶语义。
-module(attachment_lifecycle_v5_w403_predecessor_support).

-export([
    q/2,
    q/3,
    e/3,
    hex64/1,
    ns/0,
    container/0,
    disk_file/2,
    put_disk_file/3,
    disk_listing/1,
    assert_disk_intact/3,
    assert_no_queue_row/3,
    locator/1,
    token/0,
    unique_key/1,
    seed_tenants/1,
    seed_ordinary_job/5,
    seed_pending/5,
    seed_orphan/4,
    consume_ready_until/4
]).

q(Conn, Sql) -> q(Conn, Sql, []).

q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w403p_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

ns() -> <<"w403p-disk">>.
container() -> <<"c">>.

disk_file(Root, Key) ->
    filename:join([Root, binary_to_list(container()), binary_to_list(Key)]).

%% 真实 disk driver 写入（同 registry 派发路径，含 OS 锁与原子写）。
put_disk_file(Root, Key, Body) ->
    ok = attachment_storage_disk:put(ns(), container(), Key, Body),
    {ok, Body} = file:read_file(disk_file(Root, Key)),
    ok.

%% 递归文件清单（相对路径 + 字节数）；用于证明入口失败时零删除/零新增。
disk_listing(Root) ->
    lists:sort(collect_files(Root, "")).

collect_files(Dir, Prefix) ->
    case file:list_dir(Dir) of
        {ok, Names} ->
            lists:flatmap(
                fun(Name) ->
                    Path = filename:join(Dir, Name),
                    Rel = filename:join(Prefix, Name),
                    case filelib:is_dir(Path) of
                        true -> collect_files(Path, Rel);
                        false -> [{Rel, filelib:file_size(Path)}]
                    end
                end,
                Names
            );
        {error, _} ->
            []
    end.

assert_disk_intact(Root, Key, Body) ->
    Path = disk_file(Root, Key),
    case file:read_file(Path) of
        {ok, Body} -> ok;
        Other -> erlang:error({w403p_disk_bytes_changed, Path, Other})
    end,
    case attachment_storage_disk:head(ns(), container(), Key) of
        {ok, #{size := Size}} when Size =:= byte_size(Body) -> ok;
        Other2 -> erlang:error({w403p_disk_head_mismatch, Path, Other2})
    end.

assert_no_queue_row(Conn, Key, Label) ->
    case e(Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]) of
        {ok, _, [{0}]} -> ok;
        Other -> erlang:error({w403p_queue_row_leaked, Label, Key, Other})
    end.

locator(Key) ->
    {ok, #{locator_digest := Digest}} =
        attachment_storage:identity(ns(), container(), Key),
    Digest.

token() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

unique_key(Prefix) ->
    <<Prefix/binary, "-", (binary:encode_hex(crypto:strong_rand_bytes(8), lowercase))/binary>>.

seed_tenants(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w403p-u', 'w403p', '', 1, 0, '127.0.0.1', 'w403p', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id) VALUES (9101,'w403p-org',101)"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES (9101,9102,'w403p-ws',101,'{}') ON CONFLICT (id) DO NOTHING"
    ),
    ok.

%% disk 队列行：位置三元组与 registry/identity 一致，worker 才能 frozen_location 通过。
seed_ordinary_job(Conn, Path, Attempts, State, LocatorDigest) ->
    q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path,bucket,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,job_state,next_retry_at,attempts,last_error_code)"
        " VALUES ($1,'w403p-bucket','disk',$2,'c',$3,$4,now(),$5,$6)"
        " ON CONFLICT (path) DO NOTHING",
        [
            Path,
            ns(),
            LocatorDigest,
            State,
            Attempts,
            case State of
                blocked -> <<"max_attempts">>;
                _ -> null
            end
        ]
    ).

%% registered + 能力窗耗尽（sweep 资格）的 pending 会话。
seed_pending(Conn, Key, AgeHours, Mime, LocatorDigest) ->
    Declaration = iolist_to_binary([
        "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
        "\"declaration\":{\"mime\":\"",
        Mime,
        "\"}}"
    ]),
    q(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'w403p-bucket','private',101,"
        "gen_random_uuid(),$2::jsonb,now(),now() - ($3 || ' hours')::interval,"
        "'disk',$4,'c',$5)",
        [Key, Declaration, integer_to_binary(AgeHours), ns(), LocatorDigest]
    ).

%% gc_verified 明文无绑定孤儿（orphan 资格）。
seed_orphan(Conn, Key, Id, LocatorDigest) ->
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,group_file_id,file_hash256,referer_time)"
        " VALUES ($1,$2,'private',101,'disk',$3,'c',$4,true,NULL,NULL,$5,0)"
        " ON CONFLICT (id) DO NOTHING",
        [Id, Key, ns(), LocatorDigest, hex64(crypto:hash(sha256, Key))]
    ).

%% 普通队列 claim 无 path 过滤：循环消化无关 ready 行，直到拿到目标或耗尽。
consume_ready_until(Conn, Key, N, Label) ->
    case N of
        0 ->
            erlang:error({w403p_target_job_not_claimed, Key, Label});
        _ ->
            T = token(),
            case e(Conn, "SELECT out_path FROM fn_v5_claim_ordinary($1)", [T]) of
                {ok, _, [{Key}]} ->
                    ok = attachment_gc_repo:v5_ack(Conn, Key, T, true),
                    ok;
                {ok, _, [{Other}]} ->
                    ok = attachment_gc_repo:v5_ack(Conn, Other, T, true),
                    consume_ready_until(Conn, Key, N - 1, Label);
                {error, empty} ->
                    consume_ready_until(Conn, Key, N - 1, Label);
                Other ->
                    erlang:error({w403p_claim_failed, Label, Other})
            end
    end.
