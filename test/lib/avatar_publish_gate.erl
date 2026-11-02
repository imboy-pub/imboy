-module(avatar_publish_gate).
-moduledoc "V3-13 G13 gate：Admin 头像直传发布全链真 Garage（register→declare→grant→PUT→finalize→publish Copy）。".
%%%
%%% 断言组（G13 合同）：
%%%   1) 成功旅程：application 主体 direct 全链 CONFIRMED → publish 走服务端
%%%      跨桶 Copy（零正文过 BEAM）→ 目标对象字节与上传逐字节一致。
%%%   2) 发布前匿名不可达：publish 前目标键 GET 404（公开只发生在业务许可后）。
%%%   3) 失败反例：Human 主体会话不可经 Admin 面发布；未 CONFIRMED 不可发布。
%%%   4) 重复发布幂等：同键二次 publish 成功（CopyObject 覆盖语义）。
%%% 说明：gate 单桶形态（public_bucket=imboy）近似生产双桶；生产匿名读由
%%% 部署层（website/CDN）提供，gate 以签名 GET 验证目标对象内容。
-export([main/0]).

-define(APP, <<"990910">>).
-define(ORG, <<"990911">>).
-define(HUMAN_UID, 990915).
-define(GARAGE, "http://127.0.0.1:13910").

main() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    {ok, _} = application:ensure_all_started(imboy),
    Ep = list_to_binary(os:getenv("A0_ENDPOINT", ?GARAGE)),
    Cfg = #{
        endpoint => Ep,
        bucket => <<"imboy">>,
        public_bucket => <<"imboy">>,
        access_key => list_to_binary(os:getenv("A0_AK")),
        secret_key => list_to_binary(os:getenv("A0_SK")),
        region => <<"garage">>
    },
    application:set_env(imboy, garage, Cfg),
    %% registry：a0-ns 真 garage；public 上传命名空间映射（avatar application 面）
    Base = attachment_storage:registry(),
    application:set_env(imboy, attachment_storage_registry, Base#{
        roots => (maps:get(roots, Base, #{}))#{
            <<"a0-root">> => #{
                driver => <<"garage">>,
                namespace => <<"a0-ns">>,
                endpoint => Ep,
                container => <<"imboy">>,
                access_key => maps:get(access_key, Cfg),
                secret_key => maps:get(secret_key, Cfg),
                region => <<"garage">>
            }
        },
        upload_namespaces => (maps:get(upload_namespaces, Base, #{}))#{<<"public">> => <<"a0-ns">>}
    }),
    try
        run_assertions(Cfg)
    after
        cleanup_fixture()
    end.

run_assertions(Cfg) ->
    Body = <<"G13-AVATAR-PNG-BYTES-", 0:(32 * 8)>>,
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    %% 1) register→declare→grant（application 主体，creator=0 合成）
    {ok, #{object_key := Key}} = attach_direct_logic:register_application(
        ?APP, ?ORG, <<"public">>, undefined, <<"gate-avatar.png">>
    ),
    io:format("session_key: ~ts~n", [Key]),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Body), <<"image/png">>),
    {ok, #{grant := G, attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    %% 2) 客户端 staging PUT（真 presign URL + checksum 头）
    {ok, 200} = staging_put(G, Body),
    %% 3) finalize：服务端 Copy staging→candidate + checksum proof（真 Copy）
    Fin = attach_direct_logic:finalize(Key, No),
    io:format("finalize_diag: ~ts~n", [io_lib:format("~p", [Fin])]),
    case Fin of
        {error, winner_conflict} ->
            %% 诊断：confirm_winner 事务逐段重放（每段独立小事务，定位 23510 源）
            {ok, [A0]} = elib_pg:query(
                <<
                    "SELECT attempt_no, source_key, target_key, target_namespace, target_container"
                    " FROM attach_direct_attempt WHERE object_key=$1 ORDER BY attempt_no DESC LIMIT 1"
                >>,
                [Key]
            ),
            No2 = maps:get(<<"attempt_no">>, A0),
            Tk = maps:get(<<"target_key">>, A0),
            R1 = elib_pg:with_tx(
                fun(C) ->
                    elib_pg:query(
                        C,
                        <<"SELECT fn_dv1_attempt_transition($1,$2,'DISPATCHED','COPY_COMPLETED')">>,
                        [Key, No2]
                    )
                end,
                [{reraise, false}]
            ),
            io:format("step1_outcome: ~ts~n", [io_lib:format("~p", [R1])]),
            R2 = elib_pg:with_tx(
                fun(C) ->
                    elib_pg:query(
                        C,
                        <<
                            "UPDATE attach_direct_attempt SET attempt_state='PROOF_VERIFIED',"
                            " proof_sha256=$3, proof_size=$4, proof_received_at=now(), updated_at=now()"
                            " WHERE object_key=$1 AND attempt_no=$2 AND attempt_state='COPY_COMPLETED' RETURNING attempt_no"
                        >>,
                        [Key, No2, Sha, byte_size(Body)]
                    )
                end,
                [{reraise, false}]
            ),
            io:format("step2_proof: ~ts~n", [io_lib:format("~p", [R2])]),
            {ok, RootRec2} = attachment_storage:resolve(<<"a0-ns">>),
            LocDigest = attachment_storage:locator_digest(<<"a0-ns">>, RootRec2),
            R3 = elib_pg:with_tx(
                fun(C) ->
                    elib_pg:query(
                        C,
                        <<
                            "INSERT INTO attachment(file_hash256, mime_type, ext, name, path, url, size,"
                            " info, referer_time, creator_user_id, scope, cipher, status, storage_driver,"
                            " storage_namespace, storage_container, storage_locator_sha256, storage_physical_key)"
                            " VALUES ($1,'image/png','.png','gate-avatar.png',$2,$2,$3,'{}',1,0,'public',null,1,"
                            " 'garage',$4,$5,$6,$2) ON CONFLICT (path) DO NOTHING RETURNING id"
                        >>,
                        [
                            Sha,
                            Key,
                            byte_size(Body),
                            maps:get(<<"target_namespace">>, A0),
                            maps:get(<<"target_container">>, A0),
                            LocDigest
                        ]
                    )
                end,
                [{reraise, false}]
            ),
            io:format("step3_attach_row: ~ts~n", [io_lib:format("~p", [R3])]),
            R4 = elib_pg:with_tx(
                fun(C) ->
                    elib_pg:query(C, <<"SELECT fn_dv1_winner_pin($1,$2,$3,$4)">>, [
                        Key, No2, Tk, Sha
                    ])
                end,
                [{reraise, false}]
            ),
            io:format("step4_winner_pin: ~ts~n", [io_lib:format("~p", [R4])]),
            erlang:error({gate_abort, winner_conflict});
        _ ->
            ok
    end,
    {ok, #{state := <<"CONFIRMED">>}} = Fin,
    io:format("finalize_confirmed: ok~n"),
    %% 4) 发布前目标键不可达（公开只发生在业务许可后）
    PublicKey = Key,
    {error, not_found} = public_head(Cfg, PublicKey),
    io:format("pre_publish_target_absent: ok~n"),
    %% 5) publish：服务端跨桶 Copy（零 BEAM 正文）
    {ok, #{public_url := _PubUrl, public_key := PublicKey}} =
        attach_direct_logic:publish_avatar(Key, <<"agent-gate">>, <<>>),
    %% 6) 目标对象字节与上传逐字节一致（签名 GET 验证内容）
    {ok, Body} = public_get(Cfg, PublicKey),
    io:format("publish_copy_bytes_exact: ~pB ok~n", [byte_size(Body)]),
    %% 7) 重复发布幂等（CopyObject 覆盖语义；不产生错误引用）
    {ok, _} = attach_direct_logic:publish_avatar(Key, <<"agent-gate">>, <<>>),
    {ok, Body} = public_get(Cfg, PublicKey),
    io:format("republish_idempotent: ok~n"),
    %% 8) 反例：Human 主体会话不可经 Admin 面发布（T28 身份域隔离；
    %%    publish 先拒未确认态——主体断言的 CONFIRMED 态细节由
    %%   attach_direct_avatar_tests 的 human_session_rejected_on_admin_face 锁定）
    {ok, HumanKey} = human_session_key(),
    {error, not_confirmed} =
        attach_direct_logic:publish_avatar(HumanKey, <<"agent-x">>, <<>>),
    ok =
        case attach_direct_logic:require_application_session(HumanKey) of
            {error, not_application_session} -> ok;
            Other2 -> erlang:error({human_subject_assert, Other2})
        end,
    io:format("human_session_rejected: ok (~ts)~n", [HumanKey]),
    %% 9) 反例：未 CONFIRMED 不可发布
    {ok, #{object_key := Key2}} = attach_direct_logic:register_application(
        ?APP, ?ORG, <<"public">>, undefined, <<"gate-avatar-2.png">>
    ),
    {error, not_confirmed} =
        attach_direct_logic:publish_avatar(Key2, <<"agent-x">>, <<>>),
    io:format("unconfirmed_rejected: ok~n"),
    io:format("fixture_key: ~ts (synthetic app990910 band)~n", [Key]),
    io:format("AVATAR_PUBLISH_GATE_ALL_PASS~n"),
    halt(0).

staging_put(#{url := Url, required_headers := Headers}, Body) ->
    %% presign 已把 content-type 纳入签名——请求 ContentType 必须逐字使用
    %% descriptor 的值（不能默认 octet-stream，否则签名头不匹配 403）。
    Hs = [{string:lowercase(binary_to_list(K)), binary_to_list(V)} || {K, V} <- Headers],
    {_, Ctype} = lists:keyfind("content-type", 1, Hs),
    case
        httpc:request(
            put,
            {binary_to_list(Url), Hs, Ctype, Body},
            [{timeout, 30000}, {autoredirect, false}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, S, _}, _, _}} when S >= 200, S < 300 -> {ok, 200};
        {ok, {{_, S, _}, _, B}} -> erlang:error({staging_put_failed, S, B});
        {error, R} -> erlang:error({staging_put_error, R})
    end.

signed_get(Cfg, Key) ->
    Url = elib_s3_sign:presign_signed_url_at(
        <<"GET">>,
        maps:get(endpoint, Cfg),
        <<"imboy">>,
        Key,
        [],
        calendar:universal_time(),
        120,
        Cfg
    ),
    httpc:request(
        get,
        {binary_to_list(Url), []},
        [{timeout, 15000}, {autoredirect, false}],
        [{body_format, binary}]
    ).

public_head(Cfg, Key) ->
    case signed_get(Cfg, Key) of
        {ok, {{_, 404, _}, _, _}} -> {error, not_found};
        {ok, {{_, 200, _}, _, _}} -> {ok, exists};
        {ok, {{_, S, _}, _, _}} -> {error, {status, S}};
        {error, R} -> {error, R}
    end.

public_get(Cfg, Key) ->
    case signed_get(Cfg, Key) of
        {ok, {{_, 200, _}, _, Body}} -> {ok, Body};
        {ok, {{_, S, _}, _, B}} -> erlang:error({public_get_failed, S, B});
        {error, R} -> erlang:error({public_get_error, R})
    end.

%% 最小 Human 会话（T28 反例）：user 主体 register（经通用 register 入口），
%% 只需存在一行会话——publish 的主体断言在状态断言之前拒绝。
human_session_key() ->
    {ok, #{object_key := K}} = attach_direct_logic:register(
        ?HUMAN_UID, <<"public">>, undefined, undefined, <<"avatar-human.png">>
    ),
    {ok, K}.

cleanup_fixture() ->
    %% 合成段清理（app990910/990911、creator=0/uid 合成；V5 守卫下的
    %% pending/attempt 会话行；Garage 对象残留由 run 收口按资源账登记）
    catch elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(
            Conn, <<"SET LOCAL session_replication_role = replica">>, []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<
                "DELETE FROM attach_direct_attempt WHERE object_key LIKE 'app990910/%'"
                " OR object_key LIKE 'app990911/%' OR object_key LIKE 'u990915/%'"
            >>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<
                "DELETE FROM attachment WHERE (creator_user_id = 0 AND path LIKE 'app990910/%')"
                " OR (creator_user_id = 990915 AND (path LIKE 'app990911/%' OR path LIKE 'u990915/%'))"
            >>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<
                "DELETE FROM attach_pending WHERE (creator_user_id = 0 AND object_key LIKE 'app990910/%')"
                " OR (creator_user_id = 990911 AND (object_key LIKE 'app990911/%' OR object_key LIKE 'u990915/%'))"
            >>,
            []
        ),
        ok
    end).
