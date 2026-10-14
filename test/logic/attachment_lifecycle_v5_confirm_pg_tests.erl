-module(attachment_lifecycle_v5_confirm_pg_tests).
%%%
% W2-02 真实证据（confirm 幂等与兼容回读 + 内容不可变）：
%
% 真 PG（eunit 池，fn_v5_* 受限函数真路径）+ 真 Garage 字节往返
% （W202_GARAGE_* 环境变量注入；缺省整套跳过=0 用例，gate 按精确
% 通过数断言，空转不算绿）。
%
% 入口口径：confirm 一律走 attach_logic:confirm/5（ordinary）/
% enterprise_asset_logic:confirm_tx/3（enterprise internal），即生产
% handler 的逻辑层入口；session 登记/pin 走同一 repo 受限路径。
%
% 字节裁决（02 §4「HEAD 本身不充分」）：elib_oss:object_sha256 GET
% 全量字节；覆盖 overwrite_between_stat_and_confirm（stat 后换字节
% confirm 必须拒绝）等 HEAD 无法证明的场景。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(UID, 9).
-define(MIME, <<"text/plain">>).
%% enterprise 域租户经 eb fixture 建（真实 org 外键链）；app 行按需种子。

%% ===================================================================
%% 生成器：无 garage 环境时不产生用例（gate 以精确计数兜底空转）；
%% eunit_setup 启动 app + DB 池（纯套件单跑时 app env 需显式 boot）。
%% ===================================================================

%% 慢测试超时下推配方（eunit_setup.hrl 注意事项）：timeout 放在 setup 体内
%% 的测试表述上；真库+真字节往返按用例给足额。
ordinary_test_() ->
    case garage_ready() of
        true ->
            {setup,
                fun() ->
                    Booted = eunit_runner:eunit_setup(),
                    garage_apply(),
                    Booted
                end,
                fun eunit_runner:eunit_cleanup/1, fun(_State) ->
                    [
                        {timeout, 180, ?_test(t_sequential_100_replays())},
                        {timeout, 60, ?_test(t_concurrent_two_confirms())},
                        {timeout, 60, ?_test(t_confirmed_legacy_request())},
                        {timeout, 60, ?_test(t_unknown_legacy_pending())},
                        {timeout, 60, ?_test(t_old_capability_put_b_rejected())},
                        {timeout, 60, ?_test(t_overwrite_between_stat_and_confirm())},
                        {timeout, 60, ?_test(t_hash_mismatch_no_confirm())},
                        {timeout, 60, ?_test(t_crash_after_pin_before_put_resume())},
                        {timeout, 60, ?_test(t_inflight_same_content_after_confirm())},
                        {timeout, 60, ?_test(t_ciphertext_only_no_plaintext_hash())},
                        {timeout, 60, ?_test(t_scope_conflict_rejected())},
                        {timeout, 60, ?_test(t_cipher_conflict_rejected())}
                    ]
                end};
        false ->
            []
    end.

enterprise_test_() ->
    case garage_ready() of
        true ->
            {setup,
                fun() ->
                    Booted = eunit_runner:eunit_setup(),
                    garage_apply(),
                    Booted
                end,
                fun eunit_runner:eunit_cleanup/1, fun(_State) ->
                    [
                        {timeout, 180, ?_test(ent_sequential_100_replays())},
                        {timeout, 60, ?_test(ent_concurrent_two_confirms())},
                        {timeout, 60, ?_test(ent_confirmed_legacy_request())},
                        {timeout, 60, ?_test(ent_unknown_legacy_pending())}
                    ]
                end};
        false ->
            []
    end.

garage_ready() ->
    case
        {
            os:getenv("W202_GARAGE_ENDPOINT"),
            os:getenv("W202_GARAGE_ACCESS_KEY"),
            os:getenv("W202_GARAGE_SECRET_KEY")
        }
    of
        {EP, AK, SK} when EP =/= false, AK =/= false, SK =/= false ->
            true;
        _ ->
            false
    end.

%% 必须在 eunit_setup（app boot）之后执行：boot 会按 {env, VAR, Default}
%% 重新装配 garage 配置（默认 GKxxxx），先设会被覆盖（真 403 实证）。
garage_apply() ->
    EP = os:getenv("W202_GARAGE_ENDPOINT"),
    AK = os:getenv("W202_GARAGE_ACCESS_KEY"),
    SK = os:getenv("W202_GARAGE_SECRET_KEY"),
    ok = application:set_env(imboy, garage, #{
        endpoint => list_to_binary(EP),
        bucket => <<"imboy">>,
        access_key => list_to_binary(AK),
        secret_key => list_to_binary(SK)
    }).

%% ===================================================================
%% ordinary（个人域：u<uid>/ key + attach_logic:confirm/5）
%% ===================================================================

new_ordinary_key() ->
    <<"u9/file/w202/", (unique())/binary, "/t.bin">>.

unique() ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(6)),
    <<Ts/binary, "_", Rand/binary>>.

%% ck_v5_pending_content 要求 64 位小写 hex。
hex_lower(Digest) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Digest)]).

%% V5 会话登记 + 内容 pin + 真 Garage PUT（A 字节）。
seed_session(Key, Bytes) ->
    Bucket = elib_oss:get_bucket(<<"private">>),
    Cid = elib_uuid:gen_v7(),
    {ok, _} =
        attach_pending_repo:register_v5(
            Key, Bucket, <<"private">>, ?UID, Cid, #{<<"declaration">> => #{<<"mime">> => ?MIME}}
        ),
    ok = elib_oss:put_object(Bucket, Key, Bytes, ?MIME),
    {ok, Sha, Size} = elib_oss:object_sha256(Bucket, Key),
    ok =
        attach_pending_repo:register_content_pin(Key, #{
            <<"version">> => 1,
            <<"sha256">> => Sha,
            <<"size_bytes">> => integer_to_binary(Size),
            <<"mime">> => ?MIME
        }),
    {Bucket, Sha, Size}.

confirm_ordinary(Key) ->
    attach_logic:confirm(?UID, Key, <<"private">>, undefined, #{}).

attachment_row(Key) ->
    {ok, Row} =
        elib_pg:one(
            <<"SELECT id, referer_time, file_hash256, size FROM attachment WHERE path=$1">>,
            [Key]
        ),
    Row.

session_state(Key) ->
    {ok, Row} =
        elib_pg:one(
            <<"SELECT session_state, confirmed_attachment_id FROM attach_pending WHERE object_key=$1">>,
            [Key]
        ),
    Row.

pending_count(Key) ->
    {ok, Row} =
        elib_pg:one(<<"SELECT COUNT(*)::int AS n FROM attach_pending WHERE object_key=$1">>, [Key]),
    maps:get(<<"n">>, Row).

attachment_count(Key) ->
    {ok, Row} =
        elib_pg:one(<<"SELECT COUNT(*)::int AS n FROM attachment WHERE path=$1">>, [Key]),
    maps:get(<<"n">>, Row).

t_sequential_100_replays() ->
    Key = new_ordinary_key(),
    Bytes = <<"w2-02 sequential replays payload">>,
    {_, _, _} = seed_session(Key, Bytes),
    {ok, First} = confirm_ordinary(Key),
    #{<<"attachment_id">> := AttId} = First,
    %% 100 次同请求重放：同一 attachment_id、row=1、referer 不增
    lists:foreach(
        fun(_) ->
            {ok, Replay} = confirm_ordinary(Key),
            ?assertEqual(AttId, maps:get(<<"attachment_id">>, Replay))
        end,
        lists:seq(1, 100)
    ),
    ?assertEqual(1, attachment_count(Key)),
    Row = attachment_row(Key),
    ?assertEqual(1, maps:get(<<"referer_time">>, Row)),
    #{<<"session_state">> := <<"confirmed">>} = session_state(Key),
    %% 会话确认后仍在（V5 终态保留，W4 GC 回收），能力签发不再发生
    ?assertEqual(1, pending_count(Key)).

t_concurrent_two_confirms() ->
    Key = new_ordinary_key(),
    {_, _, _} = seed_session(Key, <<"w2-02 concurrent confirms payload">>),
    Parent = self(),
    Workers = [
        spawn_link(fun() ->
            Parent ! {w202_confirm, self(), confirm_ordinary(Key)}
        end)
     || _ <- lists:seq(1, 2)
    ],
    Results = [
        receive
            {w202_confirm, W, R} -> {W, R}
        end
     || W <- Workers
    ],
    lists:foreach(
        fun({_W, R}) -> {ok, _} = R end,
        Results
    ),
    [{_, {ok, First}}, {_, {ok, Second}}] = Results,
    ?assertEqual(
        maps:get(<<"attachment_id">>, First), maps:get(<<"attachment_id">>, Second)
    ),
    ?assertEqual(1, attachment_count(Key)),
    ?assertEqual(1, maps:get(<<"referer_time">>, attachment_row(Key))).

t_confirmed_legacy_request() ->
    %% 旧客户端兼容回读：无 pending 会话、附件已转正 → confirm 重放
    %% 接受且返回同一对象（不新建会话、referer 不变）。
    Key = new_ordinary_key(),
    Bucket = elib_oss:get_bucket(<<"private">>),
    ok = elib_oss:put_object(Bucket, Key, <<"legacy confirmed payload">>, ?MIME),
    ok =
        elib_pg:with_tx(fun(Conn) ->
            attachment_ds:save(
                Conn,
                elib_dt:now(),
                ?UID,
                [
                    #{
                        <<"file_hash256">> => <<"legacy">>,
                        <<"mime_type">> => ?MIME,
                        <<"name">> => <<"t.bin">>,
                        <<"path">> => Key,
                        <<"url">> => Key,
                        <<"size">> => 22,
                        <<"scope">> => <<"private">>
                    }
                ]
            )
        end),
    {ok, First} = confirm_ordinary(Key),
    #{<<"attachment_id">> := AttId} = First,
    {ok, Replay} = confirm_ordinary(Key),
    ?assertEqual(AttId, maps:get(<<"attachment_id">>, Replay)),
    ?assertEqual(1, attachment_count(Key)),
    ?assertEqual(1, maps:get(<<"referer_time">>, attachment_row(Key))).

t_unknown_legacy_pending() ->
    %% 未登记/未上传对象 confirm：稳定失败（object_not_found），不落库、
    %% 不静默删除任何对象。
    Key = new_ordinary_key(),
    {error, object_not_found} = confirm_ordinary(Key),
    ?assertEqual(0, attachment_count(Key)).

t_old_capability_put_b_rejected() ->
    %% 旧能力不能把已确认 A 改为 B：网关 pin 对不同 receipt 拒绝（409 语义
    %% 在 W2-01 gateway eunit 已证）；此处证 DB 面——不同 receipt pin 拒绝
    %% 且已确认会话的 confirm 重放仍返回原 id、元数据 hash 不变。
    Key = new_ordinary_key(),
    BytesA = <<"w2-02 payload A">>,
    {Bucket, ShaA, _} = seed_session(Key, BytesA),
    {ok, First} = confirm_ordinary(Key),
    #{<<"attachment_id">> := AttId} = First,
    %% 换字节 B 的 pin → content_mismatch（provider PUT=0 同语义）
    {error, content_mismatch} =
        attach_pending_repo:register_content_pin(Key, #{
            <<"version">> => 1,
            <<"sha256">> => hex_lower(crypto:hash(sha256, <<"payload B">>)),
            <<"size_bytes">> => <<"99">>,
            <<"mime">> => ?MIME
        }),
    {ok, Replay} = confirm_ordinary(Key),
    ?assertEqual(AttId, maps:get(<<"attachment_id">>, Replay)),
    Row = attachment_row(Key),
    ?assertEqual(1, maps:get(<<"referer_time">>, Row)),
    %% 对象字节仍是 A（在途写未污染已确认对象）
    {ok, ShaNow, _} = elib_oss:object_sha256(Bucket, Key),
    ?assertEqual(ShaA, ShaNow).

t_overwrite_between_stat_and_confirm() ->
    %% HEAD（stat）之后、confirm 之前换字节：真实读流必须识破，拒绝确认。
    Key = new_ordinary_key(),
    BytesA = <<"w2-02 overwrite scenario A">>,
    {Bucket, _, _} = seed_session(Key, BytesA),
    {ok, _} = elib_oss:head_object(Bucket, Key),
    ok = elib_oss:put_object(Bucket, Key, <<"w2-02 overwrite scenario B">>, ?MIME),
    {error, content_hash_mismatch} = confirm_ordinary(Key),
    ?assertEqual(0, attachment_count(Key)),
    #{<<"session_state">> := <<"registered">>} = session_state(Key).

t_hash_mismatch_no_confirm() ->
    Key = new_ordinary_key(),
    Bucket = elib_oss:get_bucket(<<"private">>),
    Cid = elib_uuid:gen_v7(),
    {ok, _} =
        attach_pending_repo:register_v5(
            Key, Bucket, <<"private">>, ?UID, Cid, #{<<"declaration">> => #{<<"mime">> => ?MIME}}
        ),
    ok = elib_oss:put_object(Bucket, Key, <<"real bytes">>, ?MIME),
    %% pin 声明与真实字节不一致（脏快照/换字节）
    ok =
        attach_pending_repo:register_content_pin(Key, #{
            <<"version">> => 1,
            <<"sha256">> => hex_lower(crypto:hash(sha256, <<"other bytes">>)),
            <<"size_bytes">> => integer_to_binary(byte_size(<<"real bytes">>)),
            <<"mime">> => ?MIME
        }),
    {error, content_hash_mismatch} = confirm_ordinary(Key),
    ?assertEqual(0, attachment_count(Key)).

t_crash_after_pin_before_put_resume() ->
    %% pin 已 COMMIT、对象 PUT 未发生（崩溃恢复面）：confirm 稳定失败；
    %% 同字节恢复 PUT 后 confirm 成功，重放同一 id。
    Key = new_ordinary_key(),
    Bucket = elib_oss:get_bucket(<<"private">>),
    Cid = elib_uuid:gen_v7(),
    Bytes = <<"w2-02 crash resume payload">>,
    {ok, _} =
        attach_pending_repo:register_v5(
            Key, Bucket, <<"private">>, ?UID, Cid, #{<<"declaration">> => #{<<"mime">> => ?MIME}}
        ),
    ok =
        attach_pending_repo:register_content_pin(Key, #{
            <<"version">> => 1,
            <<"sha256">> => hex_lower(crypto:hash(sha256, Bytes)),
            <<"size_bytes">> => integer_to_binary(byte_size(Bytes)),
            <<"mime">> => ?MIME
        }),
    {error, object_not_found} = confirm_ordinary(Key),
    ?assertEqual(0, attachment_count(Key)),
    %% 恢复：同 receipt 幂等 pin 重放 + 真 PUT → confirm 成功
    ok =
        attach_pending_repo:register_content_pin(Key, #{
            <<"version">> => 1,
            <<"sha256">> => hex_lower(crypto:hash(sha256, Bytes)),
            <<"size_bytes">> => integer_to_binary(byte_size(Bytes)),
            <<"mime">> => ?MIME
        }),
    ok = elib_oss:put_object(Bucket, Key, Bytes, ?MIME),
    {ok, First} = confirm_ordinary(Key),
    #{<<"attachment_id">> := AttId} = First,
    {ok, Replay} = confirm_ordinary(Key),
    ?assertEqual(AttId, maps:get(<<"attachment_id">>, Replay)).

t_inflight_same_content_after_confirm() ->
    Key = new_ordinary_key(),
    Bytes = <<"w2-02 inflight same content">>,
    {Bucket, _, _} = seed_session(Key, Bytes),
    {ok, #{<<"attachment_id">> := AttId}} = confirm_ordinary(Key),
    %% 确认后同字节在途重写：对象不变，confirm 幂等回读
    ok = elib_oss:put_object(Bucket, Key, Bytes, ?MIME),
    {ok, Replay} = confirm_ordinary(Key),
    ?assertEqual(AttId, maps:get(<<"attachment_id">>, Replay)),
    {ok, Sha, _} = elib_oss:object_sha256(Bucket, Key),
    Row = attachment_row(Key),
    ?assertEqual(Sha, maps:get(<<"file_hash256">>, Row)).

t_ciphertext_only_no_plaintext_hash() ->
    %% 密文对象：服务端只见密文 pin/密文字节，confirm 全程无需明文 hash
    %% （E2EE-061：明文哈希只在客户端 descriptor 内）。
    Key = new_ordinary_key(),
    CipherBytes = crypto:strong_rand_bytes(48),
    Bucket = elib_oss:get_bucket(<<"private">>),
    Cid = elib_uuid:gen_v7(),
    {ok, _} =
        attach_pending_repo:register_v5(
            Key, Bucket, <<"private">>, ?UID, Cid, #{<<"declaration">> => #{<<"mime">> => ?MIME}}
        ),
    ok = elib_oss:put_object(Bucket, Key, CipherBytes, <<"application/octet-stream">>),
    {ok, Sha, Size} = elib_oss:object_sha256(Bucket, Key),
    ok =
        attach_pending_repo:register_content_pin(Key, #{
            <<"version">> => 1,
            <<"sha256">> => Sha,
            <<"size_bytes">> => integer_to_binary(Size),
            <<"mime">> => <<"application/octet-stream">>
        }),
    {ok, #{<<"attachment_id">> := _}} =
        attach_logic:confirm(?UID, Key, <<"private">>, undefined, #{
            <<"cipher">> => <<"AES-256-GCM">>
        }),
    Row = attachment_row(Key),
    ?assertEqual(Sha, maps:get(<<"file_hash256">>, Row)),
    ?assertEqual(1, maps:get(<<"referer_time">>, Row)).

t_scope_conflict_rejected() ->
    Key = new_ordinary_key(),
    {_, _, _} = seed_session(Key, <<"w2-02 scope conflict payload">>),
    %% 会话登记 private，confirm 声明 c2c（同默认桶、can_upload 以合法
    %% conv ref 放行）：持锁复核 pending scope 冲突拒绝。
    {error, scope_conflict} =
        attach_logic:confirm(?UID, Key, <<"c2c">>, <<"c2c:9:10">>, #{}),
    ?assertEqual(0, attachment_count(Key)).

t_cipher_conflict_rejected() ->
    Key = new_ordinary_key(),
    {_, _, _} = seed_session(Key, <<"w2-02 cipher conflict payload">>),
    {ok, #{<<"attachment_id">> := _}} = confirm_ordinary(Key),
    %% 同对象重放改声明加密形态：拒绝（cipher 冲突）
    {error, cipher_conflict} =
        attach_logic:confirm(?UID, Key, <<"private">>, undefined, #{
            <<"cipher">> => <<"AES-256-GCM">>
        }).

%% ===================================================================
%% enterprise（内部应用：eoa/ key + enterprise_asset_logic:confirm_tx/3）
%% ===================================================================

new_ent_key(Org, App) ->
    Prefix =
        <<"eoa/", (integer_to_binary(Org))/binary, "/", (integer_to_binary(App))/binary, "/w202/">>,
    <<Prefix/binary, (unique())/binary, "/t.bin">>.

%% ent 电池公共装置：fixture 建 org（真外键链）+ 幂等种子应用行。
ent_setup() ->
    Scope = eb_asset_it_lib:new_scope(),
    {Org, _Ws} = eb_asset_it_lib:tenant(Scope),
    App = eb_pg_test_fixture:id(),
    ok = seed_ent_app(Org, App),
    Ctx = #{organization_id => Org, application_id => App},
    {Ctx, Scope}.

ent_confirm(Key, Ctx) ->
    elib_pg:with_tx(fun(Conn) ->
        enterprise_asset_logic:confirm_tx(Conn, Ctx, #{
            object_key => Key, file_hash256 => undefined
        })
    end).

seed_ent_session(Key, Bytes, Principal) ->
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    Cid = elib_uuid:gen_v7(),
    {ok, _} =
        attach_pending_repo:register_v5(
            Key, Bucket, <<"enterprise">>, 0, Cid, #{
                <<"subject">> => #{
                    <<"kind">> => <<"application">>,
                    <<"principal">> => Principal
                },
                <<"declaration">> => #{<<"mime">> => ?MIME}
            }
        ),
    ok = elib_oss:put_object(Bucket, Key, Bytes, ?MIME),
    {ok, Sha, Size} = elib_oss:object_sha256(Bucket, Key),
    ok =
        attach_pending_repo:register_content_pin(Key, #{
            <<"version">> => 1,
            <<"sha256">> => Sha,
            <<"size_bytes">> => integer_to_binary(Size),
            <<"mime">> => ?MIME
        }),
    {Bucket, Sha, Size}.

%% content_policy_tx fail-closed 需要应用行；按 org+app 幂等种子。
seed_ent_app(Org, App) ->
    elib_pg:with_tx(fun(Conn) ->
        {ok, _} =
            elib_pg:query(
                Conn,
                <<
                    "INSERT INTO enterprise_application"
                    " (id, organization_id, application_key, name, status)"
                    " VALUES ($1, $2, $3, 'w202 app', 'active')"
                    " ON CONFLICT (organization_id, id) DO NOTHING"
                >>,
                [App, Org, <<"w202-", (integer_to_binary(App))/binary>>]
            ),
        ok
    end).

ent_attachment_row(Key) ->
    {ok, Row} =
        elib_pg:one(<<"SELECT id, referer_time FROM attachment WHERE path=$1">>, [Key]),
    Row.

ent_audit_count(AttId) ->
    {ok, Row} =
        elib_pg:one(
            <<
                "SELECT COUNT(*)::int AS n FROM enterprise_audit_event"
                " WHERE resource_type='attachment' AND resource_id=$1"
                " AND action='file.confirmed'"
            >>,
            [AttId]
        ),
    maps:get(<<"n">>, Row).

ent_sequential_100_replays() ->
    {Ctx, Scope} = ent_setup(),
    try
        Key = new_ent_key(maps:get(organization_id, Ctx), maps:get(application_id, Ctx)),
        {_, _, _} = seed_ent_session(
            Key,
            <<"w2-02 ent sequential payload">>,
            maps:get(application_id, Ctx)
        ),
        {ok, First} = ent_confirm(Key, Ctx),
        AttId = maps:get(<<"file_id">>, First),
        lists:foreach(
            fun(_) ->
                {ok, Replay} = ent_confirm(Key, Ctx),
                ?assertEqual(AttId, maps:get(<<"file_id">>, Replay))
            end,
            lists:seq(1, 100)
        ),
        Row = ent_attachment_row(Key),
        ?assertEqual(1, maps:get(<<"referer_time">>, Row)),
        ?assertEqual(1, ent_audit_count(maps:get(<<"id">>, Row))),
        #{<<"session_state">> := <<"confirmed">>} = session_state(Key)
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

ent_concurrent_two_confirms() ->
    {Ctx, Scope} = ent_setup(),
    try
        Key = new_ent_key(maps:get(organization_id, Ctx), maps:get(application_id, Ctx)),
        {_, _, _} = seed_ent_session(
            Key,
            <<"w2-02 ent concurrent payload">>,
            maps:get(application_id, Ctx)
        ),
        Parent = self(),
        Workers = [
            spawn_link(fun() -> Parent ! {w202_ent, self(), ent_confirm(Key, Ctx)} end)
         || _ <- lists:seq(1, 2)
        ],
        Results = [
            receive
                {w202_ent, W, R} -> {W, R}
            end
         || W <- Workers
        ],
        [{_, {ok, First}}, {_, {ok, Second}}] = Results,
        ?assertEqual(maps:get(<<"file_id">>, First), maps:get(<<"file_id">>, Second)),
        Row = ent_attachment_row(Key),
        ?assertEqual(1, maps:get(<<"referer_time">>, Row)),
        ?assertEqual(1, ent_audit_count(maps:get(<<"id">>, Row)))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

ent_confirmed_legacy_request() ->
    {Ctx, Scope} = ent_setup(),
    Org = maps:get(organization_id, Ctx),
    App = maps:get(application_id, Ctx),
    %% 兼容回读：legacy eoa/ 附件已转正且 pending 已销账 → confirm 重放
    %% 同一 file_id（响应丢失恢复，不要求新建会话）。
    Key = new_ent_key(Org, App),
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    ok = elib_oss:put_object(Bucket, Key, <<"ent legacy payload">>, ?MIME),
    ScopeRef = enterprise_asset_repo:scope_ref(Org, App),
    {ok, _, _} =
        elib_pg:with_tx(fun(Conn) ->
            enterprise_asset_repo:confirm_save_tx(Conn, Key, ?MIME, 18, #{
                file_hash256 => <<"legacy">>, scope_ref => ScopeRef
            })
        end),
    try
        {ok, First} = ent_confirm(Key, Ctx),
        AttId = maps:get(<<"file_id">>, First),
        {ok, Replay} = ent_confirm(Key, Ctx),
        ?assertEqual(AttId, maps:get(<<"file_id">>, Replay)),
        ?assertEqual(1, maps:get(<<"referer_time">>, ent_attachment_row(Key)))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

ent_unknown_legacy_pending() ->
    {Ctx, Scope} = ent_setup(),
    try
        %% 从未登记的 eoa/ key：稳定 not_presigned，不落库不删除。
        Key = new_ent_key(maps:get(organization_id, Ctx), maps:get(application_id, Ctx)),
        {error, {<<"invalid_request">>, not_presigned}} = ent_confirm(Key, Ctx),
        {ok, Row} =
            elib_pg:one(<<"SELECT COUNT(*)::int AS n FROM attachment WHERE path=$1">>, [Key]),
        ?assertEqual(0, maps:get(<<"n">>, Row))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.
