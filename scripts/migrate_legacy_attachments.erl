%%%-------------------------------------------------------------------
%%% @doc
%%% 迁移脚本：将存量明文附件（cipher IS NULL）加密为 AES-256-GCM。
%%%
%%% 背景：
%%%   E2EE 启用前上传的附件以明文存储在 Garage S3 中，DB 中 cipher=NULL。
%%%   本脚本逐条读取明文 → AES-256-GCM 加密 → 写回 Garage → 更新 cipher。
%%%
%%% 密钥管理：
%%%   每附件使用随机 content key，经 postgre_aes_key 加密后存入
%%%   attachment.legacy_key 列。客户端下载时 view_url 返回该 key。
%%%
%%% 幂等：
%%%   只处理 cipher IS NULL 的行。已加密或已迁移的跳过。
%%%   中途失败可重跑，已处理的不会重复。
%%% @end
%%%-------------------------------------------------------------------
-module(migrate_legacy_attachments).
-include_lib("eunit/include/eunit.hrl").

-export([run/0, run/1]).

%% 每批处理条数
-define(BATCH_SIZE, 10).

%% 密钥派生 salt
-define(KEY_SALT, <<"imboy-legacy-attach-migrate-v1">>).

%% @doc 运行迁移（默认处理全部 cipher IS NULL 的附件）。
run() -> run(all).

%% @doc 运行迁移。
%% Mode = all | {count, pos_integer()}（仅处理前 N 条，用于测试）。
run(Mode) ->
    ok = ensure_legacy_key_column(),
    Rows = find_plaintext_attachments(Mode),
    Count = length(Rows),
    io:format("[migrate_legacy_attachments] 发现 ~p 条明文附件待迁移~n", [Count]),
    Results = lists:map(fun migrate_one/1, Rows),
    Ok = length([ok || ok <- Results]),
    Error = length([E || E = {error, _} <- Results]),
    io:format("[migrate_legacy_attachments] 完成: ~p OK / ~p FAIL / ~p total~n", [Ok, Error, Count]),
    case Error of
        0 -> ok;
        _ -> {error, {Error, "部分附件迁移失败"}}
    end.

%% @doc 确保 legacy_key 列存在。
ensure_legacy_key_column() ->
    {ok, _, _, [{0}]} = elib_pg:query(
        "SELECT 1 FROM information_schema.columns "
        "WHERE table_schema='public' AND table_name='attachment' "
        "AND column_name='legacy_key'",
        []
    ),
    ok;
ensure_legacy_key_column() ->
    {ok, _, _, _} = elib_pg:query(
        "ALTER TABLE public.attachment "
        "ADD COLUMN IF NOT EXISTS legacy_key text DEFAULT NULL",
        []
    ),
    ok.

%% @doc 查找所有明文附件（cipher IS NULL）。
find_plaintext_attachments(all) ->
    {ok, _, _, Rows} = elib_pg:query(
        "SELECT id, path, bucket, file_hash256, mime_type "
        "FROM public.attachment "
        "WHERE cipher IS NULL "
        "ORDER BY id",
        []
    ),
    Rows;
find_plaintext_attachments({count, N}) ->
    {ok, _, _, Rows} = elib_pg:query(
        "SELECT id, path, bucket, file_hash256, mime_type "
        "FROM public.attachment "
        "WHERE cipher IS NULL "
        "ORDER BY id LIMIT $1",
        [N]
    ),
    Rows.

%% @doc 迁移单条附件。
migrate_one({Id, ObjectKey, Bucket, _Hash, _MimeType}) ->
    try
        io:format("  [~p] 处理 attachment id=~p key=~s~n", [self(), Id, ObjectKey]),
        %% 1. 从 Garage 读取明文
        case read_from_garage(Bucket, ObjectKey) of
            {ok, Plaintext} ->
                %% 2. 生成随机 content key
                ContentKey = crypto:strong_rand_bytes(32),
                %% 3. AES-256-GCM 加密
                {ok, CiphertextB64} = elib_cipher:aes_gcm_encrypt(Plaintext, ContentKey),
                %% 4. 写回 Garage（覆盖原 object key）
                ok = write_to_garage(Bucket, ObjectKey, CiphertextB64),
                %% 5. 用 postgre_aes_key 加密 content key 以备后用
                MasterKey = get_master_key(),
                {ok, EncryptedKey} = elib_cipher:aes_gcm_encrypt(ContentKey, MasterKey),
                KeyB64 = base64:encode(EncryptedKey),
                %% 6. 更新 DB
                {ok, _, _, [{1}]} = elib_pg:query(
                    "UPDATE public.attachment "
                    "SET cipher = 'AES-256-GCM', legacy_key = $1 "
                    "WHERE id = $2 AND cipher IS NULL",
                    [KeyB64, Id]
                ),
                io:format("  [~p] 完成 attachment id=~p~n", [self(), Id]),
                ok;
            {error, Reason} ->
                io:format("  [~p] 读取失败 attachment id=~p: ~p~n", [self(), Id, Reason]),
                {error, {Id, Reason}}
        end
    catch
        Class:Reason:Stack ->
            io:format("  [~p] 异常 attachment id=~p: ~p:~p~n  ~p~n", [self(), Id, Class, Reason, Stack]),
            {error, {Id, {Class, Reason}}}
    end.

%% @doc 从 Garage 读取对象。
read_from_garage(Bucket, ObjectKey) ->
    %% 使用 presign_get 签名 URL 后通过 httpc GET
    %% 或直接使用 Garage API（取决于 elib_oss 实现）
    Url = elib_oss:presign_get_for_key(Bucket, ObjectKey, 120),
    {ok, {{_, 200, _}, _Headers, Body}} = elib_http:get(Url, [], []),
    {ok, Body}.

%% @doc 写入对象到 Garage。
write_to_garage(Bucket, ObjectKey, CiphertextB64) ->
    %% 使用 presign_put 签名 URL 后通过 httpc PUT
    MimeType = <<"application/octet-stream">>,
    Url = elib_oss:presign_put_for_key(Bucket, ObjectKey, MimeType, 120),
    {ok, {{_, 200, _}, _Headers, _Body}} = elib_http:put(Url, [], [], CiphertextB64),
    ok.

%% @doc 获取部署主密钥（postgre_aes_key）。
get_master_key() ->
    case config_ds:env(postgre_aes_key, <<>>) of
        <<>> -> erlang:error(postgre_aes_key_not_configured);
        Key -> crypto:hash(sha256, <<?KEY_SALT/binary, Key/binary>>)
    end.