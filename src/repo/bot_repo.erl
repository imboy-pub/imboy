-module(bot_repo).

%%%
% Bot 元数据仓库 / Bot metadata repository
% 表 bot（见 priv/migrations/00000070_bot）：user_id 主键 = bot 的 user.id（account_type=3）
% 所有 SQL 经 elib_pg 参数化；commands/permissions/events 为 jsonb。
%%%

-export([tablename/0]).
-export([create/1]).
-export([create_tx/2]).
-export([find/1]).
-export([find_by_username/1]).
-export([find_by_token/1]).
-export([update/2]).
-export([set_status/2]).
-export([page/2]).
-export([page_by_owner/3]).
-export([search/3]).
-export([has_exchange/2]).
%% WH-01：凭证安全
-export([find_by_api_token/1, set_api_token_credential/2, prepare_credentials/2]).
-export([set_verify_token_enc/2, get_verify_token/1, list_encrypted_bot_ids/0]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"bot">>).

%% @doc 创建 Bot 行
%% Data 键：user_id(必填), name, username, owner_uid, webhook_url,
%%          api_token_digest/api_token_prefix/verify_token_enc,
%%          commands, permissions, events, is_public, status
-spec create(map()) -> {ok, [map()]} | {error, term()}.
create(#{user_id := _UserId, name := _Name, owner_uid := _OwnerUid} = Data) ->
    {Sql, Params} = create_sql(Data),
    case elib_pg:query(Sql, Params) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:create error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 事务内创建 Bot 行（DS 层 with_tx Fun(Conn) 内调用，TX-01）；
%% SQL 与 create/1 完全同源（create_sql/1），仅复用调用方事务连接。
%% username 唯一约束（bot_username_key）在事务内即时拦截重复注册。
-spec create_tx(any(), map()) -> {ok, [map()]} | {error, term()}.
create_tx(Conn, #{user_id := UserId, name := _Name, owner_uid := _OwnerUid} = Data) ->
    {Sql, Params} = create_sql(Data),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:create_tx user_id=~p error ~p~n", [UserId, Reason]),
            {error, Reason}
    end.

%% @doc create 的 SQL 构造（create/1 与 create_tx/2 共享，保证两入口永不漂移）
-spec create_sql(map()) -> {binary(), [term()]}.
create_sql(#{user_id := UserId, name := Name, owner_uid := OwnerUid} = Data) ->
    Tb = tablename(),
    Username = maps:get(username, Data, <<>>),
    Description = maps:get(description, Data, <<>>),
    Avatar = maps:get(avatar, Data, <<>>),
    WebhookUrl = maps:get(webhook_url, Data, <<>>),
    ApiTokenDigest = maps:get(api_token_digest, Data, <<>>),
    ApiTokenPrefix = maps:get(api_token_prefix, Data, <<>>),
    VerifyTokenEnc = maps:get(verify_token_enc, Data, <<>>),
    Commands = maps:get(commands, Data, <<"[]">>),
    Permissions = maps:get(permissions, Data, <<"[]">>),
    Events = maps:get(events, Data, <<"[]">>),
    IsPublic = maps:get(is_public, Data, false),
    Status = maps:get(status, Data, 1),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (user_id, name, username, description, avatar, owner_uid,"
            "  webhook_url, api_token, verify_token, api_token_digest, api_token_prefix,"
            "  verify_token_enc, token_migrated, commands, permissions, events,"
            "  is_public, status, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,'','',$8,$9,$10,true,"
            "  $11::jsonb,$12::jsonb,$13::jsonb,$14,$15,NOW(),NOW())"
            " RETURNING user_id">>,
    {Sql, [
        UserId,
        Name,
        Username,
        Description,
        Avatar,
        OwnerUid,
        WebhookUrl,
        ApiTokenDigest,
        ApiTokenPrefix,
        VerifyTokenEnc,
        Commands,
        Permissions,
        Events,
        IsPublic,
        Status
    ]}.

%% @doc 按 user_id 查单个 Bot 行
-spec find(integer()) -> {ok, map()} | {error, notfound | term()}.
find(UserId) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT user_id, name, username, description, avatar, owner_uid,"
            " webhook_url, commands, permissions, events,"
            " is_public, status, created_at, updated_at FROM ",
            Tb/binary,
            " WHERE user_id = $1"
        >>,
    case elib_pg:query(Sql, [UserId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按 username 查找 Bot
-spec find_by_username(binary()) -> {ok, map()} | {error, notfound | term()}.
find_by_username(Username) ->
    Tb = tablename(),
    Sql = <<
        "SELECT user_id, name, username, description, avatar, owner_uid,"
        " webhook_url, is_public, status FROM ",
        Tb/binary,
        " WHERE username = $1"
    >>,
    case elib_pg:query(Sql, [Username]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 兼容入口：认证已经统一走摘要索引，不再读取明文列。
-spec find_by_token(binary()) -> {ok, map()} | {error, notfound | term()}.
find_by_token(Token) ->
    find_by_api_token(Token).

%% @doc 更新 Bot 字段（部分更新）
-spec update(integer(), map()) -> {ok, [map()]} | {error, term()}.
update(UserId, Data) when is_integer(UserId), is_map(Data) ->
    Tb = tablename(),
    Fields = update_fields(Data),
    case Fields of
        [] ->
            {ok, []};
        _ ->
            Assignments = [
                update_assignment(Column, Cast, Index)
             || {{Column, _Value, Cast}, Index} <- lists:zip(
                    Fields, lists:seq(1, length(Fields))
                )
            ],
            WhereIndex = length(Fields) + 1,
            Sql = iolist_to_binary([
                <<"UPDATE ">>,
                Tb,
                <<" SET ">>,
                join_binary(Assignments),
                <<", updated_at = NOW()">>,
                <<" WHERE user_id = $">>,
                integer_to_binary(WhereIndex),
                <<" RETURNING user_id">>
            ]),
            Params = [Value || {_Column, Value, _Cast} <- Fields] ++ [UserId],
            case elib_pg:query(Sql, Params) of
                {ok, Rows} ->
                    {ok, Rows};
                {error, Reason} ->
                    ?ERROR_LOG("bot_repo:update user_id=~p error ~p~n", [UserId, Reason]),
                    {error, Reason}
            end
    end.

update_fields(Data) ->
    [
        {Column, Value, Cast}
     || {Key, Column, Cast} <- [
            {name, <<"name">>, <<>>},
            {username, <<"username">>, <<>>},
            {description, <<"description">>, <<>>},
            {avatar, <<"avatar">>, <<>>},
            {webhook_url, <<"webhook_url">>, <<>>},
            {commands, <<"commands">>, <<"::jsonb">>},
            {permissions, <<"permissions">>, <<"::jsonb">>},
            {events, <<"events">>, <<"::jsonb">>},
            {is_public, <<"is_public">>, <<>>}
        ],
        {ok, Value} <- [maps:find(Key, Data)]
    ].

update_assignment(Column, Cast, Index) ->
    iolist_to_binary([Column, <<" = $">>, integer_to_binary(Index), Cast]).

join_binary([]) ->
    <<>>;
join_binary([First | Rest]) ->
    lists:foldl(
        fun(Item, Acc) -> <<Acc/binary, ", ", Item/binary>> end,
        First,
        Rest
    ).

%% @doc 设置 Bot 状态（-1=deleted, 0=disabled, 1=active）
-spec set_status(integer(), -1 | 0 | 1) -> {ok, term()} | {error, term()}.
set_status(UserId, Status) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => Status, updated_at => elib_dt:now()}, <<"user_id = $1">>, [
        UserId
    ]).

%% @doc 分页列出所有 Bot（管理后台）
-spec page(pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page(Page, Size) when Page > 0, Size > 0 ->
    Tb = tablename(),
    UTb = user_repo:tablename(),
    From = <<" FROM ", Tb/binary, " b JOIN ", UTb/binary, " u ON u.id = b.user_id">>,
    case elib_pg:query(<<"SELECT count(*) AS total", From/binary>>, []) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql = <<
                "SELECT b.user_id, b.name, b.username, b.description, b.owner_uid,"
                " b.is_public, b.status, u.nickname, u.avatar",
                From/binary,
                " ORDER BY b.created_at DESC LIMIT ",
                (integer_to_binary(Size))/binary,
                " OFFSET ",
                (integer_to_binary(Offset))/binary
            >>,
            case elib_pg:query(ListSql, []) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("bot_repo:page list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:page count error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 按所有者分页列出 Bot
-spec page_by_owner(pos_integer(), pos_integer(), integer()) -> {ok, map()} | {error, term()}.
page_by_owner(Page, Size, OwnerUid) when Page > 0, Size > 0 ->
    Tb = tablename(),
    Utb = user_repo:tablename(),
    From = <<" FROM ", Tb/binary, " b JOIN ", Utb/binary, " u ON u.id = b.user_id">>,
    Where = <<" WHERE b.owner_uid = $1">>,
    case elib_pg:query(<<"SELECT count(*) AS total", From/binary, Where/binary>>, [OwnerUid]) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql = <<
                "SELECT b.user_id, b.name, b.username, b.description,"
                " b.is_public, b.status, b.webhook_url, b.events, b.created_at,"
                " u.nickname, u.avatar",
                From/binary,
                Where/binary,
                " ORDER BY b.created_at DESC LIMIT ",
                (integer_to_binary(Size))/binary,
                " OFFSET ",
                (integer_to_binary(Offset))/binary
            >>,
            case elib_pg:query(ListSql, [OwnerUid]) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("bot_repo:page_by_owner list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:page_by_owner count error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 搜索 Bot（按名称或 username 模糊匹配）
-spec search(binary(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
search(Keyword, Page, Size) when Page > 0, Size > 0 ->
    Tb = tablename(),
    UTb = user_repo:tablename(),
    Like = <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>,
    From = <<" FROM ", Tb/binary, " b JOIN ", UTb/binary, " u ON u.id = b.user_id">>,
    Where = <<" WHERE b.status = 1 AND (b.name ILIKE $1 OR b.username ILIKE $1)">>,
    case elib_pg:query(<<"SELECT count(*) AS total", From/binary, Where/binary>>, [Like]) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql = <<
                "SELECT b.user_id, b.name, b.username, b.description,"
                " b.is_public, b.owner_uid, u.nickname, u.avatar",
                From/binary,
                Where/binary,
                " ORDER BY b.created_at DESC LIMIT ",
                (integer_to_binary(Size))/binary,
                " OFFSET ",
                (integer_to_binary(Offset))/binary
            >>,
            case elib_pg:query(ListSql, [Like]) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("bot_repo:search list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:search count error ~p~n", [Reason]),
            {error, Reason}
    end.

-spec empty_page(pos_integer(), pos_integer()) -> map().
empty_page(Page, Size) ->
    #{total => 0, page => Page, size => Size, list => []}.

%% @doc 判断 Bot 与用户之间是否存在历史 C2C 消息（双向任一即算）
%% 用于 send_message 的防骚扰前置校验：用户未先发起对话则 Bot 不可主动私信
%% （Telegram started-chat 范式）。命中 i_c2c_fromid / i_c2c_toid 索引，EXISTS 短路。
-spec has_exchange(integer(), integer()) -> boolean().
has_exchange(BotId, UserId) ->
    Sql = <<
        "SELECT EXISTS ("
        "  SELECT 1 FROM public.msg_c2c"
        "  WHERE (from_id = $1 AND to_id = $2)"
        "     OR (from_id = $2 AND to_id = $1)"
        ") AS ok"
    >>,
    case elib_pg:query(Sql, [BotId, UserId]) of
        {ok, [#{<<"ok">> := Ok} | _]} ->
            Ok;
        {error, Reason} ->
            ?ERROR_LOG("bot_repo:has_exchange ~p:~p error ~p~n", [BotId, UserId, Reason]),
            %% 查询失败按无历史处理（fail closed，阻止 Bot 主动私信）
            false
    end.

%% ===================================================================
%% WH-01：凭证安全（api_token 摘要认证 + verify_token AEAD 可认证加密）
%% ===================================================================

-spec digest_hex(binary()) -> binary().
digest_hex(Token) when is_binary(Token) ->
    binary:encode_hex(crypto:hash(sha256, Token), lowercase).

%% @doc 生成可直接进入 Bot 创建事务的凭证字段；失败时不产生任何 DB 写入。
-spec prepare_credentials(binary(), binary()) -> {ok, map()} | {error, no_key | term()}.
prepare_credentials(ApiToken, VerifyToken) when
    is_binary(ApiToken),
    byte_size(ApiToken) >= 8,
    is_binary(VerifyToken),
    VerifyToken =/= <<>>
->
    case encrypt_verify_token(VerifyToken) of
        {ok, VerifyTokenEnc} ->
            {ok, #{
                api_token_digest => digest_hex(ApiToken),
                api_token_prefix => binary:part(ApiToken, 0, 8),
                verify_token_enc => VerifyTokenEnc
            }};
        {error, _} = Error ->
            Error
    end;
prepare_credentials(_, _) ->
    {error, invalid_credentials}.

%% @doc 按 api_token 摘要精确查找 Bot（认证路径；明文不再入库/比对）。
%% 兼容期：旧明文 token 行由迁移 00000092 回填摘要，认证只走摘要索引。
-spec find_by_api_token(binary()) -> {ok, map()} | {error, notfound | term()}.
find_by_api_token(ApiToken) when is_binary(ApiToken), ApiToken =/= <<>> ->
    Tb = tablename(),
    Digest = digest_hex(ApiToken),
    Q = <<
        "SELECT user_id AS bot_id, user_id, name, username, owner_uid, webhook_url,"
        " verify_token_enc, commands, permissions, events, is_public, status"
        " FROM ",
        Tb/binary,
        " WHERE api_token_digest = $1 LIMIT 1"
    >>,
    case elib_pg:query(Q, [Digest]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 写入 api_token 凭证（digest + prefix），明文不落库。
-spec set_api_token_credential(integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
set_api_token_credential(BotUserId, ApiToken) when is_binary(ApiToken), ApiToken =/= <<>> ->
    Tb = tablename(),
    elib_pg:execute(
        <<"UPDATE ", Tb/binary,
            " SET api_token_digest = $2, api_token_prefix = $3,"
            " api_token = '', updated_at = NOW() WHERE user_id = $1">>,
        [BotUserId, digest_hex(ApiToken), binary:part(ApiToken, 0, 8)]
    ).

%% @doc AEAD 加密密钥：postgre_aes_key 派生（sha256 → 32 字节）。
-spec aead_key() -> {ok, binary()} | {error, no_key}.
aead_key() ->
    derive_aead_key(config_ds:env(postgre_aes_key, <<>>)).

previous_aead_key() ->
    derive_aead_key(config_ds:env(postgre_aes_key_old, <<>>)).

derive_aead_key(<<>>) -> {error, no_key};
derive_aead_key(Key) when is_binary(Key) -> {ok, crypto:hash(sha256, Key)};
derive_aead_key(Key) when is_list(Key) -> derive_aead_key(list_to_binary(Key));
derive_aead_key(_) -> {error, no_key}.

%% @doc verify_token 可认证加密存储（AEAD）。主密钥缺失 → fail-closed。
-spec set_verify_token_enc(integer(), binary()) ->
    {ok, non_neg_integer()} | {error, no_key | term()}.
set_verify_token_enc(BotUserId, VerifyToken) when is_binary(VerifyToken) ->
    case encrypt_verify_token(VerifyToken) of
        {error, _} = E ->
            E;
        {ok, Enc} ->
            persist_verify_token_enc(BotUserId, Enc)
    end.

persist_verify_token_enc(BotUserId, Enc) ->
    Tb = tablename(),
    elib_pg:execute(
        <<"UPDATE ", Tb/binary,
            " SET verify_token_enc = $2, verify_token = '', token_migrated = true,"
            " updated_at = NOW() WHERE user_id = $1">>,
        [BotUserId, Enc]
    ).

encrypt_verify_token(VerifyToken) ->
    case aead_key() of
        {error, no_key} = E ->
            E;
        {ok, Key} ->
            elib_cipher:aes_gcm_encrypt(VerifyToken, Key)
    end.

%% @doc 取回 verify_token 明文（发送签名必需）。
%% fail-closed：密钥缺失/解密失败一律 {error, Reason}，绝不降级读明文列。
-spec get_verify_token(integer()) -> {ok, binary()} | {error, no_key | term()}.
get_verify_token(BotUserId) ->
    case aead_key() of
        {error, no_key} = E ->
            E;
        {ok, Key} ->
            Tb = tablename(),
            case
                elib_pg:query(
                    <<"SELECT verify_token_enc FROM ", Tb/binary, " WHERE user_id = $1">>,
                    [BotUserId]
                )
            of
                {ok, [#{<<"verify_token_enc">> := <<>>}]} ->
                    {error, not_encrypted};
                {ok, [#{<<"verify_token_enc">> := Enc}]} ->
                    decrypt_verify_token(BotUserId, Enc, Key);
                {ok, []} ->
                    {error, notfound};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

decrypt_verify_token(BotUserId, Enc, CurrentKey) ->
    case elib_cipher:aes_gcm_decrypt(Enc, CurrentKey) of
        {ok, _} = Ok ->
            Ok;
        {error, authentication_failed} = CurrentError ->
            decrypt_with_previous_key(BotUserId, Enc, CurrentKey, CurrentError);
        {error, _} = Error ->
            Error
    end.

decrypt_with_previous_key(BotUserId, Enc, CurrentKey, CurrentError) ->
    case previous_aead_key() of
        {ok, CurrentKey} ->
            CurrentError;
        {ok, PreviousKey} ->
            case elib_cipher:aes_gcm_decrypt(Enc, PreviousKey) of
                {ok, Plain} -> reencrypt_with_current_key(BotUserId, Plain, CurrentKey);
                {error, _} -> CurrentError
            end;
        {error, no_key} ->
            CurrentError
    end.

reencrypt_with_current_key(BotUserId, Plain, CurrentKey) ->
    case elib_cipher:aes_gcm_encrypt(Plain, CurrentKey) of
        {ok, Enc} ->
            case persist_verify_token_enc(BotUserId, Enc) of
                {ok, 1} -> {ok, Plain};
                {ok, Rows} -> {error, {reencrypt_failed, {updated_rows, Rows}}};
                {error, Reason} -> {error, {reencrypt_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {reencrypt_failed, Reason}}
    end.

%% @doc 列出需要验证/重加密的 Bot；仅返回标识，不读取或暴露明文。
-spec list_encrypted_bot_ids() -> {ok, [integer()]} | {error, term()}.
list_encrypted_bot_ids() ->
    Tb = tablename(),
    case
        elib_pg:query(
            <<"SELECT user_id FROM ", Tb/binary,
                " WHERE verify_token_enc IS NOT NULL AND verify_token_enc <> ''"
                " ORDER BY user_id">>,
            []
        )
    of
        {ok, Rows} -> {ok, [BotId || #{<<"user_id">> := BotId} <- Rows]};
        {error, Reason} -> {error, Reason}
    end.
