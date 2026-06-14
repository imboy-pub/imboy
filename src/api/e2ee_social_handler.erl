-module(e2ee_social_handler).
-dialyzer(
    {nowarn_function, [
        generate_key_version/0,
        create_shards/2,
        get_shards/2,
        decrypt_shard/2,
        add_contact/2
    ]}
).

-behavior(cowboy_handler).

-export([init/2]).
-export([handle_action/3]).
-export([create_shards/2]).
-export([get_shards/2]).
-export([recover_key/2]).
-export([get_proxy_shards/2]).
-export([decrypt_shard/2]).
-export([contacts/2]).
-export([add_contact/2]).
-export([remove_contact/2]).
-export([extract_encrypted_shard/1]).
-export([map_decrypt_shard_error/1]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% 导入错误消息函数
-import(imboy_error, [error_msg/1]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化 E2EE 社交恢复处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% ===================================================================
%% Capability Gate
%% ===================================================================

%% @doc 检查 E2EE 功能是否启用
-spec ensure_e2ee_enabled(cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_e2ee_enabled(Req0) ->
    case imboy_policy:e2ee_enabled() of
        true ->
            ok;
        false ->
            {error,
                elib_response:error(
                    Req0,
                    imboy_error:error_msg(?ERR_FEATURE_DISABLED),
                    ?ERR_FEATURE_DISABLED
                )}
    end.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create_shards, Req, State) -> create_shards(Req, State);
handle_action(get_shards, Req, State) -> get_shards(Req, State);
handle_action(recover_key, Req, State) -> recover_key(Req, State);
handle_action(get_proxy_shards, Req, State) -> get_proxy_shards(Req, State);
handle_action(decrypt_shard, Req, State) -> decrypt_shard(Req, State);
handle_action(contacts, Req, State) -> contacts(Req, State);
handle_action(add_contact, Req, State) -> add_contact(Req, State);
handle_action(remove_contact, Req, State) -> remove_contact(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Action Handlers
%% ===================================================================

%% @doc 创建恢复分片
%% POST /v1/e2ee/social/create_shards
%% Body: {"total_shards": 3, "threshold": 2, "proxies": [{"proxy_uid": 123, "encrypted_public_key": "..."}]}
-spec create_shards(cowboy_req:req(), map()) -> cowboy_req:req().
create_shards(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_create_shards(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_create_shards(cowboy_req:req(), map()) -> cowboy_req:req().
do_create_shards(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    case throttle:check(e2ee_create_shards, CurrentUid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"操作过于频繁，请稍后再试"/utf8>>, 429);
        _ ->
            case decode_json_body(Req0) of
                {error, _} ->
                    elib_response:error(Req0, <<"无效的请求体"/utf8>>, ?ERR_BAD_REQUEST);
                {ok, Data} ->
                    % C1 FIX: 客户端传入已用代理公钥加密好的分片，服务端只负责存储
                    KeyVersion = maps:get(<<"key_version">>, Data, generate_key_version()),
                    Shards = maps:get(<<"shards">>, Data, []),

                    case Shards of
                        [] ->
                            elib_response:error(Req0, <<"分片列表不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                        _ ->
                            case e2ee_social_logic:create_shards(CurrentUid, KeyVersion, Shards) of
                                {ok, StoredShards} ->
                                    elib_response:success(Req0, #{
                                        <<"key_version">> => KeyVersion,
                                        <<"shards">> => StoredShards
                                    });
                                {error, Reason} ->
                                    elib_response:error(
                                        Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR
                                    )
                            end
                    end
            end
    end.

%% @doc 获取用户的恢复分片
%% GET /v1/e2ee/social/shards?key_version=xxx
-spec get_shards(cowboy_req:req(), map()) -> cowboy_req:req().
get_shards(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_get_shards(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_get_shards(cowboy_req:req(), map()) -> cowboy_req:req().
do_get_shards(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    Qs = cowboy_req:parse_qs(Req0),
    KeyVersion = proplists:get_value(<<"key_version">>, Qs, <<"latest">>),

    case e2ee_social_logic:get_user_shards(CurrentUid, KeyVersion) of
        {ok, Shards} ->
            elib_response:success(Req0, #{<<"shards">> => Shards});
        {error, Reason} ->
            elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 恢复密钥
%% POST /v1/e2ee/social/recover
%% Body: {"decrypted_shards": ["base64_shard1", "base64_shard2"]}
%% @doc 零信任架构：客户端从代理获取解密后的分片，直接传给服务端重组
-spec recover_key(cowboy_req:req(), map()) -> cowboy_req:req().
recover_key(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_recover_key(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_recover_key(cowboy_req:req(), map()) -> cowboy_req:req().
do_recover_key(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    case decode_json_body(Req0) of
        {error, _} ->
            elib_response:error(Req0, <<"无效的请求体"/utf8>>, ?ERR_BAD_REQUEST);
        {ok, Data} ->
            % 获取已解密的分片列表
            DecryptedShardList = maps:get(<<"decrypted_shards">>, Data, []),

            % 验证参数
            case length(DecryptedShardList) < 2 of
                true ->
                    elib_response:error(Req0, <<"至少需要 2 个分片才能恢复密钥"/utf8>>, ?ERR_BAD_REQUEST);
                false ->
                    % C1 FIX: 服务端不重组私钥，只验证分片数量并将分片列表返回给客户端
                    % 客户端在本地完成 Shamir 重组，服务端永远看不到明文私钥
                    case e2ee_social_logic:validate_shards(CurrentUid, DecryptedShardList) of
                        {ok, ValidatedShards} ->
                            elib_response:success(Req0, #{
                                <<"shards">> => ValidatedShards,
                                <<"message">> => <<"分片验证成功，请在客户端完成密钥重组"/utf8>>
                            });
                        {error, Reason} ->
                            elib_response:error(
                                Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end
            end
    end.

%% @doc 获取代理的分片列表
%% GET /v1/e2ee/social/proxy_shards
-spec get_proxy_shards(cowboy_req:req(), map()) -> cowboy_req:req().
get_proxy_shards(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_get_proxy_shards(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_get_proxy_shards(cowboy_req:req(), map()) -> cowboy_req:req().
do_get_proxy_shards(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),

    case e2ee_social_logic:get_proxy_shards(CurrentUid) of
        {ok, Shards} ->
            elib_response:success(Req0, #{<<"shards">> => Shards});
        {error, Reason} ->
            elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 解密分片（代理调用）
%% POST /v1/e2ee/social/decrypt_shard
%% Body: {"shard_id": "xxx"}
%% @doc 零信任架构：代理使用自己的私钥解密为用户加密的分片
-spec decrypt_shard(cowboy_req:req(), map()) -> cowboy_req:req().
decrypt_shard(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_decrypt_shard(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_decrypt_shard(cowboy_req:req(), map()) -> cowboy_req:req().
do_decrypt_shard(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    case throttle:check(e2ee_decrypt_shard, CurrentUid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"操作过于频繁，请稍后再试"/utf8>>, 429);
        _ ->
            case decode_json_body(Req0) of
                {error, _} ->
                    elib_response:error(Req0, <<"无效的请求体"/utf8>>, ?ERR_BAD_REQUEST);
                {ok, Data} ->
                    ShardId = maps:get(<<"shard_id">>, Data, <<>>),
                    case ShardId of
                        <<>> ->
                            elib_response:error(Req0, <<"缺少 shard_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
                        _ ->
                            case e2ee_social_logic:get_proxy_shard(ShardId, CurrentUid) of
                                {ok, Shard} ->
                                    case extract_encrypted_shard(Shard) of
                                        {ok, EncryptedShard} ->
                                            %% 零信任：服务端只返回加密分片，
                                            %% 由代理客户端用本地私钥解密后回传明文分片。
                                            elib_response:success(Req0, #{
                                                <<"encrypted_shard">> => EncryptedShard
                                            });
                                        {error, Reason} ->
                                            {Msg, Code} = map_decrypt_shard_error(Reason),
                                            elib_response:error(Req0, Msg, Code)
                                    end;
                                {error, Reason} ->
                                    {Msg, Code} = map_decrypt_shard_error(Reason),
                                    elib_response:error(Req0, Msg, Code)
                            end
                    end
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 生成密钥版本号
-spec generate_key_version() -> binary().
generate_key_version() ->
    % 使用时间戳作为版本号
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("~p", [Timestamp])).

%% @doc 列出可信联系人
%% GET /v1/e2ee/social/contacts
-spec contacts(cowboy_req:req(), map()) -> cowboy_req:req().
contacts(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_contacts(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_contacts(cowboy_req:req(), map()) -> cowboy_req:req().
do_contacts(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),

    case e2ee_social_logic:list_trusted_contacts(CurrentUid) of
        {ok, Contacts} ->
            elib_response:success(Req0, #{<<"contacts">> => Contacts});
        {error, Reason} ->
            elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 添加可信联系人
%% POST /v1/e2ee/social/contacts/add
%% Body: {"contact_uid": "xxx", "nickname": "可选昵称"}
-spec add_contact(cowboy_req:req(), map()) -> cowboy_req:req().
add_contact(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_add_contact(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_add_contact(cowboy_req:req(), map()) -> cowboy_req:req().
do_add_contact(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data =
        try jsx:decode(Body, [return_maps]) of
            D when is_map(D) -> D
        catch
            _:_ -> #{}
        end,

    % 解码 contact_uid
    ContactUidEnc = maps:get(<<"contact_uid">>, Data, <<>>),
    ContactUid = ec_cnv:to_integer(ContactUidEnc),
    case ContactUid of
        0 ->
            elib_response:error(Req0, <<"无效的用户 ID"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            Nickname = maps:get(<<"nickname">>, Data, <<>>),

            case e2ee_social_logic:add_trusted_contact(CurrentUid, ContactUid, Nickname) of
                ok ->
                    elib_response:success(Req0, #{<<"message">> => <<"添加可信联系人成功"/utf8>>});
                {error, cannot_add_self} ->
                    elib_response:error(
                        Req0,
                        error_msg(?ERR_E2EE_SOCIAL_CONTACT_IS_SELF),
                        ?ERR_E2EE_SOCIAL_CONTACT_IS_SELF
                    );
                {error, not_friend} ->
                    elib_response:error(
                        Req0, <<"只能添加好友为可信联系人"/utf8>>, ?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND
                    );
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 移除可信联系人
%% POST /v1/e2ee/social/contacts/remove
%% Body: {"contact_uid": "xxx"}
-spec remove_contact(cowboy_req:req(), map()) -> cowboy_req:req().
remove_contact(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_remove_contact(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_remove_contact(cowboy_req:req(), map()) -> cowboy_req:req().
do_remove_contact(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data =
        try jsx:decode(Body, [return_maps]) of
            D when is_map(D) -> D
        catch
            _:_ -> #{}
        end,

    % 解码 contact_uid
    ContactUidEnc = maps:get(<<"contact_uid">>, Data, <<>>),
    ContactUid = ec_cnv:to_integer(ContactUidEnc),
    case ContactUid of
        0 ->
            elib_response:error(Req0, <<"无效的用户 ID"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case e2ee_social_logic:remove_trusted_contact(CurrentUid, ContactUid) of
                ok ->
                    elib_response:success(Req0, #{<<"message">> => <<"移除可信联系人成功"/utf8>>});
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 格式化错误消息（安全：不暴露内部细节）
-spec format_error(term()) -> binary().
format_error({Msg, _Code}) when is_binary(Msg) ->
    % 标准错误格式 {Message, ErrorCode}
    Msg;
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) when is_list(Reason) ->
    iolist_to_binary(Reason);
format_error(_Reason) ->
    % 安全：不暴露内部错误细节
    <<"操作失败，请稍后重试"/utf8>>.

%% @doc 提取可解密分片数据
%% 优先读取 encrypted_shard，兼容旧字段 encrypted_data
-spec extract_encrypted_shard(map()) -> {ok, binary()} | {error, term()}.
extract_encrypted_shard(Shard) when is_map(Shard) ->
    case
        maps:get(
            <<"encrypted_shard">>,
            Shard,
            maps:get(<<"encrypted_data">>, Shard, <<>>)
        )
    of
        Encrypted when is_binary(Encrypted), Encrypted =/= <<>> ->
            {ok, Encrypted};
        _ ->
            {error, shard_data_missing}
    end;
extract_encrypted_shard(_) ->
    {error, shard_data_missing}.

%% @doc decrypt_shard 错误契约映射（集中维护）
-spec map_decrypt_shard_error(term()) -> {binary(), integer()}.
map_decrypt_shard_error(not_proxy) ->
    {<<"无权解密此分片"/utf8>>, ?ERR_FORBIDDEN};
map_decrypt_shard_error(shard_not_active) ->
    {<<"分片不可用"/utf8>>, ?ERR_BAD_REQUEST};
map_decrypt_shard_error(shard_not_found) ->
    {<<"分片不存在"/utf8>>, ?ERR_NOT_FOUND};
map_decrypt_shard_error(not_found) ->
    {<<"分片不存在"/utf8>>, ?ERR_NOT_FOUND};
map_decrypt_shard_error(shard_data_missing) ->
    {<<"分片数据缺失"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR};
map_decrypt_shard_error(private_key_not_found) ->
    {error_msg(?ERR_E2EE_KEY_NOT_FOUND), ?ERR_E2EE_KEY_NOT_FOUND};
map_decrypt_shard_error(device_not_found) ->
    {error_msg(?ERR_E2EE_KEY_NOT_FOUND), ?ERR_E2EE_KEY_NOT_FOUND};
map_decrypt_shard_error(decryption_failed) ->
    {error_msg(?ERR_E2EE_DECRYPTION_FAILED), ?ERR_E2EE_DECRYPTION_FAILED};
map_decrypt_shard_error({Msg, Code}) when is_binary(Msg), is_integer(Code) ->
    {Msg, Code};
map_decrypt_shard_error(Reason) ->
    {format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR}.

%% @doc 统一 JSON 解析：防止畸形 JSON 使 handler 进程崩溃
-spec decode_json_body(cowboy_req:req()) -> {ok, map()} | {error, invalid_json}.
decode_json_body(Req0) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    try jsx:decode(Body, [return_maps]) of
        Data when is_map(Data) -> {ok, Data};
        _ -> {error, invalid_json}
    catch
        _:_ -> {error, invalid_json}
    end.
