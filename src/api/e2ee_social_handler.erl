-module(e2ee_social_handler).
-dialyzer({nowarn_function, [
    generate_key_version/0,
    get_proxy_private_key/1,
    get_sender_private_key/1,
    create_shards/2,
    get_shards/2,
    decrypt_shard/2,
    add_contact/2
]}).

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
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    % 获取参数
    TotalShards = maps:get(<<"total_shards">>, Data, 3),
    Threshold = maps:get(<<"threshold">>, Data, 2),
    Proxies = maps:get(<<"proxies">>, Data, []),

    % 验证参数
    case {TotalShards < Threshold orelse Threshold < 2, length(Proxies) < TotalShards} of
        {true, _} ->
            elib_response:error(Req0, <<"参数错误：分片数必须大于阈值"/utf8>>, ?ERR_BAD_REQUEST);
        {_, true} ->
            elib_response:error(Req0, <<"代理数量不足"/utf8>>, ?ERR_BAD_REQUEST);
        {false, false} ->
            % 获取当前用户的私钥
            case get_sender_private_key(CurrentUid) of
                {error, _Reason} ->
                    elib_response:error(Req0, <<"私钥不存在"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR);
                {ok, {PrivateKeyPem, _DeviceId}} ->
                    % 生成密钥版本号
                    KeyVersion = generate_key_version(),

                    % 创建分片
                    case e2ee_social_logic:create_shards(
                        CurrentUid, KeyVersion, TotalShards, Threshold, PrivateKeyPem, Proxies
                    ) of
                        {ok, Shards} ->
                            elib_response:success(Req0, #{
                                <<"key_version">> => KeyVersion,
                                <<"total_shards">> => TotalShards,
                                <<"threshold">> => Threshold,
                                <<"shards">> => Shards
                            });
                        {error, Reason} ->
                            elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end
    end.

%% @doc 获取用户的恢复分片
%% GET /v1/e2ee/social/shards?key_version=xxx
-spec get_shards(cowboy_req:req(), map()) -> cowboy_req:req().
get_shards(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    #{key_version := KeyVersion} = cowboy_req:match_qs([{key_version, [], <<"latest">>}], Req0),

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
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    % 获取已解密的分片列表
    DecryptedShardList = maps:get(<<"decrypted_shards">>, Data, []),

    % 验证参数
    case length(DecryptedShardList) < 2 of
        true ->
            elib_response:error(Req0, <<"至少需要 2 个分片才能恢复密钥"/utf8>>, ?ERR_BAD_REQUEST);
        false ->
            % 恢复密钥（服务端只负责重组，不需要查询数据库）
            case e2ee_social_logic:recover_key(CurrentUid, DecryptedShardList) of
                {ok, PrivateKeyPem} ->
                    % 保存恢复的密钥
                    case save_restored_key(CurrentUid, PrivateKeyPem) of
                        ok ->
                            elib_response:success(Req0, #{<<"message">> => <<"密钥恢复成功"/utf8>>});
                        {error, Reason} ->
                            elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
                    end;
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 获取代理的分片列表
%% GET /v1/e2ee/social/proxy_shards
-spec get_proxy_shards(cowboy_req:req(), map()) -> cowboy_req:req().
get_proxy_shards(Req0, State) ->
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
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    ShardId = maps:get(<<"shard_id">>, Data, <<>>),

    case ShardId of
        <<>> ->
            elib_response:error(Req0, <<"缺少 shard_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 验证当前用户是分片的代理
            case e2ee_social_ds:get_proxy_shard(ShardId, CurrentUid) of
                {ok, Shard} ->
                    EncryptedData = maps:get(<<"encrypted_data">>, Shard, <<>>),
                    case get_proxy_private_key(CurrentUid) of
                        {ok, PrivateKeyPem} ->
                            % 使用代理的私钥解密分片
                            case elib_cipher:decrypt_rsa_oaep(EncryptedData, PrivateKeyPem) of
                                {ok, DecryptedShard} ->
                                    elib_response:success(Req0, #{<<"decrypted_shard">> => DecryptedShard});
                                {error, Reason} ->
                                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
                            end;
                        {error, Reason} ->
                            elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
                    end;
                {error, not_proxy} ->
                    elib_response:error(Req0, <<"无权解密此分片"/utf8>>, ?ERR_FORBIDDEN);
                {error, shard_not_found} ->
                    elib_response:error(Req0, <<"分片不存在"/utf8>>, ?ERR_NOT_FOUND);
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 获取发送方的私钥
-spec get_sender_private_key(integer()) -> {ok, {binary(), binary()}} | {error, term()}.
get_sender_private_key(Uid) ->
    case user_device_repo:get_public_by_uid(Uid) of
        {ok, [Device | _]} ->
            DeviceId = maps:get(<<"device_id">>, Device),
            case user_device_repo:get_private_key(Uid, DeviceId) of
                {ok, PrivateKeyPem} when PrivateKeyPem /= <<>> ->
                    {ok, {PrivateKeyPem, DeviceId}};
                _ ->
                    {error, private_key_not_found}
            end;
        _ ->
            {error, device_not_found}
    end.

%% @doc 保存恢复的密钥
-spec save_restored_key(integer(), binary()) -> ok | {error, term()}.
save_restored_key(Uid, PrivateKeyPem) ->
    try
        % 获取当前用户的主设备
        case user_device_repo:get_public_by_uid(Uid) of
            {ok, [Device | _]} ->
                DeviceId = maps:get(<<"device_id">>, Device),

                % 更新设备的私钥
                case user_device_repo:update_private_key(Uid, DeviceId, PrivateKeyPem) of
                    {ok, _} ->
                        ok;
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error:Stack ->
            {error, {Error, Stack}}
    end.

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
    CurrentUid = maps:get(current_uid, State, 0),

    case e2ee_social_ds:list_trusted_contacts(CurrentUid) of
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
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    % 解码 contact_uid
    ContactUidEnc = maps:get(<<"contact_uid">>, Data, <<>>),
    case elib_hashids:decode(ContactUidEnc) of
        invalid ->
            elib_response:error(Req0, <<"无效的用户 ID"/utf8>>, ?ERR_BAD_REQUEST);
        ContactUid ->
            Nickname = maps:get(<<"nickname">>, Data, <<>>),

            case e2ee_social_ds:add_trusted_contact(CurrentUid, ContactUid, Nickname) of
                ok ->
                    elib_response:success(Req0, #{<<"message">> => <<"添加可信联系人成功"/utf8>>});
                {error, cannot_add_self} ->
                    elib_response:error(Req0, error_msg(?ERR_E2EE_SOCIAL_CONTACT_IS_SELF), ?ERR_E2EE_SOCIAL_CONTACT_IS_SELF);
                {error, not_friend} ->
                    elib_response:error(Req0, <<"只能添加好友为可信联系人"/utf8>>, ?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND);
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 移除可信联系人
%% POST /v1/e2ee/social/contacts/remove
%% Body: {"contact_uid": "xxx"}
-spec remove_contact(cowboy_req:req(), map()) -> cowboy_req:req().
remove_contact(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),

    % 解码 contact_uid
    ContactUidEnc = maps:get(<<"contact_uid">>, Data, <<>>),
    case elib_hashids:decode(ContactUidEnc) of
        invalid ->
            elib_response:error(Req0, <<"无效的用户 ID"/utf8>>, ?ERR_BAD_REQUEST);
        ContactUid ->
            case e2ee_social_ds:remove_trusted_contact(CurrentUid, ContactUid) of
                ok ->
                    elib_response:success(Req0, #{<<"message">> => <<"移除可信联系人成功"/utf8>>});
                {error, Reason} ->
                    elib_response:error(Req0, format_error(Reason), ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 格式化错误消息
-spec format_error(term()) -> binary().
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) when is_list(Reason) ->
    iolist_to_binary(Reason);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @doc 获取代理用户的私钥
%% 用于解密为用户存储的分片
-spec get_proxy_private_key(integer()) -> {ok, binary()} | {error, term()}.
get_proxy_private_key(Uid) ->
    case user_device_repo:get_public_by_uid(Uid) of
        {ok, [Device | _]} ->
            DeviceId = maps:get(<<"device_id">>, Device),
            case user_device_repo:get_private_key(Uid, DeviceId) of
                {ok, PrivateKeyPem} when PrivateKeyPem /= <<>> ->
                    {ok, PrivateKeyPem};
                _ ->
                    {error, private_key_not_found}
            end;
        _ ->
            {error, device_not_found}
    end.
