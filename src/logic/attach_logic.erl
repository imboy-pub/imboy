-module(attach_logic).
%%%
% 附件业务逻辑层（Garage S3 直传链路）
%
% 三条职责：
%   presign/3   生成绑定 uid 的上传 presigned PUT URL
%   confirm/3   客户端 PUT 成功后回调，落 attachment 表（孤儿清理依赖此表）
%   view_url/2  签发短时下载 presigned GET URL（替代 bucket 公开读）
%%%

-export([presign/3, confirm/3, view_url/2]).

-include("log.hrl").

%% 上传 URL 有效期：1 小时（足够单文件上传）
-define(PUT_EXPIRES, 3600).
%% 下载 URL 有效期：10 分钟（客户端可在临近过期前缓存复用）
-define(GET_EXPIRES, 600).

%% @doc 生成上传 presigned PUT URL（ObjectKey 绑定 uid）
-spec presign(integer(), binary(), binary()) -> {ok, map()} | {error, invalid_file_type}.
presign(Uid, FileName, MimeType) ->
    case elib_oss:validate_file_type(MimeType) of
        false ->
            {error, invalid_file_type};
        true ->
            ObjectKey = elib_oss:build_object_key(Uid, FileName),
            PutUrl = elib_oss:presign_put_for_key(ObjectKey, MimeType, ?PUT_EXPIRES),
            {ok, #{
                <<"put_url">> => PutUrl,
                <<"object_key">> => ObjectKey,
                <<"expires_at">> => erlang:system_time(second) + ?PUT_EXPIRES
            }}
    end.

%% @doc 客户端 PUT 成功后回调，落库附件元数据
%% Meta: #{<<"md5">>, <<"mime_type">>, <<"size">>}
%% 仅允许上报自己命名空间（u<Uid>/...）下的 ObjectKey，防止越权写入他人记录
-spec confirm(integer(), binary(), map()) ->
    {ok, map()} | {error, forbidden_key | invalid_key | term()}.
confirm(Uid, ObjectKey, Meta) ->
    case elib_oss:owner_of_key(ObjectKey) of
        {ok, Uid} ->
            Attach = #{
                <<"md5">> => maps:get(<<"md5">>, Meta, <<>>),
                <<"mime_type">> => maps:get(<<"mime_type">>, Meta, <<"application/octet-stream">>),
                <<"name">> => filename:basename(ObjectKey),
                %% path 存 ObjectKey，供孤儿清理 delete_object 使用
                <<"path">> => ObjectKey,
                %% url 同存 ObjectKey（不再落公开直链），读取时经 view_url 签发
                <<"url">> => ObjectKey,
                <<"size">> => maps:get(<<"size">>, Meta, 0)
            },
            Now = elib_dt:now(),
            try
                ok = elib_pg:with_tx(fun(Conn) ->
                    attachment_ds:save(Conn, Now, Uid, [Attach])
                end),
                {ok, #{<<"object_key">> => ObjectKey}}
            catch
                Class:Reason ->
                    ?ERROR_LOG(["attach_logic confirm save failed: ", Class, Reason]),
                    {error, Reason}
            end;
        {ok, _Other} ->
            {error, forbidden_key};
        {error, R} ->
            {error, R}
    end.

%% @doc 签发短时下载 URL（替代公开读），附带归属验证
%% 验证 ObjectKey 是否由 Uid 上传（creator_user_id = Uid）。
%% 未来可扩展：验证 ObjectKey 是否出现在 Uid 可见的会话消息中。
-spec view_url(integer(), binary()) -> {ok, binary()} | {error, forbidden}.
view_url(Uid, ObjectKey) ->
    case attachment_ds:find_by_path_and_uid(ObjectKey, Uid) of
        {ok, _} ->
            {ok, elib_oss:presign_get_for_key(ObjectKey, ?GET_EXPIRES)};
        {error, not_found} ->
            {error, forbidden};
        {error, _Reason} ->
            % 查询失败时降级允许（避免存储故障影响正常业务），记录日志
            ?WARN_LOG([
                "attach_logic:view_url ownership check failed for uid=",
                Uid,
                " key=",
                ObjectKey,
                " reason=",
                _Reason
            ]),
            {ok, elib_oss:presign_get_for_key(ObjectKey, ?GET_EXPIRES)}
    end.
