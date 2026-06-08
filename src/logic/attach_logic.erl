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
%% Meta: #{<<"md5">>, <<"mime_type">>, <<"size">>}（仅 md5 取自客户端，size/mime 以服务端核实为准）
%%
%% 两道防线：
%%   1) 越权守卫：仅允许上报自己命名空间（u<Uid>/...）下的 ObjectKey
%%   2) 真实性校验：向 Garage 发 HEAD 核实对象真实存在、真实大小≤上限、真实类型在白名单，
%%      用真实值落库；不一致则删除对象并拒绝。杜绝客户端伪造 size/mime 或 confirm 不存在的 key。
-spec confirm(integer(), binary(), map()) ->
    {ok, map()}
    | {error,
        forbidden_key
        | invalid_key
        | object_not_found
        | file_too_large
        | invalid_file_type
        | term()}.
confirm(Uid, ObjectKey, Meta) ->
    case elib_oss:owner_of_key(ObjectKey) of
        {ok, Uid} ->
            verify_and_save(Uid, ObjectKey, Meta);
        {ok, _Other} ->
            {error, forbidden_key};
        {error, R} ->
            {error, R}
    end.

%% @doc 服务端 HEAD 核实真实性后落库（真实值覆盖客户端自报值）
-spec verify_and_save(integer(), binary(), map()) -> {ok, map()} | {error, term()}.
verify_and_save(Uid, ObjectKey, Meta) ->
    case elib_oss:head_object(ObjectKey) of
        {error, not_found} ->
            {error, object_not_found};
        {error, R} ->
            {error, R};
        {ok, #{size := RealSize, content_type := RealType}} ->
            case RealSize > elib_oss:max_file_size() of
                true ->
                    _ = (catch elib_oss:delete_object(ObjectKey)),
                    {error, file_too_large};
                false ->
                    case elib_oss:validate_file_type(RealType) of
                        false ->
                            _ = (catch elib_oss:delete_object(ObjectKey)),
                            {error, invalid_file_type};
                        true ->
                            do_save(Uid, ObjectKey, Meta, RealSize, RealType)
                    end
            end
    end.

-spec do_save(integer(), binary(), map(), non_neg_integer(), binary()) ->
    {ok, map()} | {error, term()}.
do_save(Uid, ObjectKey, Meta, RealSize, RealType) ->
    Attach = #{
        %% md5 仅作完整性参考，不作安全边界
        <<"md5">> => maps:get(<<"md5">>, Meta, <<>>),
        %% mime_type/size 一律采用服务端 HEAD 核实的真实值
        <<"mime_type">> => RealType,
        <<"name">> => filename:basename(ObjectKey),
        %% path 存 ObjectKey，供孤儿清理 delete_object 使用
        <<"path">> => ObjectKey,
        %% url 同存 ObjectKey（不再落公开直链），读取时经 view_url 签发
        <<"url">> => ObjectKey,
        <<"size">> => RealSize
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
        {error, Reason} ->
            %% fail-closed：归属查询失败时拒绝签发（不再降级放行），
            %% 避免存储/DB 故障成为越权下载他人附件的入口。
            ?ERROR_LOG([
                "attach_logic:view_url ownership check failed (deny), uid=",
                Uid,
                " key=",
                ObjectKey,
                " reason=",
                Reason
            ]),
            {error, forbidden}
    end.
