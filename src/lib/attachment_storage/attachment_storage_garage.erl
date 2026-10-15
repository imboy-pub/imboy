%%% @doc V5 Garage driver（W4-03）：包装 elib_oss 既有 Garage/S3 调用面
%%%（复制现有行为，逐调用替换；行为等价 = storage-garage 验收）。
%%% Namespace 语义 = registry canonical container（与 elib_oss:get_bucket/1
%%% 的 bucket 同层——Garage root 一个 bucket；namespace 归一后即 bucket 名）。
-module(attachment_storage_garage).
-behaviour(attachment_storage_driver).

-export([put/4, head/3, delete/3, presign_put/4, presign_get/4, read/3]).

%% read 尚未提供整读能力；保留不存在对象的明确错误，不额外下载完整对象。
read(Namespace, _Container, Key) ->
    case elib_oss:head_object(bucket(Namespace), Key) of
        {ok, _} -> {error, not_supported};
        E -> E
    end.

put(Namespace, _Container, Key, Body) when is_binary(Body) ->
    elib_oss:put_object(bucket(Namespace), Key, Body, default_mime());
put(Namespace, _Container, Key, {file, File}) ->
    elib_oss:put_object_from_file(bucket(Namespace), Key, File, default_mime()).

head(Namespace, _Container, Key) ->
    elib_oss:head_object(bucket(Namespace), Key).

delete(Namespace, Container, Key) ->
    %% 删除不能随全局配置切到另一个物理端点或默认桶。
    case attachment_storage:resolve(Namespace) of
        {ok, #{driver := <<"garage">>, container := Container, endpoint := Endpoint}} when
            is_binary(Container), Container =/= <<>>
        ->
            case elib_oss:endpoint() of
                Endpoint -> elib_oss:delete_object(Container, Key);
                _ -> {error, storage_identity_changed}
            end;
        _ ->
            {error, storage_identity_changed}
    end.

presign_put(Namespace, _Container, Key, Expires) ->
    {ok, elib_oss:presign_put_for_key(bucket(Namespace), Key, default_mime(), Expires)}.

presign_get(Namespace, _Container, Key, Expires) ->
    {ok, elib_oss:presign_get_for_key(bucket(Namespace), Key, Expires)}.

%% registry canonical namespace ↔ elib_oss bucket：Garage root 的
%% container 即 bucket 名（V5 registry 显式列出；未配置 registry 时回退
%% elib_oss:get_bucket（既有行为零变化——storage-switch 验收切换面）。
bucket(Namespace) ->
    case attachment_storage:resolve(Namespace) of
        {ok, #{container := Bucket}} when is_binary(Bucket), Bucket =/= <<>> ->
            Bucket;
        _ ->
            elib_oss:get_bucket(Namespace)
    end.

default_mime() -> <<"application/octet-stream">>.
