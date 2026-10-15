%%% @doc V5 Garage driver（W4-03）：包装 elib_oss 既有 Garage/S3 调用面
%%%（复制现有行为，逐调用替换；行为等价 = storage-garage 验收）。
%%% Namespace 语义 = registry canonical container（与 elib_oss:get_bucket/1
%%% 的 bucket 同层——Garage root 一个 bucket；namespace 归一后即 bucket 名）。
-module(attachment_storage_garage).
-behaviour(attachment_storage_driver).

-export([put/4, head/3, delete/3, presign_put/4, presign_get/4, read/3]).

%% Garage（S3 兼容）不支持本地整读——read 走 GET 全量（对象均为附件级小
%%% 文件，max_file_size 上限保护）。
read(Namespace, _Container, Key) ->
    elib_oss:object_sha256(bucket(Namespace), Key),
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

delete(Namespace, _Container, Key) ->
    elib_oss:delete_object(bucket(Namespace), Key).

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
