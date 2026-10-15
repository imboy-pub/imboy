%%% @doc V5 统一存储边界（W4-03）：按完整 I dispatch 的 driver behaviour。
%%%
%%% 定位记录（03 M4 权威形态）：
%%%   (driver, namespace, storage_root_id, canonical_internal_endpoint_or_root,
%%%    container_rules, locator_digest)
%%% ——版本化 registry 放 config（`imboy` app env `attachment_storage_registry`），
%%% 不新建 DB 表；root_id 不随 endpoint 别名改变；canonical endpoint 保留
%%% 协议/host/端口/路径；凭据轮换不改定位摘要；Disk 取固定 root 及访问受控
%%% 的真实路径（不允许符号链接重定向）；新 registry 只允许新增根或显式别名，
%%% 不能删除/重映射已有根；namespace 曾使用即永不复用。
%%%
%%% locator_digest = SHA256(规范定位记录 JSON，不含凭据) 的 64 位小写 hex——
%%% 「namespace 字符串不变但 endpoint/root 被重映射」的防线；同一 root 的
%%% 别名归 canonical 后取同一 digest（02 §4：别名归 canonical 后使用同一
%%% storage_root_id 锁域）。
-module(attachment_storage).

-export([
    registry/0,
    resolve/1,
    canonical_namespace/1,
    locator_digest/2,
    identity/3,
    driver_module/1
]).

-define(REGISTRY_VERSION, 1).

%% @doc 版本化 registry（只读；缺省空 registry=fail-closed，未知 namespace
%% 一律拒绝——03：不同实现必须用同一规范）。
-spec registry() -> map().
registry() ->
    R = application:get_env(imboy, attachment_storage_registry, #{}),
    case maps:get(version, R, undefined) of
        ?REGISTRY_VERSION -> R;
        _ -> #{version => ?REGISTRY_VERSION, roots => #{}}
    end.

%% @doc namespace（storage_namespace）→ root 记录。别名一律归 canonical
%% root（返回记录含 canonical 标识）；未知 namespace → {error, unknown}。
-spec resolve(binary()) -> {ok, map()} | {error, unknown}.
resolve(Namespace) when is_binary(Namespace) ->
    Roots = maps:get(roots, registry(), #{}),
    Match = lists:filter(
        fun({_RootId, Rec}) -> namespace_matches(Rec, Namespace) end,
        maps:to_list(Roots)
    ),
    case Match of
        [{RootId, Rec} | _] ->
            {ok, Rec#{root_id => RootId, namespace => canonical(Rec, Namespace)}};
        [] ->
            {error, unknown}
    end;
resolve(_) ->
    {error, unknown}.

namespace_matches(Rec, Namespace) ->
    case maps:get(namespace, Rec, undefined) of
        Namespace -> true;
        _ -> lists:member(Namespace, maps:get(aliases, Rec, []))
    end.

canonical(Rec, Namespace) ->
    case maps:get(namespace, Rec, undefined) of
        undefined -> Namespace;
        Canonical -> Canonical
    end.

%% @doc 别名归 canonical：同一 root 的任何别名返回同一 canonical namespace。
-spec canonical_namespace(binary()) -> binary() | {error, unknown}.
canonical_namespace(Namespace) ->
    case resolve(Namespace) of
        {ok, #{namespace := Canonical}} -> Canonical;
        {error, unknown} -> {error, unknown}
    end.

%% @doc locator digest：规范定位记录（driver/root_id/canonical endpoint/
%% container——不含凭据、不含别名差异）的 SHA256 64 位小写 hex。
%% 同 root 别名 → 同 digest；endpoint/root 重映射（registry 版本升级）→
%% digest 变化（显式可见，不静默）。
-spec locator_digest(binary(), map()) -> binary().
locator_digest(Namespace, Rec) ->
    Canonical = canonical(Rec, Namespace),
    Record = #{
        <<"driver">> => maps:get(driver, Rec),
        <<"root_id">> => maps:get(root_id, Rec),
        <<"endpoint">> => maps:get(endpoint, Rec, <<>>),
        <<"container">> => Canonical
    },
    Json = jsone:encode(Record),
    binary:encode_hex(crypto:hash(sha256, Json), lowercase).

%% @doc 完整 I（03 行 195 的锁键四元组 + locator digest）。
-spec identity(binary(), binary(), binary()) ->
    {ok, #{
        driver := atom(),
        namespace := binary(),
        container := binary(),
        key := binary(),
        locator_digest := binary(),
        root_id := binary()
    }}
    | {error, unknown}.
identity(Namespace, Container, Key) ->
    case resolve(Namespace) of
        {ok, Rec} ->
            {ok, #{
                driver => binary_to_atom(maps:get(driver, Rec), utf8),
                namespace => canonical(Rec, Namespace),
                container => Container,
                key => Key,
                locator_digest => locator_digest(Namespace, Rec),
                root_id => maps:get(root_id, Rec)
            }};
        {error, unknown} ->
            {error, unknown}
    end.

%% @doc driver 模块派发（garage | disk；其余 fail-closed）。
-spec driver_module(binary()) -> module() | {error, unsupported_driver}.
driver_module(<<"garage">>) -> attachment_storage_garage;
driver_module(<<"disk">>) -> attachment_storage_disk;
driver_module(_) -> {error, unsupported_driver}.
