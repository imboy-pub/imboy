%%% @doc **本地替身**对象存储 adapter（进程内，无网络、无真实凭据）。
%%%
%%% ## 它是什么 / 不是什么（A11 边界声明）
%%%
%%%   * 它**是**一个满足契约形状的本地 adapter：`put/3` / `get/2` / `delete/2`，
%%%     错误一律以 `{error, Reason}` 元组传播，**绝不静默返回 `{ok, _}`**。
%%%   * 它**不是**真实对象存储：不校验真实签名的 presigned PUT、不落盘、不跨进程、
%%%     不模拟 Garage 的 multipart / 版本 / 生命周期规则。因此
%%%     **不得据此宣称「真实 Garage 验收通过」**；报告口径固定为
%%%     `adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN`。
%%%
%%% 存储介质用 `persistent_term`（进程无关、无 owner 生命周期问题），使本替身在
%%% eunit 的多进程断言下行为稳定；`reset/0` 供套件清场。
-module(eb_asset_object_stub).

-export([put/3, get/2, delete/2, reset/0, key_prefix/2]).

-define(NS, {?MODULE, object}).

%% @doc 作用域前缀：调用方**不能**解释或构造它，只能由实现派生（A11 作用域证据）。
-spec key_prefix(integer(), integer()) -> binary().
key_prefix(OrgId, WorkspaceId) ->
    iolist_to_binary([
        "enterprise/",
        integer_to_binary(OrgId),
        "/",
        integer_to_binary(WorkspaceId),
        "/"
    ]).

%% @doc 写入对象。非 binary / 空 payload 一律失败（错误传播的正向证据）。
-spec put(binary(), term(), map()) -> ok | {error, term()}.
put(Key, Bytes, _Meta) when is_binary(Key), is_binary(Bytes), byte_size(Bytes) > 0 ->
    persistent_term:put({?NS, Key}, Bytes),
    ok;
put(_Key, Bytes, _Meta) when is_binary(Bytes) ->
    {error, empty_payload};
put(_Key, _Bytes, _Meta) ->
    {error, invalid_payload}.

%% @doc 读取对象：不存在（含跨作用域）→ `{error, not_found}`。
-spec get(binary(), binary()) ->
    {ok, #{bytes := binary(), size := non_neg_integer()}} | {error, term()}.
get(Key, ScopePrefix) when is_binary(Key), is_binary(ScopePrefix) ->
    case has_prefix(ScopePrefix, Key) of
        false ->
            %% 作用域前缀不符 ⇒ 不查、不返回（跨 Org 读必须失败）
            {error, out_of_scope};
        true ->
            try persistent_term:get({?NS, Key}) of
                Bytes when is_binary(Bytes) ->
                    {ok, #{bytes => Bytes, size => byte_size(Bytes)}}
            catch
                error:badarg -> {error, not_found}
            end
    end.

%% @doc 删除对象：不存在 / 跨作用域 → `{error, not_found}`（不得假装成功）。
-spec delete(binary(), binary()) -> ok | {error, term()}.
delete(Key, ScopePrefix) when is_binary(Key), is_binary(ScopePrefix) ->
    case has_prefix(ScopePrefix, Key) of
        false ->
            {error, out_of_scope};
        true ->
            case persistent_term:get({?NS, Key}, undefined) of
                undefined ->
                    {error, not_found};
                _Bytes ->
                    persistent_term:erase({?NS, Key}),
                    ok
            end
    end.

%% @doc 清空替身桶（仅测试/清场用）。
-spec reset() -> ok.
reset() ->
    lists:foreach(
        fun
            ({{?NS, _Key} = K, _V}) -> persistent_term:erase(K);
            (_Other) -> ok
        end,
        persistent_term:get()
    ),
    ok.

%% 二进制前缀判定（`lists:prefix/2` 只接受 list，不能用于 binary key）。
has_prefix(Prefix, Bin) when is_binary(Prefix), is_binary(Bin) ->
    Size = byte_size(Prefix),
    byte_size(Bin) >= Size andalso binary:part(Bin, 0, Size) =:= Prefix.
