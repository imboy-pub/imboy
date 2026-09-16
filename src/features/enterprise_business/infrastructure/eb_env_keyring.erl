%%% @doc F6（RULING-2026-09-15 §七）：服务端环境注入的版本化企业密钥环。
%%%
%%% 裁决要点逐条落地：
%%%   * 密钥**只**从服务端 application 配置读取（`imboy` 应用的
%%%     `eb_enterprise_keyring` 键；配置值由进程环境注入 sys config，本模块
%%%     不接任何外部 KMS、不做任何 I/O）；
%%%   * 必须有 `active_version`，以及 `version -> 32-byte key` 的版本化 keyring；
%%%   * 环境值采用**明确编码**（hex，小写/大写均可）并**严格解码**：缺配置、
%%%     非 map、缺 active_version、active 版本不在 keys、版本号非正整数、
%%%     键值非 hex、解码后长度 ≠ 32 字节 —— 一律 fail-closed 返回 `{error, _}`，
%%%     绝不回落默认密钥、绝不截断/补齐；
%%%   * 重复版本在 map 形态下不可能出现（later-wins），但**同一版本对应的
%%%     值重复出现不同编码仍会因逐版本独立解码而保持确定性——不做任何合并**；
%%%   * TEST 构建可注入 synthetic keyring（`-ifdef(TEST)` 例外，仅供 eunit）；
%%%     非 TEST 构建**不得**回退 synthetic/default——缺配置即 `{error,
%%%     keyring_unavailable}`，这是装配缺陷的显式信号，不是静默降级；
%%%   * 输出形状即 `eb_managed_crypto` 的 KeyRef：`#{keys => #{Ver => Key},
%%%     key_version => Active}`（resolve_key/1 -> resolve_keyring/2 消费）。
%%%
%%% 本模块零日志：密钥材料不进日志、不进错误项（错误只含原因原子与版本号）。
-module(eb_env_keyring).

-export([
    key_ref/0,
    decode/1
]).
-ifdef(TEST).
%% synthetic 仅 TEST 构建导出（函数体也在 -ifdef(TEST) 内：非 TEST 构建
%% 导出不存在的函数本身就是编译错误——三档矩阵 neither 档实撞）。
-export([synthetic_test_key_ref/0]).
-endif.

-define(KEY_BYTES, 32).
-define(ENV_APP, imboy).
-define(ENV_KEY, eb_enterprise_keyring).

%% ===================================================================
%% 入口
%% ===================================================================

%% @doc 从服务端 application 配置读取并解码密钥环。
%% 配置形状（sys config / 环境注入）：
%%   `{imboy, [{eb_enterprise_keyring, #{active_version => 2,
%%              keys => #{1 => "64-hex-chars", 2 => "64-hex-chars"}}}]}'
-spec key_ref() -> {ok, map()} | {error, term()}.
key_ref() ->
    decode(application:get_env(?ENV_APP, ?ENV_KEY, undefined)).

%% @doc 严格解码。独立导出供单测与装配自检（不重复实现两遍判据）。
-spec decode(term()) -> {ok, map()} | {error, term()}.
decode(#{active_version := Active, keys := Keys}) when
    is_integer(Active), Active >= 1, is_map(Keys), map_size(Keys) >= 1
->
    case maps:get(Active, Keys, undefined) of
        undefined ->
            {error, {active_version_missing, Active}};
        _ActiveKeyPresent ->
            decode_versions(maps:to_list(Keys), #{}, Active)
    end;
decode(#{active_version := Active}) when is_integer(Active) ->
    {error, keys_missing};
decode(#{keys := _Keys}) ->
    {error, active_version_missing};
decode(Other) when is_map(Other) ->
    {error, {invalid_keyring_shape, map_size(Other)}};
decode(_NotAMap) ->
    {error, keyring_unavailable}.

decode_versions([], Decoded, Active) ->
    {ok, #{keys => Decoded, key_version => Active}};
decode_versions([{Version, Hex} | Rest], Acc, Active) when
    is_integer(Version), Version >= 1, is_binary(Hex)
->
    case decode_hex_key(Hex) of
        {ok, Key} ->
            decode_versions(Rest, Acc#{Version => Key}, Active);
        {error, Reason} ->
            {error, {Reason, Version}}
    end;
decode_versions([{Version, _Hex} | _Rest], _Acc, _Active) ->
    {error, {invalid_key_version_entry, Version}}.

decode_hex_key(Hex) ->
    try binary:decode_hex(Hex) of
        Key ->
            case byte_size(Key) of
                ?KEY_BYTES -> {ok, Key};
                _WrongLength -> {error, invalid_key_length}
            end
    catch
        _:_BadHex -> {error, invalid_key_encoding}
    end.

%% @doc TEST 构建专用 synthetic 密钥环（两个版本验证轮换兼容）。
%% **仅** `-ifdef(TEST)` 下可达；非 TEST 构建该函数不存在，任何生产代码
%% 引用都会在编译期暴露 —— 结构上杜绝「生产回退 synthetic」。
-ifdef(TEST).
synthetic_test_key_ref() ->
    #{
        keys => #{
            1 => <<0:256>>,
            2 => <<1:256>>
        },
        key_version => 2
    }.
-endif.
