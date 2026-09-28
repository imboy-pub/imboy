%%% @doc BE-W01 A02：企业密钥环的 `_FILE` 装载（启动期一次性）。
%%%
%%% 冻结合同（cors-auth-matrix.json / plan :519）：企业消息 keyring 支持
%%% 文件加载（`IMBOY_EB_ENTERPRISE_KEYRING_FILE`），文件权限必须 0600，
%%% 读取/权限/内容任何失败启动 fail-fast。
%%%
%%% 为什么不在 `eb_env_keyring` 里做（该模块 F6 裁决「不做任何 I/O」，
%%% eb_env_keyring_tests 有静态红线直扫源码）：
%%%   * 密钥环**判定**保持纯净（decode/1 仍是唯一判据真源，本模块复用之）；
%%%   * 装载 I/O（env 读取 / file:read_file_info / file:read_file / JSON
%%%     解析）收敛在本模块——只在启动链（imboy_app）调用一次，请求路径
%%%     零 I/O、零解析。
%%%
%%% 文件格式：JSON `{"active_version": 1, "keys": {"1": "<64-hex>"}}`
%%% （密管系统渲染挂载的常规形态；keys 的键是版本号字符串，装载时转
%%% 正整数后交 `eb_env_keyring:decode/1` 做全部严格判据）。
%%%
%%% 零日志：密钥内容不进日志、不进错误项（错误只含原因原子与路径——
%%% 路径不是密钥）。双源歧义（sys.config 已配 keyring 且又给 _FILE）
%%% fail-closed 拒启，不猜优先级。
-module(eb_keyring_file).

-export([
    load_env_file/0,
    decode_json/1
]).

-define(ENV_APP, imboy).
-define(ENV_KEY, eb_enterprise_keyring).
-define(FILE_ENV_VAR, "IMBOY_EB_ENTERPRISE_KEYRING_FILE").

-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% 启动装载入口（imboy_app 启动链调用一次）
%% ===================================================================

%% @doc 按 `IMBOY_EB_ENTERPRISE_KEYRING_FILE` 装载密钥环：
%%   * env 未设置 → no-op（sys.config / 显式注入路径照旧）；
%%   * application env 已有 keyring（sys.config 配置）→ 双源歧义，拒启；
%%   * 文件权限非 0600 / 读取失败 / JSON 或形状非法 → 拒启（fail-fast）；
%%   * 成功 → `{imboy, eb_enterprise_keyring}` 覆写为**解码前形状**
%%     （`#{active_version, keys}`，hex 原文）——运行时仍经
%%     `eb_env_keyring:key_ref/0` 统一解码，判据单源。
-spec load_env_file() -> ok.
load_env_file() ->
    case os:getenv(?FILE_ENV_VAR) of
        false ->
            ok;
        Path when is_list(Path), length(Path) > 0 ->
            ensure_no_conflicting_source(),
            {ok, Json} = read_keyring_file(Path),
            case decode_json(Json) of
                {ok, Shape} ->
                    application:set_env(?ENV_APP, ?ENV_KEY, Shape),
                    ok;
                {error, Reason} ->
                    erlang:error({keyring_file_invalid, Reason})
            end;
        _ ->
            ok
    end.

ensure_no_conflicting_source() ->
    case application:get_env(?ENV_APP, ?ENV_KEY, undefined) of
        undefined ->
            ok;
        _AlreadyConfigured ->
            erlang:error(keyring_source_conflict)
    end.

read_keyring_file(Path) ->
    case file:read_file_info(Path) of
        %% mode 含文件类型位（regular = 8#100000），权限只看低 9 位且必须
        %% 精确 0600——group/other 任何读位即拒（先验权限再读内容）。
        {ok, #file_info{mode = Mode}} when (Mode band 8#777) =:= 8#600 ->
            case file:read_file(Path) of
                {ok, Bin} -> {ok, Bin};
                {error, Reason} -> erlang:error({keyring_file_unreadable, Reason})
            end;
        {ok, #file_info{mode = Mode}} ->
            erlang:error({keyring_file_permissions, Mode});
        {error, Reason} ->
            erlang:error({keyring_file_unreadable, Reason})
    end.

%% ===================================================================
%% JSON → keyring 形状（纯函数，判据复用 eb_env_keyring:decode/1）
%% ===================================================================

%% @doc JSON 文档 → `#{active_version => N, keys => #{V => hex}}`。
%% 任何形状偏差（非 object / 缺键 / 版本字符串非正整数 / decode 判据失败）
%% 一律 `{error, _}`（错误项不含密钥内容——hex 材料不进错误项）。
-spec decode_json(binary()) -> {ok, map()} | {error, term()}.
decode_json(Json) when is_binary(Json) ->
    try jsone:decode(Json) of
        #{<<"active_version">> := Active} = Doc when is_integer(Active), Active >= 1 ->
            case maps:get(<<"keys">>, Doc, undefined) of
                Keys when is_map(Keys), map_size(Keys) >= 1 ->
                    case key_versions(Keys, #{}) of
                        {ok, Converted} ->
                            {ok, #{active_version => Active, keys => Converted}};
                        {error, _} = Err ->
                            Err
                    end;
                _ ->
                    {error, keys_missing}
            end;
        #{<<"active_version">> := _} ->
            {error, invalid_active_version};
        _ ->
            {error, invalid_keyring_document}
    catch
        _:_ ->
            {error, invalid_json}
    end;
decode_json(_Other) ->
    {error, invalid_json}.

key_versions(Keys, Acc) ->
    try
        {ok,
            maps:fold(
                fun(VBin, Hex, Inner) ->
                    case version_of(VBin) of
                        {ok, V} -> Inner#{V => Hex};
                        error -> error(invalid_key_version)
                    end
                end,
                Acc,
                Keys
            )}
    catch
        _:_ -> {error, invalid_key_version}
    end.

version_of(VBin) when is_binary(VBin) ->
    try binary_to_integer(VBin) of
        V when V >= 1 -> {ok, V};
        _ -> error
    catch
        _:_ -> error
    end.
