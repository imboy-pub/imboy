-module(imboy_plugin_loader).

%% @status FROZEN (roadmap-only, 2026-06)：v2 动态加载子系统暂停投入。
%% 当前生产走配置驱动模块化单体路线（见 product-profile-and-plugin-registry-design.md §3.1）。
%% 修改前请确认是否真要重启动态平台方向。冻结≠移除，sup 挂载保持不变。
%% FROZEN: v2 dynamic plugin loading subsystem is suspended (roadmap-only).
%% Current production route: config-driven monolith. See §3.1 before resuming.

-behaviour(gen_server).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_loader - 启动期扫描并加载插件清单
%%% imboy_plugin_loader - scan and load plugin manifests at startup
%%%
%%% 职责 / Responsibilities:
%%%   1. 扫描 priv/plugins/<name>/plugin.config（默认）或自定义目录
%%%   2. 调用 imboy_plugin_toml:load/1 解析 + 校验
%%%   3. 把合法 manifest 写入 persistent_term:put({imboy_plugin_manifest, Name}, M)
%%%   4. 失败容错：单个文件错不影响其他插件（见 contract.md §8.2）
%%%
%%% 此 gen_server 由 imboy_sup 监督（child spec 由 P0-T3 切片 2 接线）。
%%% Supervised by imboy_sup (child spec wiring lands in P0-T3 slice 2).
%%%
%%% Public API:
%%%   - start_link/0, start_link/1: 启动（默认目录 / 自定义目录）
%%%   - scan/0: 重新扫描（运维触发）
%%%   - get_manifest/1: 从 persistent_term 读取（O(1)）
%%%   - list_plugins/0: 已加载插件列表
%%%   - list_failed/0: 失败列表（路径 + 原因）
%%%
%%% Source of truth: docs/plugin/contract.md §10
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-include("imboy_plugin.hrl").

%% Public API
-export([
    start_link/0,
    start_link/1,
    scan/0,
    get_manifest/1,
    list_plugins/0,
    list_failed/0
]).

%% gen_server callbacks (full set per actor-model instinct)
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/1
]).

-record(state, {
    plugin_dir :: file:filename_all(),
    loaded = [] :: [plugin_name()],
    failed = [] :: [{file:filename_all(), term()}]
}).

-define(SERVER, ?MODULE).
-define(PT_KEY(Name), {imboy_plugin_manifest, Name}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    DefaultDir = filename:join(code:priv_dir(imboy), "plugins"),
    start_link(DefaultDir).

-spec start_link(file:filename_all()) -> {ok, pid()} | {error, term()}.
start_link(PluginDir) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PluginDir], []).

-spec scan() -> ok.
scan() ->
    gen_server:call(?SERVER, scan).

-spec get_manifest(plugin_name()) -> manifest() | undefined.
get_manifest(Name) ->
    persistent_term:get(?PT_KEY(Name), undefined).

-spec list_plugins() -> [plugin_name()].
list_plugins() ->
    gen_server:call(?SERVER, list_plugins).

-spec list_failed() -> [{file:filename_all(), term()}].
list_failed() ->
    gen_server:call(?SERVER, list_failed).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PluginDir]) ->
    State0 = #state{plugin_dir = PluginDir},
    State1 = do_scan(State0),
    {ok, State1}.

handle_call(scan, _From, State) ->
    State1 = do_scan(State),
    {reply, ok, State1};
handle_call(list_plugins, _From, State = #state{loaded = L}) ->
    {reply, L, State};
handle_call(list_failed, _From, State = #state{failed = F}) ->
    {reply, F, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    %% 优雅处理未知消息 / Handle unknown messages gracefully
    {noreply, State}.

terminate(_Reason, _State) ->
    %% 不主动清理 persistent_term：core 重启时新 loader 会覆盖
    %% Do not erase persistent_term on terminate; new loader on restart overwrites
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% OTP 26+ format_status/1（替代已废弃的 /2）
%% V2 评审 LOW-3：过滤敏感字段 — 不暴露 failed 列表（含磁盘路径）到 sys:get_status 结果。
%% Filter sensitive fields: don't expose failed list (with disk paths) via sys:get_status.
format_status(#{state := State} = Status) when is_record(State, state) ->
    %% 仅暴露 plugin 名单与计数，隐藏完整 failed 路径列表
    Sanitized = #{
        plugin_dir => State#state.plugin_dir,
        loaded => State#state.loaded,
        failed_count => length(State#state.failed)
    },
    Status#{state => Sanitized};
format_status(Status) ->
    Status.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 扫描目录，更新 persistent_term 与 state。
%% V2 评审 MEDIUM-1：先扫描新清单写入临时 acc，再原子地清理旧 PT 条目并写入新条目。
%% 这消除了原"先清理→再扫描"模式下的 ABA 删除窗口（rescan 期间 get_manifest 短暂返回 undefined）。
%% Slice scan into "scan-then-swap" to eliminate the ABA window during rescan.
do_scan(#state{plugin_dir = Dir, loaded = OldLoaded} = State) ->
    case list_plugin_dirs(Dir) of
        {ok, PluginDirs} ->
            %% 阶段 1：扫描所有插件，仅在内存中累积结果（不动 persistent_term）
            {NewLoaded, Failed} = lists:foldl(
                fun load_one_plugin_dryrun/2,
                {[], []},
                PluginDirs
            ),
            %% 阶段 2：原子替换 — 先清理旧 PT 条目（仅 loader 拥有的），再写入新条目
            cleanup_persistent_terms(OldLoaded -- proplists:get_keys(NewLoaded)),
            lists:foreach(
                fun({Name, Manifest}) ->
                    persistent_term:put(?PT_KEY(Name), Manifest)
                end,
                NewLoaded
            ),
            LoadedNames = [Name || {Name, _} <- NewLoaded],
            State#state{loaded = LoadedNames, failed = Failed};
        {error, Reason} ->
            cleanup_persistent_terms(OldLoaded),
            State#state{loaded = [], failed = [{Dir, Reason}]}
    end.

%% @doc 干跑加载：解析 + 校验 + 签名验证，返回 {Name, Manifest} 或 acc 失败列表。
%% 不写 persistent_term — 由 do_scan 阶段 2 统一原子写入。
load_one_plugin_dryrun(PluginDir, {LAcc, FAcc}) ->
    ConfigPath = filename:join(PluginDir, "plugin.config"),
    case imboy_plugin_toml:load(ConfigPath) of
        {ok, Manifest} ->
            case verify_plugin_signature(PluginDir, ConfigPath) of
                ok ->
                    Name = maps:get(name, Manifest),
                    {[{Name, Manifest} | LAcc], FAcc};
                {error, SigReason} ->
                    {LAcc, [{ConfigPath, {signature_invalid, SigReason}} | FAcc]}
            end;
        {error, Reason} ->
            {LAcc, [{ConfigPath, Reason} | FAcc]}
    end.

%% @doc 校验插件签名。
%% 策略 / Strategy:
%%   - 无可信公钥配置（undefined / []）→ 跳过校验，返回 ok（向后兼容）
%%   - SIGNATURE 文件不存在 → 跳过校验，返回 ok（无签名 = 不强制）
%%   - SIGNATURE 存在 + 有公钥 → 逐一尝试，任一公钥验证通过即 ok
%%   - SIGNATURE 存在 + 全部公钥验证失败 → {error, no_matching_key}
verify_plugin_signature(PluginDir, ConfigPath) ->
    TrustedKeys = application:get_env(imboy, plugin_trusted_public_keys, []),
    HasKeys = is_list(TrustedKeys) andalso TrustedKeys =/= [],
    case {HasKeys, file:read_file(filename:join(PluginDir, "SIGNATURE"))} of
        {false, _} ->
            ok;
        {_, {error, enoent}} ->
            ok;
        {true, {ok, Signature}} ->
            verify_against_keys(ConfigPath, TrustedKeys, Signature)
    end.

%% @doc 逐一尝试可信公钥验证签名
verify_against_keys(_ConfigPath, [], _Signature) ->
    {error, no_matching_key};
verify_against_keys(ConfigPath, [PubKey | Rest], Signature) ->
    case imboy_plugin_signature:verify_file(ConfigPath, PubKey, Signature) of
        ok -> ok;
        {error, _} -> verify_against_keys(ConfigPath, Rest, Signature)
    end.

%% @doc 列出目录下所有子目录（仅一层深度）。
%% 仅返回真目录，过滤普通文件；目录不存在返回 {error, enoent}。
list_plugin_dirs(Dir) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            PluginDirs = lists:filtermap(
                fun(Entry) ->
                    Full = filename:join(Dir, Entry),
                    case filelib:is_dir(Full) of
                        true -> {true, Full};
                        false -> false
                    end
                end,
                Entries
            ),
            {ok, PluginDirs};
        {error, _} = E ->
            E
    end.

%% V2 评审 LOW-4：风格统一为 lists:foreach，与项目其他位置一致。
cleanup_persistent_terms(Names) when is_list(Names) ->
    lists:foreach(
        fun(Name) -> _ = persistent_term:erase(?PT_KEY(Name)) end,
        Names
    ).
