-module(imboy_plugin_path).

%% @status FROZEN (roadmap-only, 2026-06)：v2 动态加载子系统暂停投入。
%% 当前生产走配置驱动模块化单体路线（见 product-profile-and-plugin-registry-design.md §3.1）。
%% 修改前请确认是否真要重启动态平台方向。冻结≠移除。
%% FROZEN: v2 dynamic plugin loading subsystem is suspended (roadmap-only).
%% Current production route: config-driven monolith. See §3.1 before resuming.

%%%-------------------------------------------------------------------
%%% @doc
%%% SEC-02（审计 #43）：install Path 参数的受控插件根白名单收口。
%%%
%%% 之前 adm_plugin_handler:do_install/3 把 POST path 原样传给
%%% imboy_plugin_manager:install/2，lifecycle 直接用该 Path 拼
%%% plugin.config/SIGNATURE 读取 —— 无 realpath、无 `..`/symlink/越界检查，
%%% admin 可达即任意目录读取面。
%%%
%%% 收口规则（fail-closed）：
%%%   - 受控插件根：{imboy, plugin_root}（IMBOY_PLUGIN_ROOT 可覆盖），
%%%     默认仓内受控目录 priv/plugins（相对时按 cwd absname）。
%%%   - resolve/1：realpath 解析（消解 `..` 与 symlink）后必须位于插件根内，
%%%     且必须是已存在目录；根本身无效 / 路径越界 / 非目录 / 不存在一律
%%%     返回 {error, Reason}，不 crash。
%%%   - ensure_file_within/1：plugin.config/SIGNATURE 文件级收口 —— 存在则
%%%     realpath 必须在根内（堵“根内目录 + symlink 外读”）；不存在放行
%%%     （缺席语义由签名校验 / manifest 解析步骤拒绝，见 imboy_plugin_signature）。
%%%
%%% 与 manager:install/2（入口层）和 imboy_plugin_lifecycle:run_install_steps/2
%%% （读路径处）双层接线：绕过任一层，另一层仍 fail-closed。
%%%
%%% 残余风险（TOCTOU）：resolve 与后续读取之间根内文件可被并发替换，
%%% 静态校验无法消除；动态插件子系统整体 @status FROZEN 且 lifecycle
%%% 默认禁用，该窗口不构成当前攻击面。
%%% @end
%%%-------------------------------------------------------------------

-export([
    plugin_root/0,
    resolve/1,
    ensure_file_within/1
]).

-include_lib("kernel/include/file.hrl").

%% 受控插件根默认值：仓内受控目录（发布物内 priv/plugins）。
-define(DEFAULT_PLUGIN_ROOT, <<"priv/plugins">>).

%% symlink 展开上限（对齐 POSIX ELOOP 语义，防 symlink 环死循环）。
-define(MAX_SYMLINK_EXPANSIONS, 40).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 受控插件根（absname 后的绝对路径 binary）。
%% 配置项 {imboy, plugin_root}，环境变量 IMBOY_PLUGIN_ROOT 可覆盖
%% （见 imboy_env:override_plugin_root/0）。
-spec plugin_root() -> binary().
plugin_root() ->
    Raw = config_ds:env(plugin_root, ?DEFAULT_PLUGIN_ROOT),
    to_bin(Raw).

%% @doc 校验 install Path 并返回 canonical（realpath）路径。
%%
%% 放行条件（全部满足，唯一放行面）：
%%   1. 插件根存在且为目录（否则 {error, plugin_root_invalid} 拒绝一切）；
%%   2. Path 经 realpath 解析（消解 `..`、`.`、symlink）后位于插件根内
%%      （含等于根；前缀组件级比较，杜绝 /root-evil 对 /root 的前缀陷阱）；
%%   3. 解析结果是目录。
%%
%% 错误码（明确、不 crash）：
%%   {error, plugin_root_invalid}        根不存在 / 非目录
%%   {error, path_outside_plugin_root}   `..` 穿越 / 绝对越界 / symlink 逃逸
%%   {error, path_not_directory}         根内普通文件
%%   {error, path_not_found}             路径不存在（含中间目录缺失）
%%   {error, {file_error, Reason}}       其他文件系统错误
-spec resolve(binary() | string()) -> {ok, binary()} | {error, term()}.
resolve(Path) ->
    case canonical_root() of
        {ok, RealRoot} ->
            Abs = filename:absname(to_bin(Path)),
            case real_path(Abs) of
                {ok, RealPath} ->
                    case is_within(RealRoot, RealPath) of
                        true ->
                            ensure_directory(RealPath);
                        false ->
                            {error, path_outside_plugin_root}
                    end;
                {error, enoent} ->
                    {error, path_not_found};
                {error, Reason} ->
                    {error, {file_error, Reason}}
            end;
        {error, _} = E ->
            E
    end.

%% @doc 文件级收口：FilePath（plugin.config / SIGNATURE 等）存在时，
%% realpath 必须位于插件根内；不存在放行（由读取方按缺席语义拒绝）。
-spec ensure_file_within(binary() | string()) -> ok | {error, term()}.
ensure_file_within(FilePath) ->
    case canonical_root() of
        {ok, RealRoot} ->
            Abs = filename:absname(to_bin(FilePath)),
            case real_path(Abs) of
                {ok, RealFile} ->
                    case is_within(RealRoot, RealFile) of
                        true -> ok;
                        false -> {error, path_outside_plugin_root}
                    end;
                {error, enoent} ->
                    %% 文件不存在：OS 层同样无法解析（含 `..` 的不存在路径
                    %% read 也 enoent），放行交由缺席语义处理。
                    ok;
                {error, Reason} ->
                    {error, {file_error, Reason}}
            end;
        {error, _} = E ->
            E
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 自实现 realpath（物理语义，OTP file 模块无此函数）：
%% 逐组件解析，symlink 展开（相对目标基于当前已解析栈），`..` 按已解析
%% 路径回退（kernel 路径解析语义，非 lexical），`.` 跳过；
%% 展开次数超上限返回 {error, eloop}。
%% 组件统一 binary：file:read_link/1 的 symlink 目标按文件系统存储原样
%% 返回（常为 string），不转换会污染整个组件表导致 join 结果类型漂移。
real_path(AbsPath) ->
    Components = [to_bin(C) || C <- filename:split(to_bin(AbsPath))],
    resolve_path(Components, [], 0).

resolve_path([], Acc, _Depth) ->
    {ok, join_path(Acc)};
resolve_path([<<".">> | Rest], Acc, Depth) ->
    resolve_path(Rest, Acc, Depth);
resolve_path([<<"..">> | Rest], Acc, Depth) ->
    resolve_path(Rest, pop_component(Acc), Depth);
resolve_path([Comp | Rest], Acc, Depth) when Depth < ?MAX_SYMLINK_EXPANSIONS ->
    Current = join_path(Acc ++ [Comp]),
    case file:read_link_info(Current) of
        {ok, #file_info{type = symlink}} ->
            case file:read_link(Current) of
                {ok, Target0} ->
                    Target = to_bin(Target0),
                    Depth1 = Depth + 1,
                    case filename:pathtype(Target) of
                        absolute ->
                            resolve_path(
                                [to_bin(C) || C <- filename:split(Target)] ++ Rest, [], Depth1
                            );
                        relative ->
                            resolve_path(
                                [to_bin(C) || C <- filename:split(Target)] ++ Rest, Acc, Depth1
                            )
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, _} ->
            resolve_path(Rest, Acc ++ [Comp], Depth);
        {error, Reason} ->
            {error, Reason}
    end;
resolve_path(_Comps, _Acc, _Depth) ->
    {error, eloop}.

join_path([]) ->
    <<"/">>;
join_path(Components) ->
    filename:join(Components).

%% `..` 回退：移除**末尾**组件（已解析栈的最后一级目录）。
pop_component([]) -> [];
pop_component(Components) -> lists:droplast(Components).

%% @doc 插件根 realpath：根必须存在且为目录，否则拒绝一切（fail-closed）。
canonical_root() ->
    AbsRoot = filename:absname(plugin_root()),
    case real_path(AbsRoot) of
        {ok, RealRoot} ->
            case file:read_file_info(RealRoot) of
                {ok, #file_info{type = directory}} ->
                    {ok, RealRoot};
                {ok, _} ->
                    {error, plugin_root_invalid};
                {error, Reason} ->
                    {error, {file_error, Reason}}
            end;
        {error, enoent} ->
            {error, plugin_root_invalid};
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

ensure_directory(RealPath) ->
    case file:read_file_info(RealPath) of
        {ok, #file_info{type = directory}} ->
            {ok, RealPath};
        {ok, _} ->
            {error, path_not_directory};
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% @doc 组件级前缀包含判断：RealPath 等于根或位于根的直接/深层之下。
%% 用 filename:split 组件比较而非二进制前缀，杜绝 /a-evil 误匹配 /a。
is_within(Root, Path) ->
    RootComponents = filename:split(Root),
    PathComponents = filename:split(Path),
    lists:prefix(RootComponents, PathComponents).

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8).
