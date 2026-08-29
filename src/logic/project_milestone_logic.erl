-module(project_milestone_logic).
-compile([nowarn_deprecated_catch]).
%%%
% project_milestone_logic 项目里程碑业务逻辑（迁移 00000081，W2 / ZC-03）
%
% 字段边界（计划契约：字段只允许 name/due_date/status）：
%   * create：name 必填（≤200 字符，varchar(200) 列宽）；due_date 可选
%     （YYYY-MM-DD | null）；status 不可经 create 指定（里程碑恒以 planned
%     诞生，达成只走 reach 端点——携带 status 的请求在 handler 层 400）；
%   * update：name/due_date 可选提交（undefined=保留；due_date null=清空）；
%     status 同样不可经 update 变更（handler 层 400）；
%   * 其余未知字段一律忽略（拒绝或忽略的契约取"忽略"，status 例外为显式拒绝）。
%
% W2 权限：
%   写（create/update/reach）：Project Owner 或 active Project Member，
%     且为 active Workspace Member 且 role≠guest；guest 只读 403；
%     非 active Project Member 403（读同样受 Project 访问边界约束）。
%   读（list）：active Workspace Member 且（Owner 或 active Project Member）。
%     guest 若在册可读。
%   归档 Workspace：拒写 980（workspace_guard 同事务守卫）、允许读。
%   校验实现：DS 层事务内 ensure_writer_tx（owner 直判 + project_member 只读
%   直查 project_milestone_repo:find_project_member*——ZC-05 整合时统一到
%   project_member_logic）。
%
% 状态机：planned→reached 单向；重复 reach 幂等（already_reached，不重复写
% 事件）；reached→planned 无端点即拒绝。
%
% 错误约定：{error, {Code, Msg}}，稳定码 980=工作区已归档。
%%%

-export([create/4]).
-export([list/5]).
-export([update/4]).
-export([reach/2]).
-export([admin_page/4]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Admin 治理只读：项目里程碑分页（adm ACL workspaces:read 门；
%% 不走 workspace 成员权限语义。total 由独立 COUNT 查询提供——M-7：
%% 分页时 total 不再以当前页行数近似）
-spec admin_page(integer(), binary() | all, integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_page(ProjectId, Status, Page0, Size0) ->
    Page = max(1, Page0),
    Size = max(1, min(100, Size0)),
    case project_milestone_repo:list_by_project(ProjectId, Status, Page, Size) of
        {ok, Rows} ->
            case project_milestone_repo:count_by_project(ProjectId, Status) of
                {ok, Total} ->
                    TotalPage =
                        case Total > 0 of
                            true -> ((Total - 1) div Size) + 1;
                            false -> 0
                        end,
                    {ok, #{
                        list => Rows,
                        page => Page,
                        size => Size,
                        total => Total,
                        total_page => TotalPage
                    }};
                {error, Reason} ->
                    _ = ?ERROR_LOG([admin_milestone_page_failed, ProjectId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end;
        {error, Reason} ->
            _ = ?ERROR_LOG([admin_milestone_page_failed, ProjectId, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

%% @doc 创建里程碑（DueDateBin = <<"YYYY-MM-DD">> | null | undefined）
-spec create(integer(), integer(), binary(), binary() | null | undefined) ->
    {ok, map()} | {error, {integer(), binary()}}.
create(Uid, ProjectId, Name, DueDateBin) ->
    case valid_name(Name) of
        false ->
            {error, {400, <<"里程碑名称不能为空且不超过 200 字符"/utf8>>}};
        true ->
            case parse_due_date(DueDateBin) of
                {error, Reason} ->
                    {error, Reason};
                DueDate ->
                    wrap_ds(project_milestone_ds:create(Uid, ProjectId, Name, DueDate))
            end
    end.

%% @doc 项目里程碑列表（分页；Status 过滤 all|planned|reached，其余归一 all）
-spec list(integer(), integer(), binary() | all, integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
list(Uid, ProjectId, Status, Page, Size) ->
    case ensure_can_read(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            Status2 = normalize_status(Status),
            {Page2, Size2} = normalize_page(Page, Size),
            case project_milestone_ds:list_by_project(ProjectId, Status2, Page2, Size2) of
                {ok, Rows} ->
                    {ok, #{list => Rows, page => Page2, size => Size2}};
                {error, Reason2} ->
                    _ = ?ERROR_LOG([project_milestone_list_failed, ProjectId, Reason2]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc 更新里程碑（Name/DueDateBin = undefined 表示保留；DueDateBin = null 清空）
-spec update(
    integer(),
    integer(),
    binary() | undefined,
    binary() | null | undefined
) ->
    {ok, map()} | {error, {integer(), binary()}}.
update(Uid, MsId, Name, DueDateBin) ->
    case valid_optional_name(Name) of
        false ->
            {error, {400, <<"里程碑名称不能为空且不超过 200 字符"/utf8>>}};
        true ->
            case parse_due_date_update(DueDateBin) of
                {error, Reason} ->
                    {error, Reason};
                DueDate ->
                    wrap_ds(project_milestone_ds:update(Uid, MsId, Name, DueDate))
            end
    end.

%% @doc 达成里程碑（planned→reached 单向；幂等）
-spec reach(integer(), integer()) ->
    {ok, map(), reached | already_reached} | {error, {integer(), binary()}}.
reach(Uid, MsId) ->
    case project_milestone_ds:reach(Uid, MsId) of
        {ok, Ms, Flag} ->
            {ok, Ms, Flag};
        {error, {Code, Msg}} when is_integer(Code) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([project_milestone_reach_failed, MsId, Uid, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end.

%% ===================================================================
%% 读权限与归一（Internal）
%% ===================================================================

%% @doc 读权限：active 工作区成员（guest 可读）+ Project 访问边界
%% （Owner 或 active Project Member；非项目成员 403——W2 project_member
%% 即访问边界，直达 milestone 接口同拒）
-spec ensure_can_read(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_read(Uid, ProjectId) ->
    case project_logic:detail(Uid, ProjectId) of
        {error, Reason} ->
            %% 404 / 非工作区成员 403 原样透传
            {error, Reason};
        {ok, Project} ->
            case is_project_member(Uid, ProjectId, Project) of
                true -> {ok, Project};
                false -> {error, {403, <<"仅项目成员可访问里程碑"/utf8>>}}
            end
    end.

%% Owner 直判；否则直查 project_member active（ZC-05 统一到 project_member_logic）
-spec is_project_member(integer(), integer(), map()) -> boolean().
is_project_member(Uid, ProjectId, Project) ->
    case maps:get(<<"owner_id">>, Project, 0) of
        Uid ->
            true;
        _ ->
            case project_milestone_repo:find_project_member(ProjectId, Uid, <<"status">>) of
                #{<<"status">> := <<"active">>} -> true;
                _ -> false
            end
    end.

-spec normalize_status(binary() | all) -> binary() | all.
normalize_status(<<"planned">>) -> <<"planned">>;
normalize_status(<<"reached">>) -> <<"reached">>;
normalize_status(_) -> all.

-spec normalize_page(integer(), integer()) -> {integer(), integer()}.
normalize_page(Page, Size) ->
    {max(1, to_pos(Page)), max(1, min(100, to_pos(Size)))}.

-spec to_pos(integer()) -> integer().
to_pos(V) when is_integer(V), V > 0 -> V;
to_pos(_) -> 1.

-spec valid_name(term()) -> boolean().
valid_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    %% varchar(200) 列宽，按字符数校验防截断报错
    string:length(Name) =< 200;
valid_name(_) ->
    false.

-spec valid_optional_name(term()) -> boolean().
valid_optional_name(undefined) -> true;
valid_optional_name(Name) -> valid_name(Name).

%% create：无 due_date 视为无到期日；非法格式 400
-spec parse_due_date(binary() | null | undefined) ->
    {integer(), integer(), integer()} | null | {error, {400, binary()}}.
parse_due_date(Date) when Date =:= undefined; Date =:= null; Date =:= <<>> ->
    null;
parse_due_date(Date) when is_binary(Date) ->
    case parse_date_tuple(Date) of
        {ok, D} -> D;
        false -> {error, {400, <<"due_date 格式须为 YYYY-MM-DD"/utf8>>}}
    end;
parse_due_date(_) ->
    {error, {400, <<"due_date 格式须为 YYYY-MM-DD"/utf8>>}}.

%% update：undefined=保留原值；null/空串=清空；合法 YYYY-MM-DD=tuple
-spec parse_due_date_update(binary() | null | undefined) ->
    {integer(), integer(), integer()} | null | undefined | {error, {400, binary()}}.
parse_due_date_update(undefined) ->
    undefined;
parse_due_date_update(Date) ->
    parse_due_date(Date).

%% 严格 YYYY-MM-DD → {Y,M,D}（含 calendar:valid_date 语义校验，防溢出）
-spec parse_date_tuple(binary()) -> {ok, {integer(), integer(), integer()}} | false.
parse_date_tuple(Date) ->
    case binary:split(Date, <<"-">>, [global]) of
        [Y, M, D] when byte_size(Y) =:= 4, byte_size(M) =:= 2, byte_size(D) =:= 2 ->
            try
                Yi = binary_to_integer(Y),
                Mi = binary_to_integer(M),
                Di = binary_to_integer(D),
                case calendar:valid_date(Yi, Mi, Di) of
                    true -> {ok, {Yi, Mi, Di}};
                    false -> false
                end
            catch
                _:_ -> false
            end;
        _ ->
            false
    end.

%% DS 稳定错误码（404/403/980）原样透传；未知错误归一 500
-spec wrap_ds({ok, map()} | {error, term()}) -> {ok, map()} | {error, {integer(), binary()}}.
wrap_ds({ok, Ms}) ->
    {ok, Ms};
wrap_ds({error, {Code, Msg}}) when is_integer(Code) ->
    {error, {Code, Msg}};
wrap_ds({error, Reason}) ->
    _ = ?ERROR_LOG([project_milestone_ds_failed, Reason]),
    {error, {500, <<"操作失败，请稍后重试"/utf8>>}}.
