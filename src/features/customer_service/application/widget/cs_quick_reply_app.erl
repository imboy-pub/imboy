%% CS Quick Reply 应用层（合同 v3/specs/cs-quick-reply.md §2/§3/§4/§5）。
%% 字段校验（trim 长度界）、行投影、越权裁决（申报范围 vs 行归属）、
%% 软删幂等、Seat 只读（assignment=active org member + workspace 归属）。
%% 本层不做认证（cs_auth 承担），不直连 HTTP 形状（cs_http 承担）。
%% 持久化只经扩展点 cs_pg_quick_reply（铁律5：不经 repo/elib_pg 直连取数，
%% 事务内多步裁决经 with_tx 组合，连接句柄不透明）。
-module(cs_quick_reply_app).

-export([
    page/1,
    create/1,
    update/1,
    delete/1,
    seat_list/1
]).

-define(TITLE_MAX, 64).
-define(CONTENT_MAX, 2000).

%% ===== Admin =====

%% Params: organization_id, workspace_id, page, size, search（全部 snake_case；
%% organization_id/workspace_id 已由 handler/cs_actions 门收敛为 integer）。
page(#{organization_id := OrgId, workspace_id := WorkspaceId} = Params) when
    is_integer(OrgId), is_integer(WorkspaceId)
->
    %% 合同 §2.1：page/size 越界 → 422（invalid_argument），不是静默回落。
    Page = maps:get(page, Params, 1),
    Size = maps:get(size, Params, 10),
    case valid_page(Page) andalso valid_size(Size) of
        false ->
            {error, invalid_argument};
        true ->
            page_ok(Params, Page, Size)
    end;
page(_Params) ->
    {error, invalid_argument}.

page_ok(#{organization_id := OrgId, workspace_id := WorkspaceId} = Params, Page, Size) ->
    Opts = #{
        page => Page,
        size => Size,
        search => maps:get(search, Params, <<>>)
    },
    case
        cs_pg_quick_reply:with_tx(fun(C) ->
            cs_pg_quick_reply:page_tx(C, OrgId, WorkspaceId, Opts)
        end)
    of
        {ok, Rows, Meta} ->
            {ok, #{
                list => [view(Row) || Row <- Rows],
                page => Page,
                size => Size,
                total => maps:get(total, Meta),
                total_page => maps:get(total_page, Meta)
            }};
        {error, _} = E ->
            E
    end.

%% Params: organization_id, workspace_id, title, content, sort?, at, actor_id
%%（created_by=认证派生 admin id；body 申报 id 由 cs_actions 字段表拒绝）。
create(
    #{
        organization_id := OrgId,
        workspace_id := WorkspaceId,
        title := Title0,
        content := Content0
    } = Params
) when
    is_integer(OrgId), is_integer(WorkspaceId)
->
    case validate_text(Title0, Content0) of
        {ok, Title, Content} ->
            Row = #{
                id => cs_tsid:new_id(cs_quick_reply),
                organization_id => OrgId,
                workspace_id => WorkspaceId,
                title => Title,
                content => Content,
                sort => maps:get(sort, Params, 0),
                created_by => maps:get(actor_id, Params, 0)
            },
            case
                cs_pg_quick_reply:with_tx(fun(C) ->
                    cs_pg_quick_reply:insert_tx(C, Row)
                end)
            of
                {ok, Rec} -> {ok, #{quick_reply => view(Rec)}};
                {error, _} = E -> E
            end;
        {error, _} = E ->
            E
    end;
create(_Params) ->
    {error, invalid_argument}.

%% Params: id, organization_id, workspace_id, title, content, sort?, at
update(
    #{
        id := Id,
        organization_id := OrgId,
        workspace_id := WorkspaceId,
        title := Title0,
        content := Content0
    } = Params
) when
    is_integer(Id), is_integer(OrgId), is_integer(WorkspaceId)
->
    case validate_text(Title0, Content0) of
        {ok, Title, Content} ->
            case
                cs_pg_quick_reply:with_tx(fun(C) ->
                    case cs_pg_quick_reply:find_tx(C, Id) of
                        {ok, Row} ->
                            case
                                maps:get(<<"organization_id">>, Row) =:= OrgId andalso
                                    maps:get(<<"workspace_id">>, Row) =:= WorkspaceId
                            of
                                %% 合同 §4.3：跨 workspace/组织 → boundary（统一
                                %% 403，不泄露存在性）。
                                true ->
                                    cs_pg_quick_reply:update_tx(C, Id, OrgId, WorkspaceId, #{
                                        title => Title,
                                        content => Content,
                                        sort => maps:get(sort, Params, 0)
                                    });
                                false ->
                                    {error, organization_boundary_violation}
                            end;
                        {error, not_found} ->
                            %% 合同 §4.4：软删/不存在 → 404 not_found。
                            {error, not_found};
                        {error, _} = E ->
                            E
                    end
                end)
            of
                {ok, Rec} -> {ok, #{quick_reply => view(Rec)}};
                {error, _} = E -> E
            end;
        {error, _} = E ->
            E
    end;
update(_Params) ->
    {error, invalid_argument}.

%% Params: id, organization_id, workspace_id, at（幂等；跨范围 → boundary）。
delete(#{id := Id, organization_id := OrgId, workspace_id := WorkspaceId}) when
    is_integer(Id), is_integer(OrgId), is_integer(WorkspaceId)
->
    case
        cs_pg_quick_reply:with_tx(fun(C) ->
            case cs_pg_quick_reply:find_tx(C, Id) of
                {ok, Row} ->
                    case
                        maps:get(<<"organization_id">>, Row) =:= OrgId andalso
                            maps:get(<<"workspace_id">>, Row) =:= WorkspaceId
                    of
                        true -> cs_pg_quick_reply:soft_delete_tx(C, Id, OrgId, WorkspaceId);
                        false -> {error, organization_boundary_violation}
                    end;
                {error, not_found} ->
                    %% 合同 §2.4：对已删目标重复 DELETE 幂等成功。
                    ok;
                {error, _} = E ->
                    E
            end
        end)
    of
        ok -> {ok, #{ok => true}};
        {ok, _} -> {ok, #{ok => true}};
        {error, _} = E -> E
    end;
delete(_Params) ->
    {error, invalid_argument}.

%% ===== Seat（只读；assignment=active org member + workspace 归属） =====

%% Params: organization_id（path 收敛）, workspace_id（query 必填）,
%% seat_user_id（cs_seat 上下文派生）。
seat_list(#{organization_id := OrgId, workspace_id := WorkspaceId} = Params) when
    is_integer(OrgId), is_integer(WorkspaceId)
->
    %% 坐席必须是该 org active member，且 workspace 属于该 org（合同 §3.1：
    %% 只信 token+assignment 派生事实；申报不可见 workspace → forbidden）。
    case
        cs_pg_quick_reply:seat_assignment_facts(
            OrgId, WorkspaceId, maps:get(seat_user_id, Params, 0)
        )
    of
        {true, true} ->
            case
                cs_pg_quick_reply:with_tx(fun(C) ->
                    cs_pg_quick_reply:list_tx(C, OrgId, WorkspaceId)
                end)
            of
                {ok, Rows} ->
                    %% Seat 投影：id/title/content/sort 白名单（合同 §7）。
                    {ok, #{
                        list => [
                            #{
                                id => maps:get(<<"id">>, Row),
                                title => maps:get(<<"title">>, Row),
                                content => maps:get(<<"content">>, Row),
                                sort => maps:get(<<"sort">>, Row)
                            }
                         || Row <- Rows
                        ]
                    }};
                {error, _} = E ->
                    E
            end;
        _ ->
            {error, forbidden}
    end;
seat_list(_Params) ->
    {error, invalid_argument}.

%% ===== 内部 =====

validate_text(Title0, Content0) when is_binary(Title0), is_binary(Content0) ->
    Title = string:trim(Title0),
    Content = string:trim(Content0),
    LenT = string:length(Title),
    LenC = string:length(Content),
    case
        LenT > 0 andalso LenT =< ?TITLE_MAX andalso
            LenC > 0 andalso LenC =< ?CONTENT_MAX
    of
        true -> {ok, Title, Content};
        false -> {error, invalid_argument}
    end;
validate_text(_, _) ->
    {error, invalid_argument}.

valid_page(N) when is_integer(N), N >= 1 -> true;
valid_page(_) -> false.
valid_size(N) when is_integer(N), N >= 1, N =< 100 -> true;
valid_size(_) -> false.
valid_sort(N) when is_integer(N) -> true;
valid_sort(_) -> false.

view(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        organization_id => maps:get(<<"organization_id">>, Row),
        workspace_id => maps:get(<<"workspace_id">>, Row),
        title => maps:get(<<"title">>, Row),
        content => maps:get(<<"content">>, Row),
        sort => maps:get(<<"sort">>, Row),
        created_by => maps:get(<<"created_by">>, Row),
        created_at => maps:get(<<"created_at">>, Row),
        updated_at => maps:get(<<"updated_at">>, Row)
    }.
