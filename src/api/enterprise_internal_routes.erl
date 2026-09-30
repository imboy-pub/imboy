-module(enterprise_internal_routes).

%%%
% enterprise_internal_routes 是 /api/internal/v1/* 冻结路由注册表
% （EPGZ-02 起，FULL-02/03 扩到 23 条；V2.1 扩到 31 条 = plan §6.1
% INT-01..INT-31 逐字映射）。
%
% 注册表是 internal 面的唯一路由真源：method+path 不在表内一律
% {error, not_found}（上层映射 resource_not_found，fail-closed，
% 不落人类 /api/v1/* 或 /api/adm/* —— INV-2）。
%
% Route map 字段（atom 键）：
%   id            :: <<"INT-XX">>（manifest 路由 ID）
%   method        :: HTTP 方法二进制
%   path          :: 模式二进制（{seg} 为单段占位符，如 {group_id}）
%   scope         :: 固定 scope 二进制 | {dynamic, messages_send}
%                    （INT-09/INT-10 按 sender_mode 在 handler 侧裁决）
%   rate_bucket   :: internal_read | internal_write | internal_sso
%   idempotency   :: required | not_required | single_use_code（INT-14 豁免）
%   sender_mode   :: none | application | human | application_human
%%%

-export([routes/0, match/2, prefix/0]).

-define(PREFIX, <<"/api/internal/v1/">>).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc internal 面路径前缀（manifest allowed_prefix）。
-spec prefix() -> binary().
prefix() ->
    ?PREFIX.

%% @doc 冻结路由表（plan §6.1 INT-01..INT-31 逐行对应；31 unique method+path）。
-spec routes() -> [map(), ...].
routes() ->
    [
        #{
            id => <<"INT-01">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/application">>,
            scope => <<"application:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        #{
            id => <<"INT-02">>,
            method => <<"PUT">>,
            path => <<"/api/internal/v1/identity-mappings">>,
            scope => <<"identities:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-03">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/identity-mappings/resolve">>,
            scope => <<"identities:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        #{
            id => <<"INT-04">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/groups">>,
            scope => <<"groups:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-05">>,
            method => <<"PUT">>,
            path => <<"/api/internal/v1/groups/{group_id}/members">>,
            scope => <<"groups:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-06">>,
            method => <<"DELETE">>,
            path => <<"/api/internal/v1/groups/{group_id}/members">>,
            scope => <<"groups:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-07">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/files/presign">>,
            scope => <<"files:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-08">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/files/confirm">>,
            scope => <<"files:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-09">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/messages/direct">>,
            scope => {dynamic, messages_send},
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => application_human
        },
        #{
            id => <<"INT-10">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/groups/{group_id}/messages">>,
            scope => {dynamic, messages_send},
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => application_human
        },
        #{
            id => <<"INT-11">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/friend-requests">>,
            scope => <<"friend_requests:create">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => human
        },
        #{
            id => <<"INT-12">>,
            method => <<"PUT">>,
            path => <<"/api/internal/v1/webhook">>,
            scope => <<"webhooks:manage">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-13">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/webhook/deliveries/{delivery_id}/replay">>,
            scope => <<"webhooks:manage">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-14">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/oa/sso/exchange">>,
            scope => <<"sso:exchange">>,
            rate_bucket => internal_sso,
            idempotency => single_use_code,
            sender_mode => none
        },
        %% ---- FULL-02 新增（A0 接线）。grant 边界规格见
        %%      enterprise_internal_boundary:spec/1，两侧必须逐条一致
        %%      （有测试机械比对）。----
        %% INT-15 撤销 external identity 映射（org scoped）。DELETE 不便带
        %% body 语义，故固定为集合端点 + body 指定 external_user_id。
        #{
            id => <<"INT-15">>,
            method => <<"DELETE">>,
            path => <<"/api/internal/v1/identity-mappings">>,
            scope => <<"identities:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        %% INT-16 映射 cursor directory（受限分页；无全量导出形态）
        #{
            id => <<"INT-16">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/identity-mappings/directory">>,
            scope => <<"identities:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-17 成员 cursor directory（受限分页）
        #{
            id => <<"INT-17">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/directory/users">>,
            scope => <<"identities:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-18 群详情（V2.1 scope 修正：读操作降为 groups:read——§6.2/§7；
        %%      此前误用 groups:write 导致只读查看被迫要求写授权）
        #{
            id => <<"INT-18">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/groups/{group_id}">>,
            scope => <<"groups:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-19 群更新
        #{
            id => <<"INT-19">>,
            method => <<"PATCH">>,
            path => <<"/api/internal/v1/groups/{group_id}">>,
            scope => <<"groups:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        %% INT-20 成员角色
        #{
            id => <<"INT-20">>,
            method => <<"PUT">>,
            path => <<"/api/internal/v1/groups/{group_id}/members/roles">>,
            scope => <<"groups:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        %% INT-21 群归档（DELETE = 归档语义）
        #{
            id => <<"INT-21">>,
            method => <<"DELETE">>,
            path => <<"/api/internal/v1/groups/{group_id}">>,
            scope => <<"groups:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        %% INT-22 附件留存/hold/purge 治理
        #{
            id => <<"INT-22">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/files/governance">>,
            scope => <<"files:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        %% ---- FULL-03 新增（A0 接线）----
        %% INT-23 投递列表 + 健康度摘要（只读；无 payload——信封键集封闭）
        #{
            id => <<"INT-23">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/webhook/deliveries">>,
            scope => <<"webhooks:manage">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% ---- V2.1 新增资源只读面（plan §6.1 冻结 INT-24..31；A1 只登记
        %%      注册条目，handler 模块（enterprise_workspace_handler /
        %%      enterprise_group_handler 扩展 / enterprise_project_handler /
        %%      enterprise_channel_handler）由 A2 实现后经 A0 接线进
        %%      imboy_router。注册表本身不引用 handler 模块名，编译期无
        %%      依赖；match/2 在 Router 接线前即可对这 8 条做方法/路径/
        %%      scope 求值（认证链负例天然可测，正例待 A2）。----
        %% INT-24 Workspace 列表（只读 keyset；行集被 Grant 覆盖 W 收窄）
        #{
            id => <<"INT-24">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/workspaces">>,
            scope => <<"workspaces:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-25 Workspace 详情（path W 边界）
        #{
            id => <<"INT-25">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/workspaces/{workspace_id}">>,
            scope => <<"workspaces:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-26 企业群列表（只读 keyset；origin app + 覆盖 W 过滤）
        #{
            id => <<"INT-26">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/groups">>,
            scope => <<"groups:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-27 群成员列表（只读 keyset；group W 边界）
        #{
            id => <<"INT-27">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/groups/{group_id}/members">>,
            scope => <<"groups:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-28 企业项目列表（只读 keyset；W 过滤必填）
        #{
            id => <<"INT-28">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/projects">>,
            scope => <<"projects:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-29 项目详情（project W 边界）
        #{
            id => <<"INT-29">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/projects/{project_id}">>,
            scope => <<"projects:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-30 工作区频道列表（只读 keyset；scope=workspace AND status=1，W 必填）
        #{
            id => <<"INT-30">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/channels">>,
            scope => <<"channels:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-31 频道详情（channel W 边界；status=1 only）
        #{
            id => <<"INT-31">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/channels/{channel_id}">>,
            scope => <<"channels:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        %% INT-32 测试投递（v1.1.1 追加）：合成 webhook.ping 事件出站，
        %% 验证集成方回调链路。与 INT-12/13/23 同为 application 自属面。
        #{
            id => <<"INT-32">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/webhook/test-delivery">>,
            scope => <<"webhooks:manage">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-33">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/customer-service/seats">>,
            scope => <<"customer_service:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        #{
            id => <<"INT-34">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/customer-service/seats/{business_identity_id}">>,
            scope => <<"customer_service:read">>,
            rate_bucket => internal_read,
            idempotency => not_required,
            sender_mode => none
        },
        #{
            id => <<"INT-35">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/customer-service/seats">>,
            scope => <<"customer_service:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        },
        #{
            id => <<"INT-36">>,
            method => <<"PATCH">>,
            path => <<"/api/internal/v1/customer-service/seats/{business_identity_id}">>,
            scope => <<"customer_service:write">>,
            rate_bucket => internal_write,
            idempotency => required,
            sender_mode => none
        }
    ].

%% @doc method+path 精确匹配冻结路由表（{seg} 占位符匹配任意非空单段）。
%% 非表内组合一律 {error, not_found}（fail-closed；INV-2：credential
%% 只能进本表路径，本表之外的 /api/v1、/api/adm、/api/open 同样 not_found）。
-spec match(binary(), binary()) -> {ok, map()} | {error, not_found}.
match(Method, Path) when is_binary(Method), is_binary(Path) ->
    Segments = path_segments(Path),
    Matched = [
        Route
     || Route <- routes(),
        maps:get(method, Route) =:= Method,
        pattern_match(path_segments(maps:get(path, Route)), Segments)
    ],
    case Matched of
        [Route | _] -> {ok, Route};
        [] -> {error, not_found}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec path_segments(binary()) -> [binary()].
path_segments(Path) ->
    [S || S <- binary:split(Path, <<"/">>, [global, trim_all]), S =/= <<>>].

%% {name} 形态段匹配任意非空段；其余段逐字相等。
-spec pattern_match([binary()], [binary()]) -> boolean().
pattern_match([], []) ->
    true;
pattern_match([P | Pt], [S | St]) ->
    case is_placeholder(P) of
        true when S =/= <<>> -> pattern_match(Pt, St);
        false when P =:= S -> pattern_match(Pt, St);
        _ -> false
    end;
pattern_match(_, _) ->
    false.

-spec is_placeholder(binary()) -> boolean().
is_placeholder(<<"{", Rest/binary>>) ->
    case Rest of
        <<"}">> -> false;
        <<Name/binary>> -> binary:part(Name, byte_size(Name) - 1, 1) =:= <<"}">>
    end;
is_placeholder(_) ->
    false.
