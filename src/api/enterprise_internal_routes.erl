-module(enterprise_internal_routes).

%%%
% enterprise_internal_routes 是 /api/internal/v1/* 冻结路由注册表
% （EPGZ-02，manifest routes INT-01..INT-14 逐字映射）。
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

%% @doc 冻结路由表（manifest INT-01..INT-14 逐行对应）。
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
        %% INT-18 群详情
        #{
            id => <<"INT-18">>,
            method => <<"GET">>,
            path => <<"/api/internal/v1/groups/{group_id}">>,
            scope => <<"groups:write">>,
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
