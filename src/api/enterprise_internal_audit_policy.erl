-module(enterprise_internal_audit_policy).

%%%
% enterprise_internal_audit_policy 是 /api/internal/v1/* 写操作（mutation）
% 的**逐项审计政策冻结表**（INT-BE-03 / 验收 INT-API-02B）。
%
% 覆盖口径：enterprise_internal_routes:routes() 中全部非 GET 操作（20 条 =
% 16 条 idempotency=required mutation + INT-14 single_use_code + 3 条只读
% 语义 POST）。GET 只读操作天然无审计需求，不进本表；本表与路由表的一致性
% （非 GET 全覆盖、无表外条目）由测试机械比对，任一侧漏登记即红灯。
%
% verdict 两值：
%   required_audit         —— 成功必须落 enterprise_audit_event 审计行
%                             （audit_action / audit_resource_type 冻结）；
%                             审计行与业务写在**同一事务**提交：审计失败
%                             ⇒ 业务整体失败，业务失败 ⇒ 审计行一并回滚。
%   registered_deviation   —— 登记豁免 + 理由（reason 非空），不得静默缺席。
%
% 审计行七字段口径（REQUIRED 条目统一断言）：
%   organization_id（列）/ resource_type + resource_id（列）/ action（列）/
%   actor_user_id + actor_role='enterprise_application'（列）/
%   detail.origin_application_id（application）/ detail.correlation_id
%   （correlation：Idempotency-Key 或请求级随机串）。
%
% 幂等重放政策（冻结条款）：§11 幂等重放命中缓存时**不重执行业务**，因而
% 不产生重复审计行——这是 REQUIRED_AUDIT 条目的通用条款，由幂等矩阵测试
% 逐条证明；INT-14 的重放由 code CAS 一次性消费保证（重放 → resource_not_found
% 拒绝 → 无业务变更 → 无审计行）。
%%%

-export([policies/0, verdict/1, required_ids/0, deviation_ids/0, audit_action/1]).

-type verdict() :: required_audit | registered_deviation.

-export_type([verdict/0]).

%%%===================================================================
%%% 冻结政策表（INT-02..17, 19, 20, 21, 22 —— 路由表全部非 GET）
%%%===================================================================

%% @doc 逐项审计政策（20 条；顺序无关，测试按 id 比对）。
-spec policies() -> [map(), ...].
policies() ->
    [
        %% ---- REQUIRED_AUDIT（16 条）----
        #{
            id => <<"INT-02">>,
            method => <<"PUT">>,
            path => <<"/api/internal/v1/identity-mappings">>,
            verdict => required_audit,
            audit_action => <<"identity.mapping.bound">>,
            audit_resource_type => <<"enterprise_external_identity">>,
            reason => <<"映射绑定是身份解析真源的状态变更，必须留痕（绑定/重绑覆盖均审计）"/utf8>>
        },
        #{
            id => <<"INT-04">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/groups">>,
            verdict => required_audit,
            audit_action => <<"group.created">>,
            audit_resource_type => <<"group">>,
            reason => <<"建群是群生命周期的起点写操作，必须留痕"/utf8>>
        },
        #{
            id => <<"INT-05">>,
            method => <<"PUT">>,
            path => <<"/api/internal/v1/groups/{group_id}/members">>,
            verdict => required_audit,
            audit_action => <<"group.members.added">>,
            audit_resource_type => <<"group">>,
            reason => <<"成员集变更是群访问边界变更，必须留痕"/utf8>>
        },
        #{
            id => <<"INT-06">>,
            method => <<"DELETE">>,
            path => <<"/api/internal/v1/groups/{group_id}/members">>,
            verdict => required_audit,
            audit_action => <<"group.members.removed">>,
            audit_resource_type => <<"group">>,
            reason => <<"成员移除影响可见性与消息边界，必须留痕"/utf8>>
        },
        #{
            id => <<"INT-08">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/files/confirm">>,
            verdict => required_audit,
            audit_action => <<"file.confirmed">>,
            audit_resource_type => <<"attachment">>,
            reason => <<"附件转正产生可引用资源（attachment 行 + 留存治理行），必须留痕"/utf8>>
        },
        #{
            id => <<"INT-09">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/messages/direct">>,
            verdict => required_audit,
            audit_action => <<"message.enterprise.accepted">>,
            audit_resource_type => <<"msg_c2c">>,
            reason => <<"OA 代发消息（已有审计：message.enterprise.accepted，同事务）"/utf8>>
        },
        #{
            id => <<"INT-10">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/groups/{group_id}/messages">>,
            verdict => required_audit,
            audit_action => <<"message.enterprise.accepted">>,
            audit_resource_type => <<"msg_c2g">>,
            reason => <<"OA 群发消息（已有审计：message.enterprise.accepted，同事务）"/utf8>>
        },
        #{
            id => <<"INT-11">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/friend-requests">>,
            verdict => required_audit,
            audit_action => <<"friend_request.created">>,
            audit_resource_type => <<"friend_request">>,
            reason => <<"以 Human 名义代发起社交动作，必须留痕（只发起、不代接受）"/utf8>>
        },
        #{
            id => <<"INT-12">>,
            method => <<"PUT">>,
            path => <<"/api/internal/v1/webhook">>,
            verdict => required_audit,
            audit_action => <<"webhook.configured">>,
            audit_resource_type => <<"enterprise_webhook_config">>,
            reason => <<"Webhook 端点配置/轮换决定后续全部出站投递目标，必须留痕"/utf8>>
        },
        #{
            id => <<"INT-13">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/webhook/deliveries/{delivery_id}/replay">>,
            verdict => required_audit,
            audit_action => <<"webhook.delivery.replayed">>,
            audit_resource_type => <<"bot_delivery">>,
            reason => <<"重放产生新的出站投递行（事件重新外发），必须留痕"/utf8>>
        },
        #{
            id => <<"INT-14">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/oa/sso/exchange">>,
            verdict => required_audit,
            audit_action => <<"oa.sso.exchanged">>,
            audit_resource_type => <<"enterprise_oa_sso">>,
            reason =>
                <<
                    "一次性 code 消费并代用户建立会话，安全敏感必须留痕；"
                    "重放已消费 code 走统一 404 拒绝（无业务变更 → 无审计行），"
                    "幂等由 code CAS 保证，无重复审计可能"/utf8
                >>
        },
        #{
            id => <<"INT-15">>,
            method => <<"DELETE">>,
            path => <<"/api/internal/v1/identity-mappings">>,
            verdict => required_audit,
            audit_action => <<"identity.mapping.revoked">>,
            audit_resource_type => <<"enterprise_external_identity">>,
            reason => <<"撤销映射使 sender/成员解析立即失效，必须留痕"/utf8>>
        },
        #{
            id => <<"INT-19">>,
            method => <<"PATCH">>,
            path => <<"/api/internal/v1/groups/{group_id}">>,
            verdict => required_audit,
            audit_action => <<"group.updated">>,
            audit_resource_type => <<"group">>,
            reason => <<"群元数据（标题/简介）变更必须留痕"/utf8>>
        },
        #{
            id => <<"INT-20">>,
            method => <<"PUT">>,
            path => <<"/api/internal/v1/groups/{group_id}/members/roles">>,
            verdict => required_audit,
            audit_action => <<"group.member_roles.set">>,
            audit_resource_type => <<"group">>,
            reason => <<"成员角色变更影响权限面（群主角色不可经 OA 变更），必须留痕"/utf8>>
        },
        #{
            id => <<"INT-21">>,
            method => <<"DELETE">>,
            path => <<"/api/internal/v1/groups/{group_id}">>,
            verdict => required_audit,
            audit_action => <<"group.archived">>,
            audit_resource_type => <<"group">>,
            reason => <<"群归档是生命周期终态（不可逆单向门），必须留痕"/utf8>>
        },
        #{
            id => <<"INT-22">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/files/governance">>,
            verdict => required_audit,
            audit_action => <<"file.governance.op">>,
            audit_resource_type => <<"attachment">>,
            reason =>
                <<
                    "留存/hold/purge 是法务治理动作（purge 不可逆），必须留痕；"
                    "audit_action 冻结为 file.governance.op，具体 op 落 detail.op"/utf8
                >>
        },
        %% ---- REGISTERED_DEVIATION（4 条）----
        #{
            id => <<"INT-03">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/identity-mappings/resolve">>,
            verdict => registered_deviation,
            audit_action => null,
            audit_resource_type => null,
            reason =>
                <<
                    "只读语义：按入参批量解析已存在映射（POST 仅承载批量入参），"
                    "无任何状态变更；路由注册即 rate_bucket=internal_read"/utf8
                >>
        },
        #{
            id => <<"INT-07">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/files/presign">>,
            verdict => registered_deviation,
            audit_action => null,
            audit_resource_type => null,
            reason =>
                <<
                    "签发计算语义：presign 只产出短期 PUT URL，无业务状态变更；"
                    "pending 登记为 best-effort 软注册（失败不阻断签发，孤儿由"
                    "生命周期回收），审计锚由后续 REQUIRED 的 INT-08 confirm "
                    "（或未 confirm 的对象删除）提供"/utf8
                >>
        },
        #{
            id => <<"INT-16">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/identity-mappings/directory">>,
            verdict => registered_deviation,
            audit_action => null,
            audit_resource_type => null,
            reason =>
                <<
                    "只读语义：映射 cursor 目录分页查询（POST 仅承载游标入参），"
                    "无状态变更；受限分页（无全量导出形态）由 directory 套件钉死"/utf8
                >>
        },
        #{
            id => <<"INT-17">>,
            method => <<"POST">>,
            path => <<"/api/internal/v1/directory/users">>,
            verdict => registered_deviation,
            audit_action => null,
            audit_resource_type => null,
            reason =>
                <<
                    "只读语义：成员 cursor 目录分页查询（POST 仅承载游标入参），"
                    "无状态变更；最小字段投影 + 硬上限由 directory 套件钉死"/utf8
                >>
        }
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 按 operation id 查政策（未登记 → error，fail-closed）。
-spec verdict(binary()) -> verdict().
verdict(Id) ->
    case policy(Id) of
        #{verdict := V} -> V;
        false -> erlang:error({audit_policy_missing, Id})
    end.

%% @doc REQUIRED 条目的 operation id 集合（16 条）。
-spec required_ids() -> [binary()].
required_ids() ->
    [maps:get(id, P) || P <- policies(), maps:get(verdict, P) =:= required_audit].

%% @doc DEVIATION 条目的 operation id 集合（4 条）。
-spec deviation_ids() -> [binary()].
deviation_ids() ->
    [maps:get(id, P) || P <- policies(), maps:get(verdict, P) =:= registered_deviation].

%% @doc REQUIRED 条目冻结的审计 action（DEVIATION → undefined）。
-spec audit_action(binary()) -> binary() | undefined.
audit_action(Id) ->
    case policy(Id) of
        #{verdict := required_audit, audit_action := A} -> A;
        _ -> undefined
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec policy(binary()) -> map() | false.
policy(Id) ->
    case [P || P <- policies(), maps:get(id, P) =:= Id] of
        [P | _] -> P;
        [] -> false
    end.
