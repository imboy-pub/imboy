-module(cs_preflight_facts_pg).

-moduledoc "Customer Service 域 DeletionPreflightFacts provider（ORG-08 / C17，PG 实现）。".
%% Customer Service 域 DeletionPreflightFacts provider（ORG-08 / C17 / 计划 §1.6）。
%%
%% 只读：本模块是 User deletion preflight 的 CS 域实时事实源，不做 handover/
%% offboarding、不改任何 CS state（providers 只读是 §1.6 冻结纪律）。资源处理
%% 由各域显式 command 先完成（compatibility doc §6：User delete 前 Assignment
%% 必须无 active；Seat/Session 不随 User 删除）。
%%
%% 冻结 blocker（§1.6 稳定 code 清单）：
%%   * `CUSTOMER_SERVICE_OPERATION_ACTIVE` —— 用户当前**实际在岗**的客服坐席：
%%     存在 status='active' 且 function_key='customer_service' 的经办绑定，
%%     且该 Business Identity 的 Seat enabled=true（operator=active Assignment
%%     + seat enabled 门，与 cs_auth 的 A04 坐席门同一判定口径）。
%%     handover（assignment 结束）即消除本 blocker，Seat 保留——与 §6 矩阵
%%     「User delete：Assignment 必须无 active；Seat 不随 User 删除」一致。
%%
%% 不产生 blocker 的情形（有意，零重解释）：
%%   * Seat enabled 但无 active assignment —— 无 operator，历史 Seat 保留；
%%   * active assignment 但 Seat 停用 —— 坐席门已关闭，无在岗操作面；
%%   * 历史 Session —— Session 不随 User 删除（§6），不构成 blocker。
%%
%% 返回形状（§1.6 逐字段冻结；不含 PII/credential/内部 SQL；resource_id opaque）：
%%   {ok, #{subject_user_id => UserId, domain => customer_service,
%%          observed_at => Ms, fact_version => 1,
%%          blockers => [#{code => B, resource_type => T,
%%                         resource_id => OpaqueId, organization_id => OrgId}]}}
%%   | {error, unavailable}

-export([facts_customer_service/1]).
-export([fact_version/0]).

-define(FACT_VERSION, 1).

-define(SQL_CS_OPERATION, <<
    "SELECT a.id AS assignment_id, a.organization_id, a.business_identity_id"
    "  FROM organization_business_identity_assignment a"
    "  JOIN customer_service_seat s"
    "    ON s.organization_id = a.organization_id"
    "   AND s.business_identity_id = a.business_identity_id"
    " WHERE a.user_id = $1 AND a.status = 'active'"
    "   AND a.function_key = 'customer_service'"
    "   AND s.enabled = true"
    " ORDER BY a.id"
>>).

%% @doc CS 域实时 facts（§1.6 冻结形状）。查询失败一律 `{error, unavailable}`
%% ——编排器（organization_deletion_preflight）会把 unavailable 归口为
%% `DEPENDENCY_FACTS_UNAVAILABLE` 整体拒绝，本模块不做本地兜底。
-spec facts_customer_service(integer()) -> {ok, map()} | {error, unavailable}.
facts_customer_service(UserId) when is_integer(UserId), UserId > 0 ->
    case elib_pg:query(?SQL_CS_OPERATION, [UserId]) of
        {ok, Rows} ->
            Blockers = [
                #{
                    code => <<"CUSTOMER_SERVICE_OPERATION_ACTIVE">>,
                    resource_type => <<"customer_service_seat">>,
                    resource_id => opaque(maps:get(<<"business_identity_id">>, Row)),
                    organization_id => maps:get(<<"organization_id">>, Row)
                }
             || Row <- Rows
            ],
            {ok, fact(UserId, Blockers)};
        {error, _Reason} ->
            {error, unavailable}
    end;
facts_customer_service(_) ->
    {error, unavailable}.

-spec fact_version() -> pos_integer().
fact_version() ->
    ?FACT_VERSION.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

fact(UserId, Blockers) ->
    #{
        subject_user_id => UserId,
        domain => customer_service,
        observed_at => erlang:system_time(millisecond),
        fact_version => ?FACT_VERSION,
        blockers => Blockers
    }.

opaque(Id) when is_integer(Id) ->
    integer_to_binary(Id);
opaque(Id) when is_binary(Id) ->
    Id.
