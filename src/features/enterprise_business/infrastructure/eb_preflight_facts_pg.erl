-module(eb_preflight_facts_pg).

%% Enterprise Business 域 DeletionPreflightFacts provider（ORG-08 / C17 / 计划 §1.6）。
%%
%% 只读：User deletion preflight 的 EB 域实时事实源。不做 handover/offboarding
%% （那是 eb_pg_offboarding_ext 的写用例）；offboarding guard
%% （trg_organization_member_offboarding_guard，00000114）是最终 DB 裁决，
%% preflight 只是解释性证据（§1.6），两者同向 fail-closed。
%%
%% 冻结 blocker（§1.6 稳定 code 清单）：
%%   * `BUSINESS_IDENTITY_ASSIGNMENT_ACTIVE` —— 用户名下任一 active 经办绑定
%%     （organization_business_identity_assignment，不分 function_key：sales 与
%%     customer_service 都是 C17「active Assignment 未 handover/end 必拒」的
%%     覆盖面；CS 侧的坐席在岗面由 cs_preflight_facts_pg 另行冻结报告）。
%%
%% 返回形状（§1.6 逐字段冻结；不含 PII/credential/内部 SQL；resource_id opaque）：
%%   {ok, #{subject_user_id => UserId, domain => enterprise_business,
%%          observed_at => Ms, fact_version => 1,
%%          blockers => [#{code => B, resource_type => T,
%%                         resource_id => OpaqueId, organization_id => OrgId}]}}
%%   | {error, unavailable}

-export([facts_enterprise_business/1]).
-export([fact_version/0]).

-define(FACT_VERSION, 1).

-define(SQL_ACTIVE_ASSIGNMENTS, <<
    "SELECT id, organization_id"
    "  FROM organization_business_identity_assignment"
    " WHERE user_id = $1 AND status = 'active'"
    " ORDER BY id"
>>).

%% @doc EB 域实时 facts（§1.6 冻结形状）。查询失败一律 `{error, unavailable}`
%% ——编排器归口 `DEPENDENCY_FACTS_UNAVAILABLE` 整体拒绝，不做本地兜底。
-spec facts_enterprise_business(integer()) -> {ok, map()} | {error, unavailable}.
facts_enterprise_business(UserId) when is_integer(UserId), UserId > 0 ->
    case elib_pg:query(?SQL_ACTIVE_ASSIGNMENTS, [UserId]) of
        {ok, Rows} ->
            Blockers = [
                #{
                    code => <<"BUSINESS_IDENTITY_ASSIGNMENT_ACTIVE">>,
                    resource_type => <<"organization_business_identity_assignment">>,
                    resource_id => opaque(maps:get(<<"id">>, Row)),
                    organization_id => maps:get(<<"organization_id">>, Row)
                }
             || Row <- Rows
            ],
            {ok, fact(UserId, Blockers)};
        {error, _Reason} ->
            {error, unavailable}
    end;
facts_enterprise_business(_) ->
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
        domain => enterprise_business,
        observed_at => erlang:system_time(millisecond),
        fact_version => ?FACT_VERSION,
        blockers => Blockers
    }.

opaque(Id) when is_integer(Id) ->
    integer_to_binary(Id);
opaque(Id) when is_binary(Id) ->
    Id.
