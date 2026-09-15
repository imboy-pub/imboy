%%% @doc EB-09 平台面测试装配探针（test-only）：把「本动作声明的平台权限」作为
%%% **测试装配**注入，其余（成员/身份/权限之外的一切）仍走真 facade + 真 PG。
%%%
%%% 为什么需要：生产装配 `eb_platform_auth_facts` 的权限来自 DB 里的 Admin 角色
%%% ACL；本套件不造 admin 角色行（那是运维/产品数据，不属于测试夹具）。因此：
%%%   * 「真装配 + 无 ACL」的观测是 403 permission_missing（套件里已单独覆盖）；
%%%   * 授权**之后**的参数门与租户隔离用本探针驱动，权限值显式来自被测动作在动作表
%%%     里声明的那个（`enterprise_business:read|write`），不是任意放宽。
%%% 本探针不含 SQL、不读写业务表。
-module(eb09_platform_facts_probe).

-behaviour(eb_auth_port).

-export([load_request_facts/1, grant/1, clear/0, granted/0]).

-define(KEY, {?MODULE, permissions}).

-spec grant([binary()]) -> ok.
grant(Permissions) ->
    persistent_term:put(?KEY, Permissions),
    ok.

-spec clear() -> ok.
clear() ->
    persistent_term:erase(?KEY),
    ok.

-spec granted() -> [binary()].
granted() ->
    case persistent_term:get(?KEY, undefined) of
        undefined -> [];
        Permissions -> Permissions
    end.

-spec load_request_facts(map()) -> {ok, map()} | {error, term()}.
load_request_facts(Request) when is_map(Request) ->
    case maps:get(adm_user_id, Request, undefined) of
        AdmUserId when is_integer(AdmUserId), AdmUserId > 0 ->
            {ok, #{adm_user_id => AdmUserId, permissions => granted()}};
        _Missing ->
            {error, {missing_adm_user_id, Request}}
    end;
load_request_facts(_Request) ->
    {error, invalid_request}.
