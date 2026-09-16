%%% @doc EB-09 测试装配探针：**在真实只读事实上补足装配缺失的权限**（test-only）。
%%%
%%% 为什么需要它（如实登记，不粉饰）：
%%%
%%%   * route metadata 按 plan §5.4 词汇表声明写动作所需权限（`contact.write` /
%%%     `note.write` / `conversation.write` / `message.write` / `asset.read|write` /
%%%     `offboarding.manage` / `member.suspend`），但当前**唯一**的租户事实装配
%%%     `eb_pg_auth_facts` 的静态权限映射只产出 4 个值
%%%     （`org.manage` / `member.manage` / `contact.read` / `conversation.read`），
%%%     且 `member` 角色只拿到后两个。于是「真装配 + 真库」下**任何写动作恒 403**
%%%     （`permission_missing`）——这是装配缺口（EB-09-F1），不是本卡可以就地修的
%%%     （`eb_pg_auth_facts` 属 infrastructure 租约）。
%%%
%%% 本探针因此只做一件事：把**真实源**读到的事实（成员状态、有效经办关系、
%%% 角色权限集）原样取回，再把「本请求所测动作声明的那个权限」追加进 `permissions`。
%%% 它**不发明**成员关系、经办关系或角色：那些仍由 `eb_pg_auth_facts` 逐请求读库；
%%% 被追加的权限是**本套件自证要测的那一个**（并在套件里同时用真装配观察到 403，
%%% 使缺口是可复现的事实而不是被掩盖的假设）。
-module(eb09_facts_probe).

-behaviour(eb_auth_port).

-export([load_request_facts/1, grant/1, clear/0, granted/0]).

-define(KEY, {?MODULE, granted}).

%% @doc 设置本次请求要补足的权限（单进程内使用；套件串行）。
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

%% @doc 真事实 + 追加权限（追加项单列 `granted_permissions`，便于证据脚本区分
%% 「真源权限」与「测试补足的权限」）。
-spec load_request_facts(map()) -> {ok, map()} | {error, term()}.
load_request_facts(Request) ->
    case eb_pg_auth_facts:load_request_facts(Request) of
        {ok, Facts} ->
            Real = maps:get(permissions, Facts, []),
            {ok, Facts#{
                permissions => lists:usort(Real ++ granted()),
                granted_permissions => granted(),
                permissions_source => {real, eb_pg_auth_facts}
            }};
        {error, _} = Err ->
            Err
    end.
