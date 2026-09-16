%%% @doc CS-02 测试事实装配探针（test-only，不进任何 release）。
%%%
%%% 与 `eb09_facts_probe` 同职责，但**零 DB**：本套件（auth/handler/route contract）
%%% 全部是纯套件（纪律：不跑任何数据库测试）。`load_request_facts/1` 返回由
%%% `set/1` 预置的事实 map，或由 `fail_with/1` 预置的 `{error, Reason}`——
%%% 两者都经 persistent_term 传递，逐请求原样取回（零缓存语义由套件自证）。
-module(cs_fake_facts).

-export([load_request_facts/1, set/1, fail_with/1, clear/0]).

-define(KEY, {?MODULE, facts}).

-spec set(map()) -> ok.
set(Facts) ->
    persistent_term:put(?KEY, {ok, Facts}).

-spec fail_with(term()) -> ok.
fail_with(Reason) ->
    persistent_term:put(?KEY, {error, Reason}).

-spec clear() -> ok.
clear() ->
    persistent_term:erase(?KEY).

-spec load_request_facts(map()) -> {ok, map()} | {error, term()}.
load_request_facts(_Request) ->
    persistent_term:get(?KEY, {error, facts_not_configured}).
