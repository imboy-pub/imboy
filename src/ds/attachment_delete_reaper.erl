%%% Independent bounded lease recovery: provider DELETE cannot block either queue's timer.
-module(attachment_delete_reaper).
-behaviour(gen_server).
-export([start_link/1, child_specs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("generated/imboy_product_features.hrl").
-define(INTERVAL, 30000).

start_link(ordinary) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ordinary, []);
start_link(enterprise) ->
    gen_server:start_link({local, enterprise_attachment_delete_reaper}, ?MODULE, enterprise, []).

child_specs() ->
    case application:get_env(imboy, attachment_delete_worker_enabled, false) of
        true ->
            [
                #{
                    id => attachment_delete_worker,
                    start => {attachment_delete_worker, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [attachment_delete_worker]
                },
                spec(ordinary)
            ] ++ enterprise_specs();
        false ->
            []
    end.
spec(Queue) ->
    #{
        id => {attachment_delete_reaper, Queue},
        start => {?MODULE, start_link, [Queue]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    }.
-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).
enterprise_specs() ->
    case imboy_feature:enabled(enterprise_business) of
        true -> [spec(enterprise)];
        false -> []
    end.
enterprise_reap() -> eb_pg_asset_delete_queue:reap(100).
-else.
enterprise_specs() -> [].
enterprise_reap() -> {error, feature_not_compiled}.
-endif.

init(Queue) ->
    Enabled = application:get_env(imboy, attachment_delete_worker_enabled, false),
    {ok, schedule(#{queue => Queue, enabled => Enabled})}.
handle_info(reap, State = #{enabled := true, queue := Queue}) ->
    %% SQL is bounded to 100 rows and a 5s statement timeout. No byte deletion here.
    Result =
        case Queue of
            ordinary -> attachment_gc_repo:v5_reap(100);
            enterprise -> enterprise_reap()
        end,
    case Result of
        {ok, _} -> ok;
        Other -> logger:warning("Attachment ~p lease recovery failed: ~p", [Queue, Other])
    end,
    {noreply, schedule(State)};
handle_info(reap, State) ->
    {noreply, schedule(State)};
handle_info(_, State) ->
    {noreply, State}.
schedule(State) ->
    cancel(State),
    State#{timer => erlang:send_after(?INTERVAL, self(), reap)}.
cancel(#{timer := Ref}) ->
    erlang:cancel_timer(Ref),
    ok;
cancel(_) ->
    ok.
handle_call(_, _, State) -> {reply, ok, State}.
handle_cast(_, State) -> {noreply, State}.
terminate(_, State) -> cancel(State).
code_change(_, State, _) -> {ok, State}.
