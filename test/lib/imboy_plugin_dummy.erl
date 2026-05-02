-module(imboy_plugin_dummy).
-behaviour(imboy_plugin).

%%%-------------------------------------------------------------------
%%% @doc
%%% 测试用 mock 插件，验证 imboy_plugin behaviour 契约可被合规实现。
%%% Test-only mock plugin to validate the imboy_plugin behaviour contract.
%%%
%%% 仅用于 imboy_plugin_tests.erl，**不**在生产环境加载。
%%% Used only by imboy_plugin_tests.erl; NOT loaded in production.
%%%
%%% Source of truth: doc/plugin/contract.md §4.3 implementation example
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-export([
    manifest/0,
    start/1,
    stop/1,
    migrate/3,
    routes/0,
    capabilities/0,
    health/0
]).

%% ===================================================================
%% imboy_plugin callbacks
%% ===================================================================

manifest() ->
    #{
        name => imboy_plugin_dummy,
        version => <<"0.1.0">>,
        contract_version => {1, 0},
        kind => plugin,
        description => <<"测试用 mock 插件 / Test-only mock plugin"/utf8>>,
        depends_on => #{},
        requires_capabilities => [],
        min_core_version => <<"1.0.0-rc.2">>,
        children => [],
        routes => [],
        migrations => #{
            dir => <<"priv/migrations">>,
            table_prefix => <<"imboy_plugin_dummy_">>,
            preserve_on_uninstall => true
        },
        features => #{},
        limits => #{
            rps_per_uid => 0,
            max_concurrent_calls => 0,
            msg_quota_per_day => 0
        },
        budget => #{
            max_connections => 0,
            max_subscriptions => 0,
            mem_mb_soft_limit => 64
        },
        degrade => #{on_unhealthy => route_404},
        circuit_breaker => #{
            enabled => false,
            threshold_pct => 50,
            window_sec => 60
        },
        entries => #{app => [], admin => []},
        i18n => #{keys => []},
        subscribes => [],
        publishes => [],
        publishes_meta => #{},
        audit => #{events => []},
        meta => #{}
    }.

start(_StartArgs) ->
    {ok, undefined}.

stop(_State) ->
    ok.

migrate(_Direction, _From, _To) ->
    ok.

routes() ->
    [].

capabilities() ->
    [].

health() ->
    #{status => healthy}.
