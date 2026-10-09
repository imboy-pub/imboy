-module(imboy_route_wiring).
-moduledoc "Pure customer-service route metadata wiring; route literals stay in imboy_router.".
-export([customer_service/1]).

%% @doc 给客服面的路由注入三个**面级**不变量键（CS-02，模式照 enterprise_wire/1）。
%%
%% `surface`（由 handler 决定）、`feature`（恒为 customer_service）与 `auth_facts`
%% （本面的只读事实装配：租户成员事实 `eb_pg_auth_facts`、平台事实
%% `eb_platform_auth_facts`；访客/门店凭证类不用事实，注入无害）在同一个面上
%% 恒等，21 条路由逐条手写只会制造漂移面。逐条**可变**的键（path / handler /
%% action / auth_context / required_*）仍全部字面登记在路由表里。
-spec customer_service(list()) -> list().
customer_service(Routes) ->
    [customer_service_route(Route) || Route <- Routes].

customer_service_route({Path, cs_tenant_handler, Opts}) when is_map(Opts) ->
    {Path, cs_tenant_handler, Opts#{
        surface => tenant,
        feature => customer_service,
        auth_facts => eb_pg_auth_facts
    }};
customer_service_route({Path, cs_widget_handler, Opts}) when is_map(Opts) ->
    {Path, cs_widget_handler, Opts#{
        surface => widget,
        feature => customer_service,
        %% widget 面的凭证校验在 application 用例内（bootstrap 令牌 digest），
        %% 不用事实装配；注入与租户面同键保持面级不变量一致（无害）。
        auth_facts => eb_pg_auth_facts
    }};
%% BE-W01 A05：动态 frame HTML 与 cs_widget_handler 同属 widget 面——面级
%% 不变量键（surface/feature/auth_facts）逐字同款，防面间漂移。
customer_service_route({Path, cs_widget_frame_handler, Opts}) when is_map(Opts) ->
    {Path, cs_widget_frame_handler, Opts#{
        surface => widget,
        feature => customer_service,
        auth_facts => eb_pg_auth_facts
    }};
%% seat-console-embed SC-BE：/seat/:public_seat_console_id 嵌入面与
%% cs_widget_handler 同属 widget 面——面级不变量键（surface/feature/auth_facts）
%% 逐字同款，防面间漂移。
customer_service_route({Path, cs_seat_console_handler, Opts}) when is_map(Opts) ->
    {Path, cs_seat_console_handler, Opts#{
        surface => widget,
        feature => customer_service,
        auth_facts => eb_pg_auth_facts
    }};
customer_service_route({Path, cs_platform_handler, Opts}) when is_map(Opts) ->
    {Path, cs_platform_handler, Opts#{
        surface => platform,
        feature => customer_service,
        auth_facts => eb_platform_auth_facts
    }};
customer_service_route(Route) ->
    Route.
