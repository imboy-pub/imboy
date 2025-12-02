-module(imadm_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


%% @doc 启动管理后台应用
%% 初始化验证码 ETS 表、配置路由、启动 Cowboy 监听器和监督者
%% @param Type 启动类型（通常为 normal）
%% @param Args 启动参数
%% @return {ok, Pid} | {error, Reason} 启动结果
-spec start(atom(), any()) -> {ok, pid()} | {error, any()}.
start(_Type, _Args) ->
    simple_captcha_ets:init(),
    Routes = [
        {
            config_ds:env(host),
            [
                % need auth
                {"/adm", adm_index_handler, #{action => index}},
                {"/adm/index", adm_index_handler, #{action => index}},
                {"/adm/welcome", adm_index_handler, #{action => welcome}},
                % feedback
                {"/adm/feedback/index", adm_feedback_handler, #{action => index}},
                {"/adm/feedback/reply", adm_feedback_handler, #{action => reply}},
                % app ddl
                {"/adm/app_ddl/index", adm_app_ddl_handler, #{action => index}},
                {"/adm/app_ddl/save", adm_app_ddl_handler, #{action => save}},
                {"/adm/app_ddl/delete", adm_app_ddl_handler, #{action => delete}},
                % app version
                {"/adm/app_version/index", adm_app_version_handler, #{action => index}},
                {"/adm/app_version/save", adm_app_version_handler, #{action => save}},
                {"/adm/app_version/delete", adm_app_version_handler, #{action => delete}},
                % attach
                {"/adm/attach/auth", adm_attach_handler, #{action => auth}},
                % need auth end

                % open
                {"/adm/passport/login", adm_passport_handler, #{action => login}},
                {"/adm/passport/captcha", adm_passport_handler, #{action => captcha}},
                {"/adm/passport/do_login", adm_passport_handler, #{action => do_login}},
                %%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%
                {"/static/[...]", cowboy_static, {priv_dir, imadm, "static", [{mimetypes, cow_mimetypes, all}]}}
            ]
        }
    ],
    Dispatch = cowboy_router:compile(Routes),

    ProtoOpts = #{
        middlewares => [
            cowboy_router % 必须是第一个元素
            , adm_auth_middleware % 必须是第二个元素
            , cowboy_handler
        ],
        % metrics_callback => do_metrics_callback(),
        env => #{dispatch => Dispatch}
    },

    Port = config_ds:env(http_port_adm, 9806),
    start_clear(ProtoOpts, Port),
    imadm_sup:start_link().


%% @doc 停止管理后台应用
%% 停止 Cowboy 监听器并清理资源
%% @param State 应用状态
%% @return ok 停止成功标识
-spec stop(any()) -> ok.
stop(_State) ->
    cowboy:stop_listener(imadm_listener),
    ok.


%% @doc 启动明文 HTTP 监听器
%% 配置并启动 Cowboy HTTP 监听器
%% @param ProtoOpts 协议选项配置
%% @param Port 监听端口号
%% @return {ok, Pid} | {error, Reason} 启动结果
-spec start_clear(map(), integer()) -> {ok, pid()} | {error, any()}.
start_clear(ProtoOpts, Port) ->
    cowboy:start_clear(imadm_listener, [{port, Port}], ProtoOpts).
