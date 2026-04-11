-module(adm_setup_handler).
%%%
%%% adm_setup_handler — 首启初始化向导 API（P0-5）
%%%
%%% 路由（免鉴权，需加入 imboy_router:open/0 白名单）：
%%%   GET  /adm/setup/status  → 查询是否已初始化
%%%   POST /adm/setup/init    → 创建超级管理员（仅允许一次）
%%%
%%% 安全：
%%%   - is_initialized() 双重防线：配置 flag + adm_user 表存在性
%%%   - 成功后永久 flag，保证不可重复调用
%%%%
-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        status -> status(Method, Req0);
        init_setup -> init_setup(Method, Req0);
        _ -> cowboy_req:reply(404, #{}, <<"Not Found">>, Req0)
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec status(binary(), cowboy_req:req()) -> cowboy_req:req().
status(<<"GET">>, Req0) ->
    Initialized = adm_setup_logic:is_initialized(),
    elib_response:success(Req0, #{<<"initialized">> => Initialized});
status(_, Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec init_setup(binary(), cowboy_req:req()) -> cowboy_req:req().
init_setup(<<"POST">>, Req0) ->
    PostVals = elib_param:post(Req0),
    Params = #{
        <<"account">>  => to_bin(maps:get(<<"account">>,  PostVals, <<>>)),
        <<"password">> => to_bin(maps:get(<<"password">>, PostVals, <<>>)),
        <<"nickname">> => to_bin(maps:get(<<"nickname">>, PostVals, <<>>))
    },
    case adm_setup_logic:do_init(Params) of
        {ok, AdminId} ->
            elib_response:success(Req0, #{<<"id">> => AdminId}, <<"初始化成功"/utf8>>);
        {error, Code} when is_integer(Code) ->
            Msg = maps:get(Code, ?ERROR_MSG_MAP, <<"首启参数无效"/utf8>>),
            elib_response:error(Req0, Msg, Code);
        {error, _} ->
            elib_response:error(Req0,
                                <<"首启初始化失败"/utf8>>,
                                ?ERR_SETUP_INVALID_PARAMS)
    end;
init_setup(_, Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_list(V) -> list_to_binary(V);
to_bin(_) -> <<>>.
