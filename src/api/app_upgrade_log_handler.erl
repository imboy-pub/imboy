-module(app_upgrade_log_handler).

%%%
% app_upgrade_log 控制器模块
% 客户端升级事件上报 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            report ->
                report(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 上报升级事件
%% POST /v1/app_upgrade/report
%%
%% Body:
%%   event        - 事件类型（必填）
%%   client_vsn   - 客户端当前版本（必填）
%%   target_vsn   - 目标版本（可选）
%%   upgrade_type - 升级类型（可选）
%%   extra        - 额外信息（可选 JSON）
%%
%% Header:
%%   cos - 平台类型
%%   did - 设备ID
-spec report(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
report(<<"POST">>, Req0, _State) ->
    {ok, Body0, _Req1} = elib_req:body(Req0, []),
    Cos = cowboy_req:header(<<"cos">>, Req0, <<>>),
    DID = cowboy_req:header(<<"did">>, Req0, <<>>),

    Event = maps:get(<<"event">>, Body0, <<>>),
    ClientVsn = maps:get(<<"client_vsn">>, Body0, <<>>),

    case {Event, ClientVsn} of
        {<<>>, _} ->
            elib_response:error(Req0, <<"missing event">>, 400);
        {_, <<>>} ->
            elib_response:error(Req0, <<"missing client_vsn">>, 400);
        _ ->
            %% 异步写入，不阻塞客户端
            Data = #{
                <<"did">> => DID,
                <<"uid">> => ec_cnv:to_integer(maps:get(<<"uid">>, Body0, 0)),
                <<"cos">> => Cos,
                <<"client_vsn">> => ClientVsn,
                <<"target_vsn">> => maps:get(<<"target_vsn">>, Body0, <<>>),
                <<"event">> => Event,
                <<"upgrade_type">> => maps:get(<<"upgrade_type">>, Body0, <<>>),
                <<"extra">> => jsone:encode(
                    maps:get(<<"extra">>, Body0, #{}),
                    [native_utf8, {float_format, [{decimals, 4}, compact]}]
                )
            },
            elib_async:async(fun() -> app_upgrade_log_ds:insert(Data) end),
            elib_response:success(Req0, #{<<"ok">> => true})
    end;
report(_, Req0, _State) ->
    elib_response:error(Req0, <<"Method Not Allowed">>, 405).

%% ===================================================================
%% EUnit tests.
%% ===================================================================
