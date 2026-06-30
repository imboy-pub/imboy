-module(adm_operation_log_handler).
-compile([nowarn_deprecated_catch]).

%%%
% 管理员操作审计日志查询接口
% GET /adm/operation_logs?adm_user_id=&action=&page=1&size=20
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            list -> list(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"audit:read">>, Req0) of
        {error, Req1} ->
            Req1;
        ok ->
            Qs = cowboy_req:parse_qs(Req0),
            Page = elib_cnv:to_integer(proplists:get_value(<<"page">>, Qs, <<"1">>)),
            Size = min(elib_cnv:to_integer(proplists:get_value(<<"size">>, Qs, <<"20">>)), 100),
            Offset = (max(Page, 1) - 1) * Size,
            Opts0 = #{limit => Size, offset => Offset},
            Opts1 =
                case proplists:get_value(<<"adm_user_id">>, Qs) of
                    undefined -> Opts0;
                    V -> Opts0#{adm_user_id => elib_cnv:to_integer(V)}
                end,
            Opts2 =
                case proplists:get_value(<<"action">>, Qs) of
                    undefined -> Opts1;
                    V2 -> Opts1#{action => V2}
                end,
            case adm_operation_log_ds:list(Opts2) of
                {ok, Rows} ->
                    elib_response:success(Req0, #{
                        list => Rows,
                        page => Page,
                        size => Size
                    });
                {error, _} ->
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_SERVER_ERROR)
            end
    end;
list(_, Req0, _State) ->
    elib_response:error(Req0, <<"Method Not Allowed">>, ?ERR_METHOD_NOT_ALLOWED).
