-module(adm_announcement_handler).
-dialyzer({nowarn_function, [index/3]}).
-dialyzer({nowarn_function, [filter_opts/3]}).

%%%
% 全局公告管理控制器模块
% Global announcement management controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

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
    Req1 =
        case Action of
            index ->
                index(Method, Req0, State);
            create ->
                create(Method, Req0, State);
            update ->
                update(Method, Req0, State);
            delete ->
                delete(Method, Req0, State);
            publish ->
                publish(Method, Req0, State);
            unpublish ->
                unpublish(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 公告列表
-spec index(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    {ok, Status} = elib_param:int(<<"status">>, Req0, undefined),
    {ok, Type} = elib_param:binary(<<"type">>, Req0, undefined),
    {ok, Keyword} = elib_param:binary(<<"keyword">>, Req0, undefined),
    Filters = filter_opts(Status, Type, Keyword),
    {ok, P} = announcement_ds:list(Page, Size, Filters),
    elib_response:success(Req0, P);
index(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 创建公告
-spec create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
create(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"announcements:create">>, Req0) of
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            PostVals = elib_param:post(Req0),
            Data = maps:merge(PostVals, #{<<"adm_user_id">> => AdmUserId}),
            case announcement_ds:create(Data) of
                {ok, Result} ->
                    elib_response:success(Req0, Result, <<"创建成功"/utf8>>);
                {error, Reason} ->
                    elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
create(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 更新公告
-spec update(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
update(<<"PUT">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"announcements:update">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Id = maps:get(<<"id">>, PostVals, 0),
            if
                is_integer(Id), Id > 0 ->
                    case announcement_ds:update(Id, PostVals) of
                        {ok, Result} ->
                            elib_response:success(Req0, Result, <<"更新成功"/utf8>>);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
                    end;
                true ->
                    elib_response:error(Req0, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
update(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 删除公告（软删除）
-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"announcements:delete">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Id = maps:get(<<"id">>, PostVals, 0),
            if
                is_integer(Id), Id > 0 ->
                    case announcement_ds:delete_by_id(Id) of
                        {ok, Result} ->
                            elib_response:success(Req0, Result, <<"删除成功"/utf8>>);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
                    end;
                true ->
                    elib_response:error(Req0, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
delete(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 发布公告
-spec publish(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
publish(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"announcements:publish">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Id = maps:get(<<"id">>, PostVals, 0),
            if
                is_integer(Id), Id > 0 ->
                    case announcement_ds:publish(Id) of
                        {ok, Result} ->
                            elib_response:success(Req0, Result, <<"发布成功"/utf8>>);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
                    end;
                true ->
                    elib_response:error(Req0, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
publish(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 撤回公告
-spec unpublish(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
unpublish(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"announcements:publish">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Id = maps:get(<<"id">>, PostVals, 0),
            if
                is_integer(Id), Id > 0 ->
                    case announcement_ds:unpublish(Id) of
                        {ok, Result} ->
                            elib_response:success(Req0, Result, <<"撤回成功"/utf8>>);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
                    end;
                true ->
                    elib_response:error(Req0, <<"参数错误"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
unpublish(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% Internal helpers
%% ===================================================================

-spec filter_opts(term(), term(), term()) -> map().
filter_opts(undefined, undefined, undefined) ->
    #{};
filter_opts(Status, Type, Keyword) ->
    Opts0 = #{},
    Opts1 =
        case Status of
            undefined -> Opts0;
            _ -> maps:put(status, Status, Opts0)
        end,
    Opts2 =
        case Type of
            undefined -> Opts1;
            _ -> maps:put(type, Type, Opts1)
        end,
    Opts3 =
        case Keyword of
            undefined -> Opts2;
            _ -> maps:put(keyword, Keyword, Opts2)
        end,
    Opts3.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
