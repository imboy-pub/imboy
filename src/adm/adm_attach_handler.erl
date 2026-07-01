-module(adm_attach_handler).
-dialyzer({nowarn_function, [index/3]}).

%%%
% adm_attach 控制器模块
% adm_attach controller module
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
            stats -> stats(Method, Req0, State);
            index -> index(Method, Req0, State);
            download -> download(Method, Req0, State);
            disable -> disable(Method, Req0, State);
            enable -> enable(Method, Req0, State);
            delete -> delete(Method, Req0, State);
            orphan -> orphan(Method, Req0, State);
            orphan_cleanup -> orphan_cleanup(Method, Req0, State);
            false -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec stats(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
stats(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"storage:view">>, Req0) of
        ok ->
            Result = attachment_ds:stats(),
            elib_response:success(Req0, Result, "success.");
        {error, Req1} ->
            Req1
    end;
stats(_, Req0, _State) ->
    Req0.

-spec index(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"storage:view">>, Req0) of
        ok ->
            {Page, Size} = elib_param:page(Req0),
            Qs = cowboy_req:parse_qs(Req0),
            MimeType = proplists:get_value(<<"mime_type">>, Qs, undefined),
            Keyword = proplists:get_value(<<"keyword">>, Qs, undefined),
            Opts = #{mime_type => MimeType, keyword => Keyword},
            case attachment_ds:page(Page, Size, Opts) of
                {ok, Result} ->
                    elib_response:success(Req0, Result, "success.");
                {error, _Reason} ->
                    elib_response:error(Req0, <<"查询失败"/utf8>>)
            end;
        {error, Req1} ->
            Req1
    end;
index(_, Req0, _State) ->
    Req0.

-spec disable(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
disable(<<"POST">>, Req0, State) ->
    handle_id_action(State, <<"storage:disable">>, fun attachment_ds:disable/1, Req0);
disable(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec enable(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
enable(<<"POST">>, Req0, State) ->
    handle_id_action(State, <<"storage:enable">>, fun attachment_ds:enable/1, Req0);
enable(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"POST">>, Req0, State) ->
    handle_id_action(State, <<"storage:delete">>, fun attachment_ds:soft_delete/1, Req0);
delete(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 签发短时下载 presigned GET URL（GET，id 取自 query）
%% admin 下载：按 id 查 path(ObjectKey) → presign GET(10min) → 返回 {url}。
%% 前端拿到 url 后 window.open 触发浏览器直连 Garage 下载；
%% presign 实时签发，避免列表预签导致的批量过期与性能开销。
-spec download(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
download(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"storage:view">>, Req0) of
        ok ->
            Qs = cowboy_req:parse_qs(Req0),
            IdRaw = proplists:get_value(<<"id">>, Qs, <<"0">>),
            case safe_int(IdRaw, 0) of
                Id when Id > 0 ->
                    case attachment_ds:find_path_by_id(Id) of
                        {ok, Path} ->
                            Url = elib_oss:presign_get_for_key(Path, 600),
                            elib_response:success(Req0, #{<<"url">> => Url}, "success.");
                        {error, not_found} ->
                            elib_response:error(Req0, <<"附件不存在"/utf8>>, ?ERR_BAD_REQUEST);
                        {error, R} ->
                            elib_response:error(
                                Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                _ ->
                    elib_response:error(Req0, <<"id 无效"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
download(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc disable/enable/delete 的公共流程：鉴权 → 解析 id → 执行 DS 操作 → 标准响应
-spec handle_id_action(map(), binary(), fun((integer()) -> ok | {error, term()}), cowboy_req:req()) ->
    cowboy_req:req().
handle_id_action(State, Permission, DsFun, Req0) ->
    case adm_acl:ensure_permission(State, Permission, Req0) of
        ok ->
            case parse_id(Req0) of
                {ok, Id} ->
                    case DsFun(Id) of
                        ok ->
                            elib_response:success(Req0, #{});
                        {error, R} ->
                            elib_response:error(
                                Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                {error, Req1} ->
                    Req1
            end;
        {error, Req1} ->
            Req1
    end.

-spec orphan(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
orphan(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"storage:cleanup">>, Req0) of
        ok ->
            Qs = cowboy_req:parse_qs(Req0),
            AgeDaysRaw = proplists:get_value(<<"age_days">>, Qs, <<"30">>),
            AgeDays = max(7, safe_int(AgeDaysRaw, 30)),
            case attachment_ds:orphan_stats(#{age_days => AgeDays}) of
                {ok, Stats} ->
                    elib_response:success(Req0, Stats, "success.");
                {error, R} ->
                    elib_response:error(Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR)
            end;
        {error, Req1} ->
            Req1
    end;
orphan(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec orphan_cleanup(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
orphan_cleanup(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"storage:cleanup">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            AgeDays = max(7, safe_int(maps:get(<<"age_days">>, PostVals, 30), 30)),
            case attachment_ds:orphan_cleanup(#{age_days => AgeDays}) of
                {ok, Stats} ->
                    elib_response:success(Req0, Stats, "success.");
                {error, R} ->
                    elib_response:error(Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR)
            end;
        {error, Req1} ->
            Req1
    end;
orphan_cleanup(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec parse_id(cowboy_req:req()) -> {ok, integer()} | {error, cowboy_req:req()}.
parse_id(Req0) ->
    PostVals = elib_param:post(Req0),
    IdRaw = maps:get(<<"id">>, PostVals, 0),
    case safe_int(IdRaw, 0) of
        Id when is_integer(Id), Id > 0 ->
            {ok, Id};
        _ ->
            {error, elib_response:error(Req0, <<"id 无效"/utf8>>, ?ERR_BAD_REQUEST)}
    end.

%% @doc 安全解析整数，非法输入回退默认值（ec_cnv:to_integer 对非数字会抛 badarg）
-spec safe_int(term(), integer()) -> integer().
safe_int(V, Default) ->
    try
        ec_cnv:to_integer(V)
    catch
        _:_ -> Default
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
