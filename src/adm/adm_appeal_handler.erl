-module(adm_appeal_handler).

%%%
% R-04 处置申诉复审控制器
% Moderation appeal review controller
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").
-include("error_code.hrl").
-include("log.hrl").

%% 复用举报处置链权限族；"独立复审"由 logic 层 reviewer≠原执行者硬约束
%% 保证（比权限键更本质：任何有处置权者均可复审，唯原执行者回避）。
-define(PERM_READ, <<"reports:read">>).
-define(PERM_WRITE, <<"reports:handle">>).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            list -> list(Method, Req0, State);
            review -> review(Method, Req0, State);
            _ -> cowboy_req:reply(404, #{}, <<"Not Found">>, Req0)
        end,
    {ok, Req1, State}.

%% GET /api/adm/appeal/list?page=&size=&status=
-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, State) ->
    with_perm(State, ?PERM_READ, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Status = qs_binary(Req0, <<"status">>),
        case moderation_appeal_logic:admin_list(Page, Size, Status) of
            {ok, Rows} ->
                elib_response:success(Req0, #{<<"list">> => Rows, <<"page">> => Page});
            {error, _Msg} ->
                elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
list(_, Req0, _State) ->
    elib_response:error(Req0, <<"方法不允许"/utf8>>, ?ERR_BAD_REQUEST).

%% POST /api/adm/appeal/review  {id, verdict: accept|reject, review_reason}
-spec review(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
review(<<"POST">>, Req0, State) ->
    with_perm(State, ?PERM_WRITE, Req0, fun() ->
        ReviewerId = maps:get(adm_user_id, State, 0),
        Vals = elib_param:post(Req0),
        Id = ec_cnv:to_integer(maps:get(<<"id">>, Vals, 0)),
        Verdict = maps:get(<<"verdict">>, Vals, <<>>),
        ReviewReason = maps:get(<<"review_reason">>, Vals, <<>>),
        case ReviewerId > 0 of
            false ->
                elib_response:error(Req0, <<"身份无效"/utf8>>, ?ERR_BAD_REQUEST);
            true ->
                case moderation_appeal_logic:review(ReviewerId, Id, Verdict, ReviewReason) of
                    {ok, Result} ->
                        elib_response:success(Req0, #{<<"appeal">> => Result}, <<"复审完成"/utf8>>);
                    {error, Msg} ->
                        elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                end
        end
    end);
review(_, Req0, _State) ->
    elib_response:error(Req0, <<"方法不允许"/utf8>>, ?ERR_BAD_REQUEST).

%% ===================================================================
%% Internal
%% ===================================================================

-spec with_perm(map(), binary(), cowboy_req:req(), fun(() -> cowboy_req:req())) ->
    cowboy_req:req().
with_perm(State, Perm, Req0, Fun) ->
    case adm_acl:ensure_permission(State, Perm, Req0) of
        ok -> Fun();
        {error, Req1} -> Req1
    end.

-spec qs_binary(cowboy_req:req(), binary()) -> binary() | undefined.
qs_binary(Req0, Key) ->
    Qs = cowboy_req:parse_qs(Req0),
    case proplists:get_value(Key, Qs) of
        undefined -> undefined;
        V -> V
    end.
