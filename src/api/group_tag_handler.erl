-module(group_tag_handler).

%%%
% group_tag_handler 是群组标签 API 处理器
% 处理群组标签相关的 HTTP REST API 请求
%%%

-dialyzer({nowarn_function, [init/2, handle_action/3]}).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群标签处理器
%% 根据请求中的 action 参数调用相应的处理函数
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(add, Req, State) -> add(Req, State);
handle_action(remove, Req, State) -> remove(Req, State);
handle_action(list, Req, State) -> list(Req, State);
handle_action(search, Req, State) -> search(Req, State);
handle_action(hot, Req, State) -> hot(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 添加群组标签
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),

    Gid = maps:get(<<"gid">>, PostVals, undefined),
    TagName = maps:get(<<"tag_name">>, PostVals, undefined),

    % 参数验证
    case {Gid, TagName} of
        {undefined, _} ->
            elib_response:error(Req0, <<"群组ID不能为空"/utf8>>);
        {_, undefined} ->
            elib_response:error(Req0, <<"标签名不能为空"/utf8>>);
        {Gid2, TagName2} ->
            Gid3 = elib_cnv:safe_to_integer(Gid2),
            case Gid3 of
                0 ->
                    elib_response:error(Req0, <<"无效的群组ID"/utf8>>);
                _ ->
                    case group_tag_logic:add(Gid3, Uid, TagName2) of
                        {ok, TagId} ->
                            elib_response:success(Req0, #{<<"tag_id">> => TagId}, <<"标签添加成功"/utf8>>);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason)
                    end
            end
    end.

%% @doc 删除群组标签
-spec remove(cowboy_req:req(), map()) -> cowboy_req:req().
remove(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),

    Gid = maps:get(<<"gid">>, PostVals, undefined),
    TagName = maps:get(<<"tag_name">>, PostVals, undefined),

    % 参数验证
    case {Gid, TagName} of
        {undefined, _} ->
            elib_response:error(Req0, <<"群组ID不能为空"/utf8>>);
        {_, undefined} ->
            elib_response:error(Req0, <<"标签名不能为空"/utf8>>);
        {Gid2, TagName2} ->
            Gid3 = elib_cnv:safe_to_integer(Gid2),
            case Gid3 of
                0 ->
                    elib_response:error(Req0, <<"无效的群组ID"/utf8>>);
                _ ->
                    case group_tag_logic:remove(Gid3, Uid, TagName2) of
                        ok ->
                            elib_response:success(Req0, #{}, <<"标签删除成功"/utf8>>);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason)
                    end
            end
    end.

%% @doc 查询群组的标签列表
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),

    Gid = proplists:get_value(<<"gid">>, Qs, undefined),

    % 参数验证
    case Gid of
        undefined ->
            elib_response:error(Req0, <<"群组ID不能为空"/utf8>>);
        Gid2 ->
            Gid3 = elib_cnv:safe_to_integer(Gid2),
            case Gid3 of
                0 ->
                    elib_response:error(Req0, <<"无效的群组ID"/utf8>>);
                _ ->
                    case group_tag_logic:list(Gid3, Uid) of
                        {ok, Tags} ->
                            % 编码标签ID
                            Tags2 = Tags,
                            elib_response:success(Req0, #{<<"list">> => Tags2}, <<"success."/utf8>>);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason)
                    end
            end
    end.

%% @doc 按标签名搜索群组
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),

    TagName = proplists:get_value(<<"tag_name">>, Qs, undefined),

    % 参数验证
    case TagName of
        undefined ->
            elib_response:error(Req0, <<"标签名不能为空"/utf8>>);
        TagName2 ->
            case group_tag_logic:search(TagName2) of
                {ok, Groups} ->
                    % 编码群组ID
                    Groups2 = Groups,
                    elib_response:success(Req0, #{<<"list">> => Groups2}, <<"success."/utf8>>);
                {error, Reason} ->
                    elib_response:error(Req0, Reason)
            end
    end.

%% @doc 获取热门标签
-spec hot(cowboy_req:req(), map()) -> cowboy_req:req().
hot(Req0, _State) ->
    _Qs = cowboy_req:parse_qs(Req0),

    Limit = case elib_param:int(limit, Req0, 20) of
        {ok, Val} when Val > 0, Val =< 100 -> Val;
        _ -> 20
    end,

    case group_tag_logic:hot_tags(Limit) of
        {ok, Tags} ->
            elib_response:success(Req0, #{<<"list">> => Tags}, <<"success."/utf8>>);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.
