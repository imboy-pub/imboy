-module(mention_handler).

%% cowboy_rest 是一个 behavior，但我们不需要在这里使用
%% -behavior(cowboy_rest).

-export([init/2]).
-export([list_mentions/2]).
-export([unread/2]).
-export([mark_read/2]).
-export([suggest/2]).

-include("log.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化@提及处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @private
%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(list, Req, State) -> list_mentions(Req, State);
handle_action(unread, Req, State) -> unread(Req, State);
handle_action(mark_read, Req, State) -> mark_read(Req, State);
handle_action(suggest, Req, State) -> suggest(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 查询@我的消息列表
%%
%% 支持分页和已读/未读过滤
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回包含@消息列表的响应
%% @end
-spec list_mentions(cowboy_req:req(), map()) -> cowboy_req:req().
list_mentions(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = elib_param:page(Req0),

    % 获取 is_read 参数
    Qs = cowboy_req:parse_qs(Req0),
    IsRead = case proplists:get_value(<<"is_read">>, Qs, undefined) of
        <<"true">> -> true;
        <<"false">> -> false;
        _ -> undefined
    end,

    case mention_logic:list_mentions(CurrentUid, IsRead, #{page => Page, size => Size}) of
        {ok, Mentions} ->
            % 编码ID并返回
            EncodedMentions = encode_mention_ids(Mentions),
            ResponseData = #{
                total => length(EncodedMentions),
                page => Page,
                size => Size,
                list => EncodedMentions
            },
            elib_response:success(Req0, ResponseData);
        {error, _Reason} ->
            elib_response:error(Req0, "获取@消息列表失败")
    end.

%% @doc 查询未读@消息数量
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回包含未读数量的响应
%% @end
-spec unread(cowboy_req:req(), map()) -> cowboy_req:req().
unread(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Count = mention_logic:count_unread(CurrentUid),
    elib_response:success(Req0, #{<<"count">> => Count}).

%% @doc 标记@消息为已读
%%
%% @param Req0 Cowboy请求对象，包含 msg_id
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec mark_read(cowboy_req:req(), map()) -> cowboy_req:req().
mark_read(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MsgId = maps:get(<<"msg_id">>, PostVals, <<>>),

    case MsgId of
        <<>> ->
            elib_response:error(Req0, "消息ID必须提供");
        _ ->
            case mention_logic:mark_as_read(MsgId, CurrentUid) of
                ok ->
                    elib_response:success(Req0, #{<<"msg_id">> => MsgId});
                {error, _Reason} ->
                    elib_response:error(Req0, "获取@消息列表失败")
            end
    end.

%% @doc 获取群成员建议列表（用于@输入）
%%
%% @param Req0 Cowboy请求对象，包含 gid 和 keyword
%% @param State 状态映射，包含 current_uid
%% @return 返回包含成员建议列表的响应
%% @end
-spec suggest(cowboy_req:req(), map()) -> cowboy_req:req().
suggest(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Gid = proplists:get_value(<<"gid">>, Qs, <<>>),
    Keyword = proplists:get_value(<<"keyword">>, Qs, <<>>),

    case Gid of
        <<>> ->
            elib_response:error(Req0, "群组ID必须提供", ?ERR_BAD_REQUEST);
        _ ->
            Gid2 = elib_hashids:decode(Gid),
            case mention_logic:get_member_suggestions(Gid2, CurrentUid, Keyword) of
                {ok, Members} ->
                    elib_response:success(Req0, #{<<"members">> => Members});
                {error, not_group_member} ->
                    elib_response:error(Req0, "你不是群组成员", ?ERR_GROUP_PERMISSION_DENIED);
                {error, _Reason} ->
                    elib_response:error(Req0, "获取@消息列表失败")
            end
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @private
%% @doc 编码@提及记录中的ID
-spec encode_mention_ids(list(map())) -> list(map).
encode_mention_ids(Mentions) ->
    lists:map(fun(Mention) ->
        MsgId = maps:get(<<"msg_id">>, Mention),
        Gid = maps:get(<<"group_id">>, Mention),
        Mention#{
            <<"msg_id">> => MsgId,  % msg_id 已经是字符串，不需要编码
            <<"group_id">> => elib_hashids:encode(Gid),
            <<"from_uid">> => elib_hashids:encode(maps:get(<<"from_uid">>, Mention, 0)),
            <<"mentioned_uid">> => elib_hashids:encode(maps:get(<<"mentioned_uid">>, Mention, 0))
        }
    end, Mentions).
