-module(channel_handler_comment).
%%%
% channel_handler_comment — 频道评论 HTTP handler
% 镜像 channel_handler_message 模式，提供评论的 CRUD + 点赞 API。
%%%
-behavior(cowboy_rest).
-export([init/2, handle_action/3]).
-export([
    list_comments/2,
    create_comment/2,
    delete_comment/2,
    like_comment/2,
    unlike_comment/2
]).
-include("error_code.hrl").

init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

handle_action(list_comments, Req, State) -> list_comments(Req, State);
handle_action(create_comment, Req, State) -> create_comment(Req, State);
handle_action(delete_comment, Req, State) -> delete_comment(Req, State);
handle_action(like_comment, Req, State) -> like_comment(Req, State);
handle_action(unlike_comment, Req, State) -> unlike_comment(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 查询消息评论列表
%% GET /api/v1/channel/:channel_id/message/:message_id/comments?cursor=&limit=
-spec list_comments(cowboy_req:req(), map()) -> cowboy_req:req().
list_comments(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case cowboy_req:binding(message_id, Req0) of
                undefined ->
                    elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
                MessageId ->
                    Qs = cowboy_req:parse_qs(Req0),
                    Cursor = parse_qs_int(proplists:get_value(<<"cursor">>, Qs, <<"0">>), 0),
                    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 20, 1, 200),
                    case channel_comment_logic:list_by_message(Uid, ChannelId, MessageId, Cursor, Limit) of
                        {ok, Comments} ->
                            elib_response:success(Req0, #{list => Comments});
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end
    end.

%% @doc 创建评论
%% POST /api/v1/channel/:channel_id/message/:message_id/comment
-spec create_comment(cowboy_req:req(), map()) -> cowboy_req:req().
create_comment(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case cowboy_req:binding(message_id, Req0) of
                undefined ->
                    elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
                MessageId ->
                    PostVals = elib_param:post(Req0),
                    Content = maps:get(<<"content">>, PostVals, <<>>),
                    ParentId = maps:get(<<"parent_id">>, PostVals, 0),
                    case channel_comment_logic:create(Uid, ChannelId, MessageId, Content, ParentId) of
                        {ok, Comment} ->
                            elib_response:success(Req0, Comment);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end
    end.

%% @doc 删除评论
%% POST /api/v1/channel/:channel_id/comment/:comment_id/delete
-spec delete_comment(cowboy_req:req(), map()) -> cowboy_req:req().
delete_comment(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(comment_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"评论ID不能为空"/utf8>>);
        CommentId ->
            case channel_comment_logic:delete(Uid, elib_cnv:safe_to_integer(CommentId)) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 点赞评论
%% POST /api/v1/channel/:channel_id/comment/:comment_id/like
-spec like_comment(cowboy_req:req(), map()) -> cowboy_req:req().
like_comment(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(comment_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"评论ID不能为空"/utf8>>);
        CommentId ->
            case channel_comment_logic:like(Uid, elib_cnv:safe_to_integer(CommentId)) of
                ok -> elib_response:success(Req0, #{});
                {error, Msg} -> elib_response:error(Req0, Msg)
            end
    end.

%% @doc 取消点赞
%% POST /api/v1/channel/:channel_id/comment/:comment_id/unlike
-spec unlike_comment(cowboy_req:req(), map()) -> cowboy_req:req().
unlike_comment(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(comment_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"评论ID不能为空"/utf8>>);
        CommentId ->
            case channel_comment_logic:unlike(Uid, elib_cnv:safe_to_integer(CommentId)) of
                ok -> elib_response:success(Req0, #{});
                {error, Msg} -> elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec parse_qs_int(binary() | undefined, integer()) -> integer().
parse_qs_int(undefined, Default) -> Default;
parse_qs_int(Bin, Default) ->
    case safe_to_integer(Bin) of
        {ok, I} -> I;
        error -> Default
    end.

-spec parse_qs_int(binary() | undefined, integer(), integer(), integer()) -> integer().
parse_qs_int(undefined, Default, _Min, _Max) -> Default;
parse_qs_int(Bin, Default, Min, Max) ->
    case safe_to_integer(Bin) of
        {ok, I} when I >= Min, I =< Max -> I;
        _ -> Default
    end.

-spec safe_to_integer(term()) -> {ok, integer()} | error.
safe_to_integer(Value) when is_integer(Value) -> {ok, Value};
safe_to_integer(Value) when is_binary(Value) ->
    try
        {ok, binary_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(Value) when is_list(Value) ->
    try
        {ok, list_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(_) -> error.
