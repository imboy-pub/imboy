-module(moment_handler).

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case imboy_plugin_registry:required_feature(api, moment_handler, Action) of
            undefined ->
                handle_action(Action, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        handle_action(Action, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create(Req, State);
handle_action(show, Req, State) -> show(Req, State);
handle_action(delete, Req, State) -> delete(Req, State);
handle_action(feed, Req, State) -> feed(Req, State);
handle_action(user_posts, Req, State) -> user_posts(Req, State);
handle_action(like, Req, State) -> like(Req, State);
handle_action(unlike, Req, State) -> unlike(Req, State);
handle_action(add_comment, Req, State) -> add_comment(Req, State);
handle_action(comments, Req, State) -> comments(Req, State);
handle_action(delete_comment, Req, State) -> delete_comment(Req, State);
handle_action(report, Req, State) -> report(Req, State);
handle_action(false, Req, _State) -> Req.

-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    case moment_logic:create_post(Uid, PostVals) of
        {ok, Payload} ->
            elib_response:success(Req0, Payload);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

-spec show(cowboy_req:req(), map()) -> cowboy_req:req().
show(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MomentId = resolve_moment_id(Req0, PostVals),
    case MomentId of
        <<>> ->
            elib_response:error(Req0, <<"动态ID不能为空"/utf8>>);
        _ ->
            case moment_logic:get_post(Uid, MomentId) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MomentId = resolve_moment_id(Req0, PostVals),
    case MomentId of
        <<>> ->
            elib_response:error(Req0, <<"动态ID不能为空"/utf8>>);
        _ ->
            case moment_logic:delete_post(Uid, MomentId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec feed(cowboy_req:req(), map()) -> cowboy_req:req().
feed(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Cursor = decode_cursor_id(proplists:get_value(<<"cursor">>, Qs, 0)),
    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs, 20), 20, 1, 100),
    case moment_logic:feed(Uid, Cursor, Limit) of
        {ok, Payload} ->
            elib_response:success(Req0, Payload);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

-spec user_posts(cowboy_req:req(), map()) -> cowboy_req:req().
user_posts(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    Cursor = decode_cursor_id(proplists:get_value(<<"cursor">>, Qs, 0)),
    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs, 20), 20, 1, 100),
    TargetUid = case cowboy_req:binding(uid, Req0) of
        undefined -> proplists:get_value(<<"uid">>, Qs, <<>>);
        Value -> Value
    end,
    case moment_logic:user_posts(Uid, TargetUid, Cursor, Limit) of
        {ok, Payload} ->
            elib_response:success(Req0, Payload);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

-spec like(cowboy_req:req(), map()) -> cowboy_req:req().
like(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MomentId = resolve_moment_id(Req0, PostVals),
    case MomentId of
        <<>> ->
            elib_response:error(Req0, <<"动态ID不能为空"/utf8>>);
        _ ->
            case moment_logic:like_post(Uid, MomentId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec unlike(cowboy_req:req(), map()) -> cowboy_req:req().
unlike(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MomentId = resolve_moment_id(Req0, PostVals),
    case MomentId of
        <<>> ->
            elib_response:error(Req0, <<"动态ID不能为空"/utf8>>);
        _ ->
            case moment_logic:unlike_post(Uid, MomentId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec add_comment(cowboy_req:req(), map()) -> cowboy_req:req().
add_comment(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MomentId = resolve_moment_id(Req0, PostVals),
    Content = maps:get(<<"content">>, PostVals, <<>>),
    ReplyToUid = maps:get(<<"reply_to_uid">>, PostVals, undefined),
    case MomentId of
        <<>> ->
            elib_response:error(Req0, <<"动态ID不能为空"/utf8>>);
        _ ->
            case moment_logic:add_comment(Uid, MomentId, Content, ReplyToUid) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec comments(cowboy_req:req(), map()) -> cowboy_req:req().
comments(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MomentId = resolve_moment_id(Req0, PostVals),
    Qs = cowboy_req:parse_qs(Req0),
    Cursor = decode_cursor_id(proplists:get_value(<<"cursor">>, Qs, 0)),
    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs, 20), 20, 1, 100),
    case MomentId of
        <<>> ->
            elib_response:error(Req0, <<"动态ID不能为空"/utf8>>);
        _ ->
            case moment_logic:list_comments(Uid, MomentId, Cursor, Limit) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec delete_comment(cowboy_req:req(), map()) -> cowboy_req:req().
delete_comment(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MomentId = resolve_moment_id(Req0, PostVals),
    CommentId = resolve_comment_id(Req0, PostVals),
    case {MomentId, CommentId} of
        {<<>>, _} ->
            elib_response:error(Req0, <<"动态ID不能为空"/utf8>>);
        {_, <<>>} ->
            elib_response:error(Req0, <<"评论ID不能为空"/utf8>>);
        _ ->
            case moment_logic:delete_comment(Uid, MomentId, CommentId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec report(cowboy_req:req(), map()) -> cowboy_req:req().
report(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    MomentId = resolve_moment_id(Req0, PostVals),
    Reason = maps:get(<<"reason">>, PostVals, <<>>),
    Desc = maps:get(<<"description">>, PostVals, <<>>),
    case MomentId of
        <<>> ->
            elib_response:error(Req0, <<"动态ID不能为空"/utf8>>);
        _ ->
            case moment_logic:report_post(Uid, MomentId, Reason, Desc) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec resolve_moment_id(cowboy_req:req(), map()) -> binary().
resolve_moment_id(Req0, PostVals) ->
    case cowboy_req:binding(moment_id, Req0) of
        undefined ->
            normalize_binary(maps:get(<<"moment_id">>, PostVals, <<>>));
        Value ->
            normalize_binary(Value)
    end.

-spec resolve_comment_id(cowboy_req:req(), map()) -> binary().
resolve_comment_id(Req0, PostVals) ->
    case cowboy_req:binding(comment_id, Req0) of
        undefined ->
            normalize_binary(maps:get(<<"comment_id">>, PostVals, <<>>));
        Value ->
            normalize_binary(Value)
    end.

-spec parse_qs_int(term(), integer(), integer(), integer()) -> integer().
parse_qs_int(undefined, Default, _Min, _Max) ->
    Default;
parse_qs_int(Value, Default, Min, Max) ->
    case safe_to_integer(Value) of
        {ok, Int} when Int < Min ->
            Min;
        {ok, Int} when Int > Max ->
            Max;
        {ok, Int} ->
            Int;
        error ->
            Default
    end.

-spec decode_cursor_id(term()) -> integer().
decode_cursor_id(undefined) ->
    0;
decode_cursor_id(0) ->
    0;
decode_cursor_id(<<>>) ->
    0;
decode_cursor_id(Value) ->
    decode_positive_id(Value).

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) when is_integer(Value), Value > 0 ->
    Value;
decode_positive_id(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            Int = ec_cnv:to_integer(Value),
            case Int > 0 of true -> Int; false -> 0 end;
        false ->
            case catch elib_hashids:decode(Value) of
                Id when is_integer(Id), Id > 0 -> Id;
                _ -> 0
            end
    end;
decode_positive_id(Value) when is_list(Value) ->
    decode_positive_id(ec_cnv:to_binary(Value));
decode_positive_id(_) ->
    0.

-spec normalize_binary(term()) -> binary().
normalize_binary(undefined) ->
    <<>>;
normalize_binary(Value) when is_binary(Value) ->
    Value;
normalize_binary(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value);
normalize_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_binary(_) ->
    <<>>.

-spec safe_to_integer(term()) -> {ok, integer()} | error.
safe_to_integer(Value) when is_integer(Value) ->
    {ok, Value};
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
safe_to_integer(_) ->
    error.
