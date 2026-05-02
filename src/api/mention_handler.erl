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
    PostVals = safe_post(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    {Page, Size} = resolve_page_size(Req0, PostVals),
    IsReadRaw = get_param([<<"is_read">>], PostVals, Qs, undefined),
    IsRead = normalize_is_read(IsReadRaw),
    GidRaw = get_param([<<"gid">>, <<"group_id">>], PostVals, Qs, undefined),
    Options = #{page => Page, size => Size},
    MentionRes = case decode_optional_gid(GidRaw) of
        undefined ->
            mention_logic:list_mentions(CurrentUid, IsRead, Options);
        {ok, Gid2} ->
            mention_logic:list_group_mentions(Gid2, CurrentUid, IsRead, Options);
        invalid ->
            {error, invalid_gid}
    end,
    case MentionRes of
        {ok, Mentions} ->
            % 编码ID并返回
            EncodedMentions = encode_mention_ids(Mentions),
            ResponseData = #{
                total => length(EncodedMentions),
                page => Page,
                size => Size,
                list => EncodedMentions,
                items => EncodedMentions
            },
            elib_response:success(Req0, ResponseData);
        {error, invalid_gid} ->
            elib_response:error(Req0, "群组ID格式有误", ?ERR_BAD_REQUEST);
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
    PostVals = safe_post(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    GidRaw = get_param([<<"gid">>, <<"group_id">>], PostVals, Qs, undefined),
    case decode_optional_gid(GidRaw) of
        undefined ->
            Count = mention_logic:count_unread(CurrentUid),
            elib_response:success(Req0, #{<<"count">> => Count});
        {ok, Gid2} ->
            Count = mention_logic:count_group_unread(CurrentUid, Gid2),
            elib_response:success(Req0, #{<<"count">> => Count});
        invalid ->
            elib_response:error(Req0, "群组ID格式有误", ?ERR_BAD_REQUEST)
    end.

%% @doc 标记@消息为已读
%%
%% @param Req0 Cowboy请求对象，包含 msg_id
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec mark_read(cowboy_req:req(), map()) -> cowboy_req:req().
mark_read(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = safe_post(Req0),
    case normalize_bool(maps:get(<<"all">>, PostVals, undefined)) of
        true ->
            mark_read_all(Req0, PostVals, CurrentUid);
        _ ->
            MsgId = normalize_msg_id(maps:get(<<"msg_id">>, PostVals, <<>>)),
            MentionId = normalize_positive_int(maps:get(<<"mention_id">>, PostVals, 0), 0),
            case {MsgId, MentionId > 0} of
                {<<>>, false} ->
                    elib_response:error(Req0, "消息ID必须提供");
                {MsgId2, _} when MsgId2 =/= <<>> ->
                    %% 验证该 mention 记录确实属于当前用户（mentioned_uid = CurrentUid）
                    case mention_ds:find_by_msg_id(MsgId2) of
                        {ok, Mentions} ->
                            BelongsToUser = lists:any(fun(M) ->
                                maps:get(<<"mentioned_uid">>, M, 0) =:= CurrentUid
                            end, Mentions),
                            case BelongsToUser of
                                true ->
                                    case mention_logic:mark_as_read(MsgId2, CurrentUid) of
                                        ok ->
                                            elib_response:success(Req0, #{<<"msg_id">> => MsgId2});
                                        {error, _Reason} ->
                                            elib_response:error(Req0, "标记已读失败")
                                    end;
                                false ->
                                    elib_response:error(Req0, "无权操作该@消息", ?ERR_BAD_REQUEST)
                            end;
                        {error, _Reason} ->
                            elib_response:error(Req0, "标记已读失败")
                    end;
                {<<>>, true} ->
                    case mention_logic:mark_as_read_by_mention_id(MentionId, CurrentUid) of
                        {ok, MsgId2} ->
                            elib_response:success(Req0, #{
                                <<"mention_id">> => MentionId,
                                <<"msg_id">> => MsgId2
                            });
                        {error, not_found} ->
                            %% mention_id 不存在或不属于当前用户
                            elib_response:error(Req0, "无权操作该@消息或记录不存在", ?ERR_BAD_REQUEST);
                        {error, _Reason} ->
                            elib_response:error(Req0, "标记已读失败")
                    end
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
    PostVals = safe_post(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    GidRaw = get_param([<<"gid">>, <<"group_id">>], PostVals, Qs, <<>>),
    Keyword = get_param([<<"keyword">>], PostVals, Qs, <<>>),
    case decode_optional_gid(GidRaw) of
        undefined ->
            elib_response:error(Req0, "群组ID必须提供", ?ERR_BAD_REQUEST);
        invalid ->
            elib_response:error(Req0, "群组ID必须提供", ?ERR_BAD_REQUEST);
        {ok, Gid2} ->
            case mention_logic:get_member_suggestions(Gid2, CurrentUid, Keyword) of
                {ok, Members} ->
                    elib_response:success(Req0, #{
                        <<"members">> => Members,
                        <<"items">> => Members
                    });
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
        MsgId = maps:get(<<"msg_id">>, Mention, <<>>),
        Gid = maps:get(<<"group_id">>, Mention, 0),
        IsReadRaw = maps:get(<<"is_read">>, Mention, false),
        IsRead = case IsReadRaw of
            true -> 1;
            1 -> 1;
            <<"1">> -> 1;
            _ -> 0
        end,
        CreatedAt = normalize_created_at(maps:get(<<"created_at">>, Mention, 0)),
        MentionId = normalize_positive_int(maps:get(<<"id">>, Mention, 0), 0),
        Mention#{
            <<"id">> => MentionId,
            <<"msg_id">> => MsgId,  % msg_id 已经是字符串，不需要编码
            <<"group_id">> => maybe_encode_id(Gid),
            <<"from_uid">> => maps:get(<<"from_uid">>, Mention, 0),
            <<"mentioned_uid">> => maps:get(<<"mentioned_uid">>, Mention, 0),
            <<"is_read">> => IsRead,
            <<"is_read_bool">> => IsReadRaw,
            <<"created_at">> => CreatedAt
        }
    end, Mentions).

%% @private
-spec mark_read_all(cowboy_req:req(), map(), integer()) -> cowboy_req:req().
mark_read_all(Req0, PostVals, CurrentUid) ->
    case decode_optional_gid(maps:get(<<"group_id">>, PostVals, maps:get(<<"gid">>, PostVals, undefined))) of
        undefined ->
            case mention_logic:mark_all_as_read(CurrentUid) of
                ok ->
                    elib_response:success(Req0, #{<<"all">> => true});
                {error, _Reason} ->
                    elib_response:error(Req0, "获取@消息列表失败")
            end;
        {ok, Gid2} ->
            case mention_logic:mark_group_as_read(CurrentUid, Gid2) of
                ok ->
                    elib_response:success(Req0, #{
                        <<"all">> => true,
                        <<"group_id">> => Gid2
                    });
                {error, _Reason} ->
                    elib_response:error(Req0, "获取@消息列表失败")
            end;
        invalid ->
            elib_response:error(Req0, "群组ID格式有误", ?ERR_BAD_REQUEST)
    end.

%% @private
-spec safe_post(cowboy_req:req()) -> map().
safe_post(Req0) ->
    try elib_param:post(Req0) of
        PostVals when is_map(PostVals) ->
            PostVals;
        PostVals when is_list(PostVals) ->
            maps:from_list(PostVals);
        _ ->
            #{}
    catch
        _:_ ->
            #{}
    end.

%% @private
-spec resolve_page_size(cowboy_req:req(), map()) -> {integer(), integer()}.
resolve_page_size(Req0, PostVals) ->
    {Page0, Size0} = elib_param:page(Req0),
    Page = normalize_positive_int(maps:get(<<"page">>, PostVals, Page0), Page0),
    Size = normalize_positive_int(maps:get(<<"size">>, PostVals, Size0), Size0),
    {Page, Size}.

%% @private
-spec get_param([binary()], map(), [{binary(), binary()}], term()) -> term().
get_param(Keys, PostVals, Qs, Default) ->
    case find_in_post(Keys, PostVals) of
        undefined ->
            case find_in_qs(Keys, Qs) of
                undefined -> Default;
                V -> V
            end;
        V ->
            V
    end.

%% @private
-spec find_in_post([binary()], map()) -> term().
find_in_post([], _PostVals) ->
    undefined;
find_in_post([Key | Rest], PostVals) ->
    case maps:find(Key, PostVals) of
        {ok, Value} ->
            Value;
        error ->
            find_in_post(Rest, PostVals)
    end.

%% @private
-spec find_in_qs([binary()], [{binary(), binary()}]) -> term().
find_in_qs([], _Qs) ->
    undefined;
find_in_qs([Key | Rest], Qs) ->
    case proplists:get_value(Key, Qs, undefined) of
        undefined ->
            find_in_qs(Rest, Qs);
        Value ->
            Value
    end.

%% @private
-spec normalize_is_read(term()) -> true | false | undefined.
normalize_is_read(Value) ->
    normalize_bool(Value).

%% @private
-spec normalize_bool(term()) -> true | false | undefined.
normalize_bool(Value) ->
    case Value of
        true -> true;
        false -> false;
        1 -> true;
        0 -> false;
        <<"1">> -> true;
        <<"0">> -> false;
        <<"true">> -> true;
        <<"false">> -> false;
        <<"TRUE">> -> true;
        <<"FALSE">> -> false;
        _ -> undefined
    end.

%% @private
-spec decode_optional_gid(term()) -> undefined | {ok, integer()} | invalid.
decode_optional_gid(undefined) ->
    undefined;
decode_optional_gid(<<>>) ->
    undefined;
decode_optional_gid(Value) ->
    case decode_gid(Value) of
        Gid when is_integer(Gid), Gid > 0 ->
            {ok, Gid};
        _ ->
            invalid
    end.

%% @private
-spec decode_gid(term()) -> integer().
decode_gid(Gid) when is_integer(Gid), Gid > 0 ->
    Gid;
decode_gid(Gid) when is_binary(Gid) ->
    case elib_cnv:safe_to_integer(Gid) of
        Decoded when is_integer(Decoded), Decoded > 0 ->
            Decoded;
        _ ->
            normalize_positive_int(Gid, 0)
    end;
decode_gid(Gid) when is_list(Gid) ->
    decode_gid(unicode:characters_to_binary(Gid));
decode_gid(_) ->
    0.

%% @private
-spec normalize_positive_int(term(), integer()) -> integer().
normalize_positive_int(Value, Default) ->
    Int = to_integer(Value),
    case Int > 0 of
        true -> Int;
        false -> Default
    end.

%% @private
-spec normalize_msg_id(term()) -> binary().
normalize_msg_id(undefined) ->
    <<>>;
normalize_msg_id(<<>>) ->
    <<>>;
normalize_msg_id(Value) when is_binary(Value) ->
    Value;
normalize_msg_id(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value);
normalize_msg_id(Value) when is_integer(Value), Value > 0 ->
    integer_to_binary(Value);
normalize_msg_id(_) ->
    <<>>.

%% @private
-spec to_integer(term()) -> integer().
to_integer(Value) when is_integer(Value) ->
    Value;
to_integer(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int -> Int
    catch
        _:_ -> 0
    end;
to_integer(Value) when is_list(Value) ->
    try list_to_integer(Value) of
        Int -> Int
    catch
        _:_ -> 0
    end;
to_integer(_) ->
    0.

%% @private
-spec maybe_encode_id(integer() | binary()) -> integer() | binary().
maybe_encode_id(Id) ->
    Id.

%% @private
-spec normalize_created_at(term()) -> term().
normalize_created_at(Ts) when is_integer(Ts), Ts > 1000000000000 ->
    Ts div 1000;
normalize_created_at(Ts) when is_integer(Ts) ->
    Ts;
normalize_created_at(Ts) when is_binary(Ts) ->
    case normalize_positive_int(Ts, 0) of
        0 ->
            try elib_dt:rfc3339_to(Ts) of
                ParsedTs when is_integer(ParsedTs) ->
                    ParsedTs;
                _ ->
                    Ts
            catch
                _:_ ->
                    Ts
            end;
        Num when Num > 1000000000000 ->
            Num div 1000;
        Num ->
            Num
    end;
normalize_created_at(Ts) when is_list(Ts) ->
    normalize_created_at(unicode:characters_to_binary(Ts));
normalize_created_at(Ts) ->
    Ts.
