-module(msg_reaction_logic).
%%%
% msg_reaction_logic 是消息表情回应业务逻辑模块
% 提供消息表情回应的业务逻辑处理
%%%

-include("log.hrl").
-include("error_code.hrl").

-export([add/4]).
-export([remove/4]).
-export([list/2]).
-export([stats/2]).
-export([is_reacted/4]).

%% 支持的 emoji 白名单（可选）
-define(SUPPORTED_EMOJIS, [
    <<"👍"/utf8>>,  % 点赞
    <<"👎"/utf8>>,  % 踩
    <<"❤️"/utf8>>,  % 爱心
    <<"😄"/utf8>>,  % 微笑
    <<"🎉"/utf8>>,  % 庆祝
    <<"😢"/utf8>>,  % 难过
    <<"😮"/utf8>>,  % 惊讶
    <<"🔥"/utf8>>,  % 火焰
    <<"👏"/utf8>>,  % 鼓掌
    <<"🙏"/utf8>>   % 祈祷
]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 添加表情回应
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param CurrentUid 当前用户ID
%% @param Emoji emoji字符
%% @return {ok, map()} | {error, Reason}
-spec add(binary(), binary(), integer(), binary()) -> {ok, map()} | {error, term()}.
add(MsgId, MsgType, CurrentUid, Emoji) ->
    % 1. 验证 emoji 有效性（可选白名单）
    case validate_emoji(Emoji) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            % 2. 验证消息存在
            case verify_message_exists(MsgId, MsgType) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    % 3. 验证用户权限
                    case verify_permission(MsgId, MsgType, CurrentUid) of
                        {error, Reason} ->
                            {error, Reason};
                        ok ->
                            % 4. 添加表情
                            case msg_reaction_ds:add_reaction(MsgId, MsgType, CurrentUid, Emoji) of
                                {ok, Result} ->
                                    % 5. 如果是群聊，通知其他成员
                                    notify_reaction_added(MsgId, MsgType, CurrentUid, Emoji),
                                    {ok, Result};
                                {error, Reason} ->
                                    {error, Reason}
                            end
                    end
            end
    end.

%% @doc 移除表情回应
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param CurrentUid 当前用户ID
%% @param Emoji emoji字符
%% @return ok | {error, Reason}
-spec remove(binary(), binary(), integer(), binary()) -> ok | {error, term()}.
remove(MsgId, MsgType, CurrentUid, Emoji) ->
    % 1. 验证消息存在
    case verify_message_exists(MsgId, MsgType) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            % 2. 移除表情
            ok = msg_reaction_ds:remove_reaction(MsgId, MsgType, CurrentUid, Emoji),
            % 3. 如果是群聊，通知其他成员
            notify_reaction_removed(MsgId, MsgType, CurrentUid, Emoji),
            ok
    end.

%% @doc 查询表情列表
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @return {ok, map()} | {error, Reason}
%% 返回格式: #{
%%   <<"msg_id">> => MsgId,
%%   <<"reactions">> => [
%%     {<<"👍"/utf8>>, #{<<"count">> => 5, <<"users">> => [uid1, uid2, ...]}},
%%     ...
%%   ],
%%   <<"total_count">> => 8
%% }
-spec list(binary(), binary()) -> {ok, map()} | {error, term()}.
list(MsgId, MsgType) ->
    case msg_reaction_ds:get_reactions(MsgId, MsgType) of
        {ok, Reactions} ->
            TotalCount = lists:foldl(fun({_, Data}, Acc) ->
                Acc + maps:get(<<"count">>, Data, 0)
            end, 0, Reactions),
            {ok, #{
                <<"msg_id">> => MsgId,
                <<"reactions">> => Reactions,
                <<"total_count">> => TotalCount
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取表情统计信息
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @return {ok, map()} | {error, Reason}
%% 返回格式: #{
%%   <<"msg_id">> => MsgId,
%%   <<"reactions">> => [
%%     #{<<"emoji">> => <<"👍"/utf8>>, <<"count">> => 5, <<"users">> => [uid1, uid2, ...]},
%%     ...
%%   ],
%%   <<"total_count">> => 8
%% }
-spec stats(binary(), binary()) -> {ok, map()} | {error, term()}.
stats(MsgId, MsgType) ->
    case msg_reaction_ds:get_reaction_stats(MsgId, MsgType) of
        {ok, Stats} ->
            TotalCount = lists:foldl(fun(Stat, Acc) ->
                Acc + maps:get(<<"count">>, Stat, 0)
            end, 0, Stats),
            {ok, #{
                <<"msg_id">> => MsgId,
                <<"reactions">> => Stats,
                <<"total_count">> => TotalCount
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 检查用户是否已添加指定表情
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param CurrentUid 当前用户ID
%% @param Emoji emoji字符
%% @return true | false
-spec is_reacted(binary(), binary(), integer(), binary()) -> boolean().
is_reacted(MsgId, MsgType, CurrentUid, Emoji) ->
    msg_reaction_ds:is_reacted(MsgId, MsgType, CurrentUid, Emoji).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 验证 emoji 有效性
%% @param Emoji emoji字符
%% @return ok | {error, Reason}
-spec validate_emoji(binary()) -> ok | {error, term()}.
validate_emoji(Emoji) ->
    case Emoji of
        <<>> ->
            {error, {invalid_param, <<"emoji不能为空"/utf8>>}};
        _ when byte_size(Emoji) > 100 ->
            {error, {invalid_param, <<"emoji长度超出限制"/utf8>>}};
        _ ->
            % 可选：检查是否在白名单中
            % case lists:member(Emoji, ?SUPPORTED_EMOJIS) of
            %     true -> ok;
            %     false -> {error, {invalid_param, <<"不支持的emoji"/utf8>>}}
            % end
            ok
    end.

%% @doc 验证消息是否存在
%% @param MsgId 消息ID
%% @param MsgType 消息类型
%% @return ok | {error, Reason}
-spec verify_message_exists(binary(), binary()) -> ok | {error, term()}.
verify_message_exists(MsgId, <<"c2c">>) ->
    case msg_c2c_ds:find_msg_by_id(MsgId) of
        {ok, _} -> ok;
        {error, not_found} -> {error, msg_not_found};
        {error, _} -> {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;
verify_message_exists(MsgId, <<"c2g">>) ->
    case msg_c2g_ds:find_msg_by_id(MsgId) of
        {ok, _} -> ok;
        {error, not_found} -> {error, msg_not_found};
        {error, _} -> {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;
verify_message_exists(_, _) ->
    {error, {invalid_param, <<"不支持的消息类型"/utf8>>}}.

%% @doc 验证用户权限
%% @param MsgId 消息ID
%% @param MsgType 消息类型
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec verify_permission(binary(), binary(), integer()) -> ok | {error, term()}.
verify_permission(MsgId, <<"c2c">>, CurrentUid) ->
    % 单聊：必须是消息的发送者或接收者
    case msg_c2c_ds:find_msg_by_id(MsgId) of
        {ok, #{<<"from_id">> := FromId, <<"to_id">> := ToId}} ->
            case CurrentUid =:= FromId orelse CurrentUid =:= ToId of
                true -> ok;
                false -> {error, permission_denied}
            end;
        {error, _} ->
            {error, msg_not_found}
    end;
verify_permission(MsgId, <<"c2g">>, CurrentUid) ->
    % 群聊：必须是群成员
    case msg_c2g_ds:find_msg_by_id(MsgId) of
        {ok, #{<<"to_id">> := Gid}} ->
            case group_member_ds:is_member(Gid, CurrentUid) of
                true -> ok;
                false -> {error, not_group_member}
            end;
        {error, _} ->
            {error, msg_not_found}
    end;
verify_permission(_, _, _) ->
    {error, {invalid_param, <<"不支持的消息类型"/utf8>>}}.

%% @doc 通知表情添加
%% @param MsgId 消息ID
%% @param MsgType 消息类型
%% @param CurrentUid 当前用户ID
%% @param Emoji emoji字符
-spec notify_reaction_added(binary(), binary(), integer(), binary()) -> ok.
notify_reaction_added(MsgId, <<"c2c">>, CurrentUid, Emoji) ->
    % 单聊：通知对方
    case msg_c2c_ds:find_msg_by_id(MsgId) of
        {ok, #{<<"from_id">> := FromId, <<"to_id">> := ToId}} ->
            % 确定接收者
            ToNotify = case CurrentUid of
                FromId -> ToId;
                ToId -> FromId;
                _ -> undefined
            end,
            case ToNotify of
                undefined ->
                    ok;
                _ ->
                    % 构建通知消息
                    Payload = #{
                        <<"msg_id">> => MsgId,
                        <<"emoji">> => Emoji,
                        <<"action">> => <<"add">>,
                        <<"user_id">> => CurrentUid
                    },
                    Action = <<"message_reaction">>,
                    % 通过 WebSocket 发送通知
                    msg_s2c_ds:send(CurrentUid, [ToNotify], Action, MsgId, null, Payload, nosave)
            end;
        _ ->
            ok
    end;
notify_reaction_added(MsgId, <<"c2g">>, CurrentUid, Emoji) ->
    % 群聊：通知所有群成员（除了自己）
    case msg_c2g_ds:find_msg_by_id(MsgId) of
        {ok, #{<<"to_id">> := Gid}} ->
            % 获取群成员列表
            MemberUids = group_ds:member_uids(Gid),
            % 过滤掉自己
            ToNotify = lists:filter(fun(Uid) -> Uid =/= CurrentUid end, MemberUids),
            % 构建通知消息
            Payload = #{
                <<"msg_id">> => MsgId,
                <<"emoji">> => Emoji,
                <<"action">> => <<"add">>,
                <<"user_id">> => CurrentUid,
                <<"gid">> => Gid
            },
            Action = <<"message_reaction">>,
            % 通过 WebSocket 发送通知
            msg_s2c_ds:send(CurrentUid, ToNotify, Action, MsgId, null, Payload, nosave);
        _ ->
            ok
    end;
notify_reaction_added(_, _, _, _) ->
    ok.

%% @doc 通知表情移除
%% @param MsgId 消息ID
%% @param MsgType 消息类型
%% @param CurrentUid 当前用户ID
%% @param Emoji emoji字符
-spec notify_reaction_removed(binary(), binary(), integer(), binary()) -> ok.
notify_reaction_removed(MsgId, <<"c2c">>, CurrentUid, Emoji) ->
    % 单聊：通知对方
    case msg_c2c_ds:find_msg_by_id(MsgId) of
        {ok, #{<<"from_id">> := FromId, <<"to_id">> := ToId}} ->
            % 确定接收者
            ToNotify = case CurrentUid of
                FromId -> ToId;
                ToId -> FromId;
                _ -> undefined
            end,
            case ToNotify of
                undefined ->
                    ok;
                _ ->
                    % 构建通知消息
                    Payload = #{
                        <<"msg_id">> => MsgId,
                        <<"emoji">> => Emoji,
                        <<"action">> => <<"remove">>,
                        <<"user_id">> => CurrentUid
                    },
                    Action = <<"message_reaction">>,
                    % 通过 WebSocket 发送通知
                    msg_s2c_ds:send(CurrentUid, [ToNotify], Action, MsgId, null, Payload, nosave)
            end;
        _ ->
            ok
    end;
notify_reaction_removed(MsgId, <<"c2g">>, CurrentUid, Emoji) ->
    % 群聊：通知所有群成员（除了自己）
    case msg_c2g_ds:find_msg_by_id(MsgId) of
        {ok, #{<<"to_id">> := Gid}} ->
            % 获取群成员列表
            MemberUids = group_ds:member_uids(Gid),
            % 过滤掉自己
            ToNotify = lists:filter(fun(Uid) -> Uid =/= CurrentUid end, MemberUids),
            % 构建通知消息
            Payload = #{
                <<"msg_id">> => MsgId,
                <<"emoji">> => Emoji,
                <<"action">> => <<"remove">>,
                <<"user_id">> => CurrentUid,
                <<"gid">> => Gid
            },
            Action = <<"message_reaction">>,
            % 通过 WebSocket 发送通知
            msg_s2c_ds:send(CurrentUid, ToNotify, Action, MsgId, null, Payload, nosave);
        _ ->
            ok
    end;
notify_reaction_removed(_, _, _, _) ->
    ok.
