-module(msg_reaction_ds).
%%%
% msg_reaction_ds 是消息表情回应数据服务层
% 提供消息表情回应的业务数据操作
%%%

-include("log.hrl").
-include("error_code.hrl").

-export([add_reaction/4]).
-export([remove_reaction/4]).
-export([get_reactions/2]).
-export([get_reaction_stats/2]).
-export([is_reacted/4]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 添加表情回应
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param UserId 用户ID
%% @param Emoji emoji字符
%% @return {ok, map()} | {error, Reason}
-spec add_reaction(binary(), binary(), integer(), binary()) -> {ok, map()} | {error, term()}.
add_reaction(MsgId, MsgType, UserId, Emoji) ->
    case msg_reaction_repo:add(MsgId, MsgType, UserId, Emoji) of
        ok ->
            % 清除缓存
            clear_reaction_cache(MsgId, MsgType),
            % 返回添加的表情信息
            {ok, #{
                <<"msg_id">> => MsgId,
                <<"msg_type">> => MsgType,
                <<"user_id">> => UserId,
                <<"emoji">> => Emoji,
                <<"created_at">> => elib_dt:to_rfc3339(elib_dt:now())
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 移除表情回应
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param UserId 用户ID
%% @param Emoji emoji字符
%% @return ok
-spec remove_reaction(binary(), binary(), integer(), binary()) -> ok.
remove_reaction(MsgId, MsgType, UserId, Emoji) ->
    ok = msg_reaction_repo:remove(MsgId, MsgType, UserId, Emoji),
    % 清除缓存
    clear_reaction_cache(MsgId, MsgType),
    ok.

%% @doc 获取消息的表情列表（按emoji分组）
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @return {ok, list({binary(), map()})} | {error, Reason}
%% 返回格式: [{<<"👍"/utf8>>, #{<<"count">> => 5, <<"users">> => [uid1, uid2, ...]}}]
-spec get_reactions(binary(), binary()) -> {ok, list()} | {error, term()}.
get_reactions(MsgId, MsgType) ->
    case msg_reaction_repo:find_by_msg(MsgId, MsgType) of
        {ok, []} ->
            {ok, []};
        {ok, Reactions} ->
            % 按emoji分组
            Grouped = group_reactions_by_emoji(Reactions),
            {ok, Grouped};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取表情统计信息
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @return {ok, list(map())} | {error, Reason}
%% 返回格式: [#{
%%   <<"emoji">> => <<"👍"/utf8>>,
%%   <<"count">> => 5,
%%   <<"users">> => [encoded_uid1, encoded_uid2, ...]
%% }]
-spec get_reaction_stats(binary(), binary()) -> {ok, list(map())} | {error, term()}.
get_reaction_stats(MsgId, MsgType) ->
    case msg_reaction_repo:find_by_msg(MsgId, MsgType) of
        {ok, []} ->
            {ok, []};
        {ok, Reactions} ->
            % 按emoji分组统计
            Stats = build_reaction_stats(Reactions),
            {ok, Stats};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 检查用户是否已添加指定表情
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param UserId 用户ID
%% @param Emoji emoji字符
%% @return true | false
-spec is_reacted(binary(), binary(), integer(), binary()) -> boolean().
is_reacted(MsgId, MsgType, UserId, Emoji) ->
    case msg_reaction_repo:find_by_msg_emoji(MsgId, MsgType, Emoji) of
        {ok, Reactions} ->
            % 检查用户是否在列表中
            lists:any(fun(R) ->
                maps:get(<<"user_id">>, R) =:= UserId
            end, Reactions);
        _ ->
            false
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 按emoji分组表情回应
%% @param Reactions 表情回应列表
%% @return 分组后的列表
-spec group_reactions_by_emoji(list(map())) -> list({binary(), map()}).
group_reactions_by_emoji(Reactions) ->
    % 使用 maps:fold 按 emoji 分组
    GroupedMap = lists:foldl(fun(Reaction, Acc) ->
        Emoji = maps:get(<<"emoji">>, Reaction),
        UserId = maps:get(<<"user_id">>, Reaction),

        case maps:find(Emoji, Acc) of
            {ok, #{<<"count">> := Count, <<"users">> := Users}} ->
                % 添加用户到现有组
                Acc#{Emoji => #{
                    <<"count">> => Count + 1,
                    <<"users">> => [UserId | Users]
                }};
            error ->
                % 创建新组
                Acc#{Emoji => #{
                    <<"count">> => 1,
                    <<"users">> => [UserId]
                }}
        end
    end, #{}, Reactions),

    % 转换为列表并排序（按数量降序）
    List = maps:to_list(GroupedMap),
    lists:sort(fun({_, A}, {_, B}) ->
        maps:get(<<"count">>, A) >= maps:get(<<"count">>, B)
    end, List).

%% @doc 构建表情统计信息
%% @param Reactions 表情回应列表
%% @return 统计信息列表
-spec build_reaction_stats(list(map())) -> list(map()).
build_reaction_stats(Reactions) ->
    % 按emoji分组
    GroupedMap = lists:foldl(fun(Reaction, Acc) ->
        Emoji = maps:get(<<"emoji">>, Reaction),
        UserId = maps:get(<<"user_id">>, Reaction),

        case maps:find(Emoji, Acc) of
            {ok, Users} ->
                Acc#{Emoji => [UserId | Users]};
            error ->
                Acc#{Emoji => [UserId]}
        end
    end, #{}, Reactions),

    % 构建统计信息
    maps:fold(fun(Emoji, UserIds, Acc) ->
        [#{
            <<"emoji">> => Emoji,
            <<"count">> => length(UserIds),
            <<"users">> => UserIds
        } | Acc]
    end, [], GroupedMap).

%% @doc 清除表情缓存
%% @param MsgId 消息ID
%% @param MsgType 消息类型
-spec clear_reaction_cache(binary(), binary()) -> ok.
clear_reaction_cache(MsgId, MsgType) ->
    % 构建缓存键
    CacheKey = {msg_reaction, MsgId, MsgType},
    % 清除缓存
    imboy_cache:delete(CacheKey),
    ok.
