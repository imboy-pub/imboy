-module(group_vote_logic).

%%%
% group_vote 业务逻辑模块
%%%
%% @doc 群投票业务逻辑模块
% 提供群内投票创建、投票、查询、统计等功能

-include("log.hrl").

-export([create_vote/6]).
-export([cast_vote/3]).
-export([update_vote/3]).
-export([cancel_vote/2]).
-export([get_vote_detail/1]).
-export([list_votes/3]).
-export([close_vote/1]).
-export([get_my_vote/2]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 创建群投票
%% @param Gid 群组ID
%% @param CreatorId 创建者ID
%% @param Title 投票标题
%% @param Options 投票选项列表 [#{option_text => ..., sort_order => ...}]
%% @param Extra 额外参数 #{vote_type, is_anonymous, end_at}
%% @param ExtConfig 扩展配置
%% @return {ok, VoteMap} | {error, Reason}
-spec create_vote(integer(), integer(), binary(), [map()], map(), map()) ->
                     {ok, map()} | {error, term()}.
create_vote(Gid, CreatorId, Title, Options, Extra, _ExtConfig) ->
    % 验证必填参数
    case Title of
        <<>> ->
            {error, {missing_param, title}};
        _ when byte_size(Title) > 200 ->
            {error, {invalid_param, title_too_long}};
        _ ->
            case Options of
                [] ->
                    {error, {missing_param, options}};
                _ when length(Options) > 20 ->
                    {error, {invalid_param, too_many_options}};
                _ ->
                    do_create_vote(Gid, CreatorId, Title, Options, Extra)
            end
    end.

%% @doc 内部创建投票逻辑
do_create_vote(Gid, CreatorId, Title, Options, Extra) ->
    % 生成唯一的 vote_id
    VoteId = elib_id:gen(<<"vote">>),

    % 提取参数
    VoteType = maps:get(vote_type, Extra, 1),
    IsAnonymous = maps:get(is_anonymous, Extra, false),
    Description = maps:get(description, Extra, <<>>),
    EndAt = maps:get(end_at, Extra, undefined),

    % 准备投票数据
    VoteData = #{
        group_id => Gid,
        vote_id => VoteId,
        title => Title,
        description => Description,
        creator_id => CreatorId,
        vote_type => VoteType,
        is_anonymous => IsAnonymous,
        end_at => EndAt
    },

    % 准备选项数据
    OptionsData = lists:map(fun(Option) ->
        OptionText = maps:get(option_text, Option),
        SortOrder = maps:get(sort_order, Option, 0),
        OptionId = elib_id:gen(<<"opt">>),
        #{
            vote_id => VoteId,
            option_id => OptionId,
            option_text => OptionText,
            sort_order => SortOrder
        }
    end, Options),

    % 插入投票和选项
    case group_vote_repo:insert_vote(VoteData) of
        {ok, _VoteId, _} ->
            case group_vote_repo:insert_options_batch(OptionsData) of
                {ok, _Count} ->
                    {ok, #{
                        <<"vote_id">> => VoteId,
                        <<"title">> => Title,
                        <<"vote_type">> => VoteType,
                        <<"is_anonymous">> => IsAnonymous
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 投票
%% @param VoteId 投票ID (字符串)
%% @param UserId 用户ID (整数)
%% @param OptionIds 选项ID列表
%% @return {ok, VoteMap} | {error, Reason}
-spec cast_vote(binary(), integer(), [binary()]) -> {ok, map()} | {error, term()}.
cast_vote(VoteId, UserId, OptionIds) ->
    % 检查投票是否存在
    case group_vote_repo:find_by_vote_id(VoteId) of
        {error, not_found} ->
            {error, vote_not_found};
        {ok, Vote} ->
            validate_and_cast_vote(Vote, VoteId, UserId, OptionIds)
    end.

%% @doc 验证并执行投票
validate_and_cast_vote(Vote, VoteId, UserId, OptionIds) ->
    Status = maps:get(<<"status">>, Vote),
    VoteType = maps:get(<<"vote_type">>, Vote),
    EndAt = maps:get(<<"end_at">>, Vote),

    % 检查投票状态
    case Status of
        2 ->
            {error, vote_is_closed};
        3 ->
            {error, vote_is_cancelled};
        1 ->
            % 检查是否过期
            case EndAt of
                null ->
                    check_and_cast(VoteId, UserId, OptionIds, VoteType);
                EndAt2 ->
                    Now = elib_dt:now(),
                    case elib_dt:rfc3339_to(EndAt2) > elib_dt:rfc3339_to(Now) of
                        true ->
                            check_and_cast(VoteId, UserId, OptionIds, VoteType);
                        false ->
                            {error, vote_is_expired}
                    end
            end;
        _ ->
            {error, invalid_vote_status}
    end.

%% @doc 检查并执行投票
check_and_cast(VoteId, UserId, OptionIds, VoteType) ->
    % 检查是否已投票
    case group_vote_repo:find_record_by_vote_and_user(VoteId, UserId) of
        {ok, _Record} ->
            {error, already_voted};
        {error, not_found} ->
            % 验证选项
            case group_vote_repo:list_options_by_vote_id(VoteId) of
                {ok, Options} ->
                    ValidOptionIds = [maps:get(<<"option_id">>, O) || O <- Options],
                    validate_options(OptionIds, ValidOptionIds, VoteType, VoteId, UserId);
                {error, _Reason} ->
                    {error, options_not_found}
            end
    end.

%% @doc 验证选项有效性
validate_options(OptionIds, ValidOptionIds, VoteType, VoteId, UserId) ->
    % 检查选项是否有效
    case lists:all(fun(O) -> lists:member(O, ValidOptionIds) end, OptionIds) of
        false ->
            {error, invalid_option};
        true ->
            % 检查选项数量
            case VoteType of
                1 when length(OptionIds) =/= 1 ->
                    {error, single_choice_requires_one_option};
                2 when length(OptionIds) < 2 ->
                    {error, multiple_choice_requires_at_least_two_options};
                _ ->
                    do_cast_vote(VoteId, UserId, OptionIds)
            end
    end.

%% @doc 执行投票
do_cast_vote(VoteId, UserId, OptionIds) ->
    % 将选项ID列表转为JSON字符串
    OptionIdsJson = jsone:encode(OptionIds),
    RecordData = #{
        vote_id => VoteId,
        user_id => UserId,
        option_ids => OptionIdsJson
    },
    case group_vote_repo:insert_record(RecordData) of
        {ok, _RecordId, _} ->
            {ok, #{<<"vote_id">> => VoteId}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 修改投票
%% @param VoteId 投票ID (字符串)
%% @param UserId 用户ID (整数)
%% @param OptionIds 新的选项ID列表
%% @return {ok, VoteMap} | {error, Reason}
-spec update_vote(binary(), integer(), [binary()]) -> {ok, map()} | {error, term()}.
update_vote(VoteId, UserId, OptionIds) ->
    % 检查是否已投票
    case group_vote_repo:find_record_by_vote_and_user(VoteId, UserId) of
        {error, not_found} ->
            {error, not_voted_yet};
        {ok, Record} ->
            % 检查投票是否有效
            case group_vote_repo:find_by_vote_id(VoteId) of
                {error, not_found} ->
                    {error, vote_not_found};
                {ok, Vote} ->
                    validate_and_update_vote(Vote, Record, VoteId, UserId, OptionIds)
            end
    end.

%% @doc 验证并更新投票
validate_and_update_vote(Vote, Record, VoteId, UserId, OptionIds) ->
    Status = maps:get(<<"status">>, Vote),
    VoteType = maps:get(<<"vote_type">>, Vote),

    case Status of
        1 ->
            % 验证选项
            case group_vote_repo:list_options_by_vote_id(VoteId) of
                {ok, Options} ->
                    ValidOptionIds = [maps:get(<<"option_id">>, O) || O <- Options],
                    case lists:all(fun(O) -> lists:member(O, ValidOptionIds) end, OptionIds) of
                        false ->
                            {error, invalid_option};
                        true ->
                            case VoteType of
                                1 when length(OptionIds) =/= 1 ->
                                    {error, single_choice_requires_one_option};
                                2 when length(OptionIds) < 2 ->
                                    {error, multiple_choice_requires_at_least_two_options};
                                _ ->
                                    do_update_vote(Record, VoteId, UserId, OptionIds)
                            end
                    end;
                {error, _Reason} ->
                    {error, options_not_found}
            end;
        _ ->
            {error, vote_is_closed}
    end.

%% @doc 执行更新投票
do_update_vote(Record, VoteId, _UserId, OptionIds) ->
    RecordId = maps:get(<<"id">>, Record),
    OptionIdsJson = jsone:encode(OptionIds),
    Data = #{option_ids => OptionIdsJson},
    case group_vote_repo:update_record(RecordId, Data) of
        {ok, _Count} ->
            {ok, #{<<"vote_id">> => VoteId}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 取消投票
%% @param VoteId 投票ID (字符串)
%% @param UserId 用户ID (整数)
%% @return ok | {error, Reason}
-spec cancel_vote(binary(), integer()) -> ok | {error, term()}.
cancel_vote(VoteId, UserId) ->
    case group_vote_repo:find_record_by_vote_and_user(VoteId, UserId) of
        {error, not_found} ->
            {error, not_voted_yet};
        {ok, Record} ->
            RecordId = maps:get(<<"id">>, Record),
            case group_vote_repo:delete_record(RecordId) of
                {ok, _Count} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 获取投票详情
%% @param VoteId 投票ID (字符串)
%% @return {ok, VoteDetail} | {error, Reason}
-spec get_vote_detail(binary()) -> {ok, map()} | {error, term()}.
get_vote_detail(VoteId) ->
    case group_vote_repo:find_by_vote_id(VoteId) of
        {error, not_found} ->
            {error, vote_not_found};
        {ok, Vote} ->
            build_vote_detail(Vote, VoteId)
    end.

%% @doc 构建投票详情
build_vote_detail(Vote, VoteId) ->
    % 获取选项列表
    case group_vote_repo:list_options_by_vote_id(VoteId) of
        {ok, Options} ->
            % 获取总投票人数
            {ok, TotalVotes} = group_vote_repo:count_total_votes_by_vote_id(VoteId),

            % 为每个选项添加得票数
            OptionsWithCount = lists:map(fun(Option) ->
                OptionId = maps:get(<<"option_id">>, Option),
                {ok, Count} = group_vote_repo:count_votes_by_option_id(OptionId),
                Option#{<<"vote_count">> => Count}
            end, Options),

            {ok, Vote#{
                <<"options">> => OptionsWithCount,
                <<"total_votes">> => TotalVotes
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查询群投票列表
%% @param Gid 群组ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, VotesMap} | {error, Reason}
-spec list_votes(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_votes(Gid, Page, Size) ->
    case group_vote_repo:list_votes_by_group_id(Gid, Page, Size) of
        {ok, Votes} ->
            case group_vote_repo:count_votes_by_group_id(Gid) of
                {ok, Total} ->
                    {ok, #{
                        <<"total">> => Total,
                        <<"page">> => Page,
                        <<"size">> => Size,
                        <<"list">> => Votes
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 结束投票
%% @param VoteId 投票ID (字符串)
%% @return ok | {error, Reason}
-spec close_vote(binary()) -> ok | {error, term()}.
close_vote(VoteId) ->
    case group_vote_repo:find_by_vote_id(VoteId) of
        {error, not_found} ->
            {error, vote_not_found};
        {ok, Vote} ->
            Status = maps:get(<<"status">>, Vote),
            case Status of
                2 ->
                    {error, vote_already_closed};
                1 ->
                    case group_vote_repo:update_vote_status(VoteId, 2) of
                        {ok, _Count} ->
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, invalid_vote_status}
            end
    end.

%% @doc 获取我的投票记录
%% @param VoteId 投票ID (字符串)
%% @param UserId 用户ID (整数)
%% @return {ok, MyVote} | {error, Reason}
-spec get_my_vote(binary(), integer()) -> {ok, map()} | {error, term()}.
get_my_vote(VoteId, UserId) ->
    case group_vote_repo:find_record_by_vote_and_user(VoteId, UserId) of
        {error, not_found} ->
            {error, not_voted_yet};
        {ok, Record} ->
            OptionIdsJson = maps:get(<<"option_ids">>, Record),
            OptionIds = jsone:decode(OptionIdsJson),
            CreatedAt = maps:get(<<"created_at">>, Record),
            {ok, #{
                <<"vote_id">> => VoteId,
                <<"option_ids">> => OptionIds,
                <<"created_at">> => CreatedAt
            }}
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
