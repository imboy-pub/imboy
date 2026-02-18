-module(msg_forward_ds).
%%%
% msg_forward_ds 是 msg_forward domain service 缩写
% 消息转发数据服务层，封装消息转发相关的数据操作
%%%

-include_lib("kernel/include/logger.hrl").
-include("chat.hrl").
-include("log.hrl").

-export([save_forward_record/8]).
-export([find_forward_records/1]).
-export([find_user_forward_history/2]).
-export([count_forward_records/1]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 保存转发记录
%% @param OriginalMsgId 原始消息ID
%% @param OriginalFromId 原始消息发送者ID
%% @param OriginalToId 原始消息接收者ID（单聊为用户ID，群聊为群组ID）
%% @param OriginalType 原始消息类型（c2c/c2g）
%% @param ForwardMsgId 转发后的消息ID
%% @param ForwardFromId 转发操作者ID
%% @param ForwardToId 转发目标ID（单聊为用户ID，群聊为群组ID）
%% @param ForwardType 转发目标类型（c2c/c2g）
%% @return ok | {error, Reason}
-spec save_forward_record(binary(), integer(), integer(), binary(), binary(), integer(), integer(), binary()) -> ok | {error, term()}.
save_forward_record(OriginalMsgId, OriginalFromId, OriginalToId, OriginalType,
                    ForwardMsgId, ForwardFromId, ForwardToId, ForwardType) ->
    ForwardRecord = #{
        <<"original_msg_id">> => OriginalMsgId,
        <<"original_from_id">> => OriginalFromId,
        <<"original_to_id">> => OriginalToId,
        <<"original_type">> => OriginalType,
        <<"forward_msg_id">> => ForwardMsgId,
        <<"forward_from_id">> => ForwardFromId,
        <<"forward_to_id">> => ForwardToId,
        <<"forward_type">> => ForwardType
    },
    case msg_forward_repo:insert(ForwardRecord) of
        {ok, _} -> ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to save forward record: ~p", [Reason]),
            {error, Reason}
    end.


%% @doc 根据原始消息ID查找转发记录
%% @param MsgId 原始消息ID
%% @return {ok, Records} | {error, Reason}
-spec find_forward_records(binary()) -> {ok, list(map())} | {error, term()}.
find_forward_records(MsgId) ->
    msg_forward_repo:find_by_original_msg_id(MsgId).


%% @doc 查找用户的转发历史记录
%% @param FromUid 用户ID
%% @param Limit 查询结果数量限制
%% @return {ok, Records} | {error, Reason}
-spec find_user_forward_history(integer(), non_neg_integer()) -> {ok, list(map())} | {error, term()}.
find_user_forward_history(FromUid, Limit) ->
    msg_forward_repo:find_by_forward_from_id(FromUid, Limit).


%% @doc 统计消息的转发次数
%% @param MsgId 消息ID
%% @return {ok, Count} | {error, Reason}
-spec count_forward_records(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_forward_records(MsgId) ->
    msg_forward_repo:count_by_original_msg_id(MsgId).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
