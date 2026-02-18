%%%
%  msg_pinned 业务逻辑模块 - 消息置顶功能
%%%

-module(msg_pinned_logic).

-export([pin/2, unpin/2]).

-include("log.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 消息置顶
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec pin(binary(), integer()) -> ok | {error, any()}.
pin(MsgId, CurrentUid) ->
    % 检查消息是否存在并验证权限
    case msg_c2c_repo:find_msg_by_id(MsgId) of
        {ok, _} ->
            % C2C消息，更新置顶状态
            case msg_c2c_repo:update_pinned(MsgId, CurrentUid, true) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _} ->
            % 检查是否是群消息
            case msg_c2g_repo:find_msg_by_id(MsgId) of
                {ok, _} ->
                    % C2G消息，更新置顶状态
                    case msg_c2g_repo:update_pinned(MsgId, CurrentUid, true) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, _} ->
                    {error, not_found}
            end
    end.

%% @doc 取消消息置顶
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec unpin(binary(), integer()) -> ok | {error, any()}.
unpin(MsgId, CurrentUid) ->
    % 检查消息是否存在并验证权限
    case msg_c2c_repo:find_msg_by_id(MsgId) of
        {ok, _} ->
            % C2C消息，取消置顶状态
            case msg_c2c_repo:update_pinned(MsgId, CurrentUid, false) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _} ->
            % 检查是否是群消息
            case msg_c2g_repo:find_msg_by_id(MsgId) of
                {ok, _} ->
                    % C2G消息，取消置顶状态
                    case msg_c2g_repo:update_pinned(MsgId, CurrentUid, false) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, _} ->
                    {error, not_found}
            end
    end.
