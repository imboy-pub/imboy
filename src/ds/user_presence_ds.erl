-module(user_presence_ds).

%%%
% user_presence_ds — 用户在线状态投影 / User presence projection
%
% 职责：基于 imboy_syn 计数 + user_setting_ds 隐身设置，将一组用户记录
%       投影出 status (online | offline) 与 last_seen_at 字段。
%
% 设计动机 / Why this lives in DS layer:
% - 原 user_logic:batch_online_state/1 是纯数据投影，无业务规则
% - 过去被 friend_ds 反向调用，造成 DS→Logic 反向跨层
% - 抽到 user_presence_ds 后，friend_ds / 任何其他 DS 都可平级调用，
%   彻底消除反向依赖（2026-04 G2-c 同类拆解）
%
% Originally lived in user_logic as a pure projection helper, but was
% reverse-called from friend_ds — violating the Handler→Logic→DS→Repo
% direction. Moved to the DS layer in 2026-04 to break that anti-pattern.
%%%

-export([batch_online_state/1, check_online_status/1]).

-include("log.hrl").

%% @doc 批量给一组用户记录注入 status / last_seen_at 字段
%% Batch-inject status / last_seen_at into a list of user maps.
%%
%% @param Users [#{<<"id">> := Uid, ...}]
%% @returns 同结构列表，每条额外含 <<"status">>(online|offline) 与 <<"last_seen_at">>
-spec batch_online_state([map()]) -> [map()].
batch_online_state(Users) when is_list(Users) ->
    UidStatusMap = lists:foldl(fun(User, Acc) ->
        Uid = maps:get(<<"id">>, User),
        Status = check_online_status(Uid),
        maps:put(Uid, Status, Acc)
    end, #{}, Users),
    lists:map(fun(User) ->
        Uid = maps:get(<<"id">>, User),
        LastSeenAt = maps:get(<<"last_seen_at">>, User, <<>>),
        Status = maps:get(Uid, UidStatusMap, offline),
        User#{<<"status">> => Status, <<"last_seen_at">> => LastSeenAt}
    end, Users).

%% @doc 检查单个用户是否在线
%% Check single-user online status; respects the user's chat_state_hide setting.
-spec check_online_status(integer()) -> online | offline.
check_online_status(Uid) ->
    case imboy_syn:count_user(Uid) of
        0 ->
            offline;
        _Count ->
            case user_setting_ds:chat_state_hide(Uid) of
                true ->
                    offline;
                _ ->
                    online
            end
    end.
