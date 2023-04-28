-module(imboy_kv).
%%%
% imboy 对 khepri 的封装
%%%

-include_lib("imboy/include/log.hrl").
-include_lib("imboy/include/common.hrl").

-export([start/0]).

start() ->
    % khepri:start(),
    ok.

% % imboy_kv:put(Uid).
% -spec put(Uid::integer(), DType::binary(), DID :: binary(), Pid::pid()) -> ok.
% put(Uid, DType, DID, Pid) ->
%     Path = [s, integer_to_binary(Uid), DType],
%     khepri:put(Path, {Uid, DType, Pid, DID}).

% % imboy_kv:get(1).
% -spec get(integer()) -> {ok, map()}.
% get(Uid) ->
%     UidBin = integer_to_binary(Uid),
%     Path = <<"/:s/", UidBin/binary, "/*">>,
%     khepri:get_many(Path).

% % imboy_kv:get(1, <<"ios">>).
% -spec get(integer(), binary()) -> {ok, map()}.
% get(Uid, DType) ->
%     Path = [s, integer_to_binary(Uid), DType],
%     khepri:get_many(Path).

% % exists(Uid, DType) ->
% %     Path = [s, <<"uid">>, <<"dtype">>, <<"did">>],
% %     khepri:exists(Path, #{}).

% -spec delete(integer()) -> ok.
% delete(Uid) ->
%     UidBin = integer_to_binary(Uid),
%     Path = <<"/:s/", UidBin/binary, "/*">>,
%     khepri:delete(Path).

% -spec delete(integer(), binary()) -> ok.
% delete(Uid, DType) ->
%     Path = [s, integer_to_binary(Uid), DType],
%     khepri:delete(Path).

% % imboy_kv:user_count()
% -spec user_count() -> non_neg_integer().
% user_count() ->
%     {ok, Li} = khepri:fold("/:s/*/*", fun([s, U, _DType] , _NP, Acc) ->
%         [U|Acc]
%     end, []),
%     length(lists:uniq(Li)).

% % imboy_kv:list(1).
% -spec list(integer()) -> list().
% list(Limit) ->
%     {ok, Li} = khepri:fold("/:s/*/*", fun(_P , NP, Acc) ->
%         if length(Acc) > Limit ->
%             Acc;
%         true ->
%             [maps:get(data, NP) | Acc]
%         end
%     end, []),
%     Li.

% % imboy_kv:device_count(Uid).
% -spec device_count(integer()) -> non_neg_integer().
% device_count(Uid) ->
%     case imboy_kv:get(Uid) of
%         {ok, M1} ->
%             maps:size(M1);
%         _ ->
%             0
%     end.
