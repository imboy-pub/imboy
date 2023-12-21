-module(adm_user_logic).
%%%
% adm_user 业务逻辑模块
% adm_user business logic module
%%%

-export ([find/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% demo方法描述
-spec find(integer(), binary(), tuple()) -> ok.
find(Uid, Column, Key) ->
    % Key = {adm_user, Column, Uid},
    Fun = fun() ->
        case adm_user_repo:find_by_id(Uid, Column) of
            {ok, _ColumnLi, [Val]} ->
                {true, Val};
            _ ->
                {false, <<>>}
        end
    end,
    %  缓存key挺多，是针对用户ID的，缓存时间不宜过长
    imboy_cache:memo(Fun, Key, 7200).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
