-module(user_device_logic).
%%%
% user_device 业务逻辑模块
% user_device business logic module
%%%

-export([device_name/2,
         change_name/3,
         delete/2]).
-export([page/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


-spec device_name(integer(), binary()) -> binary().
% Uid = 1.
% DID = <<"3f039a2b4724a5b7">>.
% Key = {user_device_name, Uid, DID}.
% user_device_logic:device_name(1, <<"HUAWEIMRD-AL00">>).
% user_device_repo:device_name(1, <<"HUAWEIMRD-AL00">>).
%  imboy_cache:get(Key).
device_name(Uid, DID) ->
    Key = {user_device_name, 2, Uid, DID},
    Fun = fun() -> user_device_repo:device_name(Uid, DID) end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).


-spec change_name(integer(), binary(), binary()) -> ok.
change_name(Uid, DID, Name) ->
    Set = <<"device_name = $1">>,
    SetArgs = [Name],
    user_device_repo:update_by_did(Uid, DID, Set, SetArgs),

    Key = {user_device_name, 2, Uid, DID},
    imboy_cache:flush(Key),
    ok.


-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    user_device_repo:delete(Uid, DID),
    Key = {user_device_name, 2, Uid, DID},
    imboy_cache:flush(Key),
    ok.


% user_device_logic:page(1, 1, 10).
-spec page(Uid :: integer(), Page :: integer(), Size :: integer()) -> list().
page(Uid, Page, Size) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = user_device_repo:count_by_uid(Uid),
    case user_device_repo:page(Uid, Size, Offset) of
        {ok, []} ->
            #{total => Total, page => Page, size => Size, list => []};
        {ok, Items0} ->
            OnlineDids = imboy_syn:online_dids(Uid),
            Items2 = [imboy_response:json_decode_field(
                         #{<<"online">> => lists:member(maps:get(<<"device_id">>, Row), OnlineDids),
                           <<"device_id">> => maps:get(<<"device_id">>, Row),
                           <<"device_name">> => maps:get(<<"device_name">>, Row, <<>>),
                           <<"device_type">> => maps:get(<<"device_type">>, Row, <<>>),
                           <<"last_active_at">> => maps:get(<<"last_active_at">>, Row, <<>>),
                           <<"device_vsn">> => maps:get(<<"device_vsn">>, Row, <<>>)},
                         <<"device_vsn">>)
                      || Row <- Items0],
            #{total => Total, page => Page, size => Size, list => Items2};
        _ ->
            #{total => Total, page => Page, size => Size, list => []}
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
