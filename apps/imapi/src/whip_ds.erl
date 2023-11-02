-module(whip_ds).
%%%
% whip 领域服务模块
% whip domain service 缩写
%%%

-export([parse/1]).
-export([make/1]).
-export([make_ip4_addr/1]).

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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

parse(Bin) ->
    case ersip_sdp_conn:parse(<<Bin/binary, "\r\n">>) of
        {ok, V, <<>>} ->
            {ok, V};
        {error, _} = Error ->
            Error
    end.


make(Bin) ->
    {ok, Conn} = parse(Bin),
    Conn.


make_ip4_addr(AddrBin) ->
    {ok, Addr} = ersip_sdp_addr:parse(<<"in">>, <<"ip4">>, AddrBin),
    Addr.

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
