-module(app_version_ds).
%%%
% app_version 领域服务模块
% app_version domain service 缩写
%%%

-export ([sign_key/3]).
-export ([get_sign_key/4]).
-export ([set_sign_key/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

% app_version_ds:sign_key(<<"android">>, <<"1">>, <<"pub.imboy.apk">>).
% app_version_ds:sign_key(<<"ios">>, <<"1">>, <<"pub.imboy.2">>).
% app_version_ds:sign_key(<<"macos">>, <<"1">>, <<"pub.imboy.macos">>).
sign_key(ClientOS, Vsn, Pkg) when is_binary(ClientOS), is_binary(Vsn),is_binary(Pkg) ->
    %
    Key = <<Pkg/binary, "_", ClientOS/binary, "_", Vsn/binary>>,
    case config_ds:get(Key) of
        <<>> ->
            % 兼容旧之前版本，一年后可以不兼容 2024-08-16
            get_sign_key(ClientOS, Vsn, Pkg, <<"sign_key">>);
        Val ->
            Val
    end.

set_sign_key(ClientOS, Vsn, Pkg, Val) when is_binary(ClientOS), is_binary(Vsn),is_binary(Pkg) ->
    Key = <<Pkg/binary, "_", ClientOS/binary, "_", Vsn/binary>>,
    config_ds:set(Key, Val).


get_sign_key(ClientOS, Vsn, Pkg, Field) ->
    Where = [
        <<"vsn = '", Vsn/binary, "'">>,
        <<"package_name = '", Pkg/binary, "'">>,
        <<"type = '", ClientOS/binary, "'">>
    ],
    Where2 = imboy_cnv:implode(" AND ", Where),
    % Defalut = config_ds:env(solidified_key),
    imboy_db:pluck(<<"app_version">>, Where2, Field, undefined).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-
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
