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

% app_version_ds:get_sign_key(<<"android">>, <<"0.5.0">>, <<"pub.imboy.apk">>, <<"sign_key">>).
% app_version_ds:sign_key(<<"android">>, <<"0.5.0">>, <<"pub.imboy.apk">>).
sign_key(ClientOS, Vsn, Pkg) ->
    % get_sign_key(ClientOS, Vsn, Pkg, <<"sign_key">>).
    Key = {sign_key, ClientOS, Vsn, Pkg},
    Fun = fun() -> get_sign_key(ClientOS, Vsn, Pkg, <<"sign_key">>) end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).


set_sign_key(ClientOS, Vsn, Pkg, Key) ->
    OldKey = get_sign_key(ClientOS, Vsn, Pkg, <<"sign_key">>),
    case OldKey of
        undefined ->
            app_version_repo:add(#{
                <<"type">> => ClientOS,
                <<"package_name">> => Pkg,
                <<"app_name">> => <<>>,
                <<"vsn">> => Vsn,
                <<"download_url">> => <<>>,
                <<"description">> => <<>>,
                <<"force_update">> => 2,
                created_at => imboy_dt:utc(millisecond),
                <<"sign_key">> => Key});
        _ ->
            Where = [
                <<"vsn = '", Vsn/binary, "'">>,
                <<"package_name = '", Pkg/binary, "'">>,
                <<"type = '", ClientOS/binary, "'">>
            ],
            Where2 = imboy_cnv:implode(" AND ", Where),
            imboy_db:update(<<"app_version">>, Where2, #{
                <<"sign_key">> => Key
            })
    end,
    imboy_cache:flush({sign_key, ClientOS, Vsn, Pkg}).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-
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
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
