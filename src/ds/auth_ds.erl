-module(auth_ds).

%%%
% auth 领域服务模块
% auth domain service 缩写
%%%

-export([get_token/3]).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取资源服务访问token
%% 生成用于访问资源服务的认证令牌。
%% 将上传密钥和资源标识符拼接后进行MD5哈希，取中间16位作为token。
%%
%% 使用示例：
%% {imboy_dt:utc(second), auth_ds:get_token(assets, <<"dev">>, integer_to_list(imboy_dt:utc(second)))}.
%% auth_ds:get_token(assets, <<"open">>, "/img/20225/25_21/ca73910gph0gio9q2pg0.png?1687988290").
%%
%% @param ResourceType 资源类型，通常为 'assets'
%% @param Scene 场景标识（当前未使用）
%% @param Num 资源标识符，可以是字符串或数字
%% @returns 16字节的二进制token
-spec get_token(atom(), binary() | string(), binary() | string() | integer()) -> binary().
get_token(assets, _Scene, Num) ->
    % TODO public key sign
    Key = config_ds:get(<<"upload_key">>),
    Num2 = ec_cnv:to_binary(Num),
    binary:part(
        imboy_hasher:md5(<<Key/binary, Num2/binary>>), {8, 16}).

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
