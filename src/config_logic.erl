-module(config_logic).
%%%
%  config 业务逻辑模块
%%%
-export([get/1]).
-export([get/2]).


% config_logic:get(<<"site_name">>).
get(Key) ->
    get(Key, "").

get(Key, Defalut) when is_list(Key) ->
    get(list_to_binary(Key), Defalut);
get(ConfigKey, Defalut) ->
    Key = {config, ConfigKey},
    Fun = fun() ->
        imboy_db:pluck(
            <<"config">>
            , <<"key = '", ConfigKey/binary, "'">>
            , <<"value">>
            , Defalut
        )
    end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).
