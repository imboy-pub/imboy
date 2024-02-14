-module(config_ds).
%%%
% config 领域服务模块
% config domain service 缩写
%%%

-export([get/1, get/2]).
-export([set/2,
         save/2]).
-export([aes_encrypt/1]).

-export([env/1, env/2, env/3]).
-export([reload/0,
         local_reload/0]).

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


% config_ds:env(test).
% config_ds:env(lager, colors, undefined).
env(Attr) ->
    env(Attr, undefined).


env(Attr, Def) ->
    env(imboy, Attr, Def).


env(App, Attr, Def) ->
    case application:get_env(App, Attr) of
        {ok, Value} ->
            Value;
        _ ->
            Def
    end.


% config_ds:reload().
reload() ->
    Path = config_file(),
    reload(Path).


%% 重新加载 sys.config 配置
%% [config_ds:env(test), config_ds:local_reload(), config_ds:env(test)].
local_reload() ->
    IMBoyEnv = os:getenv("IMBOYENV"),
    From = code:root_dir() ++ "/../../config/sys." ++ IMBoyEnv ++ ".config",
    To = config_file(),
    % Res1 = file:delete(To),
    % imboy_log:error("~p~n", [Res1]),
    imboy_log:info("~p~n", [#{from => From, to => To}]),
    file:copy(From, To, infinity),
    % Res2 = file:copy(From, To, infinity),
    % imboy_log:error("copy file res: ~p~n", [Res2]),
    reload(To),
    ok.


% config_ds:get(<<"site_name">>).
get(Key) ->
    get(Key, <<>>).


get(Key, Defalut) when is_list(Key) ->
    get(list_to_binary(Key), Defalut);
get(ConfigKey, Defalut) ->
    % Key = {config3, ConfigKey},
    Fun = fun() ->
                  Val = imboy_hasher:decoded_field(<<"value">>),
                  imboy_db:pluck(<<"config">>, <<"key = '", ConfigKey/binary, "'">>, Val, Defalut)
          end,
    % 缓存10天
    imboy_cache:memo(Fun, cache_key(ConfigKey), 864000).


% config_ds:set(<<"dbc">>, <<"ddd2">>).
% config_ds:get(<<"dbc">>).
set(Key, Val) when is_list(Key) ->
    set(list_to_binary(Key), Val);
set(Key, Val) when is_list(Val) ->
    set(Key, list_to_binary(Val));
set(Key, Val) ->
    save(Key, [{<<"value">>, Val}, {<<"tab">>, <<"sys">>}, {<<"title">>, <<"">>}, {<<"remark">>, <<"">>}]).


save(Key, Data) ->
    % ?LOG([Key, Val, Tab]),
    Now = imboy_dt:utc(millisecond),
    Now2 = integer_to_binary(Now),
    case imboy_db:pluck(<<"config">>, <<"key = '", Key/binary, "'">>, <<"count(*) as count">>, 0) of
        0 ->
            Data2 = [{<<"created_at">>, Now2} | [{<<"key">>, Key} | Data]],
            % Data2 = [{<<"key">>, Key} | Data],
            Column = [ K || {K, _} <- Data2 ],
            Value = [ <<"'", V/binary, "'">> || {_, V} <- Data2 ],
            imboy_db:insert_into(<<"config">>, Column, Value, <<"">>);
        _ ->
            Data2 = [{<<"updated_at">>, Now2} | Data],
            imboy_db:update(<<"config">>
                , <<"key = '", Key/binary, "'">>
                , Data2)
    end,
    imboy_cache:flush(cache_key(Key)),
    aes_encrypt(Key).


% config_ds:aes_encrypt(<<"login_rsa_pub_key">>).
% config_ds:get(<<"login_rsa_pub_key">>).

% config_ds:get(<<"login_rsa_priv_key">>).
% config_ds:aes_encrypt(<<"login_rsa_priv_key">>).


% config_ds:aes_encrypt(<<"login_pwd_rsa_encrypt">>).
% config_ds:aes_encrypt(<<"site_name">>).
aes_encrypt(Key) when is_list(Key) ->
    aes_encrypt(list_to_binary(Key));
aes_encrypt(Key) ->
    Val = imboy_db:pluck(<<"config">>, <<"key = '", Key/binary, "'">>, <<"value">>, <<>>),
    do_aes_encrypt(Key, Val).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


cache_key(K) ->
    {config3, K}.


reload(Path) ->
    {ok, Items} = file:consult(Path),
    % imboy_log:error("~p~n", [Items]),
    [ application:set_env(Conf) || Conf <- Items ],
    ok.


% config_ds:config_file().
config_file() ->
    {imboy, _, Vsn} = lists:keyfind(imboy, 1, application:which_applications()),
    code:root_dir() ++ "/releases/" ++ Vsn ++ "/sys.config".


do_aes_encrypt(Key, <<"aes_cbc_", _Val/binary>>) ->
    imboy_db:pluck(<<"config">>, <<"key = '", Key/binary, "'">>, <<"value">>, <<>>);
do_aes_encrypt(Key, Val) ->
    AesKey = config_ds:env(postgre_aes_key),
    Where = <<"key = '", Key/binary, "'">>,
    Val1 = base64:encode(Val),
    Set = <<"value = 'aes_cbc_' || encode(encrypt('", Val1/binary, "', '", AesKey/binary,
            "', 'aes-cbc/pad:pkcs'), 'base64')">>,
    imboy_db:update(<<"config">>, Where, Set),
    imboy_db:pluck(<<"config">>, <<"key = '", Key/binary, "'">>, <<"value">>, <<>>).
