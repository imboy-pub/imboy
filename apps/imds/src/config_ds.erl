-module(config_ds).
%%%
% config 领域服务模块
% config domain service 缩写
%%%

-export([get/1, get/2]).
-export([set/2, set/4,
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

get(Key, Defalut) ->
    Key2 = ec_cnv:to_binary(Key),
    % Val = imboy_hasher:decoded_field(<<"value">>),
    % Res = imboy_db:pluck(<<"config">>, <<"key = '", Key2/binary, "'">>, Val, Defalut),
    % imboy_cnv:json_maybe(Res).
    Fun = fun() ->
        Val = imboy_hasher:decoded_field(<<"value">>),
        Res = imboy_db:pluck(<<"config">>, <<"key = '", Key2/binary, "'">>, Val, Defalut),
        imboy_cnv:json_maybe(Res)
    end,
    % 缓存10天
    imboy_cache:memo(Fun, cache_key(Key2), 864000).

% config_ds:set(<<"dbc">>, <<"ddd2">>).
% config_ds:get(<<"dbc">>).
set(Key, Val) ->
    set(Key, Val, <<>>, <<>>).

set(Key, Val, Title, Remark) ->
    Key2 = ec_cnv:to_binary(Key),
    imboy_cache:flush(cache_key(Key2)),
    save(Key2, #{
        % value 的值在 do_aes_encrypt/2 放里面处理加密，这里给明文
        <<"value">> => jsone:encode(Val, [native_utf8]),
        <<"tab">> => <<"sys">>,
        <<"system">> => 1,
        <<"title">> => ec_cnv:to_binary(Title),
        <<"remark">> => ec_cnv:to_binary(Remark)
    }).


save(Key, Data) ->
    % ?LOG([Key, Val, Tab]),
    Now = imboy_dt:utc(millisecond),
    Now2 = integer_to_binary(Now),
    Where =  <<"key = '", Key/binary, "'">>,
    Field = <<"count(*) as count">>,
    case imboy_db:pluck(<<"config">>, Where, Field, 0) of
        0 ->
            imboy_db:insert_into(<<"config">>, Data#{
                <<"key">> => Key,
                <<"updated_at">> => 0,
                <<"created_at">> => Now2
            }, <<>>);
        _ ->
            imboy_db:update(<<"config">>
                , <<"key = '", Key/binary, "'">>
                , Data#{<<"updated_at">> => Now2}
            )
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
    Where = <<"key = '", Key/binary, "'">>,
    imboy_db:pluck(<<"config">>, Where, <<"value">>, <<>>);
do_aes_encrypt(Key, Val) ->
    AesKey = config_ds:env(postgre_aes_key),
    Where = <<"key = '", Key/binary, "'">>,
    Val1 = base64:encode(Val),
    Set = <<"value = 'aes_cbc_' || encode(encrypt('",
        Val1/binary, "', '",
        AesKey/binary,
        "', 'aes-cbc/pad:pkcs'), 'base64')">>,
    imboy_db:update(<<"config">>, Where, Set),
    imboy_db:pluck(<<"config">>, Where, <<"value">>, <<>>).
