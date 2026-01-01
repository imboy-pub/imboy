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

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


% config_ds:env(test).
% config_ds:env(lager, colors, undefined).
-spec env(any()) -> any().
env(Attr) ->
    env(Attr, undefined).


-spec env(any(), any()) -> any().
env(Attr, Def) ->
    env(imboy, Attr, Def).


-spec env(any(), any(), any()) -> any().
env(App, [Attr], Def) ->
    %% Single-element list case
    env(App, Attr, Def);

env(App, [Key | SubKeys], Def) ->
    %% Multi-level key access
    case env(App, Key, undefined) of
        ConfigList when is_list(ConfigList) ->
            get_nested_value(SubKeys, ConfigList, Def);
        _ ->
            Def
    end;

env(App, Attr, Def) ->
    case application:get_env(App, Attr) of
        {ok, Value} ->
            Value;
        _ ->
            Def
    end.


% config_ds:reload().
-spec reload() -> any().
reload() ->
    Path = config_file(),
    reload(Path).


%% 重新加载 sys.config 配置
%% [config_ds:env(test), config_ds:local_reload(), config_ds:env(test)].
-spec local_reload() -> ok.
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
-spec get(any()) -> any().
get(Key) ->
    get(Key, <<>>).

-spec get(any(), any()) -> any().
get(Key, Defalut) ->
    Key2 = ec_cnv:to_binary(Key),
    % Val = imboy_hasher:decoded_field(<<"value">>),
    % Res = imboy_pg:pluck_value(<<"config">>, Val, #{key => Key2}, #{}, Defalut),
    % imboy_cnv:json_maybe(Res).
    Fun = fun() ->
        Val = imboy_hasher:decoded_field(<<"value">>),
        % 使用安全的参数化查询，避免SQL注入
        Res = imboy_pg:pluck_value(<<"config">>, Val, #{key => Key2}, #{}, Defalut),
        imboy_cnv:json_maybe(Res)
    end,
    % 缓存10天
    imboy_cache:memo(Fun, cache_key(Key2), 864000).

% config_ds:set(<<"dbc">>, <<"ddd2">>).
% config_ds:get(<<"dbc">>).
-spec set(any(), any()) -> any().
set(Key, Val) ->
    set(Key, Val, <<>>, <<>>).

-spec set(any(), any(), any(), any()) -> any().
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


-spec save(any(), any()) -> ok.
save(Key, Data) ->
    Now = imboy_dt:now(),
    % 使用安全的参数化查询，避免SQL注入
    Field = <<"count(*) as count">>,
    case imboy_pg:pluck(<<"config">>, Field, #{key => Key}, #{}) of
        {ok, 0} ->
            imboy_pg:insert(<<"config">>, Data#{
                <<"key">> => Key,
                <<"updated_at">> => null,
                <<"created_at">> => Now
            });
        {ok, _Count} ->
            imboy_pg:update(<<"config">>,
                Data#{<<"updated_at">> => Now},
                <<"key = $1">>, [Key])
    end,
    imboy_cache:flush(cache_key(Key)),
    aes_encrypt(Key).


% config_ds:aes_encrypt(<<"login_rsa_pub_key">>).
% config_ds:get(<<"login_rsa_pub_key">>).

% config_ds:get(<<"login_rsa_priv_key">>).
% config_ds:aes_encrypt(<<"login_rsa_priv_key">>).


% config_ds:aes_encrypt(<<"login_pwd_rsa_encrypt">>).
% config_ds:aes_encrypt(<<"site_name">>).
-spec aes_encrypt(any()) -> any().
aes_encrypt(Key) when is_list(Key) ->
    aes_encrypt(list_to_binary(Key));
aes_encrypt(Key) ->
    % 使用安全的参数化查询，避免SQL注入
    Val = imboy_pg:pluck_value(<<"config">>, <<"value">>, #{key => Key}, #{}, <<>>),
    do_aes_encrypt(Key, Val).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


cache_key(K) ->
    {config4, K}.


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
    % 已经加密，直接返回
    imboy_pg:pluck_value(<<"config">>, <<"value">>, #{key => Key}, #{}, <<>>);
do_aes_encrypt(Key, Val) ->
    AesKey = config_ds:env(postgre_aes_key),
    % 与 imboy_hasher:encoded_val 保持一致：先 base64 编码再加密
    % encrypt() 需要 bytea 类型，所以需要 ::bytea 转换
    Sql = <<"UPDATE config SET value = 'aes_cbc_' || encode(encrypt(encode($1, 'base64')::bytea, $2, 'aes-cbc/pad:pkcs'), 'base64') WHERE key = $3">>,
    case imboy_pg:execute(Sql, [Val, AesKey, Key]) of
        {ok, _} ->
            imboy_pg:pluck_value(<<"config">>, <<"value">>, #{key => Key}, #{}, <<>>);
        {error, Reason} ->
            ?LOG_ERROR("Failed to encrypt config value for key ~p: ~p", [Key, Reason]),
            <<>>
    end.


%% 辅助函数：递归获取嵌套值
%% Helper function with default value support
get_nested_value([], Value, _Def) -> Value;
get_nested_value([Key | Rest], ConfigList, Def) when is_list(ConfigList) ->
    case proplists:get_value(Key, ConfigList, Def) of
        Def when Rest =:= [] -> Def;
        Value -> get_nested_value(Rest, Value, Def)
    end;
get_nested_value(_, _, Def) -> Def.
