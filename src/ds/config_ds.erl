-module(config_ds).

%%%
% config 领域服务模块
% config domain service 缩写
%%%

-export([get/1, get/2]).
-export([set/2, set/4]).
-export([aes_encrypt/1]).
-export([env/1, env/2, env/3]).
-export([reload/0, local_reload/0]).
-export([ice_servers/0]).

-include_lib("kernel/include/logger.hrl").

-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

% config_ds:env(test).
% config_ds:env(lager, colors, undefined).
-spec env(atom() | binary()) -> any().
env(Attr) ->
    env(Attr, undefined).

-spec env(atom() | binary(), term()) -> any().
env(Attr, Def) ->
    env(imboy, Attr, Def).

-spec env(atom(), atom() | binary() | [binary()], term()) -> any().
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

%% @doc 获取 WebRTC ICE Server 配置列表
%% 从 sys.config 中读取 {imboy, [{ice_servers, [...]}]}
%% 返回值可直接序列化为 JSON 返回给客户端
-spec ice_servers() -> [map()].
ice_servers() ->
    case application:get_env(imboy, ice_servers) of
        {ok, Servers} when is_list(Servers) ->
            [normalize_ice_server(S) || S <- Servers];
        _ ->
            %% 默认 STUN 服务器
            [#{<<"urls">> => <<"stun:stun.l.google.com:19302">>}]
    end.

%% @doc 将 ICE server 配置中的 atom key 转为 binary key（用于 JSON 序列化）
normalize_ice_server(S) when is_map(S) ->
    maps:fold(
        fun(K, V, Acc) ->
            BinK = elib_cnv:to_binary(K),
            BinV = elib_cnv:to_binary(V),
            Acc#{BinK => BinV}
        end,
        #{},
        S
    );
normalize_ice_server(_) ->
    #{}.

% config_ds:reload().
-spec reload() -> ok.
reload() ->
    Path = config_file(),
    reload(Path).

%% 重新加载 sys.config 配置
%% [config_ds:env(test), config_ds:local_reload(), config_ds:env(test)].
-spec local_reload() -> ok.
local_reload() ->
    IMBoyEnv = binary_to_list(imboy_env:current()),
    From = code:root_dir() ++ "/../../config/sys." ++ IMBoyEnv ++ ".config",
    To = config_file(),
    % Res1 = file:delete(To),
    % elib_log:error("~p~n", [Res1]),
    ok = elib_log:info("~p~n", [#{from => From, to => To}]),
    _ = file:copy(From, To, infinity),
    % Res2 = file:copy(From, To, infinity),
    % elib_log:error("copy file res: ~p~n", [Res2]),
    reload(To),
    ok.

% config_ds:get(<<"site_name">>).
-spec get(binary()) -> any().
get(Key) ->
    get(Key, <<>>).

-spec get(binary(), term()) -> any().
get(Key, Default) ->
    Key2 = ec_cnv:to_binary(Key),
    CacheKey = cache_key(Key2),
    case imboy_cache:get(CacheKey) of
        {ok, Val} ->
            Val;
        undefined ->
            Val = elib_hasher:decoded_field(<<"value">>),
            % 使用安全的参数化查询，避免SQL注入
            case elib_pg:pluck_value(<<"config">>, Val, #{key => Key2}, #{}, Default) of
                Default ->
                    Default;
                B ->
                    Result = jsone:decode(B, [{object_format, map}]),
                    imboy_cache:set(CacheKey, Result, 864000),
                    Result
            end
    end.

% config_ds:set(<<"dbc">>, <<"ddd2">>).
% config_ds:get(<<"dbc">>).
-spec set(binary(), term()) -> ok.
set(Key, Val) ->
    set(Key, Val, <<>>, <<>>).

-spec set(binary(), term(), binary(), binary()) -> ok.
set(Key, Val, Title, Remark) ->
    Key2 = ec_cnv:to_binary(Key),
    imboy_cache:flush(cache_key(Key2)),
    save(
        Key2,
        % value 的值在 do_aes_encrypt/2 放里面处理加密，这里给明文
        #{
            <<"value">> => jsone:encode(Val, [native_utf8]),
            <<"tab">> => <<"sys">>,
            <<"system">> => true,
            <<"title">> => ec_cnv:to_binary(Title),
            <<"remark">> => ec_cnv:to_binary(Remark)
        }
    ).

-spec save(binary(), map()) -> ok.
save(Key, Data) ->
    Now = elib_dt:now(),
    % 使用安全的参数化查询，避免SQL注入
    Field = <<"count(*) as count">>,
    _ =
        case elib_pg:pluck(<<"config">>, Field, #{key => Key}, #{}) of
            {ok, 0} ->
                elib_pg:insert(
                    <<"config">>,
                    Data#{
                        <<"key">> => Key,
                        <<"updated_at">> => null,
                        <<"created_at">> => Now
                    }
                );
            {ok, _Count} ->
                elib_pg:update(<<"config">>, Data#{<<"updated_at">> => Now}, <<"key = $1">>, [Key])
        end,
    _ = imboy_cache:flush(cache_key(Key)),
    _ = aes_encrypt(Key),
    ok.

% config_ds:aes_encrypt(<<"login_rsa_pub_key">>).
% config_ds:get(<<"login_rsa_pub_key">>).

% config_ds:get(<<"login_rsa_priv_key">>).
% config_ds:aes_encrypt(<<"login_rsa_priv_key">>).

% config_ds:aes_encrypt(<<"login_pwd_rsa_encrypt">>).
% config_ds:aes_encrypt(<<"site_name">>).
-spec aes_encrypt(binary()) -> binary().
aes_encrypt(Key) when is_list(Key) ->
    aes_encrypt(list_to_binary(Key));
aes_encrypt(Key) ->
    % 使用安全的参数化查询，避免SQL注入
    Val = elib_pg:pluck_value(<<"config">>, <<"value">>, #{key => Key}, #{}, <<>>),
    do_aes_encrypt(Key, Val).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

cache_key(K) ->
    {config5, K}.

reload(Path) ->
    {ok, Items} = file:consult(Path),
    % elib_log:error("~p~n", [Items]),
    [_ = application:set_env(Conf) || Conf <- Items],
    ok.

% config_ds:config_file().
config_file() ->
    {imboy, _, Vsn} = lists:keyfind(imboy, 1, application:which_applications()),
    code:root_dir() ++ "/releases/" ++ Vsn ++ "/sys.config".

do_aes_encrypt(Key, <<"aes_cbc_", _Val/binary>>) ->
    % 已经加密，直接返回
    elib_pg:pluck_value(<<"config">>, <<"value">>, #{key => Key}, #{}, <<>>);
do_aes_encrypt(Key, Val) ->
    AesKey = config_ds:env(postgre_aes_key),
    % 与 elib_hasher:encoded_val 保持一致：先 base64 编码再加密
    % encrypt() 需要 bytea 类型，所以需要 ::bytea 转换
    Sql = <<
        "UPDATE config SET value = 'aes_cbc_' || encode(encrypt(encode($1, "
        "'base64')::bytea, $2, 'aes-cbc/pad:pkcs'), 'base64') WHERE "
        "key = $3"
    >>,
    case elib_pg:execute(Sql, [Val, AesKey, Key]) of
        {ok, _} ->
            elib_pg:pluck_value(<<"config">>, <<"value">>, #{key => Key}, #{}, <<>>);
        {error, Reason} ->
            ?LOG_ERROR("Failed to encrypt config value for key ~p: ~p", [Key, Reason]),
            <<>>
    end.

%% 辅助函数：递归获取嵌套值
%% Helper function with default value support
get_nested_value([], Value, _Def) ->
    Value;
get_nested_value([Key | Rest], ConfigList, Def) when is_list(ConfigList) ->
    case lists:keyfind(Key, 1, ConfigList) of
        false when Rest =:= [] ->
            Def;
        false ->
            Def;
        {_, Value} ->
            get_nested_value(Rest, Value, Def)
    end;
get_nested_value(_, _, Def) ->
    Def.
