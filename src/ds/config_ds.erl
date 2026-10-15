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
-spec env(atom() | binary() | [atom() | binary()]) -> any().
env(Attr) ->
    env(Attr, undefined).

-spec env(atom() | binary() | [atom() | binary()], term()) -> any().
env(Attr, Def) ->
    env(imboy, Attr, Def).

-spec env(atom(), atom() | binary() | [atom() | binary()], term()) -> any().
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
            BinK = elib_cnv:safe_to_binary(K),
            BinV = elib_cnv:safe_to_binary(V),
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
    Fenced = imboy_cache_policy:is_fenced(CacheKey),
    Fun = fun() ->
        case pluck_decrypted_value(Key2, Default) of
            Default ->
                imboy_cache:uncached(Default);
            B ->
                Value = jsone:decode(B, [{object_format, map}]),
                case Fenced of
                    true -> Value;
                    false -> imboy_cache:uncached(Value)
                end
        end
    end,
    MemoKey =
        case Fenced of
            true -> CacheKey;
            false -> {config5, Key2, uncached_v1}
        end,
    imboy_cache:memo(Fun, MemoKey, 864000).

% config_ds:set(<<"dbc">>, <<"ddd2">>).
% config_ds:get(<<"dbc">>).
-spec set(binary(), term()) -> ok.
set(Key, Val) ->
    set(Key, Val, <<>>, <<>>).

-spec set(binary(), term(), binary(), binary()) -> ok.
set(Key, Val, Title, Remark) ->
    Key2 = ec_cnv:to_binary(Key),
    case
        save(
            Key2,
            % save 在真正写入前完成加密，不落明文中间态
            #{
                <<"value">> => jsone:encode(Val, [native_utf8]),
                <<"tab">> => <<"sys">>,
                <<"system">> => true,
                <<"title">> => ec_cnv:to_binary(Title),
                <<"remark">> => ec_cnv:to_binary(Remark)
            }
        )
    of
        ok -> ok;
        {error, _Reason} -> erlang:error({config_write_failed, authority_write_failed})
    end.

-spec save(binary(), map()) -> ok | {error, term()}.
save(Key, Data) ->
    case encrypt_config_value(maps:get(<<"value">>, Data)) of
        {ok, Cipher} ->
            Now = elib_dt:now(),
            Result =
                case elib_pg:pluck(<<"config">>, <<"count(*) as count">>, #{key => Key}, #{}) of
                    {ok, 0} ->
                        elib_pg:insert(<<"config">>, Data#{
                            <<"value">> => Cipher,
                            <<"key">> => Key,
                            <<"updated_at">> => null,
                            <<"created_at">> => Now
                        });
                    {ok, Count} when is_integer(Count), Count > 0 ->
                        elib_pg:update(
                            <<"config">>,
                            Data#{
                                <<"value">> => Cipher, <<"updated_at">> => Now
                            },
                            <<"key = $1">>,
                            [Key]
                        );
                    {error, _} = Error ->
                        Error
                end,
            case Result of
                {ok, Count1} when is_integer(Count1), Count1 > 0 ->
                    %% DB 写入成功即成功；缓存同步失效尽力而为——
                    %% sync 进程不可用时 invalidate 返回 {error, sync_unavailable}，
                    %% 不得反转为写失败（f93a842a 引入的回归）。
                    _ = imboy_cache:invalidate(cache_key(Key)),
                    ok;
                {ok, 0} ->
                    {error, config_not_written};
                {error, _} = Error1 ->
                    Error1
            end;
        {error, _} = Error ->
            Error
    end.

% config_ds:aes_encrypt(<<"login_rsa_pub_key">>).
% config_ds:get(<<"login_rsa_pub_key">>).

% config_ds:get(<<"login_rsa_priv_key">>).
% config_ds:aes_encrypt(<<"login_rsa_priv_key">>).

% config_ds:aes_encrypt(<<"login_pwd_rsa_encrypt">>).
% config_ds:aes_encrypt(<<"site_name">>).
-spec aes_encrypt(binary() | list()) -> binary() | {error, term()}.
aes_encrypt(Key) when is_list(Key) -> aes_encrypt(list_to_binary(Key));
aes_encrypt(Key) ->
    case elib_pg:query(<<"SELECT value FROM public.config WHERE key = $1 LIMIT 1">>, [Key]) of
        {ok, [#{<<"value">> := Val}]} -> do_aes_encrypt(Key, Val);
        {ok, []} -> {error, config_not_found};
        {error, _} = Error -> Error
    end.

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

%% @private 读取并解密 config.value（A-07，审计缺陷 #26）
%%
%% 主密钥走绑定参数 $1，不再由 elib_hasher:decoded_field/1 内联进 SQL 列表达式 ——
%% 后者会让密钥进入慢查询日志、pg_stat_statements 归一化文本与异常堆栈。
%%
%% 说明：config.value 走的是 pgcrypto 真加密（见 do_aes_encrypt/2 的 $2 绑定参数），
%% 与 user_collect.info 的历史脏数据不同，这里的密文有效，只需把密钥挪出 SQL 文本。
-spec pluck_decrypted_value(binary(), D) -> binary() | D when D :: term().
pluck_decrypted_value(Key, Default) ->
    Sql = <<
        "SELECT decode(encode(decrypt(decode(replace(value, 'aes_cbc_', ''), 'base64'), "
        "$1, 'aes-cbc/pad:pkcs'), 'escape'), 'base64') AS value "
        "FROM public.config WHERE key = $2 LIMIT 1"
    >>,
    case elib_pg:query(Sql, [env(postgre_aes_key), Key]) of
        {ok, [#{<<"value">> := Val} | _]} when is_binary(Val) ->
            Val;
        _ ->
            Default
    end.

%% 审计 #26 其余两点在此处的结论：
%% - pgcrypto encrypt/3 固定全零 IV → 同一明文密文确定性。config 表是运维配置、
%%   非用户数据，且 value 在写入前已 base64，未做改造；新代码一律改用
%%   elib_cipher:aes_gcm_encrypt/2（随机 IV + AEAD），见 elib_hasher:encoded_val/1。
%% - 密钥在 do_aes_encrypt/2 里走的是 $2 绑定参数，从未进入 SQL 文本，无泄漏。
%% 在写入前生成最终密文；加密失败不先落明文，也不广播失效。
encrypt_config_value(<<"aes_cbc_", _/binary>> = Cipher) ->
    {ok, Cipher};
encrypt_config_value(Val) ->
    Sql = <<
        "SELECT 'aes_cbc_' || encode(encrypt(encode($1::bytea, 'base64')::bytea, "
        "$2, 'aes-cbc/pad:pkcs'), 'base64') AS value"
    >>,
    case elib_pg:query(Sql, [Val, config_ds:env(postgre_aes_key)]) of
        {ok, [#{<<"value">> := Cipher}]} when is_binary(Cipher) -> {ok, Cipher};
        {error, _} = Error -> Error;
        _Other -> {error, invalid_encryption_result}
    end.

do_aes_encrypt(Key, Val) ->
    case encrypt_config_value(Val) of
        {ok, Cipher} ->
            case elib_pg:update(<<"config">>, #{<<"value">> => Cipher}, <<"key = $1">>, [Key]) of
                {ok, Count} when Count > 0 ->
                    ok = imboy_cache:invalidate(cache_key(Key)),
                    Cipher;
                {ok, 0} ->
                    {error, config_not_written};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
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
