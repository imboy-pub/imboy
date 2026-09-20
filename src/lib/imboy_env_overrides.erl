%%% @doc BE-W01：装配类 env 注入（direct 值 / _FILE 0600 / fail-closed）。
%%%
%%% 从 imboy_env 拆出（评审指出 imboy_env 1000 行超「文件 < 800 行」红线，
%%% 本段自包含：CS widget 三件装配键 + CORS 三面 origin 名单）。职责口径：
%%%   * 读 OS env（含 secret 类的 `_FILE` 文件变体），覆写 application env；
%%%   * 仍在启动链（imboy_env:override_from_env/0 → 本模块）调用一次，
%%%     请求路径零 I/O、零解析；
%%%   * 企业密钥环的 _FILE 装载在 eb_keyring_file（enterprise 特性模块，
%%%     判据/装载分离裁决 F6，不与本 lib 模块混同）。
%%%
%%% 消费的 env 键（→ {imboy, …}）：
%%%   IMBOY_CS_WIDGET_SUBJECT_KEY[_FILE]      -> cs_widget_subject_key
%%%   IMBOY_CS_WIDGET_IDENTITY_KEYS[_FILE]    -> cs_widget_identity_keys（"1:k1,2:k2"）
%%%   IMBOY_CS_WIDGET_INTAKE_BUSINESS_IDENTITY_ID -> cs_widget_intake_business_identity_id
%%%   IMBOY_CORS_WIDGET_ORIGINS               -> cors_widget_origins（逗号分隔）
%%%   IMBOY_CORS_ADMIN_ORIGINS                -> cors_admin_origins（逗号分隔）
-module(imboy_env_overrides).

-export([
    override_cs_widget/0,
    override_cors_face_origins/0
]).

-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% BE-W01 A01：CS widget 装配覆盖（direct 值 / _FILE 0600 / fail-closed）
%% ===================================================================

%% @doc 覆盖 customer_service widget 的三件装配键：
%%
%%   * `IMBOY_CS_WIDGET_SUBJECT_KEY` → `{imboy, cs_widget_subject_key}`（binary）
%%   * `IMBOY_CS_WIDGET_IDENTITY_KEYS` → `{imboy, cs_widget_identity_keys}`
%%     （"1:k1,2:k2" 严格解析为 [{Version, Key}]；非法条目启动期报错——
%%     消费方 `cs_identity_assertion` 对未知条目静默忽略，装配层必须把住
%%     fail-closed 关，不能让拼写错误静默变成"密钥没配上"）
%%   * `IMBOY_CS_WIDGET_INTAKE_BUSINESS_IDENTITY_ID` → integer
%%
%% secret 类键（subject_key / identity_keys）同时支持 `_FILE` 变体
%% （`IMBOY_CS_WIDGET_SUBJECT_KEY_FILE` 等）：读文件内容（剥除首尾空白）为值。
%% 文件权限必须 0600（file:read_file_info 校验）；读取失败/权限过宽/
%% direct 与 _FILE 双源同给一律 erlang:error fail-fast（启动拒绝，值与
%% 文件内容不进任何日志或错误项）。
-spec override_cs_widget() -> ok.
override_cs_widget() ->
    SubjectKey = secret_env_value(
        "IMBOY_CS_WIDGET_SUBJECT_KEY", "IMBOY_CS_WIDGET_SUBJECT_KEY_FILE"
    ),
    apply_binary_value(SubjectKey, cs_widget_subject_key),

    IdentityKeys = secret_env_value(
        "IMBOY_CS_WIDGET_IDENTITY_KEYS", "IMBOY_CS_WIDGET_IDENTITY_KEYS_FILE"
    ),
    apply_identity_keys_value(IdentityKeys),

    ok = override_positive_integer_key(
        "IMBOY_CS_WIDGET_INTAKE_BUSINESS_IDENTITY_ID",
        cs_widget_intake_business_identity_id
    ),
    ok.

%% @doc direct 值与 _FILE 变体的统一读取：双源同给报歧义错；单源 direct
%% 返回 binary；单源 _FILE 读文件（0600 + 内容剥首尾空白）；都缺省返回
%% undefined（不覆盖，sys.config 原值生效）。
-spec secret_env_value(string(), string()) -> {ok, binary()} | undefined.
secret_env_value(DirectEnv, FileEnv) ->
    Direct = os:getenv(DirectEnv),
    File = os:getenv(FileEnv),
    case {non_empty(Direct), non_empty(File)} of
        {true, true} ->
            erlang:error({conflicting_env, DirectEnv, FileEnv});
        {true, false} ->
            {ok, unicode:characters_to_binary(string:trim(Direct))};
        {false, true} ->
            read_secret_file(FileEnv, File);
        {false, false} ->
            undefined
    end.

-spec read_secret_file(string(), string()) -> {ok, binary()}.
read_secret_file(FileEnv, Path) ->
    case file:read_file_info(Path) of
        %% mode 含文件类型位（regular = 8#100000，macOS 实测 0o100600），
        %% 权限判定只看低 9 位且必须精确 0600——group/other 任何读位即拒。
        {ok, #file_info{mode = Mode}} when (Mode band 8#777) =:= 8#600 ->
            case file:read_file(Path) of
                {ok, Bin} ->
                    {ok, unicode:characters_to_binary(string:trim(Bin))};
                {error, Reason} ->
                    erlang:error({secret_file_unreadable, FileEnv, Reason})
            end;
        {ok, #file_info{mode = Mode}} ->
            erlang:error({secret_file_permissions, FileEnv, Mode});
        {error, Reason} ->
            erlang:error({secret_file_unreadable, FileEnv, Reason})
    end.

apply_binary_value(undefined, _AppKey) ->
    ok;
apply_binary_value({ok, Bin}, AppKey) ->
    application:set_env(imboy, AppKey, Bin),
    ok.

%% "1:k1,2:k2" → [{1, <<"k1">>}, {2, <<"k2">>}]（严格：任一条目非法即
%% {invalid_env, _, _} fail-fast；空串按未设置处理）。
apply_identity_keys_value(undefined) ->
    ok;
apply_identity_keys_value({ok, Bin}) when Bin =:= <<>> ->
    ok;
apply_identity_keys_value({ok, Bin}) ->
    Pairs = binary:split(Bin, <<",">>, [global, trim_all]),
    application:set_env(imboy, cs_widget_identity_keys, identity_key_pairs(Pairs, [])),
    ok.

identity_key_pairs([], Acc) ->
    lists:reverse(Acc);
identity_key_pairs([Pair | Rest], Acc) ->
    case binary:split(Pair, <<":">>) of
        [VBin, Key] when Key =/= <<>> ->
            try binary_to_integer(VBin) of
                V when V > 0 ->
                    identity_key_pairs(Rest, [{V, Key} | Acc]);
                _ ->
                    erlang:error({invalid_env, "IMBOY_CS_WIDGET_IDENTITY_KEYS", Pair})
            catch
                _:_ ->
                    erlang:error({invalid_env, "IMBOY_CS_WIDGET_IDENTITY_KEYS", Pair})
            end;
        _ ->
            erlang:error({invalid_env, "IMBOY_CS_WIDGET_IDENTITY_KEYS", Pair})
    end.

%% @doc 正整数键覆盖（intake identity 覆写是 TSID，非 secret，无 _FILE 变体）。
-spec override_positive_integer_key(string(), atom()) -> ok.
override_positive_integer_key(EnvVar, AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            ok;
        Value when is_list(Value), length(Value) > 0 ->
            try list_to_integer(string:trim(Value)) of
                N when N > 0 ->
                    application:set_env(imboy, AppKey, N),
                    ok;
                _ ->
                    erlang:error({invalid_env, EnvVar, Value})
            catch
                _:_ ->
                    erlang:error({invalid_env, EnvVar, Value})
            end;
        _ ->
            ok
    end.

%% @doc CORS 分面 origin 名单覆盖（逗号分隔，空段跳过；空名单不覆盖——
%% 避免手误清空把面锁死成"全部预检 403"）。face 面的 fail-closed 语义在
%% cors_middleware：名单为空 = 该面无跨域放行（与未配置前行为一致）。
-spec override_cors_face_origins() -> ok.
override_cors_face_origins() ->
    ok = override_origin_list_key("IMBOY_CORS_WIDGET_ORIGINS", cors_widget_origins),
    ok = override_origin_list_key("IMBOY_CORS_ADMIN_ORIGINS", cors_admin_origins).

override_origin_list_key(EnvVar, AppKey) ->
    case os:getenv(EnvVar) of
        Value when is_list(Value), length(Value) > 0 ->
            Origins = [
                unicode:characters_to_binary(string:trim(P))
             || P <- string:split(Value, ",", all),
                string:trim(P) =/= ""
            ],
            case Origins of
                [] ->
                    ok;
                _ ->
                    application:set_env(imboy, AppKey, Origins),
                    ok
            end;
        _ ->
            ok
    end.

non_empty(false) ->
    false;
non_empty(Value) when is_list(Value) ->
    string:trim(Value) =/= "";
non_empty(_) ->
    false.
