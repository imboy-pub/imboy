%%% @doc 用户资料校验策略 / User profile validation policy (pure functions)
%%%
%%% DDD 充血改造 Phase 3 / T3.1：从 user_logic:update/3（资料更新分发）
%%% 抽离「邮件格式 / 性别枚举 / 允许搜索枚举 / 字段白名单」四类**纯决策**，
%%% 使其零副作用、可 eunit 独测；email 占用查询与字段落库属 I/O，保留在
%%% logic 外壳（T3.2 退化外壳接线）。
%%%
%%% Pure validation core extracted from user_logic:update/3. No DB, no
%%% process messaging — validate_update/2 returns a normalized command the
%%% imperative shell executes ({set_field|set_gender|set_allow_search|
%%% check_email}), or a domain reject the shell maps to the wire error.
%%% Zero side effects -> eunit without mocks.
%%%
%%% 渐进策略（NOT Building）：本 task 仅新增纯领域层，不改既有 user_logic；
%%% update/3 退化外壳调用本模块留 T3.2。
%%%
%%% 语义对齐 user_logic:update/3 子句顺序（行为不变是回归契约）：
%%%   - email 仅当值为 binary 才进入格式校验；非 binary 落白名单兜底
%%%     unsupported_field（镜像原 `when is_binary(Val)` 守卫）；
%%%   - gender / allow_search 非法值落各自专属 reject（非 unsupported），
%%%     镜像原显式 catch-all 子句；
%%%   - 透传字段（sign/nickname/avatar/region/birthday）无校验原样落库。
-module(user_agg).

-export([validate_update/2]).
-export([is_valid_email/1, validate_gender/1, validate_allow_search/1, validate_bool/1]).

-export_type([update_cmd/0, reject/0]).

%% 校验通过后交给外壳执行的归一化命令。
-type update_cmd() ::
    {set_field, Field :: binary(), Val :: binary()}
    | {set_gender, 1..3}
    | {set_allow_search, 1..2}
    | {check_email, Email :: binary()}
    | {set_setting, Key :: binary(), boolean()}
    | {set_online_visibility, boolean()}
    | {set_nearby_visible, boolean()}.

%% 校验拒绝原因（外壳映射为 {error, {1, <<>>, Msg}} 线格式）。
-type reject() ::
    bad_email_format
    | bad_gender
    | bad_allow_search
    | bad_bool
    | unsupported_field.

%% 透传字段白名单（无格式约束，原样落库）。
-define(PASSTHROUGH_FIELDS, [
    <<"sign">>,
    <<"nickname">>,
    <<"avatar">>,
    <<"region">>,
    <<"birthday">>
]).

%% ===================================================================
%% 校验分发
%% ===================================================================

%% @doc 资料字段更新校验。返回归一化命令或拒绝原因（纯决策，无 I/O）。
%% 子句顺序逐字镜像 user_logic:update/3。
-spec validate_update(Field :: binary(), Val :: term()) ->
    {ok, update_cmd()} | {error, reject()}.
validate_update(<<"email">>, Val) when is_binary(Val) ->
    case is_valid_email(Val) of
        true ->
            {ok, {check_email, Val}};
        false ->
            {error, bad_email_format}
    end;
validate_update(<<"gender">>, Val) ->
    case validate_gender(Val) of
        {ok, N} ->
            {ok, {set_gender, N}};
        error ->
            {error, bad_gender}
    end;
validate_update(<<"allow_search">>, Val) ->
    case validate_allow_search(Val) of
        {ok, N} ->
            {ok, {set_allow_search, N}};
        error ->
            {error, bad_allow_search}
    end;
%% 隐私布尔开关（QA #19）：handler 已 ec_cnv:to_binary，布尔到达时为
%% <<"true">>/<<"false">>；落 user_setting.setting JSONB（前端登录回包
%% 按同名 key 回显）。show_online_status / allow_nearby_visible 带读侧
%% 生效副作用，归一为专属命令由外壳执行。
validate_update(<<"allow_add_by_phone">>, Val) ->
    case validate_bool(Val) of
        {ok, B} -> {ok, {set_setting, <<"allow_add_by_phone">>, B}};
        error -> {error, bad_bool}
    end;
validate_update(<<"allow_add_by_qr">>, Val) ->
    case validate_bool(Val) of
        {ok, B} -> {ok, {set_setting, <<"allow_add_by_qr">>, B}};
        error -> {error, bad_bool}
    end;
validate_update(<<"show_online_status">>, Val) ->
    case validate_bool(Val) of
        {ok, B} -> {ok, {set_online_visibility, B}};
        error -> {error, bad_bool}
    end;
validate_update(<<"allow_nearby_visible">>, Val) ->
    case validate_bool(Val) of
        {ok, B} -> {ok, {set_nearby_visible, B}};
        error -> {error, bad_bool}
    end;
validate_update(Field, Val) ->
    case lists:member(Field, ?PASSTHROUGH_FIELDS) of
        true ->
            {ok, {set_field, Field, Val}};
        false ->
            {error, unsupported_field}
    end.

%% ===================================================================
%% 纯谓词 / 归一化（可独立复用）
%% ===================================================================

%% @doc 邮箱格式校验。逐字保留原 `elib_type:is_email([Val])` 调用形态
%% （以单元素 list 作 iodata 传入正则）。
-spec is_valid_email(binary()) -> boolean().
is_valid_email(Val) ->
    elib_type:is_email([Val]).

%% @doc 性别枚举校验：1 男 / 2 女 / 3 保密 → {ok, N}；其余 error。
-spec validate_gender(term()) -> {ok, 1..3} | error.
validate_gender(<<"1">>) -> {ok, 1};
validate_gender(<<"2">>) -> {ok, 2};
validate_gender(<<"3">>) -> {ok, 3};
validate_gender(_) -> error.

%% @doc 允许搜索枚举校验：1 / 2 → {ok, N}；其余 error。
-spec validate_allow_search(term()) -> {ok, 1..2} | error.
validate_allow_search(<<"1">>) -> {ok, 1};
validate_allow_search(<<"2">>) -> {ok, 2};
validate_allow_search(_) -> error.

%% @doc 布尔开关校验：handler 层 ec_cnv:to_binary 后布尔为二进制形态。
-spec validate_bool(term()) -> {ok, boolean()} | error.
validate_bool(<<"true">>) -> {ok, true};
validate_bool(<<"false">>) -> {ok, false};
validate_bool(true) -> {ok, true};
validate_bool(false) -> {ok, false};
validate_bool(_) -> error.
