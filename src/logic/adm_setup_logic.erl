-module(adm_setup_logic).
%%%
%%% adm_setup_logic — 管理后台首启初始化向导（P0-5）
%%%
%%% 目标：消除默认 admin/admin888 硬编码风险，由运营方在首次部署后
%%%       通过向导创建超级管理员账号。
%%%
%%% 流程：
%%%   1. 前端访问 /setup/status → is_initialized/0 判断是否已完成
%%%   2. 未完成则引导到 /setup 页面，调用 do_init/1 创建超级管理员
%%%   3. 成功后写入 config(adm.setup.completed_at) 作为永久标记
%%%
%%% 幂等保护：
%%%   - 空库 + 无 flag → 允许初始化
%%%   - 有 flag → 拒绝 (ERR_SETUP_ALREADY_COMPLETED)
%%%   - 无 flag 但库中已有 adm_user → 视为 legacy，亦拒绝
%%%%

-export([is_initialized/0, do_init/1]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(SETUP_FLAG_KEY, <<"adm.setup.completed_at">>).
-define(SUPER_ADMIN_ROLE_ID, 1).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 判断系统是否已完成首启初始化
%% 已完成：已写入 flag 或者库中已有 adm_user (legacy 兼容)
-spec is_initialized() -> boolean().
is_initialized() ->
    case config_ds:get(?SETUP_FLAG_KEY) of
        Flag when is_binary(Flag), byte_size(Flag) > 0 ->
            true;
        _ ->
            case adm_user_ds:count() of
                {ok, N} when is_integer(N), N > 0 -> true;
                _ -> false
            end
    end.

%% @doc 执行首启初始化 — 创建超级管理员账号
%% @param Params #{<<"account">> := binary(),
%%                 <<"password">> := binary(),
%%                 <<"nickname">> := binary()}
%% @return {ok, AdminId} | {error, ErrCode}
-spec do_init(map()) -> {ok, integer()} | {error, integer() | any()}.
do_init(Params) ->
    case is_initialized() of
        true ->
            {error, ?ERR_SETUP_ALREADY_COMPLETED};
        false ->
            do_init_1(Params)
    end.

%% ===================================================================
%% Internal
%% ===================================================================

do_init_1(Params) ->
    Account = maps:get(<<"account">>, Params, <<>>),
    Pwd = maps:get(<<"password">>, Params, <<>>),
    Nickname = maps:get(<<"nickname">>, Params, <<>>),
    case validate(Account, Pwd, Nickname) of
        {ok, AccountType} ->
            create_super_admin(AccountType, Account, Pwd, Nickname);
        {error, _} = Err ->
            Err
    end.

%% 账号 + 密码 + 昵称 基础校验
validate(Account, Pwd, Nickname) ->
    case validate_account(Account) of
        {ok, Type} ->
            case validate_password_strength(Pwd) of
                ok ->
                    case validate_nickname(Nickname) of
                        ok -> {ok, Type};
                        {error, _} = E -> E
                    end;
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end.

validate_account(Account) when is_binary(Account), byte_size(Account) > 0 ->
    case elib_type:is_mobile(Account) of
        true ->
            {ok, mobile};
        false ->
            case elib_type:is_email(Account) of
                true -> {ok, email};
                false -> {error, ?ERR_SETUP_INVALID_PARAMS}
            end
    end;
validate_account(_) ->
    {error, ?ERR_SETUP_INVALID_PARAMS}.

%% 密码强度：8-64 字符 + 至少含字母和数字
validate_password_strength(Pwd) when is_binary(Pwd) ->
    Len = byte_size(Pwd),
    case Len >= 8 andalso Len =< 64 of
        false ->
            {error, ?ERR_SETUP_INVALID_PARAMS};
        true ->
            HasLetter = has_letter(Pwd),
            HasDigit = has_digit(Pwd),
            case HasLetter andalso HasDigit of
                true -> ok;
                false -> {error, ?ERR_SETUP_INVALID_PARAMS}
            end
    end;
validate_password_strength(_) ->
    {error, ?ERR_SETUP_INVALID_PARAMS}.

validate_nickname(Nickname) when is_binary(Nickname), byte_size(Nickname) > 0 ->
    case byte_size(Nickname) =< 128 of
        true -> ok;
        false -> {error, ?ERR_SETUP_INVALID_PARAMS}
    end;
validate_nickname(_) ->
    {error, ?ERR_SETUP_INVALID_PARAMS}.

has_letter(<<C, _/binary>>) when (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) ->
    true;
has_letter(<<_, Rest/binary>>) ->
    has_letter(Rest);
has_letter(<<>>) ->
    false.

has_digit(<<C, _/binary>>) when C >= $0, C =< $9 ->
    true;
has_digit(<<_, Rest/binary>>) ->
    has_digit(Rest);
has_digit(<<>>) ->
    false.

%% 创建超级管理员 + 写入 setup 完成标记
create_super_admin(AccountType, Account, Pwd, Nickname) ->
    PwdHash = elib_password:generate(Pwd),
    Now = elib_dt:now(),
    Row0 = #{
        <<"password">> => PwdHash,
        <<"nickname">> => Nickname,
        <<"status">> => 1,
        %% adm_user.role_id 是 bigint[]（多角色），标量 1 会类型不匹配致
        %% INSERT 报错并拖垮事务连接（首启向导从未真实走通的原因之一）。
        <<"role_id">> => [?SUPER_ADMIN_ROLE_ID],
        <<"created_at">> => Now
    },
    Row =
        case AccountType of
            mobile ->
                Row0#{<<"account">> => Account, <<"mobile">> => Account};
            email ->
                Row0#{<<"account">> => Account, <<"email">> => Account}
        end,
    case adm_user_ds:save(Row) of
        {ok, Id} ->
            _ = config_ds:set(?SETUP_FLAG_KEY, Now),
            {ok, Id};
        {error, _} = Err ->
            Err
    end.
