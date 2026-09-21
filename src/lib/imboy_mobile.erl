-module(imboy_mobile).

%% 手机号归一化与脱敏（GZAPP-06 / D11-D13 手机号合规口径）。
%%
%% 硬边界（任务卡 GZAPP-06）：
%%   * mobile 是业务必需明文（owner_activation_invite.mobile 落库、重发/
%%     换 Owner 按号定位），但所有日志 / 审计（admin_operation_logs）/
%%     错误消息 / 测试快照输出必须经 mask/1 脱敏（前3后4）；
%%   * 本模块是全仓唯一手机号脱敏出口——新增日志点必须经此，禁止手拼。
%%
%% 归一化口径（与 imboy_sms:filter_mobile/1 的 +86 剥离一致并收紧）：
%%   去首尾空白 → 去内部空白/连字符/圆括号 → 剥 +86 前缀；
%%   合法形状 = 纯数字且 5..20 位（容纳国内 11 位与国际号码，fail-closed）。

-export([normalize/1, mask/1, valid/1]).

-define(MAX_LEN, 20).
-define(MIN_LEN, 5).

%% @doc 归一化手机号；非法形状（含非数字、长度越界）返回 {error, invalid}。
%% 归一化口径（与 imboy_sms:filter_mobile/1 的 +86 剥离一致并收紧）：
%%   去首尾空白 → 去内部空白/连字符/圆括号 → 剥国际前缀「+」（随后按
%%   「86 + 恰 11 位国内号」形状剥中国区号，避免误剥 85x 国际号）；
%%   合法形状 = 纯数字且 5..20 位（fail-closed）。
%% 调用方必须以错误消息文案告知（错误消息本身严禁携带原始手机号）。
-spec normalize(binary()) -> {ok, binary()} | {error, invalid}.
normalize(Mobile0) when is_binary(Mobile0) ->
    Trimmed = string:trim(Mobile0),
    NoSep = strip_separators(Trimmed, <<>>),
    NoPlus = strip_plus(NoSep),
    NoCc = strip_country_code(NoPlus),
    case valid_shape(NoCc) of
        true -> {ok, NoCc};
        false -> {error, invalid}
    end;
normalize(_) ->
    {error, invalid}.

%% @doc 手机号脱敏：前 3 后 4，中间以 **** 占位（测试快照/日志/审计唯一合法形态）。
%% 长度不足 8 位的短号一律收敛为全掩码（防超短号整段泄露）。
-spec mask(binary()) -> binary().
mask(Mobile) when is_binary(Mobile), byte_size(Mobile) >= 8 ->
    Head = binary:part(Mobile, 0, 3),
    Tail = binary:part(Mobile, byte_size(Mobile) - 4, 4),
    <<Head/binary, "****", Tail/binary>>;
mask(_) ->
    <<"****"/utf8>>.

%% @doc 归一化后的形状合法性（纯数字 5..20 位）。
-spec valid(binary()) -> boolean().
valid(Mobile) when is_binary(Mobile) ->
    valid_shape(Mobile);
valid(_) ->
    false.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

strip_separators(<<C, Rest/binary>>, Acc) when
    C =:= $\s; C =:= $-; C =:= $(; C =:= $)
->
    strip_separators(Rest, Acc);
strip_separators(<<C, Rest/binary>>, Acc) ->
    strip_separators(Rest, <<Acc/binary, C>>);
strip_separators(<<>>, Acc) ->
    Acc.

strip_plus(<<$+, Rest/binary>>) ->
    Rest;
strip_plus(Bin) ->
    Bin.

%% 仅当「86 + 恰 11 位数字」的国内形状才剥 86，避免误剥 85x 国际号。
strip_country_code(<<"86", Tail/binary>>) when byte_size(Tail) =:= 11 ->
    Tail;
strip_country_code(Bin) ->
    Bin.

valid_shape(<<>>) ->
    false;
valid_shape(Bin) when byte_size(Bin) >= ?MIN_LEN, byte_size(Bin) =< ?MAX_LEN ->
    is_all_digits(Bin);
valid_shape(_) ->
    false.

is_all_digits(<<>>) ->
    true;
is_all_digits(<<C, Rest/binary>>) when C >= $0, C =< $9 ->
    is_all_digits(Rest);
is_all_digits(_) ->
    false.
