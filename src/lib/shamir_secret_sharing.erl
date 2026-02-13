-module(shamir_secret_sharing).
%%%===================================================================
%%% @doc
%%% shamir_secret_sharing - Shamir 秘密共享实现
%%%
%%% 使用大素数有限域实现 Shamir 秘密共享
%%% 与前端实现保持一致
%%%
%%% 功能：
%%% - 将秘密分割成 n 个分片
%%% - 需要至少 k 个分片才能重建秘密
%%%===================================================================

%% API 函数
-export([split_secret/3]).
-export([combine_shares/1]).
-export([create_shares/3]).

%% 类型定义
-type share() :: #{index => pos_integer(), x => pos_integer(), y => integer()}.
-type shares() :: [share()].

%%%===================================================================
%%% 常量
%%%===================================================================

-define(PRIME, 115792089237316195423570985008687907853269984665640564039457584007913129639747).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 分割秘密
%%
%% 使用 Shamir Secret Sharing 将秘密分割成 n 个分片
%% 需要至少 k 个分片才能重建秘密
%%
%% 参数:
%%   Secret: 秘密（二进制格式）
%%   N: 总分片数
%%   K: 恢复阈值
%%
%% 返回: 分片列表
%% @doc 分割秘密（别名函数）
%% @private 为了与 e2ee_social_ds 的调用保持兼容
-spec create_shares(binary(), pos_integer(), pos_integer()) -> shares().
create_shares(Secret, Threshold, TotalShards) ->
    split_secret(Secret, TotalShards, Threshold).

-spec split_secret(binary(), pos_integer(), pos_integer()) -> shares().
split_secret(Secret, N, K) when N > K, K >= 2 ->
    % 将二进制秘密转换为整数
    SecretInt = binary_to_int(Secret),

    % 生成随机多项式系数
    Coeffs = generate_coefficients(SecretInt, K),

    % 生成 N 个分片
    [create_share(I, Coeffs) || I <- lists:seq(1, N)];
split_secret(_Secret, N, K) when N =< K ->
    error({invalid_parameters, "N must be greater than K"});
split_secret(_Secret, _N, K) when K < 2 ->
    error({invalid_parameters, "K must be at least 2"}).

%% @doc 重组秘密
%%
%% 使用拉格朗日插值法从分片中重建秘密
%%
%% 参数:
%%   Shares: 分片列表（至少 K 个）
%%
%% 返回: 原始秘密（二进制格式）
-spec combine_shares(shares()) -> binary().
combine_shares(Shares) when length(Shares) >= 2 ->
    XValues = [maps:get(x, S) || S <- Shares],
    YValues = [maps:get(y, S) || S <- Shares],

    % 使用拉格朗日插值法计算 f(0)
    SecretInt = lagrange_interpolate(XValues, YValues, 0),

    % 将整数转换回二进制
    int_to_binary(SecretInt);
combine_shares(_Shares) ->
    error({invalid_parameters, "At least 2 shares are required"}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc 生成多项式系数
%%
%% 第一个系数是秘密，其余 K-1 个系数是随机数
-spec generate_coefficients(integer(), pos_integer()) -> [integer()].
generate_coefficients(Secret, K) ->
    [Secret | generate_random_coefficients(K - 1)].

%% @doc 生成随机系数（32 字节随机数）
-spec generate_random_coefficients(pos_integer()) -> [integer()].
generate_random_coefficients(Count) ->
    [binary_to_int(crypto:strong_rand_bytes(32)) || _ <- lists:seq(1, Count)].

%% @doc 创建分片
%%
%% 在 x 处计算多项式的值
-spec create_share(pos_integer(), [integer()]) -> share().
create_share(Index, Coeffs) ->
    X = Index,
    Y = evaluate_polynomial(Coeffs, X),
    #{
        index => Index,
        x => X,
        y => Y
    }.

%% @doc 在 x 处计算多项式的值
%%
%% f(x) = a0 + a1*x + a2*x^2 + ... + ak-1*x^(k-1)
-spec evaluate_polynomial([integer()], integer()) -> integer().
evaluate_polynomial(Coeffs, X) ->
    % 使用索引来跟踪当前处理的是哪个系数
    CoeffsWithIndex = lists:zip(Coeffs, lists:seq(0, length(Coeffs) - 1)),
    lists:foldl(
        fun({Coeff, Index}, Acc) ->
            Term = (Coeff * modpow(X, Index, ?PRIME)) rem ?PRIME,
            (Acc + Term) rem ?PRIME
        end,
        0,
        CoeffsWithIndex
    ).

%% @doc 模幂运算
-spec modpow(integer(), non_neg_integer(), integer()) -> integer().
modpow(_Base, 0, _Mod) ->
    1;
modpow(Base, Exp, Mod) when Exp > 0 ->
    Base * modpow(Base, Exp - 1, Mod) rem Mod.

%% @doc 拉格朗日插值法
%%
%% 给定点 (xi, yi)，计算在 x 处的值
-spec lagrange_interpolate([integer()], [integer()], integer()) -> integer().
lagrange_interpolate(XValues, YValues, X) ->
    lagrange_interpolate_acc(XValues, YValues, XValues, X, 0).

lagrange_interpolate_acc([], [], _AllX, _X, Acc) ->
    Acc rem ?PRIME;
lagrange_interpolate_acc([Xi | XRest], [Yi | YRest], AllX, X, Acc) ->
    Basis = lagrange_basis(Xi, AllX, X),
    NewAcc = (Acc + Yi * Basis) rem ?PRIME,
    lagrange_interpolate_acc(XRest, YRest, AllX, X, NewAcc).

%% @doc 计算拉格朗日基函数
%%
%% L_i(x) = Π (x - xj) / (xi - xj) for all j ≠ i
-spec lagrange_basis(integer(), [integer()], integer()) -> integer().
lagrange_basis(Xi, AllX, X) ->
    lagrange_basis_acc(Xi, AllX, X, 1).

lagrange_basis_acc(_Xi, [], _X, Acc) ->
    Acc;
lagrange_basis_acc(Xi, [Xj | XRest], X, Acc) when Xi =:= Xj ->
    % 跳过自己
    lagrange_basis_acc(Xi, XRest, X, Acc);
lagrange_basis_acc(Xi, [Xj | XRest], X, Acc) ->
    Numerator = ((X - Xj) rem ?PRIME + ?PRIME) rem ?PRIME,
    Denominator = ((Xi - Xj) rem ?PRIME + ?PRIME) rem ?PRIME,
    DenominatorInverse = mod_inverse(Denominator, ?PRIME),
    Basis = (Numerator * DenominatorInverse) rem ?PRIME,
    lagrange_basis_acc(Xi, XRest, X, (Acc * Basis) rem ?PRIME).

%% @doc 模逆元（使用扩展欧几里得算法）
-spec mod_inverse(integer(), integer()) -> integer().
mod_inverse(A, Mod) ->
    mod_inverse(A rem Mod, Mod, 1, 0, Mod, A).

%% 扩展欧几里得算法的递归实现
%% A = current remainder, B = previous remainder
%% X1, X2 = Bezout coefficients for A
%% ModInvA = modular inverse of A
mod_inverse(0, _B, _X1, _X2, _Mod, _ModInvA) ->
    error({no_inverse, "Modular inverse does not exist"});
mod_inverse(1, _B, X1, _X2, Mod, _ModInvA) ->
    % 找到了逆元，确保结果是正数
    ((X1 rem Mod) + Mod) rem Mod;
mod_inverse(A, B, X1, X2, Mod, ModInvA) ->
    Q = B div A,
    R = B rem A,
    % 更新 Bezout 系数
    NewX1 = X2 - Q * X1,
    mod_inverse(R, A, NewX1, X1, Mod, ModInvA).

%% @doc 二进制转整数
%%
%% 将二进制转换为整数，保持前导零
%% 与前端的 _bytesToInt 实现一致
-spec binary_to_int(binary()) -> integer().
binary_to_int(<<>>) ->
    0;
binary_to_int(Binary) ->
    % 将每个字节转换为 2 位十六进制字符串，然后拼接
    % 这保持了前导零
    HexList = binary_to_hex_list(Binary, []),
    erlang:list_to_integer(HexList, 16).

%% @doc 将二进制转换为十六进制字符列表（内部函数）
-spec binary_to_hex_list(binary(), list()) -> list().
binary_to_hex_list(<<>>, Acc) ->
    lists:reverse(Acc);
binary_to_hex_list(<<Byte:8, Rest/binary>>, Acc) ->
    % 将字节转换为 2 位十六进制（保持前导零）
    High = Byte div 16,
    Low = Byte rem 16,
    HexCharHigh = hex_digit(High),
    HexCharLow = hex_digit(Low),
    binary_to_hex_list(Rest, [HexCharLow, HexCharHigh | Acc]).

%% @doc 十六进制数字转字符
-spec hex_digit(0..15) -> char().
hex_digit(N) when N >= 0, N =< 9 ->
    $0 + N;
hex_digit(N) when N >= 10, N =< 15 ->
    $a + (N - 10).

%% @doc 整数转二进制
-spec int_to_binary(integer()) -> binary().
int_to_binary(Int) when Int >= 0 ->
    % 将整数转换为十六进制字符列表（小写）
    HexList = erlang:integer_to_list(Int, 16),
    % 填充到 64 个字符（32 字节）
    PaddedHexList = pad_hex_left(HexList, 64),
    % 转换为二进制
    hex_list_to_binary(PaddedHexList);
int_to_binary(Int) when Int < 0 ->
    % 处理负数（使用模运算转换为正数）
    PosInt = ((Int rem ?PRIME) + ?PRIME) rem ?PRIME,
    int_to_binary(PosInt).

%% @doc 填充十六进制列表到指定长度（左侧填充0）
-spec pad_hex_left(list(), pos_integer()) -> list().
pad_hex_left(HexList, TargetLength) when length(HexList) >= TargetLength ->
    HexList;
pad_hex_left(HexList, TargetLength) ->
    PadCount = TargetLength - length(HexList),
    lists:duplicate(PadCount, $0) ++ HexList.

%% @doc 十六进制字符列表转二进制
-spec hex_list_to_binary(list()) -> binary().
hex_list_to_binary(HexList) ->
    hex_list_to_binary_chars(HexList, <<>>).

%% @doc 十六进制字符列表转二进制（内部函数）
-spec hex_list_to_binary_chars(list(), binary()) -> binary().
hex_list_to_binary_chars([], Acc) ->
    Acc;
hex_list_to_binary_chars([H, L | Rest], Acc) ->
    Byte = erlang:list_to_integer([H, L], 16),
    hex_list_to_binary_chars(Rest, <<Acc/binary, Byte>>).
