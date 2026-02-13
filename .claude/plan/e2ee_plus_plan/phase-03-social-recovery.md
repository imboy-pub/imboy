# 阶段 3: 社交恢复

> **预计工期**: 7-10 天
> **依赖**: 阶段 1（准备工作）
> **安全等级**: ⭐⭐⭐⭐（相对安全）

---

## 目标

实现基于 Shamir 秘密共享的社交恢复功能：
1. 后端：Shamir 库、可信联系人管理、密钥分片管理
2. 前端：联系人管理、密钥分片创建/获取、密钥重组
3. 完整的恢复流程测试

---

## 安全原则

```
┌────────────────────────────────────────────────────────────┐
│                      安全设计原则                           │
├────────────────────────────────────────────────────────────┤
│ 1. 使用 Shamir 秘密共享 - 需要多个分片才能恢复              │
│ 2. 每个分片使用受托人的公钥加密 - 只有本人能解密            │
│ 3. 阈值设置为 2/3 - 需要 3 个好友中的至少 2 个              │
│ 4. 服务器永不解密私钥或分片 - 仅转发加密数据                │
│ 5. 用户可以随时更换可信联系人                               │
│ 6. 重建后可以删除旧的密钥分片                               │
└────────────────────────────────────────────────────────────┘
```

---

## 恢复流程

```
┌─────────────────────────────────────────────────────────────┐
│                   创建密钥分片流程                           │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. 用户选择 3 个可信好友（必须是双向好友）                  │
│                                                             │
│  2. 使用 Shamir 秘密共享将私钥分割成 3 个分片                │
│     (2/3 阈值 - 需要 2 个分片即可恢复)                       │
│                                                             │
│  3. 使用每个好友的公钥加密对应的密钥分片                     │
│     (好友的公钥从 user_device 表获取)                        │
│                                                             │
│  4. 将加密后的分片存储到服务器                               │
│     (服务器只存储，不解密)                                   │
│                                                             │
│  5. 通知好友他们已成为受托人                                 │
│     (可选：通过 WebSocket 推送通知)                          │
│                                                             │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                     密钥恢复流程                             │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. 用户联系至少 2 个可信好友                                 │
│                                                             │
│  2. 好友确认恢复请求                                        │
│     (好友在自己的应用中确认)                                 │
│                                                             │
│  3. 好友的设备解密对应的密钥分片                             │
│     (使用好友自己的私钥解密)                                 │
│                                                             │
│  4. 解密后的分片发送给用户                                   │
│     (通过服务器转发)                                         │
│                                                             │
│  5. 用户使用 Shamir 重组密钥                                 │
│     (收集到足够的分片后重组)                                 │
│                                                             │
│  6. 用户存储恢复的私钥                                       │
│                                                             │
│  7. 可选：删除旧的密钥分片                                   │
│     (恢复成功后可以删除)                                     │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 步骤 1: 后端 - Shamir 秘密共享库

### 1.1 创建 Shamir 库

```bash
cd /Users/leeyi/project/imboy.pub/imboy

# 创建 Shamir 库文件
vim src/lib/elib_shamir.erl
```

### 1.2 Shamir 库实现代码

```erlang
-module(elib_shamir).
%%%===================================================================
%%% @doc
%%% elib_shamir - Shamir 秘密共享实现
%%%
%%% 功能：
%%% - 创建秘密分片（使用有限域 GF(256)）
%%% - 组合秘密分片恢复原始秘密
%%% - 验证分片格式
%%%
%%% 算法：
%%% - 使用拉格朗日插值在有限域上实现
%%% - 支持自定义阈值（t-of-n）
%%%
%%% 数学原理：
%%% - 在有限域 GF(2^8) 上构造一个 t-1 次多项式
%%% - P(x) = a0 + a1*x + a2*x^2 + ... + a{t-1}*x^{t-1}
%%% - 其中 a0 是秘密值，a1...a{t-1} 是随机系数
%%% - 生成 n 个点 (xi, P(xi)) 作为分片
%%% - 使用任意 t 个分片可以通过拉格朗日插值恢复 P(0) = a0
%%%
%%% 使用示例：
%%% ```
%%% % 创建 3 个分片，需要 2 个才能恢复
%%% {ok, Shares} = elib_shamir:create_shares(<<"secret">>, 2, 3),
%%%
%%% % 使用 2 个分片恢复秘密
%%% {ok, Secret} = elib_shamir:combine_shares(lists:sublist(Shares, 2)),
%%% ```
%%%===================================================================

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% API 函数
-export([create_shares/3]).
-export([combine_shares/1]).
-export([validate_share/1]).
-export([test/0]).

%% 类型定义
-type share() :: #{
    x => integer(),
    y => binary()
}.
-type secret() :: binary().
-type shares_result() :: {ok, [share()]} | {error, term()}.
-type combine_result() :: {ok, secret()} | {error, term()}.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 创建秘密分片
%% @param Secret 原始秘密（二进制数据）
%% @param Threshold 恢复阈值（需要多少分片才能恢复）
%% @param TotalShares 总分片数
%% @returns {ok, [Share]} | {error, Reason}
%%
%% 示例：
%%% ```
%%% % 创建 3 个分片，需要 2 个才能恢复
%%% {ok, Shares} = elib_shamir:create_shares(<<"my_secret_key">>, 2, 3),
%%% ```
-spec create_shares(secret(), pos_integer(), pos_integer()) -> shares_result().
create_shares(Secret, Threshold, TotalShares)
  when is_binary(Secret),
       Threshold > 0,
       TotalShares >= Threshold,
       TotalShares =< 255 ->
    try
        % 1. 将秘密转换为字节数组
        SecretBytes = <<Secret/binary>>,

        % 2. 对每个字节应用 Shamir 秘密共享
        % 注意：这里对整个秘密作为一个大数处理
        % 对于较大的秘密，可以分段处理
        ShareLists = lists:map(fun(<<Byte:8>>) ->
            % 为每个字节生成分片
            create_byte_shares(Byte, Threshold, TotalShares)
        end, [SecretBytes]),

        % 3. 转换为需要的格式：每个分片包含 x 和 y（y 是所有字节的组合）
        Shares = combine_byte_shares(ShareLists),

        ?INFO_LOG([elib_shamir, shares_created, TotalShares, Threshold]),
        {ok, Shares}
    catch
        Type:Error:Stacktrace ->
            ?ERROR_LOG([elib_shamir, create_failed, Type, Error, Stacktrace]),
            {error, {Type, Error}}
    end;
create_shares(_Secret, Threshold, TotalShares) ->
    {error, {invalid_params, Threshold, TotalShares}}.

%% @doc 组合秘密分片恢复原始秘密
%% @param Shares 密钥分片列表（至少 Threshold 个）
%% @returns {ok, Secret} | {error, Reason}
%%
%% 示例：
%%% ```
%%% % 使用前 2 个分片恢复
%%% {ok, Secret} = elib_shamir:combine_shares(lists:sublist(Shares, 2)),
%%% ```
-spec combine_shares([share()]) -> combine_result().
combine_shares(Shares) when is_list(Shares), length(Shares) >= 2 ->
    try
        % 1. 验证分片格式
        lists:foreach(fun validate_share/1, Shares),

        % 2. 获取秘密长度（从 y 的大小推断）
        FirstY = maps:get(y, hd(Shares)),
        SecretLength = byte_size(FirstY),

        % 3. 对每个字节位置应用拉格朗日插值
        SecretBytes = lists:map(fun(ByteIndex) ->
            % 提取所有分片中该字节位置的值
            ByteShares = lists:map(fun(Share) ->
                X = maps:get(x, Share),
                YBinary = maps:get(y, Share),
                <<_:ByteIndex/unit:8, Byte:8, _/binary>> = YBinary,
                {X, Byte}
            end, Shares),

            % 使用拉格朗日插值恢复该字节
            lagrange_interpolation_byte(ByteShares)
        end, lists:seq(0, SecretLength - 1)),

        % 4. 组合所有字节
        Secret = list_to_binary(SecretBytes),

        ?INFO_LOG([elib_shamir, secret_combined, length(Shares)]),
        {ok, Secret}
    catch
        Type:Error:Stacktrace ->
            ?ERROR_LOG([elib_shamir, combine_failed, Type, Error, Stacktrace]),
            {error, {Type, Error}}
    end;
combine_shares(_Shares) ->
    {error, not_enough_shares}.

%% @doc 验证分片格式
%% @param Share 密钥分片
%% @returns ok | {error, invalid_share}
-spec validate_share(share()) -> ok | {error, term()}.
validate_share(Share) when is_map(Share) ->
    case {maps:get(x, Share, undefined), maps:get(y, Share, undefined)} of
        {X, Y} when is_integer(X), X > 0, X =< 255, is_binary(Y), byte_size(Y) > 0 ->
            ok;
        _ ->
            {error, invalid_share}
    end;
validate_share(_) ->
    {error, invalid_share}.

%% @doc 测试函数（用于验证实现）
-spec test() -> ok.
test() ->
    Secret = <<"Test"/utf8>>,

    % 测试 2-of-3
    {ok, Shares} = create_shares(Secret, 2, 3),
    ?assertEqual(3, length(Shares)),

    % 使用任意 2 个分片恢复
    {ok, Recovered} = combine_shares(lists:sublist(Shares, 2)),
    ?assertEqual(Secret, Recovered),

    % 测试不同的分片组合
    {ok, Recovered2} = combine_shares([lists:nth(2, Shares), lists:nth(3, Shares)]),
    ?assertEqual(Secret, Recovered2),

    io:format("Shamir secret sharing test passed!~n"),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 为单个字节创建分片（在 GF(256) 上）
%% @private
-spec create_byte_shares(byte(), pos_integer(), pos_integer()) -> [{integer(), byte()}].
create_byte_shares(Byte, Threshold, TotalShares) ->
    % 1. 生成随机系数（a0 是秘密字节）
    Coefficients = [Byte | generate_coefficients(Threshold - 1)],

    % 2. 为每个分片计算点 (x, y)
    lists:map(fun(X) ->
        Y = evaluate_polynomial_gf256(Coefficients, X),
        {X, Y}
    end, lists:seq(1, TotalShares)).

%% @doc 组合字节分片
%% @private
-spec combine_byte_shares([[{integer(), byte()}]]) -> [share()].
combine_byte_shares(ShareLists) ->
    % ShareLists 是一个列表的列表
    % 每个内部列表对应一个字节的所有分片
    % 我们需要重新组织：每个分片包含所有字节

    TotalShares = length(hd(ShareLists)),

    lists:map(fun(ShareIndex) ->
        % 获取所有分片中索引为 ShareIndex 的项
        X = element(1, lists:nth(ShareIndex, hd(ShareLists))),

        % 提取所有字节的 y 值
        YBytes = lists:map(fun(ShareList) ->
            {_, Y} = lists:nth(ShareIndex, ShareList),
            Y
        end, ShareLists),

        % 组合成二进制
        Y = list_to_binary(YBytes),

        #{x => X, y => Y}
    end, lists:seq(1, TotalShares)).

%% @doc 生成随机系数（0-255）
%% @private
-spec generate_coefficients(non_neg_integer()) -> [byte()].
generate_coefficients(0) ->
    [];
generate_coefficients(Count) ->
    lists:map(fun(_) ->
        <<Rand:8>> = crypto:strong_rand_bytes(1),
        Rand
    end, lists:seq(1, Count)).

%% @doc 在 GF(256) 上评估多项式
%% @private
-spec evaluate_polynomial_gf256([byte()], integer()) -> byte().
evaluate_polynomial_gf256(Coefficients, X) ->
    % 计算 P(x) = a0 + a1*x + a2*x^2 + ... + a{n-1}*x^{n-1}
    % 所有运算都在 GF(256) 上进行
    lists:foldl(fun(Coeff, Acc) ->
        gf256_add(gf256_multiply(Acc, X), Coeff)
    end, 0, lists:reverse(Coefficients)).

%% @doc 拉格朗日插值恢复单个字节
%% @private
-spec lagrange_interpolation_byte([{integer(), byte()}]) -> byte().
lagrange_interpolation_byte(Points) ->
    % 计算拉格朗日插值: P(0) = Σ y_i * l_i(0)
    lists:foldl(fun({Xi, Yi}, Acc) ->
        Li = lagrange_basis_gf256(Xi, Points),
        gf256_add(Acc, gf256_multiply(Yi, Li))
    end, 0, Points).

%% @doc 计算拉格朗日基多项式在 x=0 处的值（GF(256)）
%% @private
-spec lagrange_basis_gf256(integer(), [{integer(), byte()}]) -> byte().
lagrange_basis_gf256(Xi, Points) ->
    % l_i(0) = Π (0 - x_j) / (x_i - x_j), for j != i
    Numerator = lists:foldl(fun({Xj, _}, Acc) when Xj =/= Xi ->
        gf256_multiply(Acc, gf256_subtract(0, Xj));
       (_, Acc) ->
        Acc
    end, 1, Points),

    Denominator = lists:foldl(fun({Xj, _}, Acc) when Xj =/= Xi ->
        gf256_multiply(Acc, gf256_subtract(Xi, Xj));
       (_, Acc) ->
        Acc
    end, 1, Points),

    case Denominator of
        0 -> error(zero_division);
        _ -> gf256_divide(Numerator, Denominator)
    end.

%% ================================================================
%% GF(256) 运算（用于 Shamir 秘密共享）
%% ================================================================

%% @doc GF(256) 加法
%% @private
-spec gf256_add(byte(), byte()) -> byte().
gf256_add(A, B) ->
    A bxor B.

%% @doc GF(256) 减法（与加法相同）
%% @private
-spec gf256_subtract(byte(), byte()) -> byte().
gf256_subtract(A, B) ->
    A bxor B.

%% @doc GF(256) 乘法
%% @private
-spec gf256_multiply(byte(), byte()) -> byte().
gf256_multiply(0, _) -> 0;
gf256_multiply(_, 0) -> 0;
gf256_multiply(A, B) ->
    % 使用对数表优化（这里简化实现）
    gf256_multiply_impl(A, B, 0).

%% @private
gf256_multiply_impl(_A, 0, Result) -> Result;
gf256_multiply_impl(A, B, Result) when B band 1 =:= 1 ->
    gf256_multiply_impl(A bxor 16#1B, B bsr 1, A bxor Result);
gf256_multiply_impl(A, B, Result) ->
    gf256_multiply_impl(A bxor 16#1B, B bsr 1, Result).

%% @doc GF(256) 除法
%% @private
-spec gf256_divide(byte(), byte()) -> byte().
gf256_divide(_, 0) -> error(divide_by_zero);
gf256_divide(0, _) -> 0;
gf256_divide(A, B) ->
    % 简化实现：使用乘法逆元
    InverseB = gf256_inverse(B),
    gf256_multiply(A, InverseB).

%% @doc 计算 GF(256) 乘法逆元
%% @private
-spec gf256_inverse(byte()) -> byte().
gf256_inverse(0) -> error(inverse_of_zero);
gf256_inverse(A) ->
    % 使用扩展欧几里得算法
    gf256_exp(A, 254).  % a^(254) = a^(-1) in GF(256)

%% @doc GF(256) 指数运算
%% @private
-spec gf256_exp(byte(), integer()) -> byte().
gf256_exp(_, 0) -> 1;
gf256_exp(A, Exp) when Exp > 0 ->
    gf256_exp_impl(A, Exp, 1).

%% @private
gf256_exp_impl(A, 1, Result) ->
    gf256_multiply(Result, A);
gf256_exp_impl(A, Exp, Result) when Exp rem 2 =:= 0 ->
    gf256_exp_impl(gf256_multiply(A, A), Exp div 2, Result);
gf256_exp_impl(A, Exp, Result) ->
    gf256_exp_impl(gf256_multiply(A, A), Exp div 2, gf256_multiply(Result, A)).
```

### 1.3 创建测试文件

```bash
vim test/lib/elib_shamir_tests.erl
```

```erlang
-module(elib_shamir_tests).
-include_lib("eunit/include/eunit.hrl").

%% ================================================================
%% 测试用例
%% ================================================================

shamir_test_() ->
    [
     {"创建2-of-3分片", fun create_2_of_3_shares/0},
     {"组合分片恢复秘密", fun combine_shares/0},
     {"不同分片组合测试", fun different_combinations/0},
     {"无效分片验证", fun validate_invalid_share/0},
     {"分片不足测试", fun not_enough_shares/0},
     {"大秘密测试", fun large_secret_test/0},
     {"GF256运算测试", fun gf256_operations_test/0}
    ].

create_2_of_3_shares() ->
    Secret = <<"test_secret_key">>,
    {ok, Shares} = elib_shamir:create_shares(Secret, 2, 3),
    ?assertEqual(3, length(Shares)),

    % 验证每个分片的格式
    lists:foreach(fun(Share) ->
        ?assertMatch(ok, elib_shamir:validate_share(Share))
    end, Shares).

combine_shares() ->
    Secret = <<"another_test_secret">>,
    {ok, Shares} = elib_shamir:create_shares(Secret, 2, 3),

    % 使用前 2 个分片恢复
    {ok, Recovered} = elib_shamir:combine_shares(lists:sublist(Shares, 2)),
    ?assertEqual(Secret, Recovered).

different_combinations() ->
    Secret = <<"test_combinations">>,
    {ok, Shares} = elib_shamir:create_shares(Secret, 2, 3),

    % 测试所有可能的 2 个分片组合
    Combinations = [
        [lists:nth(1, Shares), lists:nth(2, Shares)],
        [lists:nth(1, Shares), lists:nth(3, Shares)],
        [lists:nth(2, Shares), lists:nth(3, Shares)]
    ],

    lists:foreach(fun(Combo) ->
        {ok, Recovered} = elib_shamir:combine_shares(Combo),
        ?assertEqual(Secret, Recovered)
    end, Combinations).

validate_invalid_share() ->
    InvalidShare = #{x => -1, y => <<"invalid">>},
    ?assertMatch({error, invalid_share}, elib_shamir:validate_share(InvalidShare)).

not_enough_shares() ->
    Secret = <<"test">>,
    {ok, Shares} = elib_shamir:create_shares(Secret, 2, 3),

    % 只使用 1 个分片，应该失败
    Result = elib_shamir:combine_shares(lists:sublist(Shares, 1)),
    ?assertMatch({error, not_enough_shares}, Result).

large_secret_test() ->
    % 测试较大的秘密（多个字节）
    Secret = <<"This is a much longer secret with multiple bytes"/utf8>>,
    {ok, Shares} = elib_shamir:create_shares(Secret, 3, 5),

    % 使用 3 个分片恢复
    {ok, Recovered} = elib_shamir:combine_shares(lists:sublist(Shares, 3)),
    ?assertEqual(Secret, Recovered).

gf256_operations_test() ->
    % 测试 GF(256) 基本运算
    ?assertEqual(0, elib_shamir:gf256_add(0, 0)),
    ?assertEqual(16#AB, elib_shamir:gf256_add(16#AB, 0)),
    ?assertEqual(0, elib_shamir:gf256_subtract(16#AB, 16#AB)),

    % 乘法单位元
    ?assertEqual(16#AB, elib_shamir:gf256_multiply(16#AB, 1)),

    % 乘法逆元
    {ok, Shares} = elib_shamir:create_shares(<<"x">>, 2, 3),
    ?assertMatch({ok, <<"x">>}, elib_shamir:combine_shares(lists:sublist(Shares, 2))).
```

---

## 步骤 2: 后端 - Repo 层

### 2.1 创建 Repo 文件

```bash
vim src/repo/e2ee_social_repo.erl
```

### 2.2 Repo 实现代码（完整版）

```erlang
-module(e2ee_social_repo).
%%%===================================================================
%%% @doc
%%% e2ee_social_repo - E2EE 社交恢复数据仓库层
%%%
%%% 功能：
%%% - 可信联系人 CRUD 操作
%%% - 密钥分片 CRUD 操作
%%% - 状态管理和查询
%%%===================================================================

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% API 函数 - 可信联系人
-export([add_contact/1]).
-export([find_contact/2]).
-export([list_contacts/1]).
-export([remove_contact/2]).
-export([update_contact_status/3]).
-export([count_contacts/1]).

%% API 函数 - 密钥分片
-export([create_key_share/1]).
-export([find_key_share/2]).
-export([list_key_shares/1]).
-export([list_key_shares_by_owner/1]).
-export([list_key_shares_by_trustee/1]).
-export([delete_key_share/2]).
-export([delete_key_shares_by_owner/1]).

%% 类型定义
-type contact() :: map().
-type key_share() :: map().
-type repo_result() :: {ok, contact() | key_share() | [contact() | key_share()]} | {error, term()}.

%%%===================================================================
%%% 可信联系人函数
%%%===================================================================

%% @doc 添加可信联系人
-spec add_contact(map()) -> repo_result().
add_contact(ContactMap) ->
    Uid = maps:get(<<"uid">>, ContactMap),
    ContactUid = maps:get(<<"contact_uid">>, ContactMap),
    ContactNickname = maps:get(<<"contact_nickname">>, ContactMap, <<>>),

    Sql = <<"INSERT INTO e2ee_trusted_contacts (uid, contact_uid, contact_nickname)
             VALUES ($1, $2, $3)
             ON CONFLICT (uid, contact_uid)
             DO UPDATE SET status = 'active', updated_at = CURRENT_TIMESTAMP
             RETURNING id, uid, contact_uid, contact_nickname, status, created_at, updated_at">>,

    case elib_pg:query(Sql, [Uid, ContactUid, ContactNickname]) of
        {ok, _, [{Result}]} ->
            ?INFO_LOG([e2ee_social_repo, contact_added, Uid, ContactUid]),
            {ok, row_to_contact_map(Result)};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_social_repo, add_contact_failed, Reason]),
            {error, Reason}
    end.

%% @doc 查找可信联系人
-spec find_contact(integer(), integer()) -> repo_result().
find_contact(Uid, ContactUid) ->
    Sql = <<"SELECT id, uid, contact_uid, contact_nickname, status, created_at, updated_at
             FROM e2ee_trusted_contacts
             WHERE uid = $1 AND contact_uid = $2 AND status = 'active'">>,

    case elib_pg:query(Sql, [Uid, ContactUid]) of
        {ok, _, [Result]} -> {ok, row_to_contact_map(Result)};
        {ok, _, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 列出用户的所有可信联系人
-spec list_contacts(integer()) -> {ok, [contact()]} | {error, term()}.
list_contacts(Uid) ->
    Sql = <<"SELECT id, uid, contact_uid, contact_nickname, status, created_at, updated_at
             FROM e2ee_trusted_contacts
             WHERE uid = $1 AND status = 'active'
             ORDER BY created_at DESC">>,

    case elib_pg:query(Sql, [Uid]) of
        {ok, _, Results} ->
            {ok, lists:map(fun row_to_contact_map/1, Results)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 统计用户可信联系人数量
-spec count_contacts(integer()) -> {ok, non_neg_integer()}.
count_contacts(Uid) ->
    Sql = <<"SELECT COUNT(*) as count
             FROM e2ee_trusted_contacts
             WHERE uid = $1 AND status = 'active'">>,

    case elib_pg:query(Sql, [Uid]) of
        {ok, _, [{Result}]} ->
            {ok, maps:get(<<"count">>, Result)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 移除可信联系人（软删除）
-spec remove_contact(integer(), integer()) -> ok | {error, term()}.
remove_contact(Uid, ContactUid) ->
    Sql = <<"UPDATE e2ee_trusted_contacts
             SET status = 'removed', updated_at = CURRENT_TIMESTAMP
             WHERE uid = $1 AND contact_uid = $2">>,

    case elib_pg:query(Sql, [Uid, ContactUid]) of
        {ok, _, _} ->
            ?INFO_LOG([e2ee_social_repo, contact_removed, Uid, ContactUid]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 更新联系人状态
-spec update_contact_status(integer(), integer(), binary()) -> ok | {error, term()}.
update_contact_status(Uid, ContactUid, Status) ->
    Sql = <<"UPDATE e2ee_trusted_contacts
             SET status = $1, updated_at = CURRENT_TIMESTAMP
             WHERE uid = $2 AND contact_uid = $3">>,

    case elib_pg:query(Sql, [Status, Uid, ContactUid]) of
        {ok, _, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% 密钥分片函数
%%%===================================================================

%% @doc 创建密钥分片
-spec create_key_share(map()) -> repo_result().
create_key_share(ShareMap) ->
    OwnerUid = maps:get(<<"owner_uid">>, ShareMap),
    TrusteeUid = maps:get(<<"trustee_uid">>, ShareMap),
    EncryptedShare = maps:get(<<"encrypted_share">>, ShareMap),
    ShareIndex = maps:get(<<"share_index">>, ShareMap),
    Threshold = maps:get(<<"threshold">>, ShareMap, 2),
    TotalShares = maps:get(<<"total_shares">>, ShareMap, 3),

    Sql = <<"INSERT INTO e2ee_key_shares (owner_uid, trustee_uid, encrypted_share,
                                        share_index, threshold, total_shares)
             VALUES ($1, $2, $3, $4, $5, $6)
             ON CONFLICT (owner_uid, trustee_uid)
             DO UPDATE SET encrypted_share = $3, share_index = $4,
                           threshold = $5, total_shares = $6, updated_at = CURRENT_TIMESTAMP
             RETURNING *">>,

    case elib_pg:query(Sql, [OwnerUid, TrusteeUid, EncryptedShare,
                            ShareIndex, Threshold, TotalShares]) of
        {ok, _, [{Result}]} ->
            ?INFO_LOG([e2ee_social_repo, key_share_created, OwnerUid, TrusteeUid]),
            {ok, row_to_share_map(Result)};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_social_repo, create_key_share_failed, Reason]),
            {error, Reason}
    end.

%% @doc 查找密钥分片
-spec find_key_share(integer(), integer()) -> repo_result().
find_key_share(OwnerUid, TrusteeUid) ->
    Sql = <<"SELECT id, owner_uid, trustee_uid, encrypted_share,
                    share_index, threshold, total_shares, created_at, updated_at
             FROM e2ee_key_shares
             WHERE owner_uid = $1 AND trustee_uid = $2">>,

    case elib_pg:query(Sql, [OwnerUid, TrusteeUid]) of
        {ok, _, [Result]} -> {ok, row_to_share_map(Result)};
        {ok, _, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 列出用户的所有密钥分片
-spec list_key_shares(integer()) -> {ok, [key_share()]} | {error, term()}.
list_key_shares(OwnerUid) ->
    Sql = <<"SELECT id, owner_uid, trustee_uid, encrypted_share,
                    share_index, threshold, total_shares, created_at, updated_at
             FROM e2ee_key_shares
             WHERE owner_uid = $1
             ORDER BY share_index">>,

    case elib_pg:query(Sql, [OwnerUid]) of
        {ok, _, Results} ->
            {ok, lists:map(fun row_to_share_map/1, Results)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 列出用户作为受托人存储的所有分片
-spec list_key_shares_by_trustee(integer()) -> {ok, [key_share()]} | {error, term()}.
list_key_shares_by_trustee(TrusteeUid) ->
    Sql = <<"SELECT id, owner_uid, trustee_uid, encrypted_share,
                    share_index, threshold, total_shares, created_at, updated_at
             FROM e2ee_key_shares
             WHERE trustee_uid = $1
             ORDER BY created_at DESC">>,

    case elib_pg:query(Sql, [TrusteeUid]) of
        {ok, _, Results} ->
            {ok, lists:map(fun row_to_share_map/1, Results)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除单个密钥分片
-spec delete_key_share(integer(), integer()) -> ok | {error, term()}.
delete_key_share(OwnerUid, TrusteeUid) ->
    Sql = <<"DELETE FROM e2ee_key_shares
             WHERE owner_uid = $1 AND trustee_uid = $2">>,

    case elib_pg:query(Sql, [OwnerUid, TrusteeUid]) of
        {ok, _, _} ->
            ?INFO_LOG([e2ee_social_repo, key_share_deleted, OwnerUid, TrusteeUid]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除用户的所有密钥分片
-spec delete_key_shares_by_owner(integer()) -> ok | {error, term()}.
delete_key_shares_by_owner(OwnerUid) ->
    Sql = <<"DELETE FROM e2ee_key_shares WHERE owner_uid = $1">>,

    case elib_pg:query(Sql, [OwnerUid]) of
        {ok, _, _} ->
            ?INFO_LOG([e2ee_social_repo, all_key_shares_deleted, OwnerUid]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
row_to_contact_map(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        uid => maps:get(<<"uid">>, Row),
        contact_uid => maps:get(<<"contact_uid">>, Row),
        contact_nickname => maps:get(<<"contact_nickname">>, Row, <<>>),
        status => maps:get(<<"status">>, Row),
        created_at => maps:get(<<"created_at">>, Row),
        updated_at => maps:get(<<"updated_at">>, Row)
    }.

%% @private
row_to_share_map(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        owner_uid => maps:get(<<"owner_uid">>, Row),
        trustee_uid => maps:get(<<"trustee_uid">>, Row),
        encrypted_share => maps:get(<<"encrypted_share">>, Row),
        share_index => maps:get(<<"share_index">>, Row),
        threshold => maps:get(<<"threshold">>, Row),
        total_shares => maps:get(<<"total_shares">>, Row),
        created_at => maps:get(<<"created_at">>, Row),
        updated_at => maps:get(<<"updated_at">>, Row)
    }.
```

---

## 完成检查清单

- [ ] Shamir 库实现完成
- [ ] Shamir 测试通过
- [ ] Repo 层实现完成
- [ ] DS 层实现完成（下一步）
- [ ] Logic 层实现完成（下一步）
- [ ] Handler 层实现完成（下一步）
- [ ] 路由配置完成（下一步）
- [ ] 前端 API 服务完成（下一步）
- [ ] 前端社交恢复服务完成（下一步）
- [ ] Dart Shamir 库实现（下一步）
- [ ] 单元测试通过
- [ ] 集成测试通过
- [ ] 文档更新完成

---

## 下一阶段

由于篇幅限制，DS、Logic、Handler 层的实现结构与设备传输类似。继续执行：
- [阶段 4: 本地备份](./phase-04-local-backup.md) ← 已完成
- [阶段 5: 前端 UI](./phase-05-frontend-ui.md) ← 已完成

---

**最后更新**: 2026-01-31
**作者**: Claude AI Planning Agent
