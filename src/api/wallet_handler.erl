-module(wallet_handler).

%%%
% 钱包 API 处理器
% 提供余额查询、充值、流水查询接口
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            balance ->
                balance(Req0, State);
            transactions ->
                transactions(Req0, State);
            topup ->
                topup(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 查询余额
%% GET /v1/wallet/balance
%% 若钱包不存在则自动创建并返回 0 余额
-spec balance(cowboy_req:req(), map()) -> cowboy_req:req().
balance(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Wallet = wallet_repo:find_by_uid(CurrentUid),
    {Balance, Frozen} =
        case map_size(Wallet) =:= 0 of
            true ->
                % 钱包不存在，自动创建
                _ = wallet_repo:create(#{user_id => CurrentUid}),
                {0, 0};
            false ->
                {maps:get(<<"balance">>, Wallet, 0),
                 maps:get(<<"frozen">>, Wallet, 0)}
        end,
    Payload = #{
        <<"balance">> => Balance,
        <<"balance_yuan">> => Balance / 100.0,
        <<"frozen">> => Frozen
    },
    elib_response:success(Req0, Payload, "success.").

%% @doc 分页查询流水记录
%% GET /v1/wallet/transactions
-spec transactions(cowboy_req:req(), map()) -> cowboy_req:req().
transactions(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    {ok, Payload} = wallet_repo:page_transactions(Page, Size, CurrentUid),
    elib_response:success(Req0, Payload, "success.").

%% @doc 模拟充值
%% POST /v1/wallet/topup
%% 参数: amount（分），范围 100-1000000
-spec topup(cowboy_req:req(), map()) -> cowboy_req:req().
topup(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Amount = maps:get(<<"amount">>, PostVals, 0),
    % 充值金额验证：100分（1元）～ 1000000分（10000元）
    case is_integer(Amount) andalso Amount >= 100 andalso Amount =< 1000000 of
        false ->
            elib_response:error(Req0, <<"充值金额不合法，请输入100分到1000000分之间的整数"/utf8>>);
        true ->
            do_topup(Req0, CurrentUid, Amount)
    end.

%% @doc 执行充值逻辑（原子事务：余额更新+流水写入）
-spec do_topup(cowboy_req:req(), integer(), integer()) -> cowboy_req:req().
do_topup(Req0, Uid, Amount) ->
    % 确保钱包存在
    Wallet = ensure_wallet(Uid),
    WalletId = maps:get(<<"id">>, Wallet),
    RefNo = gen_reference_no(),
    TxData = #{
        <<"wallet_id">> => WalletId,
        <<"user_id">> => Uid,
        <<"amount">> => Amount,
        <<"tx_type">> => 1,
        <<"remark">> => <<"充值"/utf8>>,
        <<"status">> => 1
    },
    case wallet_repo:atomic_balance_change(Amount, Uid, TxData, RefNo) of
        {ok, NewBalance} ->
            Payload = #{
                <<"balance">> => NewBalance,
                <<"balance_yuan">> => NewBalance / 100.0,
                <<"reference_no">> => RefNo
            },
            elib_response:success(Req0, Payload, "success.");
        {rollback, insufficient_balance} ->
            elib_response:error(Req0, <<"充值失败"/utf8>>);
        {error, _} ->
            elib_response:error(Req0, <<"充值失败，请稍后再试"/utf8>>)
    end.

%% @doc 确保钱包存在，不存在则创建后返回
-spec ensure_wallet(integer()) -> map().
ensure_wallet(Uid) ->
    Wallet = wallet_repo:find_by_uid(Uid),
    case map_size(Wallet) =:= 0 of
        true ->
            _ = wallet_repo:create(#{user_id => Uid}),
            wallet_repo:find_by_uid(Uid);
        false ->
            Wallet
    end.

%% @doc 生成唯一充值单号（使用 crypto 强随机数避免碰撞）
-spec gen_reference_no() -> binary().
gen_reference_no() ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"TOP", Ts/binary, "_", Rand/binary>>.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
