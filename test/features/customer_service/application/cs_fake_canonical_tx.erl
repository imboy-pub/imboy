%%% @doc enterprise canonical tx 的 fake（test-only；A03 委派测试用）。
%%%
%%% 实现与 `eb_tx_port:accept_message/3` 同签名；把收到的参数记录进 ETS，
%%% 返回 canonical tx 成功形状（message 只含测试断言所需键）。用于证明
%%% `cs_session_app:append_session_message/2` 的唯一写入路径确实是
%%% `enterprise_business_facade:append_message`（A03），而非任何客服私有副本表。
-module(cs_fake_canonical_tx).

-export([accept_message/3, calls/0, reset/0]).

-define(TAB, cs_fake_canonical_tx_tab).

accept_message(OrgId, WorkspaceId, TxParams) ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, public]);
        _ -> ok
    end,
    ets:insert(?TAB, {call, erlang:unique_integer(), {OrgId, WorkspaceId, TxParams}}),
    MessageId = maps:get(client_msg_id, TxParams, <<"msg">>),
    {ok, #{
        message => #{
            id => erlang:phash2(MessageId),
            conversation_id => maps:get(conversation_id, TxParams),
            sender_type => maps:get(sender_type, TxParams)
        },
        replayed => false,
        audit_id => 1
    }}.

calls() ->
    case ets:info(?TAB) of
        undefined ->
            [];
        _ ->
            Calls = [V || {call, _, V} <- ets:tab2list(?TAB)],
            lists:sort(fun({A, _, _}, {B, _, _}) -> A =< B end, Calls)
    end.

reset() ->
    catch ets:delete(?TAB),
    ok.
