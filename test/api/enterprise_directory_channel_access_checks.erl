-module(enterprise_directory_channel_access_checks).
-export([setup/2, check/3]).
-include_lib("eunit/include/eunit.hrl").

setup(C, U) ->
    [
        seed(C, Id, Ws, U, N)
     || {Id, Ws, N} <- [{995840, 995800, 7}, {995841, null, 3}, {995842, 995802, 5}]
    ].
seed(C, Id, Ws, U, N) ->
    Scope =
        case Ws of
            null -> <<"personal">>;
            _ -> <<"workspace">>
        end,
    exec(
        C,
        <<"INSERT INTO channel(id,name,creator_uid,status,scope,workspace_id) VALUES($1,'synthetic-parent-channel',$2,1,$3,$4)">>,
        [Id, U, Scope, Ws]
    ),
    exec(
        C,
        <<"INSERT INTO channel_subscription(id,channel_id,user_id,status,unread_count) VALUES($1,$2,$3,1,$4)">>,
        [Id + 100, Id, U, N]
    ),
    exec(C, <<"INSERT INTO channel_admin(id,channel_id,user_id,role) VALUES($1,$2,$3,3)">>, [
        Id + 200, Id, U
    ]).

check(C, U, Allowed) ->
    {ok, Subscribed} = channel_repo:list_subscribed(U, <<"id">>),
    {ok, Managed} = channel_repo:list_managed(U),
    {ok, Delta} = channel_repo:list_by_ids_since([995840, 995841, 995842], 0, U),
    [assert_ids(Rows, Allowed) || Rows <- [Subscribed, Managed, Delta]],
    {ok, Sync} = channel_logic_sync:sync_channels(U, 0),
    ?assertEqual(
        case Allowed of
            true -> 3;
            false -> 2
        end,
        length(maps:get(channels, Sync))
    ),
    ?assertEqual(
        case Allowed of
            true -> 15;
            false -> 8
        end,
        channel_subscription_repo:count_unread(U)
    ),
    ?assertEqual(
        case Allowed of
            true -> 3;
            false -> 2
        end,
        channel_subscription_repo:count_unread_channels(U)
    ),
    {ok, Unread} = channel_subscription_repo:list_unread_by_uid(U),
    ?assertEqual(Allowed, lists:any(fun(#{<<"channel_id">> := Id}) -> Id =:= 995840 end, Unread)),
    ?assertEqual(
        case Allowed of
            true -> 7;
            false -> 0
        end,
        channel_subscription_repo:get_unread_count(995840, U)
    ),
    {ok, Fanout} = channel_subscription_repo:list_by_channel(995840),
    {ok, UnreadPush} = channel_subscription_repo:list_unread_counts_by_channel(995840),
    ?assert(Fanout =/= []),
    %% Cold-fill while suspended, then retain this cache across restoration.
    case Allowed of
        false -> imboy_cache:invalidate({channel_subs, 995840});
        true -> ok
    end,
    ?assert(lists:member(U, channel_ds:subscriber_uids(995840))),
    ?assertEqual(Allowed, UnreadPush =/= []),
    %% Quota facts must not shrink while a retained parent is suspended.
    ?assertEqual({ok, 3}, channel_repo:count_managed_tx(C, U)),
    [delivery(U, Id, A) || {Id, A} <- [{995840, Allowed}, {995841, true}, {995842, true}]].

assert_ids(Rows, Allowed) ->
    Ids = [maps:get(<<"id">>, R) || R <- Rows],
    ?assertEqual(Allowed, lists:member(995840, Ids)),
    ?assert(lists:member(995841, Ids)),
    ?assert(lists:member(995842, Ids)).
delivery(U, Id, Allowed) ->
    Bin = jsone:encode(#{
        <<"type">> => <<"S2C">>,
        <<"action">> => <<"channel_message">>,
        <<"payload">> => #{<<"channel_id">> => Id, <<"content">> => <<"synthetic-private-body">>}
    }),
    State = #{current_uid => U, framing => v2},
    Result = websocket_handler:websocket_info({reply, Bin}, State),
    case Allowed of
        true ->
            ?assertMatch({reply, _, State, hibernate}, Result);
        false ->
            ?assertEqual({ok, State, hibernate}, Result),
            Key = {U, <<"oa-channel-device">>, <<"queued-channel">>},
            Ref = make_ref(),
            ack_retry_cache:set(Key, Ref, 10000),
            ?assertEqual(
                {ok, State, hibernate},
                websocket_handler:websocket_info({timeout, Ref, {[1], Key, Bin}}, State)
            ),
            ?assertEqual(undefined, ack_retry_cache:get(Key))
    end.
exec(C, Q, P) -> intbe02_http_support:sql_exec(C, Q, P).
