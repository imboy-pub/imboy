-module(personal_history_snapshot_ds).
-moduledoc "Account-scoped history snapshot paging and durable cursor confirmation.".

-export([account_snapshot/4, confirm_snapshot/6]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Account recovery pages scan immutable row IDs. An edit can move an event beyond
%% the saved watermark, but cannot take that row out of the snapshot's ID range.
-spec account_snapshot(integer(), binary(), binary(), binary() | undefined) ->
    {ok, pos_integer(), [map()], {more, binary()} | {complete, non_neg_integer(), binary()}}
    | {error, atom()}.
account_snapshot(AccountId, Domain, ClientInstance, Token) ->
    Scope = {AccountId, Domain, ClientInstance},
    with_snapshot_generation(AccountId, Domain, fun(Conn, Generation) ->
        account_snapshot_tx(Conn, Scope, Generation, Token)
    end).

account_snapshot_tx(Conn, {AccountId, Domain, _} = Scope, G, Token) ->
    PageSize = personal_history_ds:page_size(),
    case snapshot_position(Conn, Scope, G, Token) of
        {ok, {Cut, After, Watermark}} ->
            case
                personal_history_repo:snapshot_rows_tx(
                    Conn, AccountId, Domain, G, Cut, After, PageSize + 1
                )
            of
                {ok, Rows} ->
                    account_snapshot_result(Conn, Scope, G, Cut, Watermark, Rows, PageSize);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

snapshot_position(Conn, {AccountId, Domain, _}, G, undefined) ->
    case personal_history_repo:snapshot_bounds_tx(Conn, AccountId, Domain, G) of
        {ok, Cut, Watermark} -> {ok, {Cut, 0, Watermark}};
        {error, _} = Error -> Error
    end;
snapshot_position(_Conn, Scope, G, Token) ->
    verify_token(page, Scope, G, Token).

account_snapshot_result(_Conn, Scope, G, Cut, Watermark, Rows, PageSize) when
    length(Rows) > PageSize
->
    {Page, _} = lists:split(PageSize, Rows),
    #{<<"id">> := LastId} = lists:last(Page),
    case issue_token(page, Scope, G, Cut, LastId, Watermark) of
        {ok, Token} -> {ok, G, Page, {more, Token}};
        {error, _} = Error -> Error
    end;
account_snapshot_result(Conn, Scope, G, Cut, Watermark, Rows, _PageSize) ->
    case snapshot_tail_available(Conn, Scope, G, Watermark) of
        ok ->
            case issue_token(complete, Scope, G, Cut, Cut, Watermark) of
                {ok, Token} -> {ok, G, Rows, {complete, Watermark, Token}};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec confirm_snapshot(integer(), binary(), binary(), non_neg_integer(), pos_integer(), binary()) ->
    {ok, pos_integer(), boolean(), non_neg_integer(), non_neg_integer()} | {error, atom()}.
confirm_snapshot(AccountId, Domain, ClientInstance, Watermark, ExpectedGeneration, Token) ->
    Scope = {AccountId, Domain, ClientInstance},
    with_snapshot_generation(AccountId, Domain, fun
        (Conn, G) when G =:= ExpectedGeneration ->
            case verify_token(complete, Scope, G, Token) of
                {ok, {Cut, Cut, Watermark}} -> confirm_snapshot_tx(Conn, Scope, G, Watermark);
                {ok, _} -> {error, malformed_request};
                {error, _} = Error -> Error
            end;
        (_Conn, _G) ->
            {error, generation_expired}
    end).

confirm_snapshot_tx(Conn, {AccountId, Domain, ClientInstance} = Scope, G, Watermark) ->
    case personal_history_repo:find_cursor_tx(Conn, AccountId, Domain, G, ClientInstance) of
        {ok, #{<<"acked_cursor">> := Acked, <<"change_cursor">> := Issued}} when
            Acked >= Watermark
        ->
            {ok, G, true, Acked, Issued};
        {ok, _} ->
            case snapshot_tail_available(Conn, Scope, G, Watermark) of
                ok ->
                    ok = personal_history_repo:ensure_cursor_tx(
                        Conn, AccountId, Domain, G, ClientInstance
                    ),
                    ok = personal_history_repo:record_issued_tx(
                        Conn, AccountId, Domain, G, ClientInstance, Watermark
                    ),
                    acknowledge_snapshot_tx(Conn, AccountId, Domain, G, ClientInstance, Watermark);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

acknowledge_snapshot_tx(Conn, AccountId, Domain, G, ClientInstance, Watermark) ->
    case
        personal_history_repo:ack_cursor_tx(Conn, AccountId, Domain, G, ClientInstance, Watermark)
    of
        {ok, #{<<"acked_cursor">> := Acked, <<"change_cursor">> := Issued}} ->
            {ok, G, true, Acked, Issued};
        {ok, none} ->
            {error, transient_failure};
        {error, _} = Error ->
            Error
    end.

snapshot_tail_available(Conn, {AccountId, Domain, _}, G, Watermark) ->
    case
        personal_history_repo:changes_exceed_window_tx(
            Conn, AccountId, Domain, G, Watermark, personal_history_ds:change_window()
        )
    of
        {ok, false} -> ok;
        {ok, true} -> {error, rescan_bound_exceeded};
        {error, _} = Error -> Error
    end.

with_snapshot_generation(AccountId, Domain, Run) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
                {ok, #{<<"state">> := <<"ON">>, <<"generation">> := G}} ->
                    Run(Conn, G);
                {ok, #{<<"state">> := State}} when
                    State =:= <<"OFF_PURGE_PENDING">>; State =:= <<"OFF_PURGED">>
                ->
                    {error, generation_expired};
                {ok, _} ->
                    {error, not_enabled};
                {error, _} = Error ->
                    Error
            end
        end)
    of
        {rollback, Reason} when is_atom(Reason) -> {error, Reason};
        {rollback, _} -> {error, transient_failure};
        Result -> Result
    end.

-define(TTL_MS, 600000).
-define(WIRE_BYTES, 98).

-type purpose() :: page | complete.
-type scope() :: {pos_integer(), binary(), binary()}.
-type position() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-spec issue_token(
    purpose(),
    scope(),
    pos_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer()
) -> {ok, binary()} | {error, transient_failure}.
issue_token(Purpose, Scope, Generation, Cut, After, Watermark) ->
    case key(Purpose, Scope) of
        {ok, Key} ->
            {ok, seal(Key, Generation, Cut, After, Watermark, erlang:system_time(millisecond))};
        {error, _} = Error ->
            Error
    end.

-spec verify_token(purpose(), scope(), pos_integer(), binary()) ->
    {ok, position()} | {error, generation_expired | page_token_expired | transient_failure}.
verify_token(Purpose, Scope, Generation, Token) ->
    case key(Purpose, Scope) of
        {ok, Key} ->
            open(Key, Generation, Token, erlang:system_time(millisecond));
        {error, _} = Error ->
            Error
    end.

key(Purpose, Scope) ->
    case config_ds:env(imboy, postgre_aes_key, <<>>) of
        Master when is_binary(Master), byte_size(Master) >= 32 ->
            {ok, scoped_key(Master, Purpose, Scope)};
        _ ->
            {error, transient_failure}
    end.

scoped_key(Master, Purpose, Scope) ->
    Binding = term_to_binary({Purpose, Scope}),
    crypto:mac(
        hmac, sha256, Master, <<"imboy:personal-history:account-snapshot:v1", Binding/binary>>
    ).

seal(Key, Generation, Cut, After, Watermark, Issued) ->
    Payload = <<1, Generation:64, Cut:64, After:64, Watermark:64, Issued:64>>,
    Mac = crypto:mac(hmac, sha256, Key, Payload),
    base64:encode(<<Payload/binary, Mac/binary>>, #{mode => urlsafe, padding => false}).

open(Key, Generation, Token, Now) when is_binary(Token), byte_size(Token) =:= ?WIRE_BYTES ->
    case unpack(Token) of
        <<Payload:41/binary, Mac:32/binary>> ->
            case crypto:hash_equals(Mac, crypto:mac(hmac, sha256, Key, Payload)) of
                true -> verified_position(Payload, Generation, Now);
                false -> {error, page_token_expired}
            end;
        _ ->
            {error, page_token_expired}
    end;
open(_Key, _Generation, _Token, _Now) ->
    {error, page_token_expired}.

unpack(Token) ->
    try
        base64:decode(Token, #{mode => urlsafe, padding => false})
    catch
        error:_ -> invalid
    end.

verified_position(<<1, G:64, Cut:64, After:64, Watermark:64, Issued:64>>, G, Now) when
    After =< Cut, Issued =< Now, Now - Issued =< ?TTL_MS
->
    {ok, {Cut, After, Watermark}};
verified_position(<<1, Other:64, _/binary>>, G, _Now) when Other =/= G ->
    {error, generation_expired};
verified_position(_Payload, _Generation, _Now) ->
    {error, page_token_expired}.

-ifdef(EUNIT).
snapshot_token_test() ->
    Master = <<0:256>>,
    Scope = {14, <<"deployment">>, <<"device">>},
    PageKey = scoped_key(Master, page, Scope),
    CompleteKey = scoped_key(Master, complete, Scope),
    Token = seal(PageKey, 7, 21000, 10050, 23000, 100),
    ?assertEqual({ok, {21000, 10050, 23000}}, open(PageKey, 7, Token, 600100)),
    ?assertEqual({error, page_token_expired}, open(PageKey, 7, Token, 600101)),
    ?assertEqual({error, page_token_expired}, open(PageKey, 7, Token, 99)),
    ?assertEqual({error, generation_expired}, open(PageKey, 8, Token, 101)),
    ?assertEqual({error, page_token_expired}, open(CompleteKey, 7, Token, 101)),
    lists:foreach(
        fun(WrongScope) ->
            WrongKey = scoped_key(Master, page, WrongScope),
            ?assertEqual({error, page_token_expired}, open(WrongKey, 7, Token, 101))
        end,
        [
            {15, <<"deployment">>, <<"device">>},
            {14, <<"other">>, <<"device">>},
            {14, <<"deployment">>, <<"other-device">>}
        ]
    ),
    <<_:8, Rest/binary>> = Token,
    ?assertEqual({error, page_token_expired}, open(PageKey, 7, <<"$", Rest/binary>>, 101)),
    ?assertEqual({error, page_token_expired}, open(PageKey, 7, <<>>, 101)).
-endif.
