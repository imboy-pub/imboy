%%% Internal edge authorization: never expose storage locations to the asset API.
-module(eb_edge_asset_port).

-callback check_download(integer(), integer(), integer()) -> ok | {error, term()}.
%% Only the authenticated edge authorization response may receive this URL.
-callback presign_download(integer(), integer(), integer()) -> {ok, binary()} | {error, term()}.
