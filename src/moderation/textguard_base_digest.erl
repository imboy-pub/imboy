-module(textguard_base_digest).
-export([digest/1]).

digest(Rows) ->
    Core = lists:sort(Rows),
    Bytes = [
        <<"TGBASEv1", 0, (length(Core)):32/big-unsigned>>,
        [
            [<<(byte_size(Column)):32/big-unsigned, Column/binary>> || Column <- tuple_to_list(Row)]
         || Row <- Core
        ]
    ],
    binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).
