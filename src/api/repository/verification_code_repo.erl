-module (verification_code_repo).
%%%
% verification_code_repo 是 verification_code_repo repository 缩写
%%%

-export ([get_by_id/1]).
-export ([save/4]).

-spec get_by_id(Id::binary) -> {ok, ColumnList::list(), Values::list()}.
get_by_id(Id) ->
    Sql = <<"SELECT `id`,`code`,`validity_at`,`created_at` FROM `verification_code` WHERE `id` = ?">>,
    Row = mysql_pool:query(Sql, [Id]),
    % lager:info("~p", Row),
    Row.


% verification_code_repo:save(<<"test@imboy.pub">>, func:num_random(6), dt_util:milliseconds() + 600000, dt_util:milliseconds()).
-spec save(ToEmail::binary(), VerifyCode::integer(), ValidityAt::integer(), Now::integer()) -> ok.
save(ToEmail, VerifyCode, ValidityAt, Now) ->
    Table = <<"`verification_code`">>,
    Column = <<"(`id`,`code`,`validity_at`,`created_at`)">>,
    VerifyCode2 = integer_to_binary(VerifyCode),
    ValidityAt2 = integer_to_binary(ValidityAt),
    Now2 = integer_to_binary(Now),
    Value = <<"('",
        ToEmail/binary, "', '",
        VerifyCode2/binary, "', '",
        ValidityAt2/binary, "', '",
        Now2/binary, "')">>,

    mysql_pool:replace_into(Table, Column, Value).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
