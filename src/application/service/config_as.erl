-module (config_as).
%%%
% config_as 是 config application service 缩写
%%%
-export ([get/1]).

get(Key) ->
    {ok, _FieldList, [[Val]]} = config_repo:get_by_key(Key),
    % lager:info("~p", Val),
    Val.



% select(Table, Field, Where, Param) ->
%     Field2 = case string:find(Field, ",") of
%         nomatch ->
%             string:join(Field, ",");
%         _ ->
%             Field
%     end,
%     Query = ["SELECT", Field2, "FROM", Table, "WHERE", Where],
%     Query2 = string:join(Query, " "),
%     lager:info("Field2: ~p, Query2: ~p", [Field2, Query2]),
%     {ok, Rows} = mysql_poolboy:query(pool1, Query2, Param),
%     Rows.
