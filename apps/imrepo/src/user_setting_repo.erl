-module(user_setting_repo).
%%%
% user_setting_repo 是 user_setting repository 缩写
%%%

-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([find_by_uid/2]).
-export([update/2]).


%% ===================================================================
%% API
%% ===================================================================
tablename() ->
    imboy_db:public_tablename(<<"user_setting">>).


find_by_uid(Uid, Column) when is_binary(Uid) ->
    find_by_uid(imboy_hashids:decode(Uid), Column);
find_by_uid(Uid, Column) when is_integer(Uid) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [Uid]).


% user_setting_repo:update(3, #{people_nearby_visible => true}).
update(Uid, Setting) when is_binary(Uid) ->
    update(imboy_hashids:decode(Uid), Setting);
update(Uid, Setting) when is_integer(Uid) ->
    Data = #{
        <<"user_id">> => Uid,  % 用户ID
        <<"setting">> => jsone:encode(Setting, [
            native_utf8,         % 保持UTF8编码
            {float_format, [{decimals, 4}, compact]}  % 优化浮点数格式
        ]),
        <<"updated_at">> => imboy_dt:now()  % 自动格式化为数据库时间
    },
    %% ON CONFLICT 子句
    %% 使用EXCLUDED引用新插入的值
    OnConflict = <<
        "ON CONFLICT (user_id) DO UPDATE SET\n"
        "  setting = EXCLUDED.setting,\n"
        "  updated_at = EXCLUDED.updated_at"
    >>,

    %% 执行插入/更新
    case imboy_db:insert_into(tablename(), Data, OnConflict) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            % ?DEBUG_LOG("Update setting failed: ~p", [Reason]),
            {error, Reason}
    end.
