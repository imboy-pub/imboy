%% ========================================================================
%% 简化版ID段管理 - 通过配置文件管理
%% ========================================================================
%%
%% 说明：不使用复杂的数据库函数，而是通过应用配置管理机房
%%
%% 使用方式：
%%   1. 每个机房在配置文件中指定自己的ID范围
%%   2. 应用启动时自动配置序列
%%   3. 零数据库改动（只需修改序列配置）
%%
%% ========================================================================

-module(elib_id_config).
-export([init_sequences/1]).
-export([get_dc_info/0]).

%% 获取当前机房配置
get_dc_info() ->
    #{
        datacenter_id => application:get_env(imboy, datacenter_id, 1),
        datacenter_name => application:get_env(imboy, datacenter_name, <<"default">>),
        tables => application:get_env(imboy, id_tables, [
            <<"user">>, <<"user_device">>, <<"user_friend">>,
            <<"group">>, <<"group_member">>,
            <<"msg_c2c">>, <<"msg_c2g">>, <<"msg_c2s">>,
            <<"attachment">>, <<"conversation">>
        ])
    }.

%% 初始化所有序列
init_sequences(DatacenterId) ->
    #{tables := Tables} = get_dc_info(),

    % 计算序列起始值
    StartId = (DatacenterId - 1) * 100000 + 1,

    lists:foreach(fun(TableName) ->
        SeqName = <<TableName/binary, "_id_seq">>,
        SQL1 = io_lib:format("
            ALTER SEQUENCE IF EXISTS ~w RESTART WITH ~p;
        ", [SeqName, StartId]),

        SQL2 = io_lib:format("
            CREATE SEQUENCE IF NOT EXISTS ~w START ~p;
        ", [SeqName, StartId]),

        % 执行SQL
        case elib_pg:query(SQL2, []) of
            {ok, _} -> ok;
            {error, {error, <<"42704">>, _, _}} ->
                % 序列已存在，重置
                elib_pg:query(SQL1, []);
            {error, Reason} ->
                {error, Reason}
        end
    end, Tables),

    {ok, StartId}.
