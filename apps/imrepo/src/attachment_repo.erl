-module(attachment_repo).
%%%
% attachment 相关操作都放到该模块，存储库模块
% attachment related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([save/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"attachment">>).


%%% 保存附近信息，不存在就新增，存在就递增应用次数
-spec save(integer(), binary(), binary(), list()) -> ok.
save(_Conn, _CreatedAt, _Uid, []) ->
    ok;
save(Conn, CreatedAt, Uid, [Attach | Tail]) ->
    Md5 = maps:get(<<"md5">>, Attach),
    MimeType = maps:get(<<"mime_type">>, Attach),
    Name = maps:get(<<"name">>, Attach),
    Path = maps:get(<<"path">>, Attach),
    Url = maps:get(<<"url">>, Attach),
    Size = maps:get(<<"size">>, Attach),
    Ext = filename:extension(Path),

    Ext2 = ec_cnv:to_binary(Ext),
    Size2 = ec_cnv:to_binary(Size),
    Path2 = ec_cnv:to_binary(Path),
    % Path2 = ec_cnv:to_binary(Path),
    Attach2 = jsone:encode(Attach),

    MimeType2 =
        case binary:split(MimeType, <<"/">>) of
            [<<"image">>, _] ->
                <<".", Ext3/binary>> = Ext2,
                <<"image/", Ext3/binary>>;
            _ ->
                MimeType
        end,

    % 拼接ON CONFLICT子句
    OnConflictUpdate = <<
        "ON CONFLICT (md5) DO UPDATE SET "
        "last_referer_user_id = EXCLUDED.last_referer_user_id, "
        "last_referer_at = EXCLUDED.last_referer_at, "
        "updated_at = EXCLUDED.updated_at, "
        "referer_time = public.attachment.referer_time + 1"
    >>,
    % <<"(,,,,,,,,,,,,updated_at,created_at,status)">>,
    Attach = #{
        <<"md5">> => Md5,
        <<"mime_type">> => MimeType2,
        <<"ext">> => Ext2,
        <<"name">> => Name,
        <<"path">> => Path2,
        <<"url">> => Url,
        <<"size">> => Size2,
        <<"info">> => Attach2,
        <<"referer_time">> => 1,                % 初始引用次数
        <<"last_referer_user_id">> => Uid,      % 使用绑定变量
        <<"last_referer_at">> => CreatedAt, % 使用原生时间格式
        <<"creator_user_id">> => Uid,
        <<"updated_at">> => CreatedAt,
        <<"created_at">> => CreatedAt,
        <<"status">> => 1
    },

    imboy_db:add(Conn, tablename(), Attach, OnConflictUpdate),
    % Res = epgsql:execute_batch(Conn, [{Stmt1, []}]),
    % imboy_log:info(io_lib:format("attachment_repo:save/4: Res ~p ~n", [Res])),
    % 递归保存附近信息
    save(Conn, CreatedAt, Uid, Tail),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
