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
    Column1 =
        <<"(md5,mime_type,ext,name,path,url,size,info,referer_time,last_referer_user_id,last_referer_at,creator_user_id,updated_at,created_at,status)">>,
    Tb1 = <<"public.attachment">>,

    UpSql1 = <<" UPDATE SET last_referer_user_id = ", Uid/binary, ", last_referer_at = ", CreatedAt/binary,
               ", updated_at = ", CreatedAt/binary, ", referer_time = public.attachment.referer_time + 1;">>,
    % imboy_log:info(io_lib:format("attachment_repo:save/4: CreatedAt ~p ~n", [CreatedAt])),

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

    % imboy_log:info(io_lib:format("attachment_repo:save/4: Attach2 ~p ~n", [Attach2])),

    % imboy_log:info(io_lib:format("attachment_repo:save/4: Sql1 before ~p ~n", [[
    %     Tb1, Column1,Md5,MimeType,Ext2,Name,Path2, Url, Size2,Uid,CreatedAt,UpSql1
    %     ]])),
    Sql1 = <<"INSERT INTO ", Tb1/binary, " ", Column1/binary, " VALUES('", Md5/binary, "', '", MimeType2/binary, "', '",
             Ext2/binary, "','", Name/binary, "', '", Path2/binary, "', '", Url/binary, "', '", Size2/binary, "', '",
             Attach2/binary, "'::text, ", "1, ",  % referer_time
             Uid/binary, ", ",  % last_referer_user_id
             CreatedAt/binary, ", ",  % last_referer_at
             Uid/binary, ", ",  % creator_user_id
             CreatedAt/binary, ", ",  % updated_at, created_at, status
             CreatedAt/binary, ", 1) ON CONFLICT (md5) DO ", UpSql1/binary>>,

    % imboy_log:info(io_lib:format("attachment_repo:save/4: Sql1 ~p ~n", [Sql1])),
    {ok, Stmt1} = epgsql:parse(Conn, Sql1),
    {ok, _} = epgsql:execute_batch(Conn, [{Stmt1, []}]),
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
