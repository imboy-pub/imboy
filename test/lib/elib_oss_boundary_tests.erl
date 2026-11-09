-module(elib_oss_boundary_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_oss 100 MiB 上限边界三值 —— AT05 NC-1 补测。
%%%
%%% 出处：closure-20261108/AT05_ASSERTION_MAP.md NC-1（F01 行「100 MiB 边界
%%% 三值 104857599/104857600/104857601（elib_oss 入口）」在 imboy/imboyapp/
%%% imboyadmin 三端零覆盖，全部缺口中唯一「高」纯逻辑项）；
%%% TEST_PLAN.md:72 F01 对象完整性，执行归属 API/PG/S3。
%%%
%%% 产品源码合同（imboy/src/lib/elib_oss.erl）：
%%%   :39      -define(MAX_FILE_SIZE, 100 * 1024 * 1024).  %% = 104857600
%%%   :84-88   upload/2,3 尺寸门：FileSize > ?MAX_FILE_SIZE →
%%%            {error, file_too_large}；比较为严格大于，故上限值本身
%%%            可接受（含边界）。
%%%   :306-308 max_file_size/0 导出常量，供 confirm 阶段核实真实大小复用。
%%%
%%% 断言语义（从源码合同推导，与 ASSERTION_MAP §7 P1 一致）：
%%%   104857599 (UNDER) → 接受：尺寸门放行，完整对象送达（mock）存储 PUT
%%%   104857600 (EXACT) → 接受：恰在上限仍放行
%%%   104857601 (OVER)  → 拒绝：{error, file_too_large}，先于任何 garage
%%%                       配置与网络访问（mock httpc 兜底证明零外呼）
%%%
%%% 纯逻辑测试：无 PG / 无真实网络 / 无实桶。「接受」两例沿用
%%% elib_oss_tests:upload_success_returns_url_and_file_id_test_ 的既有
%%% ?WITH_MECKS 基座（elib_s3_sign + httpc）。
%%%===================================================================

%% eunit 命名组：可整组执行（escript scripts/quality.escript eunit
%% --group=elib_oss_boundary_tests），也可在模块内单独选择
%% eunit:test({elib_oss_boundary_tests, max_file_size_boundary})。
max_file_size_boundary_test_() ->
    {"elib_oss 100MiB 边界三值 (AT05-NC-1 / F01)", [
        {"常量与判定函数一致性 (max_file_size/0)", [
            {timeout, 35, ?_test(max_file_size_accessor_matches_contract())}
        ]},
        {"104857599 字节 UNDER 接受", [
            {timeout, 35, boundary_accept_fixture(104857599)}
        ]},
        {"104857600 字节 EXACT 接受", [
            {timeout, 35, boundary_accept_fixture(104857600)}
        ]},
        {"104857601 字节 OVER 拒绝", [
            {timeout, 35, boundary_reject_fixture()}
        ]}
    ]}.

%% ------------------------------------------------------------------
%% 常量与判定函数一致性：100 MiB 判定阈值在 ?MAX_FILE_SIZE 宏（头文件
%% 常量）+ max_file_size/0（导出访问器），此处锁定访问器与卡面三值
%% 字面量的关系（NC-1 要求测常量与判定函数一致性时按此展开）。
%% ------------------------------------------------------------------

max_file_size_accessor_matches_contract() ->
    %% ① 访问器 = 卡面合同常量：100 MiB = 104857600 字节
    ?assertEqual(104857600, elib_oss:max_file_size()),
    ?assertEqual(100 * 1024 * 1024, elib_oss:max_file_size()),
    %% ② 三值字面量锚定在判定阈值上：UNDER = Max-1、EXACT = Max、OVER = Max+1
    ?assertEqual(104857599, elib_oss:max_file_size() - 1),
    ?assertEqual(104857601, elib_oss:max_file_size() + 1).

%% ------------------------------------------------------------------
%% UNDER / EXACT 接受：尺寸门放行 → mime 白名单（text/plain）→
%% mock 存储 PUT 返回 200 → {ok, Url, FileId}。
%% mock 内同时断言送达 PUT 的正文 byte_size 与卡面值逐字节相等
%% （放行即完整对象上送，不得静默截断/丢包）。
%% ------------------------------------------------------------------

boundary_accept_fixture(Size) ->
    ?WITH_MECKS(
        [
            {elib_s3_sign, [
                {'format_amz_date', 1, fun(_) -> <<"20260528T103000Z">> end},
                {'authorization_header', 7, fun(_, _, _, _, _, _, _) ->
                    <<"AWS4-HMAC-SHA256 Cred=test, Sig=abc">>
                end}
            ]},
            {httpc, [
                {'request', 4, fun(put, {_Url, _Headers, _CT, Body}, _Opts, _HttpOpts) ->
                    %% 尺寸门放行 ⇒ 送达存储 PUT 的必须是完整 Size 字节对象
                    ?assertEqual(Size, byte_size(Body)),
                    {ok, {{<<"HTTP/1.1">>, 200, <<"OK">>}, [], <<>>}}
                end}
            ]}
        ],
        fun() ->
            application:set_env(imboy, garage, #{
                endpoint => <<"http://127.0.0.1:3900">>,
                bucket => <<"imboy">>,
                access_key => <<"GKtest">>,
                secret_key => <<"testsecret">>
            }),
            Bin = binary:copy(<<0>>, Size),
            Result = elib_oss:upload(Bin, <<"boundary.bin">>, #{mime_type => <<"text/plain">>}),
            %% NC-1 合同：Size <= 100 MiB（含上限本身）一律放行并确认
            ?assertMatch({ok, _, _}, Result),
            {ok, Url, FileId} = Result,
            %% 确认 URL 指向本桶对象、含本次上传 FileId（确认对象=本次声明对象）
            ?assertMatch(<<"http://127.0.0.1:3900/imboy/", _/binary>>, Url),
            ?assert(binary:match(Url, FileId) =/= nomatch),
            ?assertMatch(
                match, re:run(FileId, <<"^file_[0-9]+_[0-9]+$">>, [{capture, none}])
            )
        end
    ).

%% ------------------------------------------------------------------
%% OVER 拒绝：Size > ?MAX_FILE_SIZE 在读 garage 配置 / 触网之前即拒
%% （elib_oss.erl:85-88 早于 assert_garage_configured/upload_to_storage）。
%% httpc mock 返回哨兵错误：合同破坏时断言失败信息直接指向
%% 「越界对象本不该到达网络」。
%% ------------------------------------------------------------------

boundary_reject_fixture() ->
    ?WITH_MECKS(
        [
            {httpc, [
                {'request', 4, fun(_Method, _Req, _Opts, _HttpOpts) ->
                    {error, must_not_reach_network_on_oversize}
                end}
            ]}
        ],
        fun() ->
            Bin = binary:copy(<<0>>, 104857601),
            %% NC-1 合同：Size = Max+1 必拒 file_too_large
            ?assertEqual(
                {error, file_too_large},
                elib_oss:upload(Bin, <<"over.bin">>, #{mime_type => <<"text/plain">>})
            )
        end
    ).
