-module(attachment_repo_mime_tests).
%%%
% attachment_repo mime 归一化单元测试（2026-09-11 moya 报障根因修复）
% 历史行为：image/* 用 object_key 扩展名重写子类型（image/jpeg + .jpg →
% image/jpg，非法子类型）。修复后：仅归一化已知别名，标准值原样落库。
% 落库全链（confirm HEAD → save）的端到端验证见 moya backend-e2e 场景⑨。
%%%

-include_lib("eunit/include/eunit.hrl").

standard_mime_untouched_test() ->
    ?assertEqual(<<"image/jpeg">>, attachment_repo:normalize_image_mime(<<"image/jpeg">>)),
    ?assertEqual(<<"image/png">>, attachment_repo:normalize_image_mime(<<"image/png">>)),
    ?assertEqual(<<"image/webp">>, attachment_repo:normalize_image_mime(<<"image/webp">>)),
    ?assertEqual(<<"video/mp4">>, attachment_repo:normalize_image_mime(<<"video/mp4">>)),
    ?assertEqual(
        <<"application/pdf">>, attachment_repo:normalize_image_mime(<<"application/pdf">>)
    ).

legacy_jpg_alias_normalized_test() ->
    %% 历史脏值经新写入路径（转发/收藏复制）时归一为标准子类型
    ?assertEqual(<<"image/jpeg">>, attachment_repo:normalize_image_mime(<<"image/jpg">>)),
    ?assertEqual(<<"image/tiff">>, attachment_repo:normalize_image_mime(<<"image/tif">>)).
