-module(talk_record_routes).
-moduledoc "面聊记录正式路由清单；由全局路由树接入鉴权与限流。".
-export([routes/0]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc talk_record V1 路由三元组列表（T05：capabilities/creation-contexts/
%% 列表/创建/详情/编辑；T07：采集五端点 + recover/abandon + marks；
%% T15：内容域；T17：授权/分享/回放）。后续任务在既有 :id 子资源清单后
%% 按段继续增补。
-spec routes() -> [{binary(), module(), map()}].
routes() ->
    [
        {"/api/v1/talk-records/capabilities", talk_record_handler, #{
            action => capabilities
        }},
        {"/api/v1/talk-records/creation-contexts", talk_record_handler, #{
            action => creation_contexts
        }},
        %% T17 通用候选（§8 准备页入口）——字面量段必须排在 :id 之前
        %% （见模块头顺序说明）
        {"/api/v1/talk-records/share-candidates", talk_record_handler, #{
            action => share_candidates
        }},
        {"/api/v1/talk-records/by-client-recording-id", talk_record_recovery_handler, #{}},
        %% collection：GET → list；POST → create（handler 内按 method 分发）
        {"/api/v1/talk-records", talk_record_handler, #{action => collection}},
        %% item：GET → detail；PATCH → update
        {"/api/v1/talk-records/:id", talk_record_handler, #{action => item}},
        %% ---- T07 采集域（§4 采集五端点 + §11.1 恢复/放弃）----
        {"/api/v1/talk-records/:id/captures", talk_record_handler, #{
            action => captures_collection
        }},
        {"/api/v1/talk-records/:id/captures/:cid/events", talk_record_handler, #{
            action => capture_events
        }},
        {"/api/v1/talk-records/:id/captures/:cid/audio-parts", talk_record_handler, #{
            action => capture_audio_parts
        }},
        {"/api/v1/talk-records/:id/captures/:cid/audio-parts/:pid/confirm", talk_record_handler, #{
            action => capture_audio_part_confirm
        }},
        {"/api/v1/talk-records/:id/captures/:cid/recover", talk_record_handler, #{
            action => capture_recover
        }},
        {"/api/v1/talk-records/:id/captures/:cid/abandon", talk_record_handler, #{
            action => capture_abandon
        }},
        %% ---- T07 marks（§4；capture_id+capture_offset_ms 真源，00000213）----
        {"/api/v1/talk-records/:id/marks", talk_record_handler, #{
            action => marks_collection
        }},
        {"/api/v1/talk-records/:id/marks/:mark_id", talk_record_handler, #{
            action => marks_item
        }},
        %% ---- T15 内容域（§4：原文/发言人/摘要编辑 API）----
        {"/api/v1/talk-records/:id/segments", talk_record_handler, #{
            action => segments_collection
        }},
        {"/api/v1/talk-records/:id/segments/:segment_id", talk_record_handler, #{
            action => segments_item
        }},
        {"/api/v1/talk-records/:id/speakers", talk_record_handler, #{
            action => speakers_collection
        }},
        {"/api/v1/talk-records/:id/speakers/:speaker_id", talk_record_handler, #{
            action => speakers_item
        }},
        {"/api/v1/talk-records/:id/speakers/:speaker_id/claim", talk_record_handler, #{
            action => speaker_claim
        }},
        {"/api/v1/talk-records/:id/summary", talk_record_handler, #{
            action => summary_item
        }},
        %% ---- T17 授权/分享/回放域（§4 grants 四端点/share-card/playback +
        %% §8 record 级 share-candidates）----
        {"/api/v1/talk-records/:id/grants", talk_record_handler, #{
            action => grants_collection
        }},
        {"/api/v1/talk-records/:id/grants/:grant_id", talk_record_handler, #{
            action => grants_item
        }},
        {"/api/v1/talk-records/:id/transfer-candidates", talk_record_handler, #{
            action => transfer_candidates
        }},
        {"/api/v1/talk-records/:id/share-candidates", talk_record_handler, #{
            action => record_share_candidates
        }},
        {"/api/v1/talk-records/:id/share-card", talk_record_handler, #{
            action => share_card
        }},
        {"/api/v1/talk-records/:id/participants", talk_record_participant_handler, #{}},
        {"/api/v1/talk-records/:id/attachments/:attachment_id/governance",
            talk_record_governance_handler, #{}},
        {"/api/v1/talk-records/:id/playback/stream", talk_record_playback_handler, #{}},
        {"/api/v1/talk-records/:id/playback", talk_record_handler, #{
            action => playback
        }},
        {"/api/v1/talk-records/:id/exports", talk_record_handler, #{
            action => exports_collection
        }},
        {"/api/v1/talk-records/:id/exports/:export_id", talk_record_handler, #{
            action => export_item
        }},
        {"/api/v1/talk-records/:id/exports/:export_id/download", talk_record_handler, #{
            action => export_download
        }},
        {"/api/v1/talk-records/:id/ownership-transfers", talk_record_handler, #{
            action => ownership_transfers
        }},
        {"/api/v1/talk-records/:id/complete", talk_record_handler, #{action => complete}},
        {"/api/v1/talk-records/:id/jobs", talk_record_handler, #{action => jobs}},
        {"/api/v1/talk-records/:id/jobs/:job_id/retry", talk_record_handler, #{action => job_retry}},
        {"/api/v1/talk-records/:id/processing-requests", talk_record_handler, #{
            action => processing_requests
        }}
    ].
