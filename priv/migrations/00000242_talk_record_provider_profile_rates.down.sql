ALTER TABLE public.talk_record_provider_task DROP CONSTRAINT provider_task_profile_exact;
ALTER TABLE public.talk_record_provider_task ADD CONSTRAINT provider_task_profile_exact CHECK
 (request_profile='{"audio":{"bits":16,"channel":1,"format":"aac","rate":16000},"request":{"enable_ddc":false,"enable_itn":true,"enable_punc":true,"enable_speaker_info":true,"model_name":"bigmodel","show_utterances":true,"ssd_version":"300"}}'::jsonb
 AND profile_hash='d723bf03c21bf27f6d35a38d6134c2cdf33ebe4a6700b35c8bd98ec27fc4cfab');
