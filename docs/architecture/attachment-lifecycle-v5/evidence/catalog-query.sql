SELECT version();
SELECT extname,extversion FROM pg_extension ORDER BY 1;
SELECT table_name,column_name,data_type,is_nullable,column_default FROM information_schema.columns WHERE table_schema='public' AND (table_name IN ('attachment','attach_pending','enterprise_asset','enterprise_asset_delete_queue','enterprise_attachment_retention','enterprise_retention_hold','enterprise_retention_policy','group_file','user_collect','channel_message','moment') OR table_name LIKE 'cs_%asset%') ORDER BY 1,ordinal_position;
SELECT conrelid::regclass,conname,contype,convalidated,pg_get_constraintdef(oid) FROM pg_constraint WHERE connamespace='public'::regnamespace AND (conrelid::regclass::text ~ 'attach|asset|retention|group_file|user_collect' OR confrelid IN ('attachment'::regclass,'enterprise_asset'::regclass)) ORDER BY 1,2;
SELECT tablename,indexname,indexdef FROM pg_indexes WHERE schemaname='public' AND tablename ~ 'attach|asset|retention|group_file|user_collect' ORDER BY 1,2;
SELECT tgrelid::regclass,tgname,pg_get_triggerdef(oid) FROM pg_trigger WHERE NOT tgisinternal AND tgrelid::regclass::text ~ 'attach|asset|retention|group_file|user_collect' ORDER BY 1,2;
SELECT proname,pg_get_functiondef(oid) FROM pg_proc WHERE pronamespace='public'::regnamespace AND proname ~ 'attach|asset|retention' ORDER BY 1;
