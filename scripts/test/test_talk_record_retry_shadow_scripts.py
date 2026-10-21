"""Offline script structure/mutant scope; not PostgreSQL execution."""
from pathlib import Path
import importlib.util
import unittest
ROOT=Path(__file__).resolve().parents[2]
spec=importlib.util.spec_from_file_location('shadow',Path(__file__).with_name('run_talk_record_retry_shadow_checks.py'))
shadow=importlib.util.module_from_spec(spec);spec.loader.exec_module(shadow)

class ShadowScriptsTests(unittest.TestCase):
    def test_each_case_is_independent_and_unprivileged(self):
        for case in shadow.FUNCTIONS:
            text=shadow.script(case,False)
            self.assertEqual(text.count('BEGIN;'),1)
            self.assertEqual(text.count('ROLLBACK;'),1)
            before,worker=text.split('SET LOCAL ROLE v5_worker;',1)
            self.assertIn('dedicated queue not quiescent',before)
            self.assertNotIn('LIKE public.',worker)
            self.assertNotIn('SELECT 1 FROM public.attachment_object_deletion',worker)
            self.assertIn('CREATE TEMP TABLE attachment_object_deletion (',worker)
            self.assertIn('SHADOW_ORACLE:',worker)
            self.assertIn('MUTANT_INCOMPLETE:',worker)
    def test_mutates_exactly_one_protocol(self):
        for case,name in shadow.FUNCTIONS.items():
            text=shadow.script(case,True)
            self.assertEqual(text.count('CREATE OR REPLACE FUNCTION public.fn_v5_'),1)
            self.assertIn('CREATE OR REPLACE FUNCTION public.'+name,text)
            self.assertIn('search_path=public',text)
            self.assertIn('ROLLBACK;',text)
    def test_ack_exercises_matching_leased_audit_path(self):
        text=shadow.script('ack',True)
        self.assertIn("'t20-temp-shadow-ack','owned','leased'",text)
        self.assertIn('attachment_lifecycle_v5_audit:v5_fn_owner',text)
        self.assertIn("public.fn_v5_ack_ordinary('t20-temp-shadow-ack','11111111-2222-4333-8444-555555555555',true)",text)
    def test_expected_mutant_is_not_any_failure(self):
        source=Path(shadow.__file__).read_text()
        self.assertIn("rejected='SHADOW_ORACLE:' in proc.stderr",source)
        self.assertIn("'TEST_FAILURE'",source)
        self.assertIn("'EXPECTED_REJECTION'",source)

if __name__=='__main__': unittest.main()
