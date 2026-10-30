"""W403 source evidence cannot be promoted without a server-bound object proof."""
import datetime
import importlib.util
from pathlib import Path
import sys
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts/test'))
spec = importlib.util.spec_from_file_location(
    'w601_w403_guard', ROOT / 'scripts/test/attachment_lifecycle_v5_adapter_w6_01_real_pg_garage_disk.py')
adapter = importlib.util.module_from_spec(spec)
spec.loader.exec_module(adapter)


class DriverProvenanceTests(unittest.TestCase):
    def test_location_trace_is_diagnostic_only_and_scoped_to_the_leaf(self):
        leaf = {'entrypoint': 'ordinary', 'driver': 'garage',
                'source_scenario': 'confirm_old_put_different_bytes'}
        identity = {'driver': 'garage', 'namespace': 'ns', 'container': 'bucket',
                    'key': 'object', 'locator_digest': 'digest'}
        entry = {'kind': 'HTTP_EVIDENCE', 'payload': {
            'leaf': 'ordinary/garage/confirm_old_put_different_bytes',
            'requests': [{'diagnostic_untrusted_locations': [{
                'trust': 'diagnostic_untrusted', 'source': 'attachment_storage:session_location/1', 'identity': identity}]}]}}
        self.assertTrue(adapter._untrusted_location_recorded({'evidence': [entry]}, leaf))
        self.assertFalse(adapter._untrusted_location_recorded({'evidence': [entry]},
                                                          dict(leaf, driver='disk')))
        item = entry['payload']['requests'][0]['diagnostic_untrusted_locations'][0]
        item.pop('trust')
        self.assertFalse(adapter._untrusted_location_recorded({'evidence': [entry]}, leaf))
        item['trust'] = 'diagnostic_untrusted'
        item['source'] = 'client:label/1'
        self.assertFalse(adapter._untrusted_location_recorded({'evidence': [entry]}, leaf))

    def test_unrelated_successful_http_cannot_be_promoted(self):
        leaf = {'source_case_ref': 'W4-03/driver-content-immutable',
                'domain': 'ordinary', 'driver': 'garage', 'entrypoint': 'ordinary',
                'ordering_or_fault': 'W4-03/driver-content-immutable/inflight_same_content',
                'observation_deadline_seconds': 1800}
        forged = {'kind': 'HTTP_EVIDENCE', 'payload': {'requests': [{
            'request_id': 'w403-fake', 'method': 'GET', 'route': '/unrelated',
            'handler': 'fake', 'status': 200}]}}
        start = datetime.datetime(2026, 1, 1, tzinfo=datetime.timezone.utc)
        finish = start + datetime.timedelta(seconds=1)
        step = {'exit': 0, 'line': 'synthetic'}
        log = {'parsed': {'markers': ['W403_DIRECT_OK']}, 'evidence': [forged]}
        ctx = {'allowlist': {}}
        with mock.patch.object(adapter.S.C, 'argv_paths', return_value=('/run', '/case', '/repo')), \
             mock.patch.object(adapter.S, 'context', return_value=ctx), \
             mock.patch.object(adapter.S, 'leaves', return_value=[leaf]), \
             mock.patch.object(adapter.S.C, 'resolve_slot', return_value='slot'), \
             mock.patch.object(adapter.S.C, 'db_resource', return_value='db'), \
             mock.patch.object(adapter.S.C, 'garage_bucket', return_value='bucket'), \
             mock.patch.object(adapter.W4, 'resolve_disk_root', return_value='/disk'), \
             mock.patch.object(adapter.S, 'step_record', return_value=step), \
             mock.patch.object(adapter.S, 'eunit_log', return_value=log), \
             mock.patch.object(adapter.S, 'step_window', return_value=(start, finish)), \
             mock.patch.object(adapter, '_obs_index', return_value=({}, '/observations')), \
             mock.patch.object(adapter.S, 'probe_file', return_value={}), \
             mock.patch.object(adapter, '_replay_count', return_value=1), \
             mock.patch.object(adapter.S, 'validate_requests', side_effect=AssertionError('must not promote raw sidecar')), \
             mock.patch.object(adapter.S, 'report_gaps', side_effect=RuntimeError('gap')) as report:
            with self.assertRaisesRegex(RuntimeError, 'gap'):
                adapter.build()
        args = report.call_args.args
        self.assertEqual(0, args[1])
        self.assertIn('W4-03/driver-content-immutable', args[2])


if __name__ == '__main__':
    unittest.main()
