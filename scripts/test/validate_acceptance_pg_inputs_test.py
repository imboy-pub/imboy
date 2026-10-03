"""Offline boundary tests; no Docker, database or device access."""
import importlib.util
from pathlib import Path
import unittest

PATH = Path(__file__).resolve().parents[1] / 'validate_acceptance_pg_inputs.py'
SPEC = importlib.util.spec_from_file_location('pg_inputs', PATH)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def inputs():
    return dict(ACCEPTANCE_PG_IMAGE_ID='sha256:' + 'a' * 64,
                ACCEPTANCE_PG_PORT='25439', ACCEPTANCE_RUN_ID='synthetic-run',
                ACCEPTANCE_COMPOSE_PROJECT='imboy-acceptance-synthetic-run',
                ACCEPTANCE_PG_PASSWORD='synthetic-only-password-for-offline-tests')


class PgInputsTest(unittest.TestCase):
    def test_shape_never_grants_authorization(self):
        result = MODULE.validate_inputs(inputs())
        self.assertEqual(result['errors'], [])
        self.assertFalse(result['resource_authorized'])
        self.assertFalse(result['backend_ready'])

    def test_missing_values_are_rejected(self):
        for key in inputs():
            value = inputs()
            del value[key]
            self.assertTrue(MODULE.validate_inputs(value)['errors'], key)

    def test_mutable_or_invalid_image_rejected(self):
        for image in ['imboy/pg18:3.6.1-2', 'sha256:' + 'A' * 64, 'sha256:' + '0' * 64]:
            value = inputs()
            value['ACCEPTANCE_PG_IMAGE_ID'] = image
            self.assertIn('PG_IMAGE_ID_INVALID', MODULE.validate_inputs(value)['errors'])

    def test_shared_and_noncanonical_ports_rejected(self):
        for port in ['4323', '15432', '5432', '025439', '0', '65536', '80', 'x']:
            value = inputs()
            value['ACCEPTANCE_PG_PORT'] = port
            self.assertIn('PG_PORT_INVALID_OR_SHARED', MODULE.validate_inputs(value)['errors'])

    def test_project_must_match_run(self):
        value = inputs()
        value['ACCEPTANCE_COMPOSE_PROJECT'] = 'shared-project'
        self.assertIn('PG_PROJECT_RUN_MISMATCH', MODULE.validate_inputs(value)['errors'])

    def test_error_output_does_not_echo_password(self):
        value = inputs()
        value['ACCEPTANCE_PG_PASSWORD'] = 'sensitive-marker'
        result = MODULE.validate_inputs(value)
        self.assertIn('PG_SYNTHETIC_PASSWORD_INVALID', result['errors'])
        self.assertNotIn(value['ACCEPTANCE_PG_PASSWORD'], str(result))


if __name__ == '__main__':
    unittest.main()
