"""Offline isolated-PG input shape checks; never authorize or launch resources."""
import json
import os
import re
from typing import Mapping, TypedDict


class PgInputResult(TypedDict):
    scope: str
    errors: list[str]
    resource_authorized: bool
    backend_ready: bool
    product_pass: bool


def validate_inputs(values: Mapping[str, str]) -> PgInputResult:
    errors = []
    image = values.get('ACCEPTANCE_PG_IMAGE_ID')
    if (not isinstance(image, str) or not re.fullmatch(r'sha256:[0-9a-f]{64}', image)
            or image == 'sha256:' + '0' * 64):
        errors.append('PG_IMAGE_ID_INVALID')
    port = values.get('ACCEPTANCE_PG_PORT')
    if (not isinstance(port, str) or not re.fullmatch(r'[1-9][0-9]{0,4}', port)
            or not 1024 <= int(port) <= 65535 or int(port) in {4323, 15432, 5432}):
        errors.append('PG_PORT_INVALID_OR_SHARED')
    run = values.get('ACCEPTANCE_RUN_ID')
    if not isinstance(run, str) or not re.fullmatch(r'[a-z][a-z0-9-]{2,47}', run):
        errors.append('PG_RUN_ID_INVALID')
    elif values.get('ACCEPTANCE_COMPOSE_PROJECT') != 'imboy-acceptance-' + run:
        errors.append('PG_PROJECT_RUN_MISMATCH')
    password = values.get('ACCEPTANCE_PG_PASSWORD')
    if not isinstance(password, str) or len(password) < 24 or '\x00' in password:
        errors.append('PG_SYNTHETIC_PASSWORD_INVALID')
    return dict(scope='OFFLINE_ISOLATED_PG_INPUT_SHAPE_ONLY', errors=errors,
                resource_authorized=False, backend_ready=False, product_pass=False)


def main() -> int:
    result = validate_inputs(os.environ)
    print(json.dumps(result, sort_keys=True))
    return 64 if result['errors'] else 0


if __name__ == '__main__':
    raise SystemExit(main())
