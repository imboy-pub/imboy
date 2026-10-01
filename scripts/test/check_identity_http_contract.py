"""Validate real synthetic identity HTTP response bodies against the source contract."""
import json
import pathlib
import re
import sys
import yaml
from jsonschema import Draft202012Validator


def main():
    root = pathlib.Path(__file__).resolve().parents[2]
    ignored = [str(p.relative_to(root)) for p in (root / 'src').rglob('*.erl')
               if re.search(r'_\s*=\s*enterprise_internal_idempotency:complete_tx\(', p.read_text())]
    assert not ignored, ignored
    contract = yaml.safe_load((root / 'api/paths/internal/v1/identity/mappings.yaml').read_text())
    samples = json.loads(pathlib.Path(sys.argv[1]).read_text())
    assert set(samples) == {'put_first', 'put_replay', 'delete_first', 'delete_replay'}
    for name, payload in samples.items():
        method = name.split('_', 1)[0]
        schema = contract[method]['responses']['200']['content']['application/json']['schema']
        Draft202012Validator.check_schema(schema)
        Draft202012Validator(schema).validate(payload)
    assert samples['put_first'] == samples['put_replay']
    assert samples['delete_first'] == samples['delete_replay']
    print('PASS: four real identity response bodies match the closed OpenAPI schemas')


if __name__ == '__main__':
    main()
