import { expect, test } from 'bun:test';
import { createMarkdownRenderer } from 'vitepress';
import config from '../.vitepress/config';

const md = await createMarkdownRenderer(process.cwd(), config.markdown);

test('Erlang code and frozen filename placeholders remain literal', () => {
  const rendered = md.render('`{{rindex, key}, value}` and observations/<unique>.json');
  expect(rendered).toContain('<code v-pre>{{rindex, key}, value}</code>');
  expect(rendered).toContain('observations/&lt;unique&gt;.json');
});

test('existing inline HTML remains supported', () => {
  expect(md.render('text <em>emphasis</em>')).toContain('<em>emphasis</em>');
});
