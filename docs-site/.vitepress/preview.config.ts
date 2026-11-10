import { defineConfig } from 'vite';
import docs from './config';

// 预览复用构建路径，并仅监听本机。Reuse build paths and listen on loopback.
export default defineConfig({
  appType: 'mpa',
  base: docs.base,
  build: { outDir: docs.outDir },
  preview: { host: '127.0.0.1' },
});
