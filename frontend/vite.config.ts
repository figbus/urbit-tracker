import { defineConfig } from 'vite'
import { plugin } from 'vite-plugin-elm'

export default defineConfig({
  base: "./",
  plugins: [plugin()]
})
