import { defineConfig } from 'vite'
import gleam from 'vite-gleam'
import react from '@vitejs/plugin-react-swc'

export default defineConfig(({}) => ({
  plugins: [gleam(), react()],
  build: {
    sourcemap: true,
    rollupOptions: {
      output: {
        interop: 'auto',
      },
    },
  },
}))
