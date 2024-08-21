import react from '@vitejs/plugin-react-swc'
import 'dotenv/config'
import { createLogger, defineConfig } from 'vite'
import gleam from 'vite-gleam'

const customLogger = createLogger()
const loggerWarn = customLogger.warn
customLogger.warn = (msg, options) => {
  if (msg.includes('import_')) return
  loggerWarn(msg, options)
}

export default defineConfig({
  customLogger,
  plugins: [gleam(), react()],
})
