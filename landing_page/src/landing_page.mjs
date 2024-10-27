import gleamHljs from '@gleam-lang/highlight.js-gleam'
import hljs from 'highlight.js/lib/core'
import plaintext from 'highlight.js/lib/languages/plaintext'
import css from 'highlight.js/lib/languages/css'
import xml from 'highlight.js/lib/languages/xml'
import * as landing from './landing_page.gleam'

hljs.registerLanguage('gleam', gleamHljs)
hljs.registerLanguage('plaintext', plaintext)
hljs.registerLanguage('css', css)
hljs.registerLanguage('xml', xml)
landing.main()
