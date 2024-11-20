pub type Timeout

@external(javascript, "./landing_page.ffi.mjs", "setTimeout")
pub fn set_timeout(callback: fn() -> Nil, timeout: Int) -> Timeout

@external(javascript, "./landing_page.ffi.mjs", "clearTimeout")
pub fn clear_timeout(timeout: Timeout) -> Nil

@external(javascript, "./landing_page.ffi.mjs", "clipboardCopy")
pub fn clipboard_copy(text: String) -> Nil

@external(javascript, "./landing_page.ffi.mjs", "highlight")
pub fn highlight(code: String) -> String

@external(javascript, "./landing_page.ffi.mjs", "highlightCss")
pub fn highlight_css(code: String) -> String

@external(javascript, "./landing_page.ffi.mjs", "highlightXml")
pub fn highlight_xml(code: String) -> String

@external(javascript, "./landing_page.ffi.mjs", "scrollTo")
pub fn scroll_to(id: String) -> Nil
