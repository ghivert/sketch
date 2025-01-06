@target(erlang)
pub fn multitarget_title(title: String) {
  "erlang_" <> title
}

@target(javascript)
pub fn multitarget_title(title: String) {
  "js_" <> title
}
