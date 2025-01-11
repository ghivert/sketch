import sketch/css

pub const combinators = [
  #("child", css.child), #("descendant", css.descendant),
  #("next_sibling", css.next_sibling),
  #("subsequent_sibling", css.subsequent_sibling),
]

pub const pseudo_classes = [
  #("placeholder", "::placeholder"), #("selection", "::selection"),
  #("before", "::before"), #("after", "::after"), #("backdrop", "::backdrop"),
  #("cue", "::cue"), #("first_line", "::first-line"),
  #("grammar_error", "::grammar-error"), #("spelling_error", "::spelling-error"),
  #("marker", "::marker"), #("first_letter", "::first-letter"),
  #("file_selector_button", "::file-selector-button"), #("hover", ":hover"),
  #("any_link", ":any-link"), #("active", ":active"), #("focus", ":focus"),
  #("autofill", ":autofill"), #("buffering", ":buffering"),
  #("default", ":default"), #("defined", ":defined"), #("empty", ":empty"),
  #("fullscreen", ":fullscreen"), #("in_range", ":in-range"),
  #("indeterminate", ":indeterminate"), #("muted", ":muted"),
  #("paused", ":paused"), #("playing", ":playing"), #("seeking", ":seeking"),
  #("stalled", ":stalled"), #("state", ":state"),
  #("user_invalid", ":user-invalid"), #("user_valid", ":user-valid"),
  #("volume_locked", ":volume-locked"),
  #("placeholder_shown", ":placeholder-shown"),
  #("out_of_range", ":out-of-range"), #("dir", ":dir"),
  #("focus_visible", ":focus-visible"), #("focus_within", ":focus-within"),
  #("enabled", ":enabled"), #("disabled", ":disabled"),
  #("read_only", ":read-only"), #("read_write", ":read-write"),
  #("checked", ":checked"), #("valid", ":valid"), #("invalid", ":invalid"),
  #("required", ":required"), #("optional", ":optional"), #("link", ":link"),
  #("visited", ":visited"), #("target", ":target"), #("nth_child", ":nth-child"),
  #("nth_last_child", ":nth-last-child"), #("nth_of_type", ":nth-of-type"),
  #("nth_last_of_type", ":nth-last-of-type"), #("first_child", ":first-child"),
  #("last_child", ":last-child"), #("only_child", ":only-child"),
  #("first_of_type", ":first-of-type"), #("last_of_type", ":last-of-type"),
  #("only_of_type", ":only-of-type"),
]
