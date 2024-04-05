//// Defines Options to setup the runtime of craft.
//// The Options could right now be a `style` node injected in the DOM, or on
//// a CSSStyleSheet, adopted directly on the document.

/// Internal use.
pub opaque type StyleSheet {
  Node
  Browser
}

pub opaque type Options {
  Options(stylesheet: StyleSheet)
}

pub fn node() -> Options {
  Options(stylesheet: Node)
}

pub fn browser() -> Options {
  Options(stylesheet: Browser)
}

/// Internal function, can be used if you need to go from a StyleSheet to a String
/// in case you're building on top of craft. Used in FFI at the moment.
pub fn stylesheet_to_string(stylesheet: StyleSheet) -> String {
  case stylesheet {
    Node -> "node"
    Browser -> "browser"
  }
}
