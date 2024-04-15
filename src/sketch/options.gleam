//// Defines Options to setup the runtime of sketch.
//// The Options could right now be a `style` node injected in the DOM, or on
//// a CSSStyleSheet, adopted directly on the document.

import plinth/browser/shadow.{type ShadowRoot}

/// Internal use.
pub opaque type StyleSheet {
  Node
  Document
  Shadow(root: ShadowRoot)
}

pub opaque type Options {
  Options(stylesheet: StyleSheet)
}

pub fn node() -> Options {
  Options(stylesheet: Node)
}

/// document cannot be used on server.
pub fn document() -> Options {
  Options(stylesheet: Document)
}

/// shadow cannot be used on server.
pub fn shadow(root: ShadowRoot) -> Options {
  Options(stylesheet: Shadow(root: root))
}

/// Internal function, can be used if you need to go from a StyleSheet to a String
/// in case you're building on top of sketch. Used in FFI at the moment.
pub fn stylesheet_to_string(stylesheet: StyleSheet) -> String {
  case stylesheet {
    Node -> "node"
    Document -> "document"
    Shadow(_) -> "shadow-root"
  }
}
