pub opaque type StyleSheet {
  Node
  Browser
}

pub opaque type Options {
  Options(stylesheet: StyleSheet)
}

pub fn default() {
  Options(Node)
}

pub fn node(options: Options) -> Options {
  Options(..options, stylesheet: Node)
}

pub fn browser(options: Options) -> Options {
  Options(..options, stylesheet: Browser)
}

pub fn stylesheet_to_string(stylesheet: StyleSheet) -> String {
  case stylesheet {
    Node -> "node"
    Browser -> "browser"
  }
}
