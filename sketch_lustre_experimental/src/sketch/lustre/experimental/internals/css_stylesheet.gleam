import gleam/dynamic.{type Dynamic}

pub type Kind {
  Document
  ShadowRoot(root: Dynamic)
}

pub fn create(kind: Kind) {
  case kind {
    Document -> create_document_stylesheet()
    ShadowRoot(root) -> create_shadow_root_stylesheet(root)
  }
}

@external(javascript, "../../../../css-stylesheet.ffi.mjs", "createDocument")
fn create_document_stylesheet() -> Dynamic {
  dynamic.from(0)
}

@external(javascript, "../../../../css-stylesheet.ffi.mjs", "createRoot")
fn create_shadow_root_stylesheet(_root: Dynamic) -> Dynamic {
  dynamic.from(0)
}

@external(javascript, "../../../../css-stylesheet.ffi.mjs", "replaceSync")
pub fn replace(_content: String, _stylesheet: Dynamic) -> Nil {
  Nil
}
