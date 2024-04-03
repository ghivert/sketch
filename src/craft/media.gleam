//// Defines media queries directly with functions.

import craft/size.{type Size, to_string as to_str}
import gleam/string

pub opaque type ColorMode {
  Dark
  Light
}

pub opaque type Query {
  MaxWidth(Size)
  MinWidth(Size)
  MaxHeight(Size)
  MinHeight(Size)
  ColorScheme(ColorMode)
  And(Query, Query)
  Or(Query, Query)
  Not(Query)
  Orientation(String)
}

pub fn dark_theme() {
  ColorScheme(Dark)
}

pub fn light_theme() {
  ColorScheme(Light)
}

pub fn max_width(size) {
  MaxWidth(size)
}

pub fn min_width(size) {
  MinWidth(size)
}

pub fn max_height(size) {
  MaxHeight(size)
}

pub fn min_height(size) {
  MinHeight(size)
}

pub fn and(first: Query, second: Query) {
  And(first, second)
}

pub fn or(first: Query, second: Query) {
  Or(first, second)
}

pub fn not(query: Query) {
  Not(query)
}

pub fn landscape() {
  Orientation("landscape")
}

pub fn portrait() {
  Orientation("portrait")
}

fn q_to_str(query: Query) {
  case query {
    ColorScheme(Dark) -> "(prefers-color-scheme: dark)"
    ColorScheme(Light) -> "(prefers-color-scheme: light)"
    MaxWidth(s) -> string.join(["(max-width: ", to_str(s), ")"], "")
    MinWidth(s) -> string.join(["(min-width: ", to_str(s), ")"], "")
    MaxHeight(s) -> string.join(["(max-height: ", to_str(s), ")"], "")
    MinHeight(s) -> string.join(["(min-height: ", to_str(s), ")"], "")
    Orientation(o) -> string.join(["(orientation: ", o, ")"], "")
    Not(q) -> string.append("not ", q_to_str(q))
    And(fst, snd) -> string.join([q_to_str(fst), "and", q_to_str(snd)], " ")
    Or(fst, snd) -> string.join([q_to_str(fst), "or", q_to_str(snd)], " ")
  }
}

/// Mainly internal function, can be used if you need to go from a media query to a String
/// in case you're building on top of craft.
pub fn to_string(query: Query) {
  let content = q_to_str(query)
  string.append("@media ", content)
}
