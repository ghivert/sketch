//// Define media queries directly with functions.
//// Refer to the sketch module to get more details on the usage.
////
//// ## Advanced usage
////
//// Media queries can be rather complex, and the module tries to give all
//// features in a usable way. A media query takes form `(property: value)` and
//// can be combined, like `(orientation: landscape) or (min-width: 1000px)`.
//// Those media queries can be created by using the corresponding `and`,
//// `or` or `not` functions.
////
//// ---
////
//// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#all)

import sketch/css/length.{type Length, to_string as to_str}

pub opaque type ColorMode {
  Dark
  Light
}

/// Media queries can be rather complex, and the module tries to give all
/// features in a usable way. A media query takes form `(property: value)` and
/// can be combined, like `(orientation: landscape) or (min-width: 1000px)`.
/// Those media queries can be created by using the corresponding `and`,
/// `or` or `not` functions.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#all)
pub opaque type Query {
  MaxWidth(Length)
  MinWidth(Length)
  MaxHeight(Length)
  MinHeight(Length)
  ColorScheme(ColorMode)
  And(Query, Query)
  Or(Query, Query)
  Not(Query)
  Orientation(String)
  Screen
  Print
  All
  Only(Query)
}

/// The `prefers-color-scheme` CSS media feature is used to detect if a user
/// has requested light or dark color themes. A user indicates their preference
/// through an operating system setting (e.g. light or dark mode) or a user
/// agent setting.
///
/// Automatically detects dark theme.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme)
pub fn dark_theme() -> Query {
  ColorScheme(Dark)
}

/// The `prefers-color-scheme` CSS media feature is used to detect if a user
/// has requested light or dark color themes. A user indicates their preference
/// through an operating system setting (e.g. light or dark mode) or a user
/// agent setting.
///
/// Automatically detects dark theme.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme)
pub fn light_theme() -> Query {
  ColorScheme(Light)
}

/// Defines maximum width of the style. Devices with a width lesser than
/// `max_width` will be applied.
pub fn max_width(length: Length) -> Query {
  MaxWidth(length)
}

/// Defines minimum width of the style. Devices with a width greater than
/// `min_width` will be applied.
pub fn min_width(length) -> Query {
  MinWidth(length)
}

/// Defines maximum height of the style. Devices with a height lesser than
/// `max_height` will be applied.
pub fn max_height(length) -> Query {
  MaxHeight(length)
}

/// Defines minimum height of the style. Devices with a height greater than
/// `max_height` will be applied.
pub fn min_height(length) -> Query {
  MinHeight(length)
}

/// A `<media-type>` describes the general category of a device. Except when using
/// the `only` logical operator, the media type is optional and the `all` type
/// is implied.
///
/// Intended primarily for screens.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#screen)
pub fn screen() -> Query {
  Screen
}

/// A `<media-type>` describes the general category of a device. Except when using
/// the `only` logical operator, the media type is optional and the `all` type
/// is implied.
///
/// Intended for paged material and documents viewed on a screen in print preview
/// mode. (Please see [paged media](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_paged_media)
/// for information about formatting issues that are specific to these formats.)
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#screen)
pub fn print() -> Query {
  Print
}

/// A `<media-type>` describes the general category of a device. Except when using
/// the `only` logical operator, the media type is optional and the `all` type
/// is implied.
///
/// Suitable for all devices.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#all)
pub fn all() -> Query {
  All
}

/// Used for combining multiple media features together into a single media query,
/// requiring each chained feature to return `true` for the query to be `true`.
/// It is also used for joining media features with media types.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#and)
pub fn and(first: Query, second: Query) -> Query {
  And(first, second)
}

/// Applies a style only if an entire query matches. It is useful for preventing
/// older browsers from applying selected styles. When not using `only`, older
/// browsers would interpret the query `screen and (max-width: 500px)` as `screen`,
/// ignoring the remainder of the query, and applying its styles on all screens.
/// If you use the `only` operator, you _must also_ specify a media type.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#only)
pub fn only(query: Query) -> Query {
  Only(query)
}

/// Used to combine multiple media queries into a single rule. Each query in a
/// `or` list is treated separately from the others Thus, if any of the queries
/// in a list is `true`, the entire media statement returns `true`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#or)
pub fn or(first: Query, second: Query) -> Query {
  Or(first, second)
}

/// Used to negate a media query, returning `true` if the query would otherwise
/// return `false`. If present in a `or` list of queries, it will
/// only negate the specific query to which it is applied.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media#not)
pub fn not(query: Query) -> Query {
  Not(query)
}

/// The `orientation` CSS media feature can be used to test the orientation of
/// the viewport (or the page box, for
/// [paged media](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_paged_media)).
///
/// The viewport is in a landscape orientation, i.e., the width is greater than the height.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/orientation#landscape)
pub fn landscape() -> Query {
  Orientation("landscape")
}

/// The `orientation` CSS media feature can be used to test the orientation of
/// the viewport (or the page box, for
/// [paged media](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_paged_media)).
///
/// The viewport is in a portrait orientation, i.e., the height is greater than or equal to the width.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/orientation#portrait)
pub fn portrait() -> Query {
  Orientation("portrait")
}

fn q_to_str(query: Query) -> String {
  case query {
    ColorScheme(Dark) -> "(prefers-color-scheme: dark)"
    ColorScheme(Light) -> "(prefers-color-scheme: light)"
    MaxWidth(s) -> "(max-width: " <> to_str(s) <> ")"
    MinWidth(s) -> "(min-width: " <> to_str(s) <> ")"
    MaxHeight(s) -> "(max-height: " <> to_str(s) <> ")"
    MinHeight(s) -> "(min-height: " <> to_str(s) <> ")"
    Orientation(o) -> "(orientation: " <> o <> ")"
    Not(q) -> "not " <> q_to_str(q)
    Screen -> "screen"
    Print -> "print"
    All -> "all"
    And(fst, snd) -> q_to_str(fst) <> " and " <> q_to_str(snd)
    Or(fst, snd) -> q_to_str(fst) <> " or " <> q_to_str(snd)
    Only(q) -> "only " <> q_to_str(q)
  }
}

/// Internal function, can be used if you need to go from a media query to a String
/// in case you're building on top of sketch.
@internal
pub fn to_string(query: Query) -> String {
  let content = q_to_str(query)
  "@media " <> content
}
