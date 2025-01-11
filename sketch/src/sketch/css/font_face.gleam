import gleam/float

/// A `FontFace` is a part of a `@font-face` rule.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face)
pub opaque type FontFace {
  AscentOverride(Float)
  DescentOverride(Float)
  FontDisplay(String)
  FontFamily(String)
  FontStretch(String)
  FontStyle(String)
  FontWeight(String)
  FontFeatureSettings(String)
  FontVariationSettings(String)
  LineGapOverride(Float)
  SizeAdjust(Float)
  Src(String)
  UnicodeRange(String)
}

/// The `ascent-override` CSS descriptor for the @font-face at-rule defines the
/// ascent metric for the font. The ascent metric is the height above the
/// baseline that CSS uses to lay out line boxes in an inline formatting
/// context. \
/// If not defined, will default to `normal`.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/ascent-override)
pub fn ascent_override(value: Float) {
  AscentOverride(value)
}

/// The `descent-override` CSS descriptor for the @font-face at-rule defines the
/// descent metric for the font. The descent metric is the height below the
/// baseline that CSS uses to lay out line boxes in an inline formatting
/// context. \
/// If not defined, will default to `normal`.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/descent-override)
pub fn descent_override(value: Float) {
  DescentOverride(value)
}

/// The `font-display` descriptor for the `@font-face` at-rule determines how a
/// font face is displayed based on whether and when it is downloaded and ready
/// to use.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/font-display)
pub fn font_display(value: String) {
  FontDisplay(value)
}

/// The `font-family`` CSS descriptor sets the font family for a font specified
/// in an `@font-face` at-rule.
///
/// The value is used for name matching against a particular `@font-face` when
/// styling elements using the `font-family` property. Any name may be used,
/// and this overrides any name specified in the underlying font data.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/font-family)
pub fn font_family(name: String) {
  FontFamily(name)
}

/// The `font-stretch` CSS descriptor allows authors to specify a normal,
/// condensed, or expanded face for the fonts specified in the `@font-face`
/// at-rule.
///
/// For a particular font family, authors can download various font faces which
/// correspond to the different styles of the same font family, and then use the
/// `font-stretch` descriptor to explicitly specify the font face's stretch. The
/// values for the CSS descriptor is same as that of its corresponding font
/// property.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/font-stretch)
pub fn font_stretch(value: String) {
  FontStretch(value)
}

/// The `font-style` CSS descriptor allows authors to specify font styles for
/// the fonts specified in the `@font-face` at-rule.
///
/// For a particular font family, authors can download various font faces that
/// correspond to the different styles of the same font family and then use the
/// `font-style` descriptor to explicitly specify the font face's style. The
/// values for this CSS descriptor are the same as that of the corresponding
/// `font-style` property.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/font-style)
pub fn font_style(value: String) {
  FontStyle(value)
}

/// The `font-weight` CSS `@font-face` descriptor enables authors to specify a
/// single font weight, or a range of font weights, for the font specified in a
/// `@font-face` at-rule. This is then used by the browser to select the
/// appropriate font when a CSS rule sets a desired font weight.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/font-weight)
pub fn font_weight(value: String) {
  FontWeight(value)
}

/// The `font-feature-settings` CSS descriptor allows you to define the initial
/// settings to use for the font defined by the `@font-face` at-rule. You can
/// further use this descriptor to control typographic font features such as
/// ligatures, small caps, and swashes, for the font defined by `@font-face`.
/// The values for this descriptor are the same as the `font-feature-settings`
/// property, except for the global keyword values.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/font-feature-settings)
pub fn font_feature_settings(value: String) {
  FontFeatureSettings(value)
}

/// The `font-variation-settings` CSS descriptor allows authors to specify
/// low-level OpenType or TrueType font variations in the `@font-face` at-rule.
/// The values for this descriptor are the same as the `font-variation-settings`
/// property, except for the global keyword values.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/font-variation-settings)
pub fn font_variation_settings(value: String) {
  FontVariationSettings(value)
}

/// The `line-gap-override` CSS descriptor for the `@font-face` at-rule defines
/// the `line-gap` metric for the font. The `line-gap` metric is the font
/// recommended `line-gap` or external leading. \
/// If not defined, will default to `normal`.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/line-gap-override)
pub fn line_gap_override(value: Float) {
  LineGapOverride(value)
}

/// The `size-adjust` CSS descriptor for the `@font-face` at-rule defines a
/// multiplier for glyph outlines and metrics associated with this font. This
/// makes it easier to harmonize the designs of various fonts when rendered at
/// the same font size.
///
/// The `size-adjust` descriptor behaves in a similar fashion to the
/// `font-size-adjust` property. It calculates an adjustment per font by
/// matching ex heights.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/size-adjust)
pub fn size_adjust(value: Float) {
  SizeAdjust(value)
}

/// The `src` CSS descriptor for the `@font-face` at-rule specifies the resource
/// containing font data. It is required for the `@font-face` rule to be valid.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/src)
pub fn src(value: String) {
  Src(value)
}

/// The `unicode-range` CSS descriptor sets the specific range of characters to
/// be used from a font defined using the `@font-face` at-rule and made
/// available for use on the current page. If the page doesn't use any character
/// in this range, the font is not downloaded; if it uses at least one, the
/// whole font is downloaded.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@font-face/unicode-range)
pub fn unicode_range(value: String) {
  UnicodeRange(value)
}

/// Internal function, can be used if you need to go from a FontFace to a
/// String in case you're building on top of sketch.
@internal
pub fn to_string(font_face: FontFace) {
  case font_face {
    AscentOverride(value) ->
      "ascent-override: " <> float.to_string(value) <> "%"
    DescentOverride(value) ->
      "descent-override: " <> float.to_string(value) <> "%"
    FontDisplay(value) -> "font-display: " <> value
    FontFamily(value) -> "font-family: " <> value
    FontFeatureSettings(value) -> "font-feature-settings: " <> value
    FontStretch(value) -> "font-stretch: " <> value
    FontStyle(value) -> "font-style: " <> value
    FontVariationSettings(value) -> "font-variation-settings: " <> value
    FontWeight(value) -> "font-weight: " <> value
    LineGapOverride(value) ->
      "line-gap-override: " <> float.to_string(value) <> "%"
    SizeAdjust(value) -> "size-adjust: " <> float.to_string(value) <> "%"
    Src(value) -> "src: " <> value
    UnicodeRange(value) -> "unicode-range: " <> value
  }
}
