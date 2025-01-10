//// `Length` defines a [CSS Unit](https://developer.mozilla.org/docs/Web/CSS/CSS_Values_and_Units).
//// It can be either `px`, `pt`, `vh`, `vw`, `em`, `rem`, `lh`, `rlh`, `ch`, `%`.
////
//// To instanciate a `Length`, use the corresponding functions. Every unit exposes
//// two functions: the Int function (like `px(0)`) and the Float version suffixed
//// by an underscore (like `px_(0.0)`).
////
//// ---
////
//// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length)

import gleam/float
import gleam/int
import gleam/string

/// `Length` defines a [CSS Unit](https://developer.mozilla.org/docs/Web/CSS/CSS_Values_and_Units).
/// It can be either `px`, `pt`, `vh`, `vw`, `em`, `rem`, `lh`, `rlh`, `ch`, `%`.
///
/// To instanciate a `Length`, use the corresponding functions. Every unit exposes
/// two functions: the Int function (like `px(0)`) and the Float version suffixed
/// by an underscore (like `px_(0.0)`).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length)
pub opaque type Length {
  Px(Float)
  Cm(Float)
  Mm(Float)
  Q(Float)
  In(Float)
  Pc(Float)
  Pt(Float)
  Vh(Float)
  Vw(Float)
  Em(Float)
  Rem(Float)
  Lh(Float)
  Rlh(Float)
  Ch(Float)
  Pct(Float)
  Cap(Float)
  Ex(Float)
  Ic(Float)
  Rcap(Float)
  Rch(Float)
  Rex(Float)
  Ric(Float)
  Vmax(Float)
  Vmin(Float)
  Vb(Float)
  Vi(Float)
  Cqw(Float)
  Cqh(Float)
  Cqi(Float)
  Cqb(Float)
  Cqmin(Float)
  Cqmax(Float)
}

/// One pixel. For screen displays, it traditionally represents one device pixel
/// (dot). However, for _printers_ and _high-resolution screens_, one CSS pixel
/// implies multiple device pixels. `1px = 1in / 96`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#px)
pub fn px(value: Int) -> Length {
  Px(int.to_float(value))
}

/// One pixel. For screen displays, it traditionally represents one device pixel
/// (dot). However, for _printers_ and _high-resolution screens_, one CSS pixel
/// implies multiple device pixels. `1px = 1in / 96`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#px)
pub fn px_(value: Float) -> Length {
  Px(value)
}

/// One centimeter. `1cm = 96px / 2.54`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cm)
pub fn cm(value: Int) -> Length {
  Cm(int.to_float(value))
}

/// One centimeter. `1cm = 96px / 2.54`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cm)
pub fn cm_(value: Float) -> Length {
  Cm(value)
}

/// One millimeter. `1mm = 1cm / 10`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#mm)
pub fn mm(value: Int) -> Length {
  Mm(int.to_float(value))
}

/// One millimeter. `1mm = 1cm / 10`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#mm)
pub fn mm_(value: Float) -> Length {
  Mm(value)
}

/// One quarter of a millimeter. `1Q = 1cm / 40`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#q)
pub fn q(value: Int) -> Length {
  Q(int.to_float(value))
}

/// One quarter of a millimeter. `1Q = 1cm / 40`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#q)
pub fn q_(value: Float) -> Length {
  Q(value)
}

/// One inch. `1in = 2.54cm = 96px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#in)
pub fn in(value: Int) -> Length {
  In(int.to_float(value))
}

/// One inch. `1in = 2.54cm = 96px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#in)
pub fn in_(value: Float) -> Length {
  In(value)
}

/// One pica. `1pc = 12pt = 1in / 6`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#pc)
pub fn pc(value: Int) -> Length {
  Pc(int.to_float(value))
}

/// One pica. `1pc = 12pt = 1in / 6`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#pc)
pub fn pc_(value: Float) -> Length {
  Pc(value)
}

/// One point. `1pt = 1in / 72`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#pt)
pub fn pt(value: Int) -> Length {
  Pt(int.to_float(value))
}

/// One point. `1pt = 1in / 72`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#pt)
pub fn pt_(value: Float) -> Length {
  Pt(value)
}

/// Represents the "cap height" (nominal height of capital letters) of the element's
/// [font](https://developer.mozilla.org/docs/Web/CSS/font).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cap)
pub fn cap(value: Float) -> Length {
  Cap(value)
}

/// Represents a percentage value. It is often used to define a size as relative
/// to an element's parent object. Numerous properties can use percentages,
/// such as `width`, `height`, `margin`, `padding`, and `font-size`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/percentage)
pub fn percent(value: Int) -> Length {
  Pct(int.to_float(value))
}

/// Represents a percentage value. It is often used to define a size as relative
/// to an element's parent object. Numerous properties can use percentages,
/// such as `width`, `height`, `margin`, `padding`, and `font-size`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/percentage)
pub fn percent_(value: Float) -> Length {
  Pct(value)
}

/// Represents a percentage of the height of the viewport's initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block).
/// `1cqh` is 1% of the query container's height. For example, if the viewport height is
/// `300px`, then a value of `70vh` on a property will be `210px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vh)
pub fn vh(value: Int) -> Length {
  Vh(int.to_float(value))
}

/// Represents a percentage of the height of the viewport's initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block).
/// `1cqh` is 1% of the query container's height. For example, if the viewport height is
/// `300px`, then a value of `70vh` on a property will be `210px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vh)
pub fn vh_(value: Float) -> Length {
  Vh(value)
}

/// Represents a percentage of the width of the viewport's initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block).
/// `1vw` is 1% of the viewport width. For example, if the viewport width is
/// `800px`, then a value of `50vh` on a property will be `400px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vw)
pub fn vw(value: Int) -> Length {
  Vw(int.to_float(value))
}

/// Represents a percentage of the width of the viewport's initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block).
/// `1vw` is 1% of the viewport width. For example, if the viewport width is
/// `800px`, then a value of `50vh` on a property will be `400px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vw)
pub fn vw_(value: Float) -> Length {
  Vw(value)
}

/// Represents in percentage the larget of `vw` and `vh`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vmax)
pub fn vmax(value: Int) -> Length {
  Vmax(int.to_float(value))
}

/// Represents in percentage the larget of `vw` and `vh`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vmax)
pub fn vmax_(value: Float) -> Length {
  Vmax(value)
}

/// Represents in percentage the smallest of `vw` and `vh`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vmin)
pub fn vmin(value: Int) -> Length {
  Vmin(int.to_float(value))
}

/// Represents in percentage the smallest of `vw` and `vh`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vmin)
pub fn vmin_(value: Float) -> Length {
  Vmin(value)
}

/// Represents the percentage of the size of the initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block),
/// in the direction of the root element's [block axis](https://developer.mozilla.org/docs/Web/CSS/CSS_logical_properties_and_values).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vb)
pub fn vb(value: Int) -> Length {
  Vb(int.to_float(value))
}

/// Represents the percentage of the size of the initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block),
/// in the direction of the root element's [block axis](https://developer.mozilla.org/docs/Web/CSS/CSS_logical_properties_and_values).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vb)
pub fn vb_(value: Float) -> Length {
  Vb(value)
}

/// Represents the percentage of the size of the initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block),
/// in the direction of the root element's [inline axis](https://developer.mozilla.org/docs/Web/CSS/CSS_logical_properties_and_values).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vi)
pub fn vi(value: Int) -> Length {
  Vi(int.to_float(value))
}

/// Represents the percentage of the size of the initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block),
/// in the direction of the root element's [inline axis](https://developer.mozilla.org/docs/Web/CSS/CSS_logical_properties_and_values).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#vi)
pub fn vi_(value: Float) -> Length {
  Vi(value)
}

/// Represents a percentage of the height of the viewport's initial
/// [containing block](https://developer.mozilla.org/docs/Web/CSS/Containing_block).
/// `1vh` is 1% of the viewport height. For example, if the query container's height is
/// `300px`, then a value of `70vh` on a property will be `210px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqh)
pub fn cqh(value: Int) -> Length {
  Cqh(int.to_float(value))
}

/// Represents a percentage of the height of the query container.
/// `1cqh` is 1% of the query container's height. For example, if the query container's height is
/// `300px`, then a value of `70vh` on a property will be `210px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqh)
pub fn cqh_(value: Float) -> Length {
  Cqh(value)
}

/// Represents a percentage of the width of the query container.
/// `1cqw` is 1% of the query container's width. For example, if the query container's width is
/// `800px`, then a value of `50vh` on a property will be `400px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqw)
pub fn cqw(value: Int) -> Length {
  Cqw(int.to_float(value))
}

/// Represents a percentage of the width of the query container.
/// `1cqw` is 1% of the query container's width. For example, if the query container's width is
/// `800px`, then a value of `50vh` on a property will be `400px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqw)
pub fn cqw_(value: Float) -> Length {
  Cqw(value)
}

/// Represents a percentage of the larger value of either the query container's
/// inline size or block size. `1cqmax` is 1% of the larger value of either the
/// query container's inline size or block size. For example, if the query
/// container's inline size is `800px` and its block size is `300px`, then a
/// value of `50cqmax` on a property will be `400px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqmax)
pub fn cqmax(value: Int) -> Length {
  Cqmax(int.to_float(value))
}

/// Represents a percentage of the larger value of either the query container's
/// inline size or block size. `1cqmax` is 1% of the larger value of either the
/// query container's inline size or block size. For example, if the query
/// container's inline size is `800px` and its block size is `300px`, then a
/// value of `50cqmax` on a property will be `400px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqmax)
pub fn cqmax_(value: Float) -> Length {
  Cqmax(value)
}

/// Represents a percentage of the smaller value of either the query container's
/// inline size or block size. `1cqmin` is 1% of the smaller value of either the
/// query container's inline size or block size. For example, if the query
/// container's inline size is `800px` and its block size is `300px`, then a
/// value of `50cqmin` on a property will be `150px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqmin)
pub fn cqmin(value: Int) -> Length {
  Cqmin(int.to_float(value))
}

/// Represents a percentage of the smaller value of either the query container's
/// inline size or block size. `1cqmin` is 1% of the smaller value of either the
/// query container's inline size or block size. For example, if the query
/// container's inline size is `800px` and its block size is `300px`, then a
/// value of `50cqmin` on a property will be `150px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqmin)
pub fn cqmin_(value: Float) -> Length {
  Cqmin(value)
}

/// Represents a percentage of the block size of the query container. `1cqb` is
/// 1% of the query container's block size. For example, if the query container's
/// block size is `300px`, then a value of `10cqb` on a property will be `30px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqb)
pub fn cqb(value: Int) -> Length {
  Cqb(int.to_float(value))
}

/// Represents a percentage of the block size of the query container. `1cqb` is
/// 1% of the query container's block size. For example, if the query container's
/// block size is `300px`, then a value of `10cqb` on a property will be `30px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqb)
pub fn cqb_(value: Float) -> Length {
  Cqb(value)
}

/// Represents a percentage of the inline size of the query container. `1cqi`
/// is 1% of the query container's inline size. For example, if the query container's
/// inline size is `800px`, then a value of `50cqi` on a property will be `400px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqi)
pub fn cqi(value: Int) -> Length {
  Cqi(int.to_float(value))
}

/// Represents a percentage of the inline size of the query container. `1cqi`
/// is 1% of the query container's inline size. For example, if the query container's
/// inline size is `800px`, then a value of `50cqi` on a property will be `400px`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#cqi)
pub fn cqi_(value: Float) -> Length {
  Cqi(value)
}

/// Represents the calculated `font-size` of the element. If used on the
/// `font-size` property itself, it represents the _inherited_ font-size of
/// the element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#em)
pub fn em(value: Float) -> Length {
  Em(value)
}

/// Represents the [x-height](https://en.wikipedia.org/wiki/X-height) of the
/// element's `font`. In fonts with the `x` letter, this is generally the height
/// of lowercase letters in the font; `1ex ≈ 0.5em` in many fonts.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#ex)
pub fn ex(value: Float) -> Length {
  Ex(value)
}

/// Represents the [x-height](https://en.wikipedia.org/wiki/X-height) of the
/// root element's `font`. In fonts with the `x` letter, this is generally the height
/// of lowercase letters in the font; `1ex ≈ 0.5em` in many fonts.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#rex)
pub fn rex(value: Float) -> Length {
  Rex(value)
}

/// Equal to the used [advance measure](https://developer.mozilla.org/docs/Glossary/Advance_measure)
/// of the "水" glyph (CJK water ideograph, U+6C34), found in the font used to
/// render it.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#ic)
pub fn ic(value: Float) -> Length {
  Ic(value)
}

/// Equal to the value of `ic` unit on the root element's font.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#ric)
pub fn ric(value: Float) -> Length {
  Ric(value)
}

/// Represents the `font-size` of the root element (typically `<html>`). When
/// used within the root element `font-size`, it represents its initial value.
/// A common browser default is `16px`, but user-defined preferences may
/// modify this.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#rem)
pub fn rem(value: Float) -> Length {
  Rem(value)
}

/// Equal to the width or the
/// [advance measure](https://developer.mozilla.org/docs/Glossary/Advance_measure)
/// of the glyph `0` (zero, the Unicode character U+0030) in the root
/// element's `font`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#rch)
pub fn rch(value: Float) -> Length {
  Rch(value)
}

/// Equal to the "cap height" (nominal height of capital letters) of the root
/// element's `font`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#rcap)
pub fn rcap(value: Float) -> Length {
  Rcap(value)
}

/// Equal to the computed value of the `line-height` property of the element on
/// which it is used, converted to an absolute length. This unit enables length
/// calculations based on the theoretical size of an ideal empty line. However,
/// the size of actual line boxes may differ based on their content.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#lh)
pub fn lh(value: Float) -> Length {
  Lh(value)
}

/// Equal to the value of `lh` unit on the root element's font. This unit enables
/// length calculations based on the theoretical size of an ideal empty line.
/// However, the size of actual line boxes may differ based on their content.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#rlh)
pub fn rlh(value: Float) -> Length {
  Rlh(value)
}

/// Represents the width or, more precisely, the
/// [advance measure](https://developer.mozilla.org/docs/Glossary/Advance_measure)
/// of the glyph `0` (zero, the Unicode character U+0030) in the element's `font`.
/// In cases where determining the measure of the `0` glyph is impossible or
/// impractical, it must be assumed to be `0.5em` wide by `1em` tall.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/length#ch)
pub fn ch(value: Float) -> Length {
  Ch(value)
}

/// Internal function, can be used if you need to go from a Length to a String
/// in case you're building on top of sketch.
@internal
pub fn to_string(size) -> String {
  case size {
    Px(value) -> string.append(float.to_string(value), "px")
    Pt(value) -> string.append(float.to_string(value), "pt")
    Pct(value) -> string.append(float.to_string(value), "%")
    Vh(value) -> string.append(float.to_string(value), "vh")
    Vw(value) -> string.append(float.to_string(value), "vw")
    Em(value) -> string.append(float.to_string(value), "em")
    Rem(value) -> string.append(float.to_string(value), "rem")
    Lh(value) -> string.append(float.to_string(value), "lh")
    Rlh(value) -> string.append(float.to_string(value), "rlh")
    Ch(value) -> string.append(float.to_string(value), "ch")
    Cap(value) -> string.append(float.to_string(value), "cap")
    Cm(value) -> string.append(float.to_string(value), "cm")
    Cqb(value) -> string.append(float.to_string(value), "cqb")
    Cqh(value) -> string.append(float.to_string(value), "cqh")
    Cqi(value) -> string.append(float.to_string(value), "cqi")
    Cqmax(value) -> string.append(float.to_string(value), "cqmax")
    Cqmin(value) -> string.append(float.to_string(value), "cqmin")
    Cqw(value) -> string.append(float.to_string(value), "cqw")
    Ex(value) -> string.append(float.to_string(value), "ex")
    Ic(value) -> string.append(float.to_string(value), "ic")
    In(value) -> string.append(float.to_string(value), "in")
    Mm(value) -> string.append(float.to_string(value), "mm")
    Pc(value) -> string.append(float.to_string(value), "pc")
    Q(value) -> string.append(float.to_string(value), "q")
    Rcap(value) -> string.append(float.to_string(value), "rcap")
    Rch(value) -> string.append(float.to_string(value), "rch")
    Rex(value) -> string.append(float.to_string(value), "rex")
    Ric(value) -> string.append(float.to_string(value), "ric")
    Vb(value) -> string.append(float.to_string(value), "vb")
    Vi(value) -> string.append(float.to_string(value), "vi")
    Vmax(value) -> string.append(float.to_string(value), "vmax")
    Vmin(value) -> string.append(float.to_string(value), "vmin")
  }
}
