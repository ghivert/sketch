//// Craft is a small module providing CSS-in-Gleam in its simpler form.
//// Craft does not try to add complicated API on top of CSS. If you have CSS
//// knowledge, you'll feel right at home, with all the niceties offered by
//// Craft, i.e. type-checking of sizes and push-to-browser stylesheets of your
//// classes, as well as SSR support.
////
//// Craft has currently two run modes: directly in your browser and leverages on
//// all abilities of the JS runtime, and on backend, to leverages on SSR.
////
//// Craft has currently to way to use it: directly in your vanilla Gleam
//// application or in your fully-featured [Lustre](https://hexdocs.pm/lustre/) application.
//// Craft allows you to build two types of CSS classes: dynamic ones, changing
//// over time, and static ones, compiled once and for all, and reused during the
//// entire lifetime of the application, just like classic CSS stylesheets.
////
//// ## Compiling static classes
////
//// Craft exposes a single function [`class`](#class) allowing you to build your
//// class. The first time your function is called, the corresponding styles will
//// be compiled into CSS rules, and pushed in your browser or your SSR stylesheet.
//// Every time you'll call the function in the future, no computation will be done,
//// the class name will be returned, thanks to memoization.
////
//// ```
//// import craft
////
//// fn my_class() -> String {
////   craft.class([
////     craft.display("flex"),
////     craft.flex_direction("column"),
////   ])
////   |> craft.to_class_name()
//// }
//// ```
////
//// ## Compiling dynamic classes
////
//// Craft exposes another function [`variable`](#variable) allowing you to build a
//// dynamic class, changing over time. Each time the function is called, the
//// properties in the declaration will be compiled into CSS, the previous class
//// will be wiped from the browser, and the new one will pushed.
////
//// ```
//// import craft
////
//// fn my_variable_class(is_column: Bool) -> String {
////   craft.variable([
////     craft.display("flex"),
////     case is_column {
////       True -> craft.flex_direction("column")
////       False -> craft.flex_direction("row")
////     }
////   ])
////   |> craft.to_class_name()
//// }
//// ```
////
//// ## Usage with Lustre
////
//// [Lustre](https://hexdocs.pm/lustre/) is the main framework for frontend
//// development in Gleam. Because of this, craft provides a function to directly
//// use classes in Lustre views: [`to_lustre()`](#to_lustre). Just use it in place
//// of [`to_class_name()`](#to_class_name) to get a Lustre attribute and use it
//// in your views.
////
//// ```
//// import craft
//// import lustre/element/html
////
//// // With a pipeline.
//// fn my_view() {
////   [craft.background("red")]
////   |> craft.class()
////   |> craft.to_lustre()
////   |> list.repeat(1)
////   |> html.div(_, [])
//// }
////
//// // With a variable class.
//// fn my_other_view(model: Bool) {
////   let color = case model {
////     True -> "red"
////     False -> "blue"
////   }
////   html.div(
////    [craft.to_lustre(craft.variable([craft.background(color)]))],
////    [],
////   )
//// }
//// ```
////
//// ## Using media queries and pseudo-selectors
////
//// Because we're in CSS-in-Gleam, we can leverage on the full CSS power,
//// contrarily to inline styling. This mean we can use media queries and pseudo-selectors!
//// You only need to call the proper functions, and craft will take care of the rest.
////
//// ```
//// import craft
//// import craft/media
//// import craft/size.{px}
////
//// fn my_class() {
////   craft.class([
////     craft.display("flex"),
////     craft.flex_direction("row"),
////     craft.background("red"),
////     craft.hover([
////       craft.background("blue"),
////     ]),
////     craft.media(media.max_width(px(320)), [
////       craft.flex_direction("column"),
////       craft.hover([
////         craft.background("green"),
////       ]),
////     ]),
////   ])
////   |> craft.to_lustre()
//// }
//// ```
////
//// The example above will be compiled to the following CSS.
////
//// ```
//// .css-001 {
////   display: flex;
////   flex-direction: row;
////   background: red;
//// }
////
//// .css-001:hover {
////   background: blue;
//// }
////
//// @media (max-width: 320px) {
////   .css-001 {
////     flex-direction: column;
////   }
////
////   .css-001:hover {
////     background: green;
////   }
//// }
//// ```
////
//// ## Some opinions on properties
////
//// A lot of properties are accessible directly through the `craft` package.
//// But with time, some could be added, and new features for existing features
//// can appear. That's why craft will never try to be on your way: at any time
//// you can access [`property()`](#property), which allows you to push any
//// arbitrary property in a class. Another thing is that craft will always let
//// you access raw, low-level properties. If you're trying to use something like
//// `craft.width("auto")` and the property does not support String, look for a
//// variant with an underscore (`_`), it should fullfill your needs, like
//// `craft.width_("auto")`!
//// In case something is missing or a property does not have its underscore
//// alternative, [open an issue — or better, a PR — on the repo!](https://github.com/ghivert/craft)

import gleam/io
import gleam/list
import gleam/int
import gleam/string
import lustre/attribute.{type Attribute}
import craft/media.{type Query}
import craft/size.{type Size}
import craft/options.{type Options}

// Types
// Most of them are opaque because they're just JS types from the FFI.
// No one should use them directly outside of this package.
// If you end up here reading this because you want to access internals,
// consider forking the repo and working on it on your own, or submit a PR!

type Cache

pub opaque type Class

pub opaque type Style(media, pseudo) {
  ClassName(class_name: String)
  Media(query: String, styles: List(Style(NoMedia, PseudoSelector)))
  PseudoSelector(
    pseudo_selector: String,
    styles: List(Style(NoMedia, NoPseudoSelector)),
  )
  Property(key: String, value: String, important: Bool)
}

// Phantom types
// Used in styles in order to cancel using medias in medias.
// They have no utilities outside of the type namespace.

pub opaque type Media

pub opaque type NoMedia

pub opaque type PseudoSelector

pub opaque type NoPseudoSelector

pub type MediaStyle =
  Style(NoMedia, PseudoSelector)

pub type PseudoStyle =
  Style(NoMedia, NoPseudoSelector)

// FFI
// Used exclusively in the package.
// They should never be exposed.

@external(javascript, "./craft.ffi.mjs", "compileClass")
fn compile_class(styles: List(Style(media, pseudo))) -> Class

@external(javascript, "./craft.ffi.mjs", "compileClass")
fn compile_style(styles: List(Style(media, pseudo)), id: String) -> Class

@external(javascript, "./craft.ffi.mjs", "memo")
fn memo(class: Class) -> Class

@external(javascript, "./craft.ffi.mjs", "toString")
fn to_string(class: Class) -> String

@external(javascript, "./cache.ffi.mjs", "createCache")
fn create_cache(options: Options) -> Cache

@external(javascript, "./cache.ffi.mjs", "prepareCache")
fn prepare_cache(cache: Cache) -> Nil

@external(javascript, "./cache.ffi.mjs", "renderCache")
fn render_cache(cache: Cache) -> Nil

// Properties
// All the properties accessible for the user.
// All properties must have a low-level String interface.

pub fn width(width: Size) {
  Property("width", size.to_string(width), False)
}

pub fn width_(width: String) {
  Property("width", width, False)
}

pub fn max_width(width: Size) {
  Property("max-width", size.to_string(width), False)
}

pub fn max_width_(width: String) {
  Property("max-width", width, False)
}

pub fn min_width(width: Size) {
  Property("min-width", size.to_string(width), False)
}

pub fn min_width_(width: String) {
  Property("min-width", width, False)
}

pub fn height(height: Size) {
  Property("height", size.to_string(height), False)
}

pub fn height_(height: String) {
  Property("height", height, False)
}

pub fn max_height(height: Size) {
  Property("max-height", size.to_string(height), False)
}

pub fn max_height_(height: String) {
  Property("max-height", height, False)
}

pub fn min_height(height: Size) {
  Property("min-height", size.to_string(height), False)
}

pub fn min_height_(height: String) {
  Property("min-height", height, False)
}

pub fn color(color: String) {
  Property("color", color, False)
}

pub fn font_family(font_family: String) {
  Property("font-family", font_family, False)
}

pub fn font_style(font_style: String) {
  Property("font-style", font_style, False)
}

pub fn font_size(font_size: Size) {
  Property("font-size", size.to_string(font_size), False)
}

pub fn font_weight(font_weight: String) {
  Property("font-weight", font_weight, False)
}

pub fn letter_spacing(letter_spacing: String) {
  Property("letter-spacing", letter_spacing, False)
}

pub fn line_break(line_break: String) {
  Property("line-break", line_break, False)
}

pub fn line_height(line_height: String) {
  Property("line-height", line_height, False)
}

pub fn text_align(text_align: String) {
  Property("text-align", text_align, False)
}

pub fn text_decoration(text_decoration: String) {
  Property("text-decoration", text_decoration, False)
}

pub fn text_justify(text_justify: String) {
  Property("text-justify", text_justify, False)
}

pub fn text_overflow(text_overflow: String) {
  Property("text-overflow", text_overflow, False)
}

pub fn text_transform(text_transform: String) {
  Property("text-transform", text_transform, False)
}

pub fn white_space(white_space: String) {
  Property("white-space", white_space, False)
}

pub fn white_space_collapse(white_space_collapse: String) {
  Property("white-space-collapse", white_space_collapse, False)
}

pub fn word_break(word_break: String) {
  Property("word-break", word_break, False)
}

pub fn word_spacing(word_spacing: String) {
  Property("word-spacing", word_spacing, False)
}

pub fn word_wrap(word_wrap: String) {
  Property("word-wrap", word_wrap, False)
}

pub fn list_style(list_style: String) {
  Property("list-style", list_style, False)
}

pub fn list_style_image(list_style_image: String) {
  Property("list-style-image", list_style_image, False)
}

pub fn list_style_position(list_style_position: String) {
  Property("list-style-position", list_style_position, False)
}

pub fn list_style_type(list_style_type: String) {
  Property("list-style-type", list_style_type, False)
}

pub fn display(display: String) {
  Property("display", display, False)
}

pub fn z_index(z_index: Int) {
  Property("z-index", int.to_string(z_index), False)
}

pub fn visibility(visibility: String) {
  Property("visibility", visibility, False)
}

pub fn background(background: String) {
  Property("background", background, False)
}

pub fn object_fit(object_fit: String) {
  Property("object-fit", object_fit, False)
}

pub fn object_position(object_position: String) {
  Property("object-position", object_position, False)
}

pub fn opacity(opacity: String) {
  Property("opacity", opacity, False)
}

pub fn pointer_events(pointer_events: String) {
  Property("pointer-events", pointer_events, False)
}

pub fn user_select(user_select: String) {
  Property("user-select", user_select, False)
}

pub fn position(position: String) {
  Property("position", position, False)
}

pub fn outline(outline: String) {
  Property("outline", outline, False)
}

pub fn outline_color(outline_color: String) {
  Property("outline-color", outline_color, False)
}

pub fn outline_offset(outline_offset: String) {
  Property("outline-offset", outline_offset, False)
}

pub fn outline_style(outline_style: String) {
  Property("outline-style", outline_style, False)
}

pub fn outline_width(outline_width: String) {
  Property("outline-width", outline_width, False)
}

pub fn offset(offset: String) {
  Property("offset", offset, False)
}

pub fn offset_anchor(offset_anchor: String) {
  Property("offset-anchor", offset_anchor, False)
}

pub fn offset_distance(offset_distance: String) {
  Property("offset-distance", offset_distance, False)
}

pub fn offset_path(offset_path: String) {
  Property("offset-path", offset_path, False)
}

pub fn offset_position(offset_position: String) {
  Property("offset-position", offset_position, False)
}

pub fn offset_rotate(offset_rotate: String) {
  Property("offset-rotate", offset_rotate, False)
}

pub fn gap(gap: Size) {
  Property("gap", size.to_string(gap), False)
}

pub fn ga_(gap: String) {
  Property("gap", gap, False)
}

pub fn column_gap(column_gap: Size) {
  Property("column-gap", size.to_string(column_gap), False)
}

pub fn row_gap(row_gap: Size) {
  Property("row-gap", size.to_string(row_gap), False)
}

pub fn grid_area(grid_area: String) {
  Property("grid-area", grid_area, False)
}

pub fn grid_column(grid_column: String) {
  Property("grid-column", grid_column, False)
}

pub fn grid_row(grid_row: String) {
  Property("grid-row", grid_row, False)
}

pub fn grid_template(grid_template: String) {
  Property("grid-template", grid_template, False)
}

pub fn grid_auto_columns(grid_auto_columns: String) {
  Property("grid-auto-columns", grid_auto_columns, False)
}

pub fn grid_auto_rows(grid_auto_rows: String) {
  Property("grid-auto-rows", grid_auto_rows, False)
}

pub fn grid_auto_flow(grid_auto_flow: String) {
  Property("grid-auto-flow", grid_auto_flow, False)
}

pub fn grid_template_areas(grid_template_areas: String) {
  Property("grid-template-areas", grid_template_areas, False)
}

pub fn grid_template_columns(grid_template_columns: String) {
  Property("grid-template-columns", grid_template_columns, False)
}

pub fn grid_template_rows(grid_template_rows: String) {
  Property("grid-template-rows", grid_template_rows, False)
}

pub fn align_content(align: String) {
  Property("align-content", align, False)
}

pub fn align_items(align: String) {
  Property("align-items", align, False)
}

pub fn align_self(align: String) {
  Property("align-self", align, False)
}

pub fn align_tracks(align: String) {
  Property("align-tracks", align, False)
}

pub fn justify_content(justify: String) {
  Property("justify-content", justify, False)
}

pub fn justify_items(justify: String) {
  Property("justify-items", justify, False)
}

pub fn justify_self(justify: String) {
  Property("justify-self", justify, False)
}

pub fn justify_tracks(justify: String) {
  Property("justify-tracks", justify, False)
}

pub fn place_content(place: String) {
  Property("place-content", place, False)
}

pub fn place_items(place: String) {
  Property("place-items", place, False)
}

pub fn place_self(place: String) {
  Property("place-self", place, False)
}

pub fn animation(animation: String) {
  Property("animation", animation, False)
}

pub fn animation_name(animation: String) {
  Property("animation-name", animation, False)
}

pub fn animation_duration(animation: String) {
  Property("animation-duration", animation, False)
}

pub fn animation_timing_function(animation: String) {
  Property("animation-timing-function", animation, False)
}

pub fn animation_delay(animation: String) {
  Property("animation-delay", animation, False)
}

pub fn animation_iteration_count(animation: String) {
  Property("animation-iteration-count", animation, False)
}

pub fn animation_direction(animation: String) {
  Property("animation-direction", animation, False)
}

pub fn animation_fill_mode(animation: String) {
  Property("animation-fill-mode", animation, False)
}

pub fn animation_play_state(animation: String) {
  Property("animation-play-state", animation, False)
}

pub fn transition(transition: String) {
  Property("transition", transition, False)
}

pub fn translate(translate: String) {
  Property("translate", translate, False)
}

pub fn transform(transform: String) {
  Property("transform", transform, False)
}

pub fn transform_box(transform_box: String) {
  Property("transform-box", transform_box, False)
}

pub fn transform_origin(transform_origin: String) {
  Property("transform-origin", transform_origin, False)
}

pub fn transform_style(transform_style: String) {
  Property("transform-style", transform_style, False)
}

pub fn appearance(appearance: String) {
  Property("appearance", appearance, False)
}

pub fn filter(filter: String) {
  Property("filter", filter, False)
}

pub fn aspect_ratio(aspect_ratio: String) {
  Property("aspect-ratio", aspect_ratio, False)
}

pub fn top(size: Size) {
  Property("top", size.to_string(size), False)
}

pub fn bottom(size: Size) {
  Property("bottom", size.to_string(size), False)
}

pub fn right(size: Size) {
  Property("right", size.to_string(size), False)
}

pub fn left(size: Size) {
  Property("left", size.to_string(size), False)
}

pub fn top_(size: String) {
  Property("top", size, False)
}

pub fn bottom_(size: String) {
  Property("bottom", size, False)
}

pub fn right_(size: String) {
  Property("right", size, False)
}

pub fn left_(size: String) {
  Property("left", size, False)
}

pub fn box_shadow(box_shadow: String) {
  Property("box-shadow", box_shadow, False)
}

pub fn box_sizing(box_sizing: String) {
  Property("box-sizing", box_sizing, False)
}

pub fn overflow(overflow: String) {
  Property("overflow", overflow, False)
}

pub fn overflow_x(overflow_x: String) {
  Property("overflow-x", overflow_x, False)
}

pub fn overflow_y(overflow_y: String) {
  Property("overflow-y", overflow_y, False)
}

pub fn direction(direction: String) {
  Property("direction", direction, False)
}

pub fn flex(flex: String) {
  Property("flex", flex, False)
}

pub fn flex_basis(flex_basis: String) {
  Property("flex-basis", flex_basis, False)
}

pub fn flex_direction(flex_direction: String) {
  Property("flex-direction", flex_direction, False)
}

pub fn flex_grow(flex_grow: String) {
  Property("flex-grow", flex_grow, False)
}

pub fn border(border: String) {
  Property("border", border, False)
}

pub fn border_top(border_top: String) {
  Property("border-top", border_top, False)
}

pub fn border_bottom(border_bottom: String) {
  Property("border-bottom", border_bottom, False)
}

pub fn border_right(border_right: String) {
  Property("border-right", border_right, False)
}

pub fn border_left(border_left: String) {
  Property("border-left", border_left, False)
}

pub fn border_radius(border_radius: Size) {
  Property("border-radius", size.to_string(border_radius), False)
}

pub fn border_radius_(border_radius: String) {
  Property("border-radius", border_radius, False)
}

pub fn border_top_right_radius(border_top_right_radius: String) {
  Property("border-top-right-radius", border_top_right_radius, False)
}

pub fn border_top_left_radius(border_top_left_radius: String) {
  Property("border-top-left-radius", border_top_left_radius, False)
}

pub fn border_bottom_right_radius(border_bottom_right_radius: String) {
  Property("border-bottom-right-radius", border_bottom_right_radius, False)
}

pub fn border_bottom_left_radius(border_bottom_left_radius: String) {
  Property("border-bottom-left-radius", border_bottom_left_radius, False)
}

pub fn padding(padding: Size) {
  Property("padding", size.to_string(padding), False)
}

pub fn padding_(padding: String) {
  Property("padding", padding, False)
}

pub fn padding_top(padding: Size) {
  Property("padding-top", size.to_string(padding), False)
}

pub fn padding_bottom(padding: Size) {
  Property("padding-bottom", size.to_string(padding), False)
}

pub fn padding_right(padding: Size) {
  Property("padding-right", size.to_string(padding), False)
}

pub fn padding_left(padding: Size) {
  Property("padding-left", size.to_string(padding), False)
}

pub fn margin(margin: Size) {
  Property("margin", size.to_string(margin), False)
}

pub fn margin_(margin: String) {
  Property("margin", margin, False)
}

pub fn margin_top(margin: Size) {
  Property("margin-top", size.to_string(margin), False)
}

pub fn margin_bottom(margin: Size) {
  Property("margin-bottom", size.to_string(margin), False)
}

pub fn margin_right(margin: Size) {
  Property("margin-right", size.to_string(margin), False)
}

pub fn margin_left(margin: Size) {
  Property("margin-left", size.to_string(margin), False)
}

pub fn property(field: String, content: String) {
  Property(field, content, False)
}

// Media queries
// Should be used with the media module.

pub fn media(query: Query, styles: List(MediaStyle)) -> Style(Media, pseudo) {
  let media_selector = media.to_string(query)
  Media(media_selector, styles)
}

// Pseudo-selectors
// Contains pseudo-classes and pseudo-elements.

pub fn placeholder(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector("::placeholder", styles)
}

pub fn hover(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":hover", styles)
}

pub fn active(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":active", styles)
}

pub fn focus(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":focus", styles)
}

pub fn focus_visible(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":focus-visible", styles)
}

pub fn focus_within(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":focus-within", styles)
}

pub fn enabled(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":enabled", styles)
}

pub fn disabled(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":disabled", styles)
}

pub fn read_only(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":read-only", styles)
}

pub fn read_write(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":read-write", styles)
}

pub fn checked(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":checked", styles)
}

pub fn blank(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":blank", styles)
}

pub fn valid(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":valid", styles)
}

pub fn invalid(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":invalid", styles)
}

pub fn required(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":required", styles)
}

pub fn optional(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":optional", styles)
}

pub fn link(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":link", styles)
}

pub fn visited(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":visited", styles)
}

pub fn target(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":target", styles)
}

pub fn nth_child(
  selector: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  PseudoSelector(string.append(":nth-child", selector), styles)
}

pub fn nth_last_child(
  selector: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  PseudoSelector(string.append(":nth-last-child", selector), styles)
}

pub fn nth_of_type(
  selector: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  PseudoSelector(string.append(":nth-of-type", selector), styles)
}

pub fn nth_last_of_type(
  selector: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  PseudoSelector(string.append(":nth-last-of-type", selector), styles)
}

pub fn first_child(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":first-child", styles)
}

pub fn last_child(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":last-child", styles)
}

pub fn only_child(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":only-child", styles)
}

pub fn first_of_type(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":first-of-type", styles)
}

pub fn last_of_type(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":last-of-type", styles)
}

pub fn only_of_type(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  PseudoSelector(":only-of-type", styles)
}

/// Add an `!important` flag to any CSS property.
/// It won't have any effect on non-property style, like media, etc. It will
/// then act as the `identity` function.
pub fn important(style: Style(media, pseudo)) {
  case style {
    Property(key, value, _) -> Property(key, value, True)
    any -> any
  }
}

/// Compose styles by inheriting class, and later overrides them.
/// Works similarly to `composes` property in CSS modules.
pub fn compose(class: Class) {
  class
  |> to_string()
  |> ClassName()
}

/// Compiles a static class, and memoizes it.
/// Don't use dynamic styles with it, use `dynamic` instead.
pub fn class(styles: List(Style(media, pseudo))) -> Class {
  styles
  |> compile_class()
  |> memo()
}

/// Compiles a dynamic class, and not memoizing it. It means at every render,
/// the class will be re-computed, and a new version will be pushed in the browser.
/// Be careful to add a unique ID: right now, it's not possible to push
/// a dynamic class in the browser without defining an ID.
pub fn variable(id: String, styles: List(Style(media, pseudo))) -> Class {
  styles
  |> compile_style(id)
}

/// Convert a `Class` to its proper class name, to use it anywhere in your
/// application. It can have the form `class1` or `class1 class2` in case of
/// classes composition.
pub fn to_class_name(class: Class) -> String {
  class
  |> to_string()
}

/// Convert a `Class` to its equivalent lustre attribute. Use it in your
/// view functions. I.e. `html.div([craft.to_lustre(class())], [])`.
pub fn to_lustre(class: Class) -> Attribute(a) {
  class
  |> to_string()
  |> string.split(" ")
  |> list.map(fn(value) { #(value, True) })
  |> attribute.classes()
}

pub fn setup(options: Options) {
  let cache = create_cache(options)
  Ok(fn(view: fn(model) -> element) {
    fn(model: model) {
      prepare_cache(cache)
      let el = view(model)
      render_cache(cache)
      el
    }
  })
}
