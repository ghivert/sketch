//// # Table of Contents
////
//// - Setup and usage
////   - [`class`](#class)
////   - [`dynamic`](#dynamic)
////   - [`to_class_name`](#to_class_name)
////   - [`to_lustre`](#to_lustre)
////
//// - Lifecycle functions
////   - [`create_cache`](#create_cache)
////   - [`prepare`](#prepare)
////   - [`render`](#render)
////
//// - Modifiers
////   - [`important`](#important)
////   - [`compose`](#compose)
////
//// - Media Queries
////   - [`media`](#media)
////
//// - Properties
////   - [`width`](#width)
////   - [`width_`](#width_)
////   - [`max_width`](#max_width)
////   - [`max_width_`](#max_width_)
////   - [`min_width`](#min_width)
////   - [`min_width_`](#min_width_)
////   - [`height`](#height)
////   - [`height_`](#height_)
////   - [`max_height`](#max_height)
////   - [`max_height_`](#max_height_)
////   - [`min_height`](#min_height)
////   - [`min_height_`](#min_height_)
////   - [`color`](#color)
////   - [`font_family`](#font_family)
////   - [`font_style`](#font_style)
////   - [`font_size`](#font_size)
////   - [`font_size_`](#font_size_)
////   - [`font_weight`](#font_weight)
////   - [`letter_spacing`](#letter_spacing)
////   - [`line_break`](#line_break)
////   - [`line_height`](#line_height)
////   - [`text_align`](#text_align)
////   - [`text_decoration`](#text_decoration)
////   - [`text_justify`](#text_justify)
////   - [`text_overflow`](#text_overflow)
////   - [`text_transform`](#text_transform)
////   - [`white_space`](#white_space)
////   - [`white_space_collapse`](#white_space_collapse)
////   - [`word_break`](#word_break)
////   - [`word_spacing`](#word_spacing)
////   - [`word_wrap`](#word_wrap)
////   - [`list_style`](#list_style)
////   - [`list_style_image`](#list_style_image)
////   - [`list_style_position`](#list_style_position)
////   - [`list_style_type`](#list_style_type)
////   - [`display`](#display)
////   - [`z_index`](#z_index)
////   - [`visibility`](#visibility)
////   - [`background`](#background)
////   - [`object_fit`](#object_fit)
////   - [`object_position`](#object_position)
////   - [`opacity`](#opacity)
////   - [`pointer_events`](#pointer_events)
////   - [`user_select`](#user_select)
////   - [`position`](#position)
////   - [`outline`](#outline)
////   - [`outline_color`](#outline_color)
////   - [`outline_offset`](#outline_offset)
////   - [`outline_style`](#outline_style)
////   - [`outline_width`](#outline_width)
////   - [`offset`](#offset)
////   - [`offset_anchor`](#offset_anchor)
////   - [`offset_distance`](#offset_distance)
////   - [`offset_path`](#offset_path)
////   - [`offset_position`](#offset_position)
////   - [`offset_rotate`](#offset_rotate)
////   - [`gap`](#gap)
////   - [`gap_`](#gap_)
////   - [`column_gap`](#column_gap)
////   - [`row_gap`](#row_gap)
////   - [`grid_area`](#grid_area)
////   - [`grid_column`](#grid_column)
////   - [`grid_row`](#grid_row)
////   - [`grid_template`](#grid_template)
////   - [`grid_auto_columns`](#grid_auto_columns)
////   - [`grid_auto_rows`](#grid_auto_rows)
////   - [`grid_auto_flow`](#grid_auto_flow)
////   - [`grid_template_areas`](#grid_template_areas)
////   - [`grid_template_columns`](#grid_template_columns)
////   - [`grid_template_rows`](#grid_template_rows)
////   - [`align_content`](#align_content)
////   - [`align_items`](#align_items)
////   - [`align_self`](#align_self)
////   - [`align_tracks`](#align_tracks)
////   - [`justify_content`](#justify_content)
////   - [`justify_items`](#justify_items)
////   - [`justify_self`](#justify_self)
////   - [`justify_tracks`](#justify_tracks)
////   - [`place_content`](#place_content)
////   - [`place_items`](#place_items)
////   - [`place_self`](#place_self)
////   - [`animation`](#animation)
////   - [`animation_name`](#animation_name)
////   - [`animation_duration`](#animation_duration)
////   - [`animation_timing_function`](#animation_timing_function)
////   - [`animation_delay`](#animation_delay)
////   - [`animation_iteration_count`](#animation_iteration_count)
////   - [`animation_direction`](#animation_direction)
////   - [`animation_fill_mode`](#animation_fill_mode)
////   - [`animation_play_state`](#animation_play_state)
////   - [`transition`](#transition)
////   - [`translate`](#translate)
////   - [`transform`](#transform)
////   - [`transform_box`](#transform_box)
////   - [`transform_origin`](#transform_origin)
////   - [`transform_style`](#transform_style)
////   - [`appearance`](#appearance)
////   - [`filter`](#filter)
////   - [`aspect_ratio`](#aspect_ratio)
////   - [`top`](#top)
////   - [`bottom`](#bottom)
////   - [`right`](#right)
////   - [`left`](#left)
////   - [`top_`](#top_)
////   - [`bottom_`](#bottom_)
////   - [`right_`](#right_)
////   - [`left_`](#left_)
////   - [`box_shadow`](#box_shadow)
////   - [`box_sizing`](#box_sizing)
////   - [`overflow`](#overflow)
////   - [`overflow_x`](#overflow_x)
////   - [`overflow_y`](#overflow_y)
////   - [`direction`](#direction)
////   - [`flex`](#flex)
////   - [`flex_basis`](#flex_basis)
////   - [`flex_direction`](#flex_direction)
////   - [`flex_grow`](#flex_grow)
////   - [`border`](#border)
////   - [`border_top`](#border_top)
////   - [`border_bottom`](#border_bottom)
////   - [`border_right`](#border_right)
////   - [`border_left`](#border_left)
////   - [`border_radius`](#border_radius)
////   - [`border_radius_`](#border_radius_)
////   - [`border_top_right_radius`](#border_top_right_radius)
////   - [`border_top_right_radius_`](#border_top_right_radius_)
////   - [`border_top_left_radius`](#border_top_left_radius)
////   - [`border_top_left_radius_`](#border_top_left_radius_)
////   - [`border_bottom_right_radius`](#border_bottom_right_radius)
////   - [`border_bottom_right_radius_`](#border_bottom_right_radius_)
////   - [`border_bottom_left_radius`](#border_bottom_left_radius)
////   - [`border_bottom_left_radius_`](#border_bottom_left_radius_)
////   - [`padding`](#padding)
////   - [`padding_`](#padding_)
////   - [`padding_top`](#padding_top)
////   - [`padding_bottom`](#padding_bottom)
////   - [`padding_right`](#padding_right)
////   - [`padding_left`](#padding_left)
////   - [`margin`](#margin)
////   - [`margin_`](#margin_)
////   - [`margin_top`](#margin_top)
////   - [`margin_bottom`](#margin_bottom)
////   - [`margin_right`](#margin_right)
////   - [`margin_left`](#margin_left)
////   - [`cursor`](#cursor)
////   - [`property`](#property)
////
//// - Pseudo-selectors
////   - [`placeholder`](#placeholder)
////   - [`hover`](#hover)
////   - [`active`](#active)
////   - [`focus`](#focus)
////   - [`focus_visible`](#focus_visible)
////   - [`focus_within`](#focus_within)
////   - [`enabled`](#enabled)
////   - [`disabled`](#disabled)
////   - [`read_only`](#read_only)
////   - [`read_write`](#read_write)
////   - [`checked`](#checked)
////   - [`blank`](#blank)
////   - [`valid`](#valid)
////   - [`invalid`](#invalid)
////   - [`required`](#required)
////   - [`optional`](#optional)
////   - [`link`](#link)
////   - [`visited`](#visited)
////   - [`target`](#target)
////   - [`nth_child`](#nth_child)
////   - [`nth_last_child`](#nth_last_child)
////   - [`nth_of_type`](#nth_of_type)
////   - [`nth_last_of_type`](#nth_last_of_type)
////   - [`first_child`](#first_child)
////   - [`last_child`](#last_child)
////   - [`only_child`](#only_child)
////   - [`first_of_type`](#first_of_type)
////   - [`last_of_type`](#last_of_type)
////   - [`only_of_type`](#only_of_type)
////
//// ---
////
//// # Internals
////
//// Sketch tries to be to CSS what the VDOM is to the DOM: the ultimate pain-free
//// tool to manage the state of your CSS styles, without having to worry with CSS
//// while leveraging CSS skills.
////
//// ## I don't know anything about Sketch!
////
//// This documentation focuses on internal and how is working Sketch under-the-hood.
//// No worry though, just heads up to the [README](https://hexdocs.pm/sketch/index.html)
//// to get an overview of Sketch, and to get it work with your favorite framework!
////
//// ## Lifecycle
////
//// To do this, Sketch tries to maintain a cache of styles, that can be updated
//// between every render of the DOM, and will update the correct StyleSheet in DOM.
//// Sketch has a lifecycle to make it work.
//// After having created a Cache, you have to call `prepare` before every repaint,
//// and `render` after every repaint.
////
//// ```txt
////                             +--------------+
////                             | Create Cache |
////                             +--------------+
////                                    |
////                                    |
////                                    |
////                                    v
////                     +-------------------------------+
////                     | Before paint, setup the cache |  <-------+
////                     +-------------------------------+          |
////                                    |                           |
////                                    |                           |
////                                    |                           |
////                                    v                           |
////               +-------------------------------------------+    |
////               |                                           |    |
////               |      framework paints to the DOM          |    |
////               |   and calls class and dynamic functions   |    |
////               |            provided by sketch             |    |
////               |                                           |    |
////               +-------------------------------------------+    |
////                                    |                           |
////                                    |                           |
////                                    |                           |
////                                    v                           |
////                     +-------------------------------+          |
////                     | After paint, render the cache |  --------+
////                     +-------------------------------+
//// ```
////
//// - `prepare` setup the Cache in order to diff the old styles with the new styles.
////   If `prepare` is not called before every repaint, the stylesheet will not diff
////   styles, and it will continue to append styles to the stylesheet.
////
//// - `render` accepts the cache and will inject the stylesheet in the DOM or the
////   document.
////
//// ## Some notes on side-effects
////
//// Unfortunately, and because of the nature of the different frameworks and of
//// CSS, Sketch is doing some side-effects in background, to collect the styles
//// and to push them in the browser. Maybe it could be removed in the future,
//// but it would involve work with different maintainers of different packages,
//// and it would take a lot of time and energy. It's not on plan right now,
//// but rather to focus on correct UX and to find good ways of doing things.
//// When the dust will settle and that API will be stable, then we could take
//// some time to figure out how to get rid of side-effects.
//// In the meantime, if you plan to integrate Sketch in your framework, and need
//// some access to underlying and internals, open an issue with your use case,
//// I'd more than happy to help on that point and reduce side-effects.

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import lustre/attribute.{type Attribute}
import sketch/error
import sketch/internals/cache/setup as cache
import sketch/internals/class
import sketch/internals/style
import sketch/media.{type Query}
import sketch/options.{type Options}
import sketch/size.{type Size}

// Types
// Most of them are opaque because they're just JS types from the FFI.
// No one should use them directly outside of this package.
// If you end up here reading this because you want to access internals,
// consider forking the repo and working on it on your own, or submit a PR!

/// Represents a CSS class, compiled.
pub type Class =
  class.Class

/// Manages the styles. Can be instanciated with [`create_cache`](#create_cache).
pub type Cache =
  cache.Cache

/// Represents a Style. It can be a class composition, a media query with its
/// sub-properties, a pseudo-selector with its sub-properties or a property
/// directly.
/// It's not possible to put a media query in a media query, and a pseudo-selector
/// in a pseudo-selector.
pub opaque type Style(media, pseudo) {
  Style(internal: style.Style)
}

// Phantom types
// Used in styles in order to cancel using medias in medias.
// They have no utilities outside of the type namespace.

/// No direct usage, used for type-checking and to cancel impossible states.
pub type Media

/// No direct usage, used for type-checking and to cancel impossible states.
pub type NoMedia

/// No direct usage, used for type-checking and to cancel impossible states.
pub type PseudoSelector

/// No direct usage, used for type-checking and to cancel impossible states.
pub type NoPseudoSelector

type MediaStyle =
  Style(NoMedia, PseudoSelector)

type PseudoStyle =
  Style(NoMedia, NoPseudoSelector)

// FFI
// Used exclusively in the package.
// Most should not be exposed, or in a low-level way.

@external(javascript, "./sketch.ffi.mjs", "compileClass")
fn compile_class(styles: List(style.Style)) -> Class {
  cache.compile_class(styles)
}

@external(javascript, "./sketch.ffi.mjs", "compileClass")
fn compile_style(styles: List(style.Style), id: String) -> Class {
  cache.compile_style(styles, id)
}

/// Memoizes a function across multiple renders. When encountered for the first time,
/// the class will be put in long-live section of the stylesheet. Once in long-live
/// section, the computed class will never be garbaged, and will be included in every
/// render.
/// This saves some time during render by skipping every memoized class. You can use it
/// when you know that a class will be used during every render, or when you want
/// a class to not be forgetted, because you're using it often.
@external(javascript, "./sketch.ffi.mjs", "memo")
pub fn memo(class: Class) -> Class {
  cache.memo(class)
}

/// Convert a `Class` to its proper class name, to use it anywhere in your
/// application. It can have the form `class1` or `class1 class2` in case of
/// classes composition.
@external(javascript, "./sketch.ffi.mjs", "toString")
pub fn to_class_name(class: Class) -> String {
  class.class_name(class)
}

/// Create a cache manager, managing the styles for every repaint. You can
/// instanciate as much cache manager that you want, if you want to use multiple
/// render lifecycle.
/// You can output the styles directly in a node style in the DOM, or by pushing
/// them directly in a CSSStyleSheet, at the document level. The choice is up to
/// you at the initialization of the Cache.
/// On BEAM, this setting is ignored.
/// If you're using Lustre, you shouldn't have to worry about it, and consider
/// it as internal low-level.
@external(javascript, "./cache.ffi.mjs", "createCache")
pub fn create_cache(options: Options) -> Result(Cache, error.SketchError) {
  cache.create_cache(options)
}

/// Lifecycle function — not side-effect free
/// `prepare` should be called before your repaint, and before the different
/// calls to [`class`](#class) and [`dynamic`](#dynamic) functions. This setups
/// the cache to prepare for a new paint, and will allow for diffing the styles.
/// As long as you don't call `prepare`, the stylesheet output by sketch will not
/// diff, and you'll use the stylesheet as append-only. This could be done at
/// will.
/// Be careful, the styles computed by [`class`](#class) and [`dynamic`](#dynamic)
/// will be pushed in the last cache called by `prepare`, due to the styles
/// handling (and some side-effects under-the-hood for performance purposes).
@external(javascript, "./cache.ffi.mjs", "prepareCache")
pub fn prepare(cache: Cache) -> Nil {
  cache.prepare(cache)
}

/// Lifecycle function — not side-effect free
/// `render` takes a Cache, and render its content to the stylesheet, according
/// to the choice of the Cache. `render` is idempotent, and can be called as
/// much as you want.
/// You can expect `render` to return Ok(String) representing the stylesheet on
/// BEAM, and Error(Nil) on JS. It means there's not output String in JS, but the
/// modification happened in the DOM!
@external(javascript, "./cache.ffi.mjs", "renderCache")
pub fn render(cache: Cache) -> Result(String, Nil) {
  cache.render(cache)
}

/// Used internally to get rid of phantom types
fn convert_styles(styles: List(Style(media, pseudo))) -> List(style.Style) {
  use item <- list.map(styles)
  item.internal
}

// Properties
// All the properties accessible for the user.
// All properties must have a low-level String interface.

pub fn width(width: Size) {
  property("width", size.to_string(width))
}

pub fn width_(width: String) {
  property("width", width)
}

pub fn max_width(width: Size) {
  property("max-width", size.to_string(width))
}

pub fn max_width_(width: String) {
  property("max-width", width)
}

pub fn min_width(width: Size) {
  property("min-width", size.to_string(width))
}

pub fn min_width_(width: String) {
  property("min-width", width)
}

pub fn height(height: Size) {
  property("height", size.to_string(height))
}

pub fn height_(height: String) {
  property("height", height)
}

pub fn max_height(height: Size) {
  property("max-height", size.to_string(height))
}

pub fn max_height_(height: String) {
  property("max-height", height)
}

pub fn min_height(height: Size) {
  property("min-height", size.to_string(height))
}

pub fn min_height_(height: String) {
  property("min-height", height)
}

pub fn color(color: String) {
  property("color", color)
}

pub fn font_family(font_family: String) {
  property("font-family", font_family)
}

pub fn font_style(font_style: String) {
  property("font-style", font_style)
}

pub fn font_size(font_size: Size) {
  property("font-size", size.to_string(font_size))
}

pub fn font_size_(font_size: String) {
  property("font-size", font_size)
}

pub fn font_weight(font_weight: String) {
  property("font-weight", font_weight)
}

pub fn letter_spacing(letter_spacing: String) {
  property("letter-spacing", letter_spacing)
}

pub fn line_break(line_break: String) {
  property("line-break", line_break)
}

pub fn line_height(line_height: String) {
  property("line-height", line_height)
}

pub fn text_align(text_align: String) {
  property("text-align", text_align)
}

pub fn text_decoration(text_decoration: String) {
  property("text-decoration", text_decoration)
}

pub fn text_justify(text_justify: String) {
  property("text-justify", text_justify)
}

pub fn text_overflow(text_overflow: String) {
  property("text-overflow", text_overflow)
}

pub fn text_transform(text_transform: String) {
  property("text-transform", text_transform)
}

pub fn white_space(white_space: String) {
  property("white-space", white_space)
}

pub fn white_space_collapse(white_space_collapse: String) {
  property("white-space-collapse", white_space_collapse)
}

pub fn word_break(word_break: String) {
  property("word-break", word_break)
}

pub fn word_spacing(word_spacing: String) {
  property("word-spacing", word_spacing)
}

pub fn word_wrap(word_wrap: String) {
  property("word-wrap", word_wrap)
}

pub fn list_style(list_style: String) {
  property("list-style", list_style)
}

pub fn list_style_image(list_style_image: String) {
  property("list-style-image", list_style_image)
}

pub fn list_style_position(list_style_position: String) {
  property("list-style-position", list_style_position)
}

pub fn list_style_type(list_style_type: String) {
  property("list-style-type", list_style_type)
}

pub fn display(display: String) {
  property("display", display)
}

pub fn z_index(z_index: Int) {
  property("z-index", int.to_string(z_index))
}

pub fn visibility(visibility: String) {
  property("visibility", visibility)
}

pub fn background(background: String) {
  property("background", background)
}

pub fn object_fit(object_fit: String) {
  property("object-fit", object_fit)
}

pub fn object_position(object_position: String) {
  property("object-position", object_position)
}

pub fn opacity(opacity: Float) {
  property("opacity", float.to_string(opacity))
}

pub fn pointer_events(pointer_events: String) {
  property("pointer-events", pointer_events)
}

pub fn user_select(user_select: String) {
  property("user-select", user_select)
}

pub fn position(position: String) {
  property("position", position)
}

pub fn outline(outline: String) {
  property("outline", outline)
}

pub fn outline_color(outline_color: String) {
  property("outline-color", outline_color)
}

pub fn outline_offset(outline_offset: String) {
  property("outline-offset", outline_offset)
}

pub fn outline_style(outline_style: String) {
  property("outline-style", outline_style)
}

pub fn outline_width(outline_width: String) {
  property("outline-width", outline_width)
}

pub fn offset(offset: String) {
  property("offset", offset)
}

pub fn offset_anchor(offset_anchor: String) {
  property("offset-anchor", offset_anchor)
}

pub fn offset_distance(offset_distance: String) {
  property("offset-distance", offset_distance)
}

pub fn offset_path(offset_path: String) {
  property("offset-path", offset_path)
}

pub fn offset_position(offset_position: String) {
  property("offset-position", offset_position)
}

pub fn offset_rotate(offset_rotate: String) {
  property("offset-rotate", offset_rotate)
}

pub fn gap(gap: Size) {
  property("gap", size.to_string(gap))
}

pub fn gap_(gap: String) {
  property("gap", gap)
}

pub fn column_gap(column_gap: Size) {
  property("column-gap", size.to_string(column_gap))
}

pub fn row_gap(row_gap: Size) {
  property("row-gap", size.to_string(row_gap))
}

pub fn grid_area(grid_area: String) {
  property("grid-area", grid_area)
}

pub fn grid_column(grid_column: String) {
  property("grid-column", grid_column)
}

pub fn grid_row(grid_row: String) {
  property("grid-row", grid_row)
}

pub fn grid_template(grid_template: String) {
  property("grid-template", grid_template)
}

pub fn grid_auto_columns(grid_auto_columns: String) {
  property("grid-auto-columns", grid_auto_columns)
}

pub fn grid_auto_rows(grid_auto_rows: String) {
  property("grid-auto-rows", grid_auto_rows)
}

pub fn grid_auto_flow(grid_auto_flow: String) {
  property("grid-auto-flow", grid_auto_flow)
}

pub fn grid_template_areas(grid_template_areas: String) {
  property("grid-template-areas", grid_template_areas)
}

pub fn grid_template_columns(grid_template_columns: String) {
  property("grid-template-columns", grid_template_columns)
}

pub fn grid_template_rows(grid_template_rows: String) {
  property("grid-template-rows", grid_template_rows)
}

pub fn align_content(align: String) {
  property("align-content", align)
}

pub fn align_items(align: String) {
  property("align-items", align)
}

pub fn align_self(align: String) {
  property("align-self", align)
}

pub fn align_tracks(align: String) {
  property("align-tracks", align)
}

pub fn justify_content(justify: String) {
  property("justify-content", justify)
}

pub fn justify_items(justify: String) {
  property("justify-items", justify)
}

pub fn justify_self(justify: String) {
  property("justify-self", justify)
}

pub fn justify_tracks(justify: String) {
  property("justify-tracks", justify)
}

pub fn place_content(place: String) {
  property("place-content", place)
}

pub fn place_items(place: String) {
  property("place-items", place)
}

pub fn place_self(place: String) {
  property("place-self", place)
}

pub fn animation(animation: String) {
  property("animation", animation)
}

pub fn animation_name(animation: String) {
  property("animation-name", animation)
}

pub fn animation_duration(animation: String) {
  property("animation-duration", animation)
}

pub fn animation_timing_function(animation: String) {
  property("animation-timing-function", animation)
}

pub fn animation_delay(animation: String) {
  property("animation-delay", animation)
}

pub fn animation_iteration_count(animation: String) {
  property("animation-iteration-count", animation)
}

pub fn animation_direction(animation: String) {
  property("animation-direction", animation)
}

pub fn animation_fill_mode(animation: String) {
  property("animation-fill-mode", animation)
}

pub fn animation_play_state(animation: String) {
  property("animation-play-state", animation)
}

pub fn transition(transition: String) {
  property("transition", transition)
}

pub fn translate(translate: String) {
  property("translate", translate)
}

pub fn transform(transform: String) {
  property("transform", transform)
}

pub fn transform_box(transform_box: String) {
  property("transform-box", transform_box)
}

pub fn transform_origin(transform_origin: String) {
  property("transform-origin", transform_origin)
}

pub fn transform_style(transform_style: String) {
  property("transform-style", transform_style)
}

pub fn appearance(appearance: String) {
  property("appearance", appearance)
}

pub fn filter(filter: String) {
  property("filter", filter)
}

pub fn aspect_ratio(aspect_ratio: String) {
  property("aspect-ratio", aspect_ratio)
}

pub fn top(size: Size) {
  property("top", size.to_string(size))
}

pub fn bottom(size: Size) {
  property("bottom", size.to_string(size))
}

pub fn right(size: Size) {
  property("right", size.to_string(size))
}

pub fn left(size: Size) {
  property("left", size.to_string(size))
}

pub fn top_(size: String) {
  property("top", size)
}

pub fn bottom_(size: String) {
  property("bottom", size)
}

pub fn right_(size: String) {
  property("right", size)
}

pub fn left_(size: String) {
  property("left", size)
}

pub fn box_shadow(box_shadow: String) {
  property("box-shadow", box_shadow)
}

pub fn box_sizing(box_sizing: String) {
  property("box-sizing", box_sizing)
}

pub fn overflow(overflow: String) {
  property("overflow", overflow)
}

pub fn overflow_x(overflow_x: String) {
  property("overflow-x", overflow_x)
}

pub fn overflow_y(overflow_y: String) {
  property("overflow-y", overflow_y)
}

pub fn direction(direction: String) {
  property("direction", direction)
}

pub fn flex(flex: String) {
  property("flex", flex)
}

pub fn flex_basis(flex_basis: String) {
  property("flex-basis", flex_basis)
}

pub fn flex_direction(flex_direction: String) {
  property("flex-direction", flex_direction)
}

pub fn flex_grow(flex_grow: String) {
  property("flex-grow", flex_grow)
}

pub fn border(border: String) {
  property("border", border)
}

pub fn border_top(border_top: String) {
  property("border-top", border_top)
}

pub fn border_bottom(border_bottom: String) {
  property("border-bottom", border_bottom)
}

pub fn border_right(border_right: String) {
  property("border-right", border_right)
}

pub fn border_left(border_left: String) {
  property("border-left", border_left)
}

pub fn border_radius(border_radius: Size) {
  property("border-radius", size.to_string(border_radius))
}

pub fn border_radius_(border_radius: String) {
  property("border-radius", border_radius)
}

pub fn border_top_right_radius(border_top_right_radius: Size) {
  size.to_string(border_top_right_radius)
  |> property("border-top-right-radius", _)
}

pub fn border_top_left_radius(border_top_left_radius: Size) {
  size.to_string(border_top_left_radius)
  |> property("border-top-left-radius", _)
}

pub fn border_bottom_right_radius(border_bottom_right_radius: Size) {
  size.to_string(border_bottom_right_radius)
  |> property("border-bottom-right-radius", _)
}

pub fn border_bottom_left_radius(border_bottom_left_radius: Size) {
  size.to_string(border_bottom_left_radius)
  |> property("border-bottom-left-radius", _)
}

pub fn border_top_right_radius_(border_top_right_radius: String) {
  property("border-top-right-radius", border_top_right_radius)
}

pub fn border_top_left_radius_(border_top_left_radius: String) {
  property("border-top-left-radius", border_top_left_radius)
}

pub fn border_bottom_right_radius_(border_bottom_right_radius: String) {
  property("border-bottom-right-radius", border_bottom_right_radius)
}

pub fn border_bottom_left_radius_(border_bottom_left_radius: String) {
  property("border-bottom-left-radius", border_bottom_left_radius)
}

pub fn padding(padding: Size) {
  property("padding", size.to_string(padding))
}

pub fn padding_(padding: String) {
  property("padding", padding)
}

pub fn padding_top(padding: Size) {
  property("padding-top", size.to_string(padding))
}

pub fn padding_bottom(padding: Size) {
  property("padding-bottom", size.to_string(padding))
}

pub fn padding_right(padding: Size) {
  property("padding-right", size.to_string(padding))
}

pub fn padding_left(padding: Size) {
  property("padding-left", size.to_string(padding))
}

pub fn margin(margin: Size) {
  property("margin", size.to_string(margin))
}

pub fn margin_(margin: String) {
  property("margin", margin)
}

pub fn margin_top(margin: Size) {
  property("margin-top", size.to_string(margin))
}

pub fn margin_bottom(margin: Size) {
  property("margin-bottom", size.to_string(margin))
}

pub fn margin_right(margin: Size) {
  property("margin-right", size.to_string(margin))
}

pub fn margin_left(margin: Size) {
  property("margin-left", size.to_string(margin))
}

pub fn cursor(cursor: String) {
  property("cursor", cursor)
}

pub fn property(field: String, content: String) {
  Style(style.Property(field, content, False))
}

// Media queries
// Should be used with the media module.

pub fn media(query: Query, styles: List(MediaStyle)) -> Style(Media, pseudo) {
  let media_selector = media.to_string(query)
  Style(style.Media(media_selector, convert_styles(styles)))
}

// Pseudo-selectors
// Contains pseudo-classes and pseudo-elements.
pub fn placeholder(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector("::placeholder", styles)
}

pub fn hover(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":hover", styles)
}

pub fn active(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":active", styles)
}

pub fn focus(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":focus", styles)
}

pub fn focus_visible(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":focus-visible", styles)
}

pub fn focus_within(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":focus-within", styles)
}

pub fn enabled(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":enabled", styles)
}

pub fn disabled(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":disabled", styles)
}

pub fn read_only(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":read-only", styles)
}

pub fn read_write(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":read-write", styles)
}

pub fn checked(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":checked", styles)
}

pub fn blank(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":blank", styles)
}

pub fn valid(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":valid", styles)
}

pub fn invalid(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":invalid", styles)
}

pub fn required(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":required", styles)
}

pub fn optional(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":optional", styles)
}

pub fn link(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":link", styles)
}

pub fn visited(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":visited", styles)
}

pub fn target(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":target", styles)
}

pub fn nth_child(
  selector: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  pseudo_selector(string.append(":nth-child", selector), styles)
}

pub fn nth_last_child(
  selector: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  pseudo_selector(string.append(":nth-last-child", selector), styles)
}

pub fn nth_of_type(
  selector: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  pseudo_selector(string.append(":nth-of-type", selector), styles)
}

pub fn nth_last_of_type(
  selector: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  pseudo_selector(string.append(":nth-last-of-type", selector), styles)
}

pub fn first_child(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":first-child", styles)
}

pub fn last_child(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":last-child", styles)
}

pub fn only_child(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":only-child", styles)
}

pub fn first_of_type(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":first-of-type", styles)
}

pub fn last_of_type(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":last-of-type", styles)
}

pub fn only_of_type(styles: List(PseudoStyle)) -> Style(media, PseudoSelector) {
  pseudo_selector(":only-of-type", styles)
}

pub fn pseudo_selector(
  value: String,
  styles: List(PseudoStyle),
) -> Style(media, PseudoSelector) {
  Style(style.PseudoSelector(value, convert_styles(styles)))
}

/// Add an `!important` flag to any CSS property.
/// It won't have any effect on non-property style, like media, etc. It will
/// then act as the `identity` function.
pub fn important(style: Style(media, pseudo)) -> Style(media, pseudo) {
  case style {
    Style(style.Property(key, value, _)) ->
      Style(style.Property(key, value, True))
    any -> any
  }
}

/// Compose styles by inheriting class, and later overrides them.
/// Works similarly to `composes` property in CSS modules.
pub fn compose(class: Class) -> Style(media, pseudo) {
  class
  |> to_class_name()
  |> style.ClassName()
  |> Style()
}

/// Compiles a static class, and reuses it across the same render flow.
/// Don't use dynamic styles with it, use `dynamic` instead.
pub fn class(styles: List(Style(media, pseudo))) -> Class {
  styles
  |> convert_styles()
  |> compile_class()
}

/// Compiles a dynamic class. It means during the render, when the class with the id
/// is re-computed, a new version will be pushed in the browser if needed.
/// It means you can reuse the function to output different classes with the same
/// function and basis. With a `class`, once computed once during the render, it's
/// not possible to change the content of the class.
/// Be careful to add a unique ID: right now, it's not possible to push
/// a dynamic class in the browser without defining an ID. The ID should be unique
/// to the computed class, otherwise you could end up with some classes overlap.
pub fn dynamic(id: String, styles: List(Style(media, pseudo))) -> Class {
  styles
  |> convert_styles()
  |> compile_style(id)
}

/// Convert a `Class` to its equivalent lustre attribute. Use it in your
/// view functions. I.e. `html.div([sketch.to_lustre(class())], [])`.
pub fn to_lustre(class: Class) -> Attribute(a) {
  class
  |> to_class_name()
  |> string.split(" ")
  |> list.map(fn(value) { #(value, True) })
  |> attribute.classes()
}

@deprecated("Use sketch/lustre.{setup, compose} instead.")
pub fn lustre_setup(options: Options) {
  use cache <- result.then(create_cache(options))
  Ok(fn(view: fn(model) -> element) {
    fn(model: model) {
      prepare(cache)
      let el = view(model)
      render(cache)
      el
    }
  })
}
