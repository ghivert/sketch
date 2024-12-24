import gleam/float
import gleam/int
import gleam/list
import gleam/string
import sketch/internals/style
import sketch/media.{type Query}
import sketch/size.{type Size}
import sketch/transform.{type Transform}

/// Represents a Style. It can be a class composition, a media query with its
/// sub-properties, a pseudo-selector with its sub-properties or a property
/// directly.
pub type Style =
  style.Style

/// Represents a CSS class, compiled.
pub type Class =
  style.Class

pub fn class(styles: List(Style)) -> Class {
  style.class(styles)
}

// Properties
// All the properties accessible for the user.
// All properties must have a low-level String interface.

pub fn accent_color(value: String) -> Style {
  property("accent-color", value)
}

pub fn align_content(align: String) -> Style {
  property("align-content", align)
}

pub fn align_items(align: String) -> Style {
  property("align-items", align)
}

pub fn align_self(align: String) -> Style {
  property("align-self", align)
}

pub fn align_tracks(align: String) -> Style {
  property("align-tracks", align)
}

pub fn all(value: String) -> Style {
  property("all", value)
}

pub fn animation(animation: String) -> Style {
  property("animation", animation)
}

pub fn animation_composition(animation: String) -> Style {
  property("animation-composition", animation)
}

pub fn animation_delay(animation: String) -> Style {
  property("animation-delay", animation)
}

pub fn animation_direction(animation: String) -> Style {
  property("animation-direction", animation)
}

pub fn animation_duration(animation: String) -> Style {
  property("animation-duration", animation)
}

pub fn animation_fill_mode(animation: String) -> Style {
  property("animation-fill-mode", animation)
}

pub fn animation_iteration_count(animation: String) -> Style {
  property("animation-iteration-count", animation)
}

pub fn animation_name(animation: String) -> Style {
  property("animation-name", animation)
}

pub fn animation_play_state(animation: String) -> Style {
  property("animation-play-state", animation)
}

pub fn animation_timing_function(animation: String) -> Style {
  property("animation-timing-function", animation)
}

pub fn appearance(appearance: String) -> Style {
  property("appearance", appearance)
}

pub fn aspect_ratio(aspect_ratio: String) -> Style {
  property("aspect-ratio", aspect_ratio)
}

pub fn backdrop_filter(value: String) -> Style {
  property("backdrop-filter", value)
}

pub fn backface_visibility(value: String) -> Style {
  property("backface-visibility", value)
}

pub fn background(background: String) -> Style {
  property("background", background)
}

pub fn background_attachment(value: String) -> Style {
  property("background-attachment", value)
}

pub fn background_blend_mode(value: String) -> Style {
  property("background-blend-mode", value)
}

pub fn background_clip(value: String) -> Style {
  property("background-clip", value)
}

pub fn background_color(value: String) -> Style {
  property("background-color", value)
}

pub fn background_image(value: String) -> Style {
  property("background-image", value)
}

pub fn background_origin(value: String) -> Style {
  property("background-origin", value)
}

pub fn background_position(value: String) -> Style {
  property("background-position", value)
}

pub fn background_position_x(value: String) -> Style {
  property("background-position-x", value)
}

pub fn background_position_y(value: String) -> Style {
  property("background-position-y", value)
}

pub fn background_repeat(value: String) -> Style {
  property("background-repeat", value)
}

pub fn background_size(background: String) -> Style {
  property("background-size", background)
}

pub fn block_size(value: Size) -> Style {
  property("block-size", size.to_string(value))
}

pub fn block_size_(value: String) -> Style {
  property("block-size", value)
}

pub fn border(border: String) -> Style {
  property("border", border)
}

pub fn border_block(value: String) -> Style {
  property("border-block", value)
}

pub fn border_block_color(value: String) -> Style {
  property("border-block-color", value)
}

pub fn border_block_end(value: String) -> Style {
  property("border-block-end", value)
}

pub fn border_block_end_color(value: String) -> Style {
  property("border-block-end-color", value)
}

pub fn border_block_end_style(value: String) -> Style {
  property("border-block-end-style", value)
}

pub fn border_block_end_width(value: Size) -> Style {
  property("border-block-end-width", size.to_string(value))
}

pub fn border_block_end_width_(value: String) -> Style {
  property("border-block-end-width", value)
}

pub fn border_block_start(value: String) -> Style {
  property("border-block-start", value)
}

pub fn border_block_start_color(value: String) -> Style {
  property("border-block-start-color", value)
}

pub fn border_block_start_style(value: String) -> Style {
  property("border-block-start-style", value)
}

pub fn border_block_start_width(value: Size) -> Style {
  property("border-block-start-width", size.to_string(value))
}

pub fn border_block_start_width_(value: String) -> Style {
  property("border-block-start-width", value)
}

pub fn border_block_style(value: String) -> Style {
  property("border-block-style", value)
}

pub fn border_block_width(value: Size) -> Style {
  property("border-block-width", size.to_string(value))
}

pub fn border_block_width_(value: String) -> Style {
  property("border-block-width", value)
}

pub fn border_bottom(value: String) -> Style {
  property("border-bottom", value)
}

pub fn border_bottom_color(value: String) -> Style {
  property("border-bottom-color", value)
}

pub fn border_bottom_left_radius(border_bottom_left_radius: Size) -> Style {
  size.to_string(border_bottom_left_radius)
  |> property("border-bottom-left-radius", _)
}

pub fn border_bottom_left_radius_(border_bottom_left_radius: String) -> Style {
  property("border-bottom-left-radius", border_bottom_left_radius)
}

pub fn border_bottom_right_radius(border_bottom_right_radius: Size) -> Style {
  size.to_string(border_bottom_right_radius)
  |> property("border-bottom-right-radius", _)
}

pub fn border_bottom_right_radius_(border_bottom_right_radius: String) -> Style {
  property("border-bottom-right-radius", border_bottom_right_radius)
}

pub fn border_bottom_style(value: String) -> Style {
  property("border-bottom-style", value)
}

pub fn border_bottom_width(value: Size) -> Style {
  property("border-bottom-width", size.to_string(value))
}

pub fn border_bottom_width_(value: String) -> Style {
  property("border-bottom-width", value)
}

pub fn border_collapse(value: String) -> Style {
  property("border-collapse", value)
}

pub fn border_color(value: String) -> Style {
  property("border-color", value)
}

pub fn border_end_end_radius(value: String) -> Style {
  property("border-end-end-radius", value)
}

pub fn border_end_start_radius(value: String) -> Style {
  property("border-end-start-radius", value)
}

pub fn border_image(value: String) -> Style {
  property("border-image", value)
}

pub fn border_image_outset(value: Size) -> Style {
  property("border-image-outset", size.to_string(value))
}

pub fn border_image_outset_(value: String) -> Style {
  property("border-image-outset", value)
}

pub fn border_image_repeat(value: String) -> Style {
  property("border-image-repeat", value)
}

pub fn border_image_slice(value: String) -> Style {
  property("border-image-slice", value)
}

pub fn border_image_source(value: String) -> Style {
  property("border-image-source", value)
}

pub fn border_image_width(value: Size) -> Style {
  property("border-image-width", size.to_string(value))
}

pub fn border_image_width_(value: String) -> Style {
  property("border-image-width", value)
}

pub fn border_inline(value: String) -> Style {
  property("border-inline", value)
}

pub fn border_inline_color(value: String) -> Style {
  property("border-inline-color", value)
}

pub fn border_inline_end(value: String) -> Style {
  property("border-inline-end", value)
}

pub fn border_inline_end_color(value: String) -> Style {
  property("border-inline-end-color", value)
}

pub fn border_inline_end_style(value: String) -> Style {
  property("border-inline-end-style", value)
}

pub fn border_inline_end_width(value: Size) -> Style {
  property("border-inline-end-width", size.to_string(value))
}

pub fn border_inline_end_width_(value: String) -> Style {
  property("border-inline-end-width", value)
}

pub fn border_inline_start(value: String) -> Style {
  property("border-inline-start", value)
}

pub fn border_inline_start_color(value: String) -> Style {
  property("border-inline-start-color", value)
}

pub fn border_inline_start_style(value: String) -> Style {
  property("border-inline-start-style", value)
}

pub fn border_inline_start_width(value: Size) -> Style {
  property("border-inline-start-width", size.to_string(value))
}

pub fn border_inline_start_width_(value: String) -> Style {
  property("border-inline-start-width", value)
}

pub fn border_inline_style(value: String) -> Style {
  property("border-inline-style", value)
}

pub fn border_inline_width(value: Size) -> Style {
  property("border-inline-width", size.to_string(value))
}

pub fn border_inline_width_(value: String) -> Style {
  property("border-inline-width", value)
}

pub fn border_left(value: String) -> Style {
  property("border-left", value)
}

pub fn border_left_color(value: String) -> Style {
  property("border-left-color", value)
}

pub fn border_left_style(value: String) -> Style {
  property("border-left-style", value)
}

pub fn border_left_width(value: Size) -> Style {
  property("border-left-width", size.to_string(value))
}

pub fn border_left_width_(value: String) -> Style {
  property("border-left-width", value)
}

pub fn border_radius(border_radius: Size) -> Style {
  property("border-radius", size.to_string(border_radius))
}

pub fn border_radius_(border_radius: String) -> Style {
  property("border-radius", border_radius)
}

pub fn border_right(value: String) -> Style {
  property("border-right", value)
}

pub fn border_right_color(value: String) -> Style {
  property("border-right-color", value)
}

pub fn border_right_style(value: String) -> Style {
  property("border-right-style", value)
}

pub fn border_right_width(value: Size) -> Style {
  property("border-right-width", size.to_string(value))
}

pub fn border_right_width_(value: String) -> Style {
  property("border-right-width", value)
}

pub fn border_spacing(value: Size) -> Style {
  property("border-spacing", size.to_string(value))
}

pub fn border_spacing_(value: String) -> Style {
  property("border-spacing", value)
}

pub fn border_start_end_radius(value: String) -> Style {
  property("border-start-end-radius", value)
}

pub fn border_start_start_radius(value: String) -> Style {
  property("border-start-start-radius", value)
}

pub fn border_style(value: String) -> Style {
  property("border-style", value)
}

pub fn border_top(value: String) -> Style {
  property("border-top", value)
}

pub fn border_top_color(value: String) -> Style {
  property("border-top-color", value)
}

pub fn border_top_left_radius(border_top_left_radius: Size) -> Style {
  size.to_string(border_top_left_radius)
  |> property("border-top-left-radius", _)
}

pub fn border_top_left_radius_(border_top_left_radius: String) -> Style {
  property("border-top-left-radius", border_top_left_radius)
}

pub fn border_top_right_radius(border_top_right_radius: Size) -> Style {
  size.to_string(border_top_right_radius)
  |> property("border-top-right-radius", _)
}

pub fn border_top_right_radius_(border_top_right_radius: String) -> Style {
  property("border-top-right-radius", border_top_right_radius)
}

pub fn border_top_style(value: String) -> Style {
  property("border-top-style", value)
}

pub fn border_top_width(value: Size) -> Style {
  property("border-top-width", size.to_string(value))
}

pub fn border_top_width_(value: String) -> Style {
  property("border-top-width", value)
}

pub fn border_width(value: Size) -> Style {
  property("border-width", size.to_string(value))
}

pub fn border_width_(value: String) -> Style {
  property("border-width", value)
}

pub fn bottom(size: Size) -> Style {
  property("bottom", size.to_string(size))
}

pub fn bottom_(size: String) -> Style {
  property("bottom", size)
}

pub fn box_shadow(box_shadow: String) -> Style {
  property("box-shadow", box_shadow)
}

pub fn box_sizing(box_sizing: String) -> Style {
  property("box-sizing", box_sizing)
}

pub fn caption_side(value: String) -> Style {
  property("caption-side", value)
}

pub fn caret_color(value: String) -> Style {
  property("caret-color", value)
}

pub fn clear(value: String) -> Style {
  property("clear", value)
}

pub fn clip_path(value: String) -> Style {
  property("clip-path", value)
}

pub fn color(color: String) -> Style {
  property("color", color)
}

pub fn color_interpolation(value: String) -> Style {
  property("color-interpolation", value)
}

pub fn color_scheme(value: String) -> Style {
  property("color-scheme", value)
}

pub fn column_count(value: String) -> Style {
  property("column-count", value)
}

pub fn column_fill(value: String) -> Style {
  property("column-fill", value)
}

pub fn column_gap(column_gap: Size) -> Style {
  property("column-gap", size.to_string(column_gap))
}

pub fn column_rule(value: String) -> Style {
  property("column-rule", value)
}

pub fn column_rule_color(value: String) -> Style {
  property("column-rule-color", value)
}

pub fn column_rule_style(value: String) -> Style {
  property("column-rule-style", value)
}

pub fn column_rule_width(value: String) -> Style {
  property("column-rule-width", value)
}

pub fn column_span(value: String) -> Style {
  property("column-span", value)
}

pub fn column_width(value: String) -> Style {
  property("column-width", value)
}

pub fn columns(value: String) -> Style {
  property("columns", value)
}

pub fn contain(value: String) -> Style {
  property("contain", value)
}

pub fn contain_intrinsic_block_size(value: Size) -> Style {
  property("contain-intrinsic-block-size", size.to_string(value))
}

pub fn contain_intrinsic_block_size_(value: String) -> Style {
  property("contain-intrinsic-block-size", value)
}

pub fn contain_intrinsic_height(value: Size) -> Style {
  property("contain-intrinsic-height", size.to_string(value))
}

pub fn contain_intrinsic_height_(value: String) -> Style {
  property("contain-intrinsic-height", value)
}

pub fn contain_intrinsic_inline_size(value: Size) -> Style {
  property("contain-intrinsic-inline-size", size.to_string(value))
}

pub fn contain_intrinsic_inline_size_(value: String) -> Style {
  property("contain-intrinsic-inline-size", value)
}

pub fn contain_intrinsic_size(value: Size) -> Style {
  property("contain-intrinsic-size", size.to_string(value))
}

pub fn contain_intrinsic_size_(value: String) -> Style {
  property("contain-intrinsic-size", value)
}

pub fn contain_intrinsic_width(value: Size) -> Style {
  property("contain-intrinsic-width", size.to_string(value))
}

pub fn contain_intrinsic_width_(value: String) -> Style {
  property("contain-intrinsic-width", value)
}

pub fn container(value: String) -> Style {
  property("container", value)
}

pub fn container_name(value: String) -> Style {
  property("container-name", value)
}

pub fn container_type(value: String) -> Style {
  property("container-type", value)
}

pub fn content(value: String) -> Style {
  property("content", value)
}

pub fn counter_increment(value: String) -> Style {
  property("counter-increment", value)
}

pub fn counter_reset(value: String) -> Style {
  property("counter-reset", value)
}

pub fn counter_set(value: String) -> Style {
  property("counter-set", value)
}

pub fn cursor(cursor: String) -> Style {
  property("cursor", cursor)
}

pub fn cx(value: Size) -> Style {
  property("cx", size.to_string(value))
}

pub fn cx_(value: String) -> Style {
  property("cx", value)
}

pub fn cy(value: Size) -> Style {
  property("cy", size.to_string(value))
}

pub fn cy_(value: String) -> Style {
  property("cy", value)
}

pub fn direction(direction: String) -> Style {
  property("direction", direction)
}

pub fn display(display: String) -> Style {
  property("display", display)
}

pub fn empty_cells(empty_cells: String) -> Style {
  property("empty-cells", empty_cells)
}

pub fn field_sizing(field_sizing: String) -> Style {
  property("field-sizing", field_sizing)
}

pub fn filter(filter: String) -> Style {
  property("filter", filter)
}

pub fn flex(flex: String) -> Style {
  property("flex", flex)
}

pub fn flex_basis(flex_basis: String) -> Style {
  property("flex-basis", flex_basis)
}

pub fn flex_direction(flex_direction: String) -> Style {
  property("flex-direction", flex_direction)
}

pub fn flex_flow(flex_flow: String) -> Style {
  property("flex-flow", flex_flow)
}

pub fn flex_grow(flex_grow: Int) -> Style {
  property("flex-grow", int.to_string(flex_grow))
}

pub fn flex_grow_(flex_grow: String) -> Style {
  property("flex-grow", flex_grow)
}

pub fn flex_shrink(flex_shrink: Float) -> Style {
  property("flex-shrink", float.to_string(flex_shrink))
}

pub fn flex_shrink_(flex_shrink: String) -> Style {
  property("flex-shrink", flex_shrink)
}

pub fn flex_wrap(flex_wrap: String) -> Style {
  property("flex-wrap", flex_wrap)
}

pub fn float(float: String) -> Style {
  property("float", float)
}

pub fn font(value: String) -> Style {
  property("font", value)
}

pub fn font_family(font_family: String) -> Style {
  property("font-family", font_family)
}

pub fn font_feature_settings(value: String) -> Style {
  property("font-feature-settings", value)
}

pub fn font_kerning(value: String) -> Style {
  property("font-kerning", value)
}

pub fn font_language_override(value: String) -> Style {
  property("font-language-override", value)
}

pub fn font_optical_sizing(value: String) -> Style {
  property("font-optical-sizing", value)
}

pub fn font_palette(value: String) -> Style {
  property("font-palette", value)
}

pub fn font_size(font_size: Size) -> Style {
  property("font-size", size.to_string(font_size))
}

pub fn font_size_(font_size: String) -> Style {
  property("font-size", font_size)
}

pub fn font_size_adjust(value: String) -> Style {
  property("font-size-adjust", value)
}

pub fn font_stretch(value: String) -> Style {
  property("font-stretch", value)
}

pub fn font_style(font_style: String) -> Style {
  property("font-style", font_style)
}

pub fn font_synthesis(value: String) -> Style {
  property("font-synthesis", value)
}

pub fn font_synthesis_position(value: String) -> Style {
  property("font-synthesis-position", value)
}

pub fn font_synthesis_small_caps(value: String) -> Style {
  property("font-synthesis-small-caps", value)
}

pub fn font_synthesis_style(value: String) -> Style {
  property("font-synthesis-style", value)
}

pub fn font_synthesis_weight(value: String) -> Style {
  property("font-synthesis-weight", value)
}

pub fn font_variant(value: String) -> Style {
  property("font-variant", value)
}

pub fn font_variant_alternates(value: String) -> Style {
  property("font-variant-alternates", value)
}

pub fn font_variant_caps(value: String) -> Style {
  property("font-variant-caps", value)
}

pub fn font_variant_east_asian(value: String) -> Style {
  property("font-variant-east-asian", value)
}

pub fn font_variant_emoji(value: String) -> Style {
  property("font-variant-emoji", value)
}

pub fn font_variant_ligatures(value: String) -> Style {
  property("font-variant-ligatures", value)
}

pub fn font_variant_numeric(value: String) -> Style {
  property("font-variant-numeric", value)
}

pub fn font_variant_position(value: String) -> Style {
  property("font-variant-position", value)
}

pub fn font_variation_settings(value: String) -> Style {
  property("font-variation-settings", value)
}

pub fn font_weight(font_weight: String) -> Style {
  property("font-weight", font_weight)
}

pub fn forced_color_adjust(value: String) -> Style {
  property("forced-color-adjust", value)
}

pub fn gap(gap: Size) -> Style {
  property("gap", size.to_string(gap))
}

pub fn gap_(gap: String) -> Style {
  property("gap", gap)
}

pub fn grid_area(grid_area: String) -> Style {
  property("grid-area", grid_area)
}

pub fn grid_auto_columns(grid_auto_columns: String) -> Style {
  property("grid-auto-columns", grid_auto_columns)
}

pub fn grid_auto_flow(grid_auto_flow: String) -> Style {
  property("grid-auto-flow", grid_auto_flow)
}

pub fn grid_auto_rows(grid_auto_rows: String) -> Style {
  property("grid-auto-rows", grid_auto_rows)
}

pub fn grid_column(grid_column: String) -> Style {
  property("grid-column", grid_column)
}

pub fn grid_column_end(grid_column: String) -> Style {
  property("grid-column-end", grid_column)
}

pub fn grid_column_start(grid_column: String) -> Style {
  property("grid-column-start", grid_column)
}

pub fn grid_row(grid_row: String) -> Style {
  property("grid-row", grid_row)
}

pub fn grid_row_end(grid_row: String) -> Style {
  property("grid-row-end", grid_row)
}

pub fn grid_row_start(grid_row: String) -> Style {
  property("grid-row-start", grid_row)
}

pub fn grid_template(grid_template: String) -> Style {
  property("grid-template", grid_template)
}

pub fn grid_template_areas(grid_template_areas: List(String)) -> Style {
  grid_template_areas
  |> list.map(fn(content) { "\"" <> content <> "\"" })
  |> string.join("\n")
  |> property("grid-template-areas", _)
}

pub fn grid_template_columns(grid_template_columns: String) -> Style {
  property("grid-template-columns", grid_template_columns)
}

pub fn grid_template_rows(grid_template_rows: String) -> Style {
  property("grid-template-rows", grid_template_rows)
}

pub fn hanging_punctuation(value: String) -> Style {
  property("hanging-punctuation", value)
}

pub fn height(height: Size) -> Style {
  property("height", size.to_string(height))
}

pub fn height_(height: String) -> Style {
  property("height", height)
}

pub fn hyphenate_character(value: String) -> Style {
  property("hyphenate-character", value)
}

pub fn hyphenate_limit_chars(value: String) -> Style {
  property("hyphenate-limit-chars", value)
}

pub fn hyphens(value: String) -> Style {
  property("hyphens", value)
}

pub fn image_orientation(value: String) -> Style {
  property("image-orientation", value)
}

pub fn image_rendering(value: String) -> Style {
  property("image-rendering", value)
}

pub fn inline_size(value: String) -> Style {
  property("inline-size", value)
}

pub fn inset(value: String) -> Style {
  property("inset", value)
}

pub fn inset_area(value: String) -> Style {
  property("inset-area", value)
}

pub fn inset_block(value: String) -> Style {
  property("inset-block", value)
}

pub fn inset_block_end(value: String) -> Style {
  property("inset-block-end", value)
}

pub fn inset_block_start(value: String) -> Style {
  property("inset-block-start", value)
}

pub fn inset_inline(value: String) -> Style {
  property("inset-inline", value)
}

pub fn inset_inline_end(value: String) -> Style {
  property("inset-inline-end", value)
}

pub fn inset_inline_start(value: String) -> Style {
  property("inset-inline-start", value)
}

pub fn isolation(value: String) -> Style {
  property("isolation", value)
}

pub fn justify_content(justify: String) -> Style {
  property("justify-content", justify)
}

pub fn justify_items(justify: String) -> Style {
  property("justify-items", justify)
}

pub fn justify_self(justify: String) -> Style {
  property("justify-self", justify)
}

pub fn justify_tracks(justify: String) -> Style {
  property("justify-tracks", justify)
}

pub fn left(size: Size) -> Style {
  property("left", size.to_string(size))
}

pub fn left_(size: String) -> Style {
  property("left", size)
}

pub fn letter_spacing(letter_spacing: String) -> Style {
  property("letter-spacing", letter_spacing)
}

pub fn line_break(line_break: String) -> Style {
  property("line-break", line_break)
}

pub fn line_height(line_height: String) -> Style {
  property("line-height", line_height)
}

pub fn list_style(list_style: String) -> Style {
  property("list-style", list_style)
}

pub fn list_style_image(list_style_image: String) -> Style {
  property("list-style-image", list_style_image)
}

pub fn list_style_position(list_style_position: String) -> Style {
  property("list-style-position", list_style_position)
}

pub fn list_style_type(list_style_type: String) -> Style {
  property("list-style-type", list_style_type)
}

pub fn margin(margin: Size) -> Style {
  property("margin", size.to_string(margin))
}

pub fn margin_(margin: String) -> Style {
  property("margin", margin)
}

pub fn margin_block(margin: Size) -> Style {
  property("margin-block", size.to_string(margin))
}

pub fn margin_block_(margin: String) -> Style {
  property("margin-block", margin)
}

pub fn margin_block_end(margin: Size) -> Style {
  property("margin-block-end", size.to_string(margin))
}

pub fn margin_block_end_(margin: String) -> Style {
  property("margin-block-end", margin)
}

pub fn margin_block_start(margin: Size) -> Style {
  property("margin-block-start", size.to_string(margin))
}

pub fn margin_block_start_(margin: String) -> Style {
  property("margin-block-start", margin)
}

pub fn margin_bottom(margin: Size) -> Style {
  property("margin-bottom", size.to_string(margin))
}

pub fn margin_bottom_(margin: String) -> Style {
  property("margin-bottom", margin)
}

pub fn margin_inline(margin: Size) -> Style {
  property("margin-inline", size.to_string(margin))
}

pub fn margin_inline_(margin: String) -> Style {
  property("margin-inline", margin)
}

pub fn margin_inline_end(margin: Size) -> Style {
  property("margin-inline-end", size.to_string(margin))
}

pub fn margin_inline_end_(margin: String) -> Style {
  property("margin-inline-end", margin)
}

pub fn margin_inline_start(margin: Size) -> Style {
  property("margin-inline-start", size.to_string(margin))
}

pub fn margin_inline_start_(margin: String) -> Style {
  property("margin-inline-start", margin)
}

pub fn margin_left(margin: Size) -> Style {
  property("margin-left", size.to_string(margin))
}

pub fn margin_left_(margin: String) -> Style {
  property("margin-left", margin)
}

pub fn margin_right(margin: Size) -> Style {
  property("margin-right", size.to_string(margin))
}

pub fn margin_right_(margin: String) -> Style {
  property("margin-right", margin)
}

pub fn margin_top(margin: Size) -> Style {
  property("margin-top", size.to_string(margin))
}

pub fn margin_top_(margin: String) -> Style {
  property("margin-top", margin)
}

pub fn mask(value: String) -> Style {
  property("mask", value)
}

pub fn mask_border(value: String) -> Style {
  property("mask-border", value)
}

pub fn mask_border_mode(value: String) -> Style {
  property("mask-border-mode", value)
}

pub fn mask_border_outset(value: String) -> Style {
  property("mask-border-outset", value)
}

pub fn mask_border_repeat(value: String) -> Style {
  property("mask-border-repeat", value)
}

pub fn mask_border_slice(value: String) -> Style {
  property("mask-border-slice", value)
}

pub fn mask_border_source(value: String) -> Style {
  property("mask-border-source", value)
}

pub fn mask_border_width(value: String) -> Style {
  property("mask-border-width", value)
}

pub fn mask_clip(value: String) -> Style {
  property("mask-clip", value)
}

pub fn mask_composite(value: String) -> Style {
  property("mask-composite", value)
}

pub fn mask_image(value: String) -> Style {
  property("mask-image", value)
}

pub fn mask_mode(value: String) -> Style {
  property("mask-mode", value)
}

pub fn mask_origin(value: String) -> Style {
  property("mask-origin", value)
}

pub fn mask_position(value: String) -> Style {
  property("mask-position", value)
}

pub fn mask_repeat(value: String) -> Style {
  property("mask-repeat", value)
}

pub fn mask_size(value: String) -> Style {
  property("mask-size", value)
}

pub fn mask_type(value: String) -> Style {
  property("mask-type", value)
}

pub fn math_depth(value: String) -> Style {
  property("math-depth", value)
}

pub fn math_style(value: String) -> Style {
  property("math-style", value)
}

pub fn max_block_size(value: Size) -> Style {
  property("max-block-size", size.to_string(value))
}

pub fn max_block_size_(value: String) -> Style {
  property("max-block-size", value)
}

pub fn max_height(height: Size) -> Style {
  property("max-height", size.to_string(height))
}

pub fn max_height_(height: String) -> Style {
  property("max-height", height)
}

pub fn max_inline_size(value: Size) -> Style {
  property("max-inline-size", size.to_string(value))
}

pub fn max_inline_size_(value: String) -> Style {
  property("max-inline-size", value)
}

pub fn max_width(width: Size) -> Style {
  property("max-width", size.to_string(width))
}

pub fn max_width_(width: String) -> Style {
  property("max-width", width)
}

pub fn min_block_size(value: Size) -> Style {
  property("min-block-size", size.to_string(value))
}

pub fn min_block_size_(value: String) -> Style {
  property("min-block-size", value)
}

pub fn min_height(height: Size) -> Style {
  property("min-height", size.to_string(height))
}

pub fn min_height_(height: String) -> Style {
  property("min-height", height)
}

pub fn min_inline_size(value: Size) -> Style {
  property("min-inline-size", size.to_string(value))
}

pub fn min_inline_size_(value: String) -> Style {
  property("min-inline-size", value)
}

pub fn min_width(width: Size) -> Style {
  property("min-width", size.to_string(width))
}

pub fn min_width_(width: String) -> Style {
  property("min-width", width)
}

pub fn mix_blend_mode(value: String) -> Style {
  property("mix-blend-mode", value)
}

pub fn object_fit(object_fit: String) -> Style {
  property("object-fit", object_fit)
}

pub fn object_position(object_position: String) -> Style {
  property("object-position", object_position)
}

pub fn offset(offset: String) -> Style {
  property("offset", offset)
}

pub fn offset_anchor(offset_anchor: String) -> Style {
  property("offset-anchor", offset_anchor)
}

pub fn offset_distance(offset_distance: String) -> Style {
  property("offset-distance", offset_distance)
}

pub fn offset_path(offset_path: String) -> Style {
  property("offset-path", offset_path)
}

pub fn offset_position(offset_position: String) -> Style {
  property("offset-position", offset_position)
}

pub fn offset_rotate(offset_rotate: String) -> Style {
  property("offset-rotate", offset_rotate)
}

pub fn opacity(opacity: Float) -> Style {
  property("opacity", float.to_string(opacity))
}

pub fn order(value: Int) -> Style {
  property("order", int.to_string(value))
}

pub fn order_(value: String) -> Style {
  property("order", value)
}

pub fn orphans(value: Int) -> Style {
  property("orphans", int.to_string(value))
}

pub fn orphans_(value: String) -> Style {
  property("orphans", value)
}

pub fn outline(outline: String) -> Style {
  property("outline", outline)
}

pub fn outline_color(outline_color: String) -> Style {
  property("outline-color", outline_color)
}

pub fn outline_offset(outline_offset: String) -> Style {
  property("outline-offset", outline_offset)
}

pub fn outline_style(outline_style: String) -> Style {
  property("outline-style", outline_style)
}

pub fn outline_width(outline_width: String) -> Style {
  property("outline-width", outline_width)
}

pub fn overflow(overflow: String) -> Style {
  property("overflow", overflow)
}

pub fn overflow_anchor(overflow_anchor: String) -> Style {
  property("overflow-anchor", overflow_anchor)
}

pub fn overflow_block(overflow_block: String) -> Style {
  property("overflow-block", overflow_block)
}

pub fn overflow_clip_margin(overflow_clip_margin: String) -> Style {
  property("overflow-clip-margin", overflow_clip_margin)
}

pub fn overflow_inline(overflow_inline: String) -> Style {
  property("overflow-inline", overflow_inline)
}

pub fn overflow_wrap(overflow_wrap: String) -> Style {
  property("overflow-wrap", overflow_wrap)
}

pub fn overflow_x(overflow_x: String) -> Style {
  property("overflow-x", overflow_x)
}

pub fn overflow_y(overflow_y: String) -> Style {
  property("overflow-y", overflow_y)
}

pub fn overscroll_behavior(value: String) -> Style {
  property("overscroll-behavior", value)
}

pub fn overscroll_behavior_block(value: String) -> Style {
  property("overscroll-behavior-block", value)
}

pub fn overscroll_behavior_inline(value: String) -> Style {
  property("overscroll-behavior-inline", value)
}

pub fn overscroll_behavior_x(value: String) -> Style {
  property("overscroll-behavior-x", value)
}

pub fn overscroll_behavior_y(value: String) -> Style {
  property("overscroll-behavior-y", value)
}

pub fn padding(padding: Size) -> Style {
  property("padding", size.to_string(padding))
}

pub fn padding_(padding: String) -> Style {
  property("padding", padding)
}

pub fn padding_block(padding: Size) -> Style {
  property("padding-block", size.to_string(padding))
}

pub fn padding_block_(padding: String) -> Style {
  property("padding-block", padding)
}

pub fn padding_block_end(padding: Size) -> Style {
  property("padding-block-end", size.to_string(padding))
}

pub fn padding_block_end_(padding: String) -> Style {
  property("padding-block-end", padding)
}

pub fn padding_block_start(padding: Size) -> Style {
  property("padding-block-start", size.to_string(padding))
}

pub fn padding_block_start_(padding: String) -> Style {
  property("padding-block-start", padding)
}

pub fn padding_bottom(padding: Size) -> Style {
  property("padding-bottom", size.to_string(padding))
}

pub fn padding_inline(padding: Size) -> Style {
  property("padding-inline", size.to_string(padding))
}

pub fn padding_inline_(padding: String) -> Style {
  property("padding-inline", padding)
}

pub fn padding_inline_end(padding: Size) -> Style {
  property("padding-inline-end", size.to_string(padding))
}

pub fn padding_inline_end_(padding: String) -> Style {
  property("padding-inline-end", padding)
}

pub fn padding_inline_start(padding: Size) -> Style {
  property("padding-inline-start", size.to_string(padding))
}

pub fn padding_inline_start_(padding: String) -> Style {
  property("padding-inline-start", padding)
}

pub fn padding_left(padding: Size) -> Style {
  property("padding-left", size.to_string(padding))
}

pub fn padding_right(padding: Size) -> Style {
  property("padding-right", size.to_string(padding))
}

pub fn padding_top(padding: Size) -> Style {
  property("padding-top", size.to_string(padding))
}

pub fn page(value: String) -> Style {
  property("page", value)
}

pub fn page_break_after(value: String) -> Style {
  property("page-break-after", value)
}

pub fn page_break_before(value: String) -> Style {
  property("page-break-before", value)
}

pub fn page_break_inside(value: String) -> Style {
  property("page-break-inside", value)
}

pub fn paint_order(value: String) -> Style {
  property("paint-order", value)
}

pub fn perspective(value: String) -> Style {
  property("perspective", value)
}

pub fn perspective_origin(value: String) -> Style {
  property("perspective-origin", value)
}

pub fn place_content(place: String) -> Style {
  property("place-content", place)
}

pub fn place_items(place: String) -> Style {
  property("place-items", place)
}

pub fn place_self(place: String) -> Style {
  property("place-self", place)
}

pub fn pointer_events(pointer_events: String) -> Style {
  property("pointer-events", pointer_events)
}

pub fn position(position: String) -> Style {
  property("position", position)
}

pub fn print_color_adjust(print: String) -> Style {
  property("print-color-adjust", print)
}

pub fn quotes(quotes: String) -> Style {
  property("quotes", quotes)
}

pub fn r(r: String) -> Style {
  property("r", r)
}

pub fn resize(value: String) -> Style {
  property("resize", value)
}

pub fn right(size: Size) -> Style {
  property("right", size.to_string(size))
}

pub fn right_(size: String) -> Style {
  property("right", size)
}

pub fn rotate(value: String) -> Style {
  property("rotate", value)
}

pub fn row_gap(row_gap: Size) -> Style {
  property("row-gap", size.to_string(row_gap))
}

pub fn ruby_position(value: String) -> Style {
  property("ruby-position", value)
}

pub fn rx(value: Size) -> Style {
  property("rx", size.to_string(value))
}

pub fn rx_(value: String) -> Style {
  property("rx", value)
}

pub fn ry(value: Size) -> Style {
  property("ry", size.to_string(value))
}

pub fn ry_(value: String) -> Style {
  property("ry", value)
}

pub fn scale(value: String) -> Style {
  property("scale", value)
}

pub fn scroll_behavior(value: String) -> Style {
  property("scroll-behavior", value)
}

pub fn scroll_margin(value: Size) -> Style {
  property("scroll-margin", size.to_string(value))
}

pub fn scroll_margin_(value: String) -> Style {
  property("scroll-margin", value)
}

pub fn scroll_margin_block(value: Size) -> Style {
  property("scroll-margin-block", size.to_string(value))
}

pub fn scroll_margin_block_(value: String) -> Style {
  property("scroll-margin-block", value)
}

pub fn scroll_margin_block_end(value: Size) -> Style {
  property("scroll-margin-block-end", size.to_string(value))
}

pub fn scroll_margin_block_end_(value: String) -> Style {
  property("scroll-margin-block-end", value)
}

pub fn scroll_margin_block_start(value: Size) -> Style {
  property("scroll-margin-block-start", size.to_string(value))
}

pub fn scroll_margin_block_start_(value: String) -> Style {
  property("scroll-margin-block-start", value)
}

pub fn scroll_margin_bottom(value: Size) -> Style {
  property("scroll-margin-bottom", size.to_string(value))
}

pub fn scroll_margin_bottom_(value: String) -> Style {
  property("scroll-margin-bottom", value)
}

pub fn scroll_margin_inline(value: Size) -> Style {
  property("scroll-margin-inline", size.to_string(value))
}

pub fn scroll_margin_inline_(value: String) -> Style {
  property("scroll-margin-inline", value)
}

pub fn scroll_margin_inline_end(value: Size) -> Style {
  property("scroll-margin-inline-end", size.to_string(value))
}

pub fn scroll_margin_inline_end_(value: String) -> Style {
  property("scroll-margin-inline-end", value)
}

pub fn scroll_margin_inline_start(value: Size) -> Style {
  property("scroll-margin-inline-start", size.to_string(value))
}

pub fn scroll_margin_inline_start_(value: String) -> Style {
  property("scroll-margin-inline-start", value)
}

pub fn scroll_margin_left(value: Size) -> Style {
  property("scroll-margin-left", size.to_string(value))
}

pub fn scroll_margin_left_(value: String) -> Style {
  property("scroll-margin-left", value)
}

pub fn scroll_margin_right(value: Size) -> Style {
  property("scroll-margin-right", size.to_string(value))
}

pub fn scroll_margin_right_(value: String) -> Style {
  property("scroll-margin-right", value)
}

pub fn scroll_margin_top(value: Size) -> Style {
  property("scroll-margin-top", size.to_string(value))
}

pub fn scroll_margin_top_(value: String) -> Style {
  property("scroll-margin-top", value)
}

pub fn scroll_padding(value: Size) -> Style {
  property("scroll-padding", size.to_string(value))
}

pub fn scroll_padding_(value: String) -> Style {
  property("scroll-padding", value)
}

pub fn scroll_padding_block(value: Size) -> Style {
  property("scroll-padding-block", size.to_string(value))
}

pub fn scroll_padding_block_(value: String) -> Style {
  property("scroll-padding-block", value)
}

pub fn scroll_padding_block_end(value: Size) -> Style {
  property("scroll-padding-block-end", size.to_string(value))
}

pub fn scroll_padding_block_end_(value: String) -> Style {
  property("scroll-padding-block-end", value)
}

pub fn scroll_padding_block_start(value: Size) -> Style {
  property("scroll-padding-block-start", size.to_string(value))
}

pub fn scroll_padding_block_start_(value: String) -> Style {
  property("scroll-padding-block-start", value)
}

pub fn scroll_padding_bottom(value: Size) -> Style {
  property("scroll-padding-bottom", size.to_string(value))
}

pub fn scroll_padding_bottom_(value: String) -> Style {
  property("scroll-padding-bottom", value)
}

pub fn scroll_padding_inline(value: Size) -> Style {
  property("scroll-padding-inline", size.to_string(value))
}

pub fn scroll_padding_inline_(value: String) -> Style {
  property("scroll-padding-inline", value)
}

pub fn scroll_padding_inline_end(value: Size) -> Style {
  property("scroll-padding-inline-end", size.to_string(value))
}

pub fn scroll_padding_inline_end_(value: String) -> Style {
  property("scroll-padding-inline-end", value)
}

pub fn scroll_padding_inline_start(value: Size) -> Style {
  property("scroll-padding-inline-start", size.to_string(value))
}

pub fn scroll_padding_inline_start_(value: String) -> Style {
  property("scroll-padding-inline-start", value)
}

pub fn scroll_padding_left(value: Size) -> Style {
  property("scroll-padding-left", size.to_string(value))
}

pub fn scroll_padding_left_(value: String) -> Style {
  property("scroll-padding-left", value)
}

pub fn scroll_padding_right(value: Size) -> Style {
  property("scroll-padding-right", size.to_string(value))
}

pub fn scroll_padding_right_(value: String) -> Style {
  property("scroll-padding-right", value)
}

pub fn scroll_padding_top(value: Size) -> Style {
  property("scroll-padding-top", size.to_string(value))
}

pub fn scroll_padding_top_(value: String) -> Style {
  property("scroll-padding-top", value)
}

pub fn scroll_snap_align(value: String) -> Style {
  property("scroll-snap-align", value)
}

pub fn scroll_snap_stop(value: String) -> Style {
  property("scroll-snap-stop", value)
}

pub fn scroll_snap_type(value: String) -> Style {
  property("scroll-snap-type", value)
}

pub fn scrollbar_color(value: String) -> Style {
  property("scrollbar-color", value)
}

pub fn scrollbar_gutter(value: String) -> Style {
  property("scrollbar-gutter", value)
}

pub fn scrollbar_width(value: String) -> Style {
  property("scrollbar-width", value)
}

pub fn shape_image_threshold(value: Float) -> Style {
  property("shape-image-threshold", float.to_string(value))
}

pub fn shape_image_threshold_(value: String) -> Style {
  property("shape-image-threshold", value)
}

pub fn shape_margin(value: Size) -> Style {
  property("shape-margin", size.to_string(value))
}

pub fn shape_margin_(value: String) -> Style {
  property("shape-margin", value)
}

pub fn shape_outside(value: String) -> Style {
  property("shape-outside", value)
}

pub fn tab_size(size: Size) -> Style {
  property("tab-size", size.to_string(size))
}

pub fn tab_size_(size: String) -> Style {
  property("tab-size", size)
}

pub fn table_layout(layout: String) -> Style {
  property("table-layout", layout)
}

pub fn text_align(text_align: String) -> Style {
  property("text-align", text_align)
}

pub fn text_align_last(value: String) -> Style {
  property("text-align-last", value)
}

pub fn text_combine_upright(value: String) -> Style {
  property("text-combine-upright", value)
}

pub fn text_decoration(text_decoration: String) -> Style {
  property("text-decoration", text_decoration)
}

pub fn text_decoration_color(value: String) -> Style {
  property("text-decoration-color", value)
}

pub fn text_decoration_line(value: String) -> Style {
  property("text-decoration-line", value)
}

pub fn text_decoration_skip_ink(value: String) -> Style {
  property("text-decoration-skip-ink", value)
}

pub fn text_decoration_style(value: String) -> Style {
  property("text-decoration-style", value)
}

pub fn text_decoration_thickness(value: String) -> Style {
  property("text-decoration-thickness", value)
}

pub fn text_emphasis(value: String) -> Style {
  property("text-emphasis", value)
}

pub fn text_emphasis_color(value: String) -> Style {
  property("text-emphasis-color", value)
}

pub fn text_emphasis_position(value: String) -> Style {
  property("text-emphasis-position", value)
}

pub fn text_emphasis_style(value: String) -> Style {
  property("text-emphasis-style", value)
}

pub fn text_indent(value: String) -> Style {
  property("text-indent", value)
}

pub fn text_justify(text_justify: String) -> Style {
  property("text-justify", text_justify)
}

pub fn text_orientation(value: String) -> Style {
  property("text-orientation", value)
}

pub fn text_overflow(text_overflow: String) -> Style {
  property("text-overflow", text_overflow)
}

pub fn text_rendering(value: String) -> Style {
  property("text-rendering", value)
}

pub fn text_shadow(value: String) -> Style {
  property("text-shadow", value)
}

pub fn text_transform(text_transform: String) -> Style {
  property("text-transform", text_transform)
}

pub fn text_underline_offset(value: Size) -> Style {
  property("text-underline-offset", size.to_string(value))
}

pub fn text_underline_offset_(value: String) -> Style {
  property("text-underline-offset", value)
}

pub fn text_underline_position(value: String) -> Style {
  property("text-underline-position", value)
}

pub fn text_wrap(value: String) -> Style {
  property("text-wrap", value)
}

pub fn text_wrap_mode(value: String) -> Style {
  property("text-wrap-mode", value)
}

pub fn text_wrap_style(value: String) -> Style {
  property("text-wrap-style", value)
}

pub fn top(size: Size) -> Style {
  property("top", size.to_string(size))
}

pub fn top_(size: String) -> Style {
  property("top", size)
}

pub fn touch_action(value: String) -> Style {
  property("touch-action", value)
}

/// `transform` uses `css.transform` to offer an enhanced API for CSS transforms.
pub fn transform(transform_args: List(Transform)) -> Style {
  let transform_string = transform.to_string(transform_args)
  property("transform", transform_string)
}

pub fn transform_(transform: String) -> Style {
  property("transform", transform)
}

pub fn transform_box(transform_box: String) -> Style {
  property("transform-box", transform_box)
}

pub fn transform_origin(transform_origin: String) -> Style {
  property("transform-origin", transform_origin)
}

pub fn transform_style(transform_style: String) -> Style {
  property("transform-style", transform_style)
}

pub fn transition(transition: String) -> Style {
  property("transition", transition)
}

pub fn transition_behavior(value: String) -> Style {
  property("transition-behavior", value)
}

pub fn transition_delay(value: String) -> Style {
  property("transition-delay", value)
}

pub fn transition_duration(value: String) -> Style {
  property("transition-duration", value)
}

pub fn transition_property(value: String) -> Style {
  property("transition-property", value)
}

pub fn transition_timing_function(value: String) -> Style {
  property("transition-timing-function", value)
}

pub fn translate(translate: String) -> Style {
  property("translate", translate)
}

pub fn unicode_bidi(value: String) -> Style {
  property("unicode-bidi", value)
}

pub fn user_select(user_select: String) -> Style {
  property("user-select", user_select)
}

pub fn vertical_align(value: String) -> Style {
  property("vertical-align", value)
}

pub fn visibility(visibility: String) -> Style {
  property("visibility", visibility)
}

pub fn white_space(white_space: String) -> Style {
  property("white-space", white_space)
}

pub fn white_space_collapse(white_space_collapse: String) -> Style {
  property("white-space-collapse", white_space_collapse)
}

pub fn widows(value: String) -> Style {
  property("widows", value)
}

pub fn width(width: Size) -> Style {
  property("width", size.to_string(width))
}

pub fn width_(width: String) -> Style {
  property("width", width)
}

pub fn will_change(value: String) -> Style {
  property("will-change", value)
}

pub fn word_break(word_break: String) -> Style {
  property("word-break", word_break)
}

pub fn word_spacing(word_spacing: String) -> Style {
  property("word-spacing", word_spacing)
}

pub fn word_wrap(word_wrap: String) -> Style {
  property("word-wrap", word_wrap)
}

pub fn writing_mode(value: String) -> Style {
  property("writing-mode", value)
}

pub fn z_index(z_index: Int) -> Style {
  property("z-index", int.to_string(z_index))
}

pub fn zoom(value: String) -> Style {
  property("zoom", value)
}

pub fn none() -> Style {
  style.NoStyle
}

pub fn property(field: String, content: String) -> Style {
  style.Property(field, content, False)
}

// Media queries
// Should be used with the media module.

pub fn media(query: Query, styles: List(Style)) -> Style {
  let media_selector = media.to_string(query)
  style.Media(media_selector, styles)
}

// Pseudo-selectors
// Contains pseudo-classes and pseudo-elements.
pub fn placeholder(styles: List(Style)) -> Style {
  pseudo_selector("::placeholder", styles)
}

pub fn hover(styles: List(Style)) -> Style {
  pseudo_selector(":hover", styles)
}

pub fn active(styles: List(Style)) -> Style {
  pseudo_selector(":active", styles)
}

pub fn focus(styles: List(Style)) -> Style {
  pseudo_selector(":focus", styles)
}

pub fn focus_visible(styles: List(Style)) -> Style {
  pseudo_selector(":focus-visible", styles)
}

pub fn focus_within(styles: List(Style)) -> Style {
  pseudo_selector(":focus-within", styles)
}

pub fn enabled(styles: List(Style)) -> Style {
  pseudo_selector(":enabled", styles)
}

pub fn disabled(styles: List(Style)) -> Style {
  pseudo_selector(":disabled", styles)
}

pub fn read_only(styles: List(Style)) -> Style {
  pseudo_selector(":read-only", styles)
}

pub fn read_write(styles: List(Style)) -> Style {
  pseudo_selector(":read-write", styles)
}

pub fn checked(styles: List(Style)) -> Style {
  pseudo_selector(":checked", styles)
}

pub fn blank(styles: List(Style)) -> Style {
  pseudo_selector(":blank", styles)
}

pub fn valid(styles: List(Style)) -> Style {
  pseudo_selector(":valid", styles)
}

pub fn invalid(styles: List(Style)) -> Style {
  pseudo_selector(":invalid", styles)
}

pub fn required(styles: List(Style)) -> Style {
  pseudo_selector(":required", styles)
}

pub fn optional(styles: List(Style)) -> Style {
  pseudo_selector(":optional", styles)
}

pub fn link(styles: List(Style)) -> Style {
  pseudo_selector(":link", styles)
}

pub fn visited(styles: List(Style)) -> Style {
  pseudo_selector(":visited", styles)
}

pub fn target(styles: List(Style)) -> Style {
  pseudo_selector(":target", styles)
}

pub fn nth_child(selector: String, styles: List(Style)) -> Style {
  pseudo_selector(string.append(":nth-child", selector), styles)
}

pub fn nth_last_child(selector: String, styles: List(Style)) -> Style {
  pseudo_selector(string.append(":nth-last-child", selector), styles)
}

pub fn nth_of_type(selector: String, styles: List(Style)) -> Style {
  pseudo_selector(string.append(":nth-of-type", selector), styles)
}

pub fn nth_last_of_type(selector: String, styles: List(Style)) -> Style {
  pseudo_selector(string.append(":nth-last-of-type", selector), styles)
}

pub fn first_child(styles: List(Style)) -> Style {
  pseudo_selector(":first-child", styles)
}

pub fn last_child(styles: List(Style)) -> Style {
  pseudo_selector(":last-child", styles)
}

pub fn only_child(styles: List(Style)) -> Style {
  pseudo_selector(":only-child", styles)
}

pub fn first_of_type(styles: List(Style)) -> Style {
  pseudo_selector(":first-of-type", styles)
}

pub fn last_of_type(styles: List(Style)) -> Style {
  pseudo_selector(":last-of-type", styles)
}

pub fn only_of_type(styles: List(Style)) -> Style {
  pseudo_selector(":only-of-type", styles)
}

pub fn pseudo_selector(value: String, styles: List(Style)) -> Style {
  style.PseudoSelector(value, styles)
}

/// Add an `!important` flag to any CSS property.
/// It won't have any effect on non-property style, like media, etc. It will
/// then act as the `identity` function.
pub fn important(style: Style) -> Style {
  case style {
    style.Property(key, value, _) -> style.Property(key, value, True)
    any -> any
  }
}

/// Compose styles by inheriting class, and later overrides them.
/// Every styles from the class will be composed in the resulting class.
pub fn compose(class) -> Style {
  style.ClassName(class)
}
