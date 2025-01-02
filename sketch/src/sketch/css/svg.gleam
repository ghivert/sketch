import sketch/css.{type Style, property}
import sketch/css/size.{type Size}

/// The `clip-rule` CSS property defines how to determine which pixels in a
/// mask's box are inside the clipping shape defined by a clip path, and which
/// are outside, when parts of the path overlap other parts. Specifically, it
/// chooses between the "non-zero" and "even-odd" methods of determining
/// inclusion. `clip-rule` can be applied to all SVG elements, but only has an
/// effect on those which are part of a clipping path. CSS values of the
/// `clip-rule` property can override SVG values of the
/// [`clip-rule`](https://developer.mozilla.org/docs/Web/SVG/Attribute/clip-rule) attribute.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/clip-rule)
pub fn clip_rule(value: String) -> Style {
  property("clip-rule", value)
}

/// The `cx` CSS property defines the x-axis center point of an SVG `<circle>`
/// or `<ellipse>` element. If present, it overrides the element's `cx` attribute.
///
/// > While SVG the `cx` attribute is relevant to the SVG `<radialGradient>`
/// > element, the `cx` property only applies to `<circle>` and `<ellipse>`
/// > elements nested in an `<svg>`. It doesn't apply to `<radialGradient>` or
/// > other SVG elements nor to HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/cx)
pub fn cx(value: Size) -> Style {
  property("cx", size.to_string(value))
}

/// The `cx` CSS property defines the x-axis center point of an SVG `<circle>`
/// or `<ellipse>` element. If present, it overrides the element's `cx` attribute.
///
/// > While SVG the `cx` attribute is relevant to the SVG `<radialGradient>`
/// > element, the `cx` property only applies to `<circle>` and `<ellipse>`
/// > elements nested in an `<svg>`. It doesn't apply to `<radialGradient>` or
/// > other SVG elements nor to HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/cx)
pub fn cx_(value: String) -> Style {
  property("cx", value)
}

/// The `cy` CSS property defines the y-axis center point of an SVG `<circle>`
/// or `<ellipse>` element. If present, it overrides the element's `cy` attribute.
///
/// > While SVG the `cy` attribute is relevant to the SVG `<radialGradient>`
/// > element, the `cy` property only applies to `<circle>` and `<ellipse>`
/// > elements nested in an `<svg>`. It doesn't apply to `<radialGradient>` or
/// > other SVG elements nor to HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/cy)
pub fn cy(value: Size) -> Style {
  property("cy", size.to_string(value))
}

/// The `cy` CSS property defines the y-axis center point of an SVG `<circle>`
/// or `<ellipse>` element. If present, it overrides the element's `cy` attribute.
///
/// > While SVG the `cy` attribute is relevant to the SVG `<radialGradient>`
/// > element, the `cy` property only applies to `<circle>` and `<ellipse>`
/// > elements nested in an `<svg>`. It doesn't apply to `<radialGradient>` or
/// > other SVG elements nor to HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/cy)
pub fn cy_(value: String) -> Style {
  property("cy", value)
}

/// The `dominant-baseline` CSS property specifies the specific
/// [baseline](https://developer.mozilla.org/docs/Glossary/Baseline/Typography)
/// used to align the box's text and inline-level contents. It also indicates
/// the default alignment baseline of any boxes participating in baseline
/// alignment in the box's alignment context. If present, it overrides the
/// shape's [`dominant-baseline`](https://developer.mozilla.org/docs/Web/SVG/Attribute/dominant-baseline)
/// attribute.
///
/// Baselines are selected from the font baseline table. If there is no baseline
/// table in the nominal font, or if the baseline table lacks an entry for the
/// desired baseline, then the browser may use heuristics to determine the
/// position of the desired baseline.
///
/// The `dominant-baseline` property is used to determine or re-determine a
/// scaled-baseline-table. A scaled-baseline-table is a compound value with
/// three components:
/// - a baseline-identifier for the dominant-baseline,
/// - a baseline-table, and
/// - a baseline-table font-size.
///
/// Some values of `dominant-baseline` re-determine all three values. Others
/// only re-establish the baseline-table font-size. When the initial value,
/// `auto`, would give an undesired result, this property can be used to
/// explicitly set the desired scaled-baseline-table.
///
/// > The `dominant-baseline` property only has an effect on the `<text>`,
/// > `<textPath>`, `<tref>`, and `<tspan>` SVG elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/dominant-baseline)
pub fn dominant_baseline(display: String) -> Style {
  property("dominant-baseline", display)
}

/// The `fill` CSS property defines how SVG text content and the interior canvas
/// of SVG shapes are filled or painted. If present, it overrides the element's
/// [`fill`](https://developer.mozilla.org/docs/Web/SVG/Attribute/fill)
/// attribute.
///
/// The areas inside the outline of the SVG shape or text are painted. What is
/// "inside" a shape may not always be clear. The paths defining a shape may
/// overlap. The areas considered "inside" these complex shapes are clarified
/// by the `fill-rule` property or attribute.
///
/// If subpaths are open, `fill` closes the path before painting, as if a
/// "closepath" command were included connecting the last point of the subpath
/// with the first point of the subpath. In other words, `fill` applies to open
/// subpaths within `path` elements (i.e., subpaths without a closepath command)
/// and `polyline` elements.
///
/// > The `fill-opacity` property only applies to `<circle>`, `<ellipse>`,
/// > `<path>`, `<polygon>`, `<polyline>`, `<rect>`, `<text>`, `<textPath>`, and
/// > `<tspan>` elements nested in an `<svg>`. It doesn't apply to other SVG,
/// > HTML, or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/fill)
pub fn fill(fill: String) -> Style {
  property("fill", fill)
}

/// The `fill-opacity` CSS property defines the opacity of the painting operation
/// (color, gradient, pattern, etc.) applied to SVG shapes or text content
/// elements to fill the element. The property defines the opacity of the element's
/// `fill` only; it does not affect the stroke. If present, it overrides the
/// element's [`fill-opacity`](https://developer.mozilla.org/docs/Web/SVG/Attribute/fill-opacity)
/// attribute.
///
/// > The `fill-opacity` property only applies to `<circle>`, `<ellipse>`,
/// > `<path>`, `<polygon>`, `<polyline>`, `<rect>`, `<text>`, `<textPath>`, and
/// > `<tspan>` elements nested in an `<svg>`. It doesn't apply to other SVG,
/// > HTML, or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/fill-opacity)
pub fn fill_opacity(fill_opacity: String) -> Style {
  property("fill-opacity", fill_opacity)
}

/// The `fill-rule` CSS property defines the rule used to determine which parts
/// of the SVG shape's canvas are included inside a shape to be filled. If present,
/// it overrides the element's
/// [`fill-rule`](https://developer.mozilla.org/docs/Web/SVG/Attribute/fill-rule)
/// attribute.
///
/// The `fill-rule` clarifies which areas of a shape should be considered
/// "inside" the shape. It provides two values you can set to tell the browser
/// how the inside of a shape should be determined. For shapes that don't have
/// intersecting paths, like a circle, the bounds of what is inside a shape to
/// be filled are intuitively clear. With complex shapes that include intersecting
/// paths (such as a Venn diagram) or paths enclosing other paths (such as a donut),
/// the interpretation of which sections of the shape are "inside" the shape and
/// should be filled by the `fill` property, may not be obvious.
///
/// > The `fill-opacity` property only applies to `<circle>`, `<ellipse>`,
/// > `<path>`, `<polygon>`, `<polyline>`, `<rect>`, `<text>`, `<textPath>`, and
/// > `<tspan>` elements nested in an `<svg>`. It doesn't apply to other SVG,
/// > HTML, or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/fill-rule)
pub fn fill_rule(fill_rule: String) -> Style {
  property("fill-rule", fill_rule)
}

/// The `flood-color` CSS property defines the color of the current filter
/// primitive subregion in `<feFlood>` and `<feDropShadow>` elements within a
/// `<filter>.` If present, it overrides the element's
/// [`flood-color`](https://developer.mozilla.org/docs/Web/SVG/Attribute/flood-color) attribute.
///
/// > The `flood-color` property only applies to `<feFlood>` and `<feDropShadow>`
/// > elements nested in an `<svg>`. It doesn't apply to other SVG, HTML, or
/// > pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/flood-color)
pub fn flood_color(flood_color: String) -> Style {
  property("flood-color", flood_color)
}

/// The `flood-opacity` CSS property defines the opacity of the current filter
/// primitive subregion in `<feFlood>` and `<feDropShadow>` elements within a
/// `<filter>`. If present, it overrides the element's
/// [`flood-opacity`](https://developer.mozilla.org/docs/Web/SVG/Attribute/flood-opacity) attribute.
///
/// The property value impacts the `flood-color`'s alpha channel; it can
/// increase the transparency of a `flood-color` but can not make the color
/// defined by the `flood-color` property more opaque.
///
/// > The `flood-opacity` property only applies to `<feFlood>` and `<feDropShadow>`
/// > elements nested in an `<svg>`. It doesn't apply to other SVG, HTML, or
/// > pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/flood-opacity)
pub fn flood_opacity(flood_opacity: String) -> Style {
  property("flood-opacity", flood_opacity)
}

/// The `lighting-color` CSS property defines the color of the light source for
/// the `<feDiffuseLighting>` and `<feSpecularLighting>` SVG lighting filter
/// primitives within an SVG `<filter>`. If present, it overrides the element's
/// [`lighting-color`](https://developer.mozilla.org/docs/Web/SVG/Attribute/lighting-color)
/// attribute.
///
/// > The `lighting-color` property only applies to `<feDiffuseLighting>` and
/// > `<feSpecularLighting>` elements nested in an `<svg>`. It doesn't apply to
/// > other SVG, HTML, or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/lighting-color)
pub fn lighting_color(line_break: String) -> Style {
  property("lighting-color", line_break)
}

/// The `marker` CSS property points to a marker that will be drawn on the first,
/// middle, and last vertices of the element's path; that is, at all of its
/// vertices. The marker must have been defined using an SVG `<marker>` element,
/// and can only be referenced with a `<url>` value. The value of the CSS
/// property overrides any values of the `marker-start`, `marker`, and `marker-end`
/// attributes in the SVG.
///
/// For many marker-supporting shapes, the first and last vertices are in the
/// same place: for example, the top left corner of a `<rect>`. In such shapes,
/// if both the first and last markers are defined, two markers will be drawn at
/// that point, though they may not point in the same direction.
///
/// For the middle vertices, the direction each marker points is defined as the
/// direction halfway between the direction at the end of the preceding path
/// segment and the direction of the start of the following path segment. This
/// can be thought of as the cross product of the vectors defined by the two
/// path directions.
///
/// > The `marker` property will only have an effect for elements that can use
/// > SVG markers. See [`marker-start`](https://developer.mozilla.org/docs/Web/SVG/Attribute/marker-start)
/// > for a list.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/marker)
pub fn marker(marker: String) -> Style {
  property("marker", marker)
}

/// The `marker-end` CSS property points to a marker that will be drawn on the
/// last vertex of the element's path; that is, at its ending vertex. The marker
/// must have been defined using an SVG `<marker>` element, and can only be
/// referenced with a `<url>` value. The value of the CSS property overrides any
/// values of the `marker-end` attribute in the SVG.
///
/// For many marker-supporting shapes, the first and last vertices are the same
/// point: for example, the top left corner of a `<rect>`. In such shapes, if
/// both the first and last markers are defined, two markers will be drawn at
/// that point, though they may not face the same direction.
///
/// > The `marker-end` property will only have an effect for elements that can use
/// > SVG markers. See [`marker-end`](https://developer.mozilla.org/docs/Web/SVG/Attribute/marker-end)
/// > for a list.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/marker-end)
pub fn marker_end(marker: String) -> Style {
  property("marker-end", marker)
}

/// The `marker-mid` CSS property points to a marker that will be drawn on the
/// middle vertices of the element's path; that is, at each of its vertices
/// between the start and end vertices. The marker must have been defined using
/// an SVG `<marker>` element, and can only be referenced with a `<url>` value.
/// The value of the CSS property overrides any values of the `marker-mid`
/// attribute in the SVG.
///
/// The direction each marker points is defined as the direction halfway between
/// the direction at the end of the preceding path segment and the direction of
/// the start of the following path segment. This can be thought of as the cross
/// product of the vectors defined by the two path directions.
///
/// > The `marker-mid` property will only have an effect for elements that can use
/// > SVG markers. See [`marker-mid`](https://developer.mozilla.org/docs/Web/SVG/Attribute/marker-mid)
/// > for a list.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/marker-mid)
pub fn marker_mid(marker: String) -> Style {
  property("marker-mid", marker)
}

/// The `marker-start` CSS property points to a marker that will be drawn on the
/// first vertex of the element's path; that is, at its starting vertex. The marker
/// must have been defined using an SVG `<marker>` element, and can only be
/// referenced with a `<url>` value. The value of the CSS property overrides any
/// values of the `marker-start` attribute in the SVG.
///
/// For many marker-supporting shapes, the first and last vertices are the same
/// place: for example, the top left corner of a `<rect>`. In such shapes, if
/// both the first and last markers are defined, two markers will be drawn at
/// that point, though they may not face the same direction.
///
/// > The `marker-start` property will only have an effect for elements that can use
/// > SVG markers. See [`marker-start`](https://developer.mozilla.org/docs/Web/SVG/Attribute/marker-start)
/// > for a list.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/marker-start)
pub fn marker_start(marker: String) -> Style {
  property("marker-start", marker)
}

/// The `r` CSS property defines the radius of a circle. It can only be used
/// with the SVG `<circle>` element. If present, it overrides the circle's
/// [`r`](https://developer.mozilla.org/docs/Web/SVG/Attribute/r)
/// attribute.
///
/// > The `r` property only applies to `<circle>` elements nested in an `<svg>`.
/// > It doesn't apply to other SVG elements or HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/r)
pub fn r(r: Size) -> Style {
  property("r", size.to_string(r))
}

/// The `r` CSS property defines the radius of a circle. It can only be used
/// with the SVG `<circle>` element. If present, it overrides the circle's
/// [`r`](https://developer.mozilla.org/docs/Web/SVG/Attribute/r)
/// attribute.
///
/// > The `r` property only applies to `<circle>` elements nested in an `<svg>`.
/// > It doesn't apply to other SVG elements or HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/r)
pub fn r_(r: String) -> Style {
  property("r", r)
}

/// The `rx` CSS property defines the x-axis, or horizontal, radius of an SVG
/// `<ellipse>` and the horizontal curve of the corners of an SVG `<rect>`
/// rectangle. If present, it overrides the shape's
/// [`rx`](https://developer.mozilla.org/docs/Web/SVG/Attribute/rx) attribute.
///
/// > The `rx` property only applies to `<ellipse>` and `<rect>` elements
/// > nested in an `<svg>`. It doesn't apply to other SVG elements or HTML
/// > elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/rx)
pub fn rx(value: Size) -> Style {
  property("rx", size.to_string(value))
}

/// The `rx` CSS property defines the x-axis, or horizontal, radius of an SVG
/// `<ellipse>` and the horizontal curve of the corners of an SVG `<rect>`
/// rectangle. If present, it overrides the shape's
/// [`rx`](https://developer.mozilla.org/docs/Web/SVG/Attribute/rx) attribute.
///
/// > The `rx` property only applies to `<ellipse>` and `<rect>` elements
/// > nested in an `<svg>`. It doesn't apply to other SVG elements or HTML
/// > elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/rx)
pub fn rx_(value: String) -> Style {
  property("rx", value)
}

/// The `ry` CSS property defines the y-axis, or horizontal, radius of an SVG
/// `<ellipse>` and the horizontal curve of the corners of an SVG `<rect>`
/// rectangle. If present, it overrides the shape's
/// [`ry`](https://developer.mozilla.org/docs/Web/SVG/Attribute/ry) attribute.
///
/// > The `ry` property only applies to `<ellipse>` and `<rect>` elements
/// > nested in an `<svg>`. It doesn't apply to other SVG elements or HTML
/// > elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/ry)
pub fn ry(value: Size) -> Style {
  property("ry", size.to_string(value))
}

/// The `ry` CSS property defines the y-axis, or horizontal, radius of an SVG
/// `<ellipse>` and the horizontal curve of the corners of an SVG `<rect>`
/// rectangle. If present, it overrides the shape's
/// [`ry`](https://developer.mozilla.org/docs/Web/SVG/Attribute/ry) attribute.
///
/// > The `ry` property only applies to `<ellipse>` and `<rect>` elements
/// > nested in an `<svg>`. It doesn't apply to other SVG elements or HTML
/// > elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/ry)
pub fn ry_(value: String) -> Style {
  property("ry", value)
}

/// The `shape-rendering` CSS property provides hints to the renderer about what
/// tradeoffs to make when rendering shapes like paths, circles, or rectangles.
/// It only has an effect on the `<circle>`, `<ellipse>`, `<line>`, `<path>`,
/// `<polygon>`, `<polyline>`, and `<rect>` elements. If explicitly declared,
/// the value of the CSS property overrides the any values of the element's
/// [`shape-rendering`](https://developer.mozilla.org/docs/Web/SVG/Attribute/shape-rendering) attribute.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/shape-rendering)
pub fn shape_rendering(value: String) -> Style {
  property("shape-rendering", value)
}

/// The `stroke` CSS property defines the color or SVG paint server used to draw
/// an element's stroke. As such, stroke only has an effect on elements that can
/// be given a stroke (for example, `<rect>` or `<ellipse>`); see the page on
/// the SVG `stroke` attribute for a complete list. When declared, the CSS value
/// overrides any value of the element's
/// [`stroke`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stroke) SVG attribute.
///
/// > According to the 4 April 2017 draft of the CSS Fill and Stroke Module
/// > Level 3 specification, the stroke property is a shorthand for a number of
/// > other stroke properties. In practice, as of August 2024, browsers do not
/// > support the setting of other stroke-related values such as width or dash
/// > patterns via the `stroke` property, treating it instead as a direct
/// > analogue of the SVG `stroke` attribute.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stroke)
pub fn stroke(value: String) -> Style {
  property("stroke", value)
}

/// The `stroke-dasharray` CSS property defines a pattern of dashes and gaps
/// used in the painting of the SVG shape's stroke. If present, it overrides
/// the element's [`stroke-dasharray`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stroke-dasharray) attribute.
///
/// This property applies to any SVG shape or text-content element (see
/// `stroke-dasharray` for a full list), but as an inherited property, it may
/// be applied to elements such as `<g>` and still have the intended effect on
/// descendant elements' strokes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stroke-dasharray)
pub fn stroke_dasharray(value: String) -> Style {
  property("stroke-dasharray", value)
}

/// The `stroke-dashoffset` CSS property defines an offset for the starting
/// point of the rendering of an SVG element's associated dash array. If
/// present, it overrides
/// the element's [`stroke-dashoffset`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stroke-dashoffset) attribute.
///
/// This property applies to any SVG shape or text-content element (see
/// `stroke-dasharray` for a full list), but as an inherited property, it may
/// be applied to elements such as `<g>` and still have the intended effect on
/// descendant elements' strokes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stroke-dashoffset)
pub fn stroke_dashoffset(value: String) -> Style {
  property("stroke-dashoffset", value)
}

/// The `stroke-linecap` CSS property defines the shape to be used at the end
/// of open subpaths of SVG elements' unclosed strokes. If present, it overrides
/// the element's [`stroke-linecap`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stroke-linecap) attribute.
///
/// This property applies to any SVG shape that can have unclosed strokes and
/// text-content element (see `stroke-linecap` for a full list), but as an
/// inherited property, it may be applied to elements such as <g> and still
/// have the intended effect on descendant elements' strokes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stroke-linecap)
pub fn stroke_linecap(value: String) -> Style {
  property("stroke-linecap", value)
}

/// The `stroke-linejoin` CSS property defines the shape to be used at the
/// corners of an SVG element's stroked paths. If present, it overrides
/// the element's [`stroke-linejoin`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stroke-linejoin) attribute.
///
/// This property applies to any SVG corner-generating shape or text-content
/// element (see `stroke-linejoin` for a full list), but as an inherited property,
/// it may be applied to elements such as <g> and still have the intended
/// effect on descendant elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stroke-linejoin)
pub fn stroke_linejoin(value: String) -> Style {
  property("stroke-linejoin", value)
}

/// The `stroke-miterlimit` CSS property defines a limit on the ratio of the
/// miter length to the `stroke-width` when the shape to be used at the corners
/// of an SVG element's stroked path is a mitered join. If the limit defined by
/// this property is exceeded, the join is converted from miter to `bevel`,
/// thus making the corner appear truncated.
///
/// This property applies to any SVG corner-generating shape or text-content
/// element (see `stroke-miterlimit` for a full list), but as an inherited
/// property, it may be applied to elements such as `<g>` and still have the
/// intended effect on descendant elements' strokes. If present, it overrides
/// the element's `stroke-miterlimit` attribute.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stroke-miterlimit)
pub fn stroke_miterlimit(value: String) -> Style {
  property("stroke-miterlimit", value)
}

/// The `stroke-opacity` CSS property defines the opacity of an SVG shape's
/// stroke. The effect is identical to that of `opacity`, except it is applied
/// only to the stroke, not to the entire element. If present, it overrides
/// the element's [`stroke-opacity`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stroke-opacity) attribute.
///
/// This property applies to SVG shapes and text-content elements (see
/// `stroke-opacity` for a full list), but as an inherited property, it may be
/// applied to elements such as `<g>` and still have the intended effect on
/// descendant elements' strokes.
///
/// Note that a shape's stroke partially covers the fill of that shape, so a
/// stroke with an opacity less than `1` will show the fill blended with the
/// stroke where they overlap. To avoid this effect, it is possible to apply a
/// global opacity with the `opacity` property or to put the stroke behind the
/// fill with the `paint-order` attribute.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stroke-opacity)
pub fn stroke_opacity(value: String) -> Style {
  property("stroke-opacity", value)
}

/// The `stroke-width` CSS property defines the width of a stroke applied to
/// the SVG shape. If present, it overrides the element's
/// [`stroke-width`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stroke-width) attribute.
///
/// This property applies to any SVG shape or text-content element (see
/// `stroke-width` for a full list), but as an inherited property, it may be
/// applied to elements such as `<g>` and still have the intended effect on
/// descendant elements' strokes. If the value evaluates to zero, the stroke
/// is not drawn.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stroke-width)
pub fn stroke_width(value: String) -> Style {
  property("stroke-width", value)
}

/// The `stop-color` CSS property defines the color to use for an SVG `<stop>`
/// element within a gradient. If present, it overrides the element's
/// [`stop-color`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stop-color) attribute.
///
/// > The `stop-color` property only applies to <stop> elements nested in an
/// > <svg>. It doesn't apply to other SVG, HTML, or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stop-color)
pub fn stop_color(value: String) -> Style {
  property("stop-color", value)
}

/// The `stop-opacity` CSS property defines the opacity of a given color gradient
/// stop in the SVG `<stop>` element within an SVG gradient. If present, it
/// overrides the element's [`stop-opacity`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stop-opacity) attribute.
///
/// The property value impacts the `stop-color`'s alpha channel; it can increase
/// the transparency of a `<stop>`'s color but can not make the color defined by
/// the `stop-color` property more opaque.
///
/// > The `stop-opacity` property only applies to <stop> elements nested in an
/// > <svg>. It doesn't apply to other SVG, HTML, or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/stop-opacity)
pub fn stop_opacity(value: String) -> Style {
  property("stop-opacity", value)
}

/// The `text-anchor` CSS property aligns a box containing a string of text
/// where the wrapping area is determined from the `inline-size` property, and
/// the text is then placed relative to the anchor point of the element, which
/// is defined using the `x` and `y` (or `dx` and `dy`) attributes. If present,
/// the value of the CSS property overrides any value of the element's
/// [`text-anchor`](https://developer.mozilla.org/docs/Web/SVG/Attribute/stop-opacity) attribute.
///
/// Each individual text fragment within an element is aligned independently;
/// thus, a multi-line `<text>` element will have each line of text aligned as
/// per the value of text-anchor. text-anchor values only have an effect on the
/// `<text>`, `<textPath>`, `<tref>`, and `<tspan>` SVG elements. text-anchor
/// does not apply to automatically wrapped text; for that, see text-align.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/text-anchor)
pub fn text_anchor(value: String) {
  property("text-anchor", value)
}

/// The `x` CSS property defines the x-axis coordinate of the top left corner
/// of the SVG `<rect>` shape, `<image>` image, `<foreignObject>` viewport or
/// nested `<svg>` viewport relative to the nearest `<svg>` ancestor's user
/// [coordinate system](https://developer.mozilla.org/docs/Web/CSS/CSSOM_view/Coordinate_systems).
/// If present, it overrides the element's `x` attribute.
///
/// > The `x` property only applies to `<rect>`, `<image>`, `<foreignObject>`,
/// > and `<svg>` elements nested in an `<svg>`. It has no effect on the
/// > outermost `<svg>` elements itself, and does not apply to other SVG
/// > elements nor to HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/x)
pub fn x(value: Size) {
  property("x", size.to_string(value))
}

/// The `x` CSS property defines the x-axis coordinate of the top left corner
/// of the SVG `<rect>` shape, `<image>` image, `<foreignObject>` viewport or
/// nested `<svg>` viewport relative to the nearest `<svg>` ancestor's user
/// [coordinate system](https://developer.mozilla.org/docs/Web/CSS/CSSOM_view/Coordinate_systems).
/// If present, it overrides the element's `x` attribute.
///
/// > The `x` property only applies to `<rect>`, `<image>`, `<foreignObject>`,
/// > and `<svg>` elements nested in an `<svg>`. It has no effect on the
/// > outermost `<svg>` elements itself, and does not apply to other SVG
/// > elements nor to HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/x)
pub fn x_(value: String) {
  property("x", value)
}

/// The `y` CSS property defines the y-axis coordinate of the top left corner
/// of the SVG `<rect>` shape, `<image>` image, `<foreignObject>` viewport or
/// nested `<svg>` viewport relative to the nearest `<svg>` ancestor's user
/// [coordinate system](https://developer.mozilla.org/docs/Web/CSS/CSSOM_view/Coordinate_systems).
/// If present, it overrides the element's `y` attribute.
///
/// > The `y` property only applies to `<rect>`, `<image>`, `<foreignObject>`,
/// > and `<svg>` elements nested in an `<svg>`. It has no effect on the
/// > outermost `<svg>` elements itself, and does not apply to other SVG
/// > elements nor to HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/x)
pub fn y(value: Size) {
  property("y", size.to_string(value))
}

/// The `y` CSS property defines the y-axis coordinate of the top left corner
/// of the SVG `<rect>` shape, `<image>` image, `<foreignObject>` viewport or
/// nested `<svg>` viewport relative to the nearest `<svg>` ancestor's user
/// [coordinate system](https://developer.mozilla.org/docs/Web/CSS/CSSOM_view/Coordinate_systems).
/// If present, it overrides the element's `y` attribute.
///
/// > The `y` property only applies to `<rect>`, `<image>`, `<foreignObject>`,
/// > and `<svg>` elements nested in an `<svg>`. It has no effect on the
/// > outermost `<svg>` elements itself, and does not apply to other SVG
/// > elements nor to HTML elements or pseudo-elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/x)
pub fn y_(value: String) {
  property("y", value)
}
