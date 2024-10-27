import redraw/attribute as a
import redraw/svg

const content = "<rect width=\"256\" height=\"256\" fill=\"none\"/><polyline points=\"40 144 96 200 224 72\" fill=\"none\" stroke=\"currentColor\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"16\"/>"

pub fn icon() {
  svg.svg(
    [
      a.style([#("max-width", "100%"), #("max-height", "100%")]),
      a.attribute("xmlns", "http://www.w3.org/2000/svg"),
      a.attribute("viewBox", "0 0 256 256"),
      a.attribute("fill", "currentColor"),
      a.dangerously_set_inner_html(a.inner_html(content)),
    ],
    [],
  )
}
