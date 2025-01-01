//// This module is a drop-in replacement for `lustre/element/html`. Just
//// use the new functions, and everything will automagically be styled.
//// Every stylable node has two functions: `[node]` and `[node]_`, the former
//// applying a style, while the latter does not accept style, in case you don't
//// need to style a node.

import lustre/attribute.{type Attribute}
import sketch/css.{type Class}
import sketch/lustre/experimental/element.{type Element} as el

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/html)
pub fn html(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("html", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/base)
pub fn base(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("base", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/head)
pub fn head(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("head", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/link)
pub fn link(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("link", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/meta)
pub fn meta(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("meta", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/style)
pub fn style(attributes: List(Attribute(a)), child: String) -> Element(a) {
  el.element_("style", attributes, [text(child)])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/title)
pub fn title(attributes: List(Attribute(a)), title: String) -> Element(a) {
  el.element_("title", attributes, [text(title)])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/API/Text)
pub fn text(content: String) -> Element(a) {
  el.text(content)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/a)
pub fn a(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("a", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/a)
pub fn a_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("a", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/abbr)
pub fn abbr(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("abbr", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/abbr)
pub fn abbr_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("abbr", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/address)
pub fn address(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("address", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/address)
pub fn address_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("address", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/area)
pub fn area(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("area", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/area)
pub fn area_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("area", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/article)
pub fn article(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("article", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/article)
pub fn article_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("article", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/aside)
pub fn aside(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("aside", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/aside)
pub fn aside_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("aside", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/audio)
pub fn audio(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("audio", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/audio)
pub fn audio_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("audio", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/b)
pub fn b(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("b", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/b)
pub fn b_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("b", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdi)
pub fn bdi(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("bdi", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdi)
pub fn bdi_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("bdi", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdo)
pub fn bdo(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("bdo", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdo)
pub fn bdo_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("bdo", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/blockquote)
pub fn blockquote(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("blockquote", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/blockquote)
pub fn blockquote_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("blockquote", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/body)
pub fn body(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("body", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/body)
pub fn body_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("body", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/br)
pub fn br(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("br", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/br)
pub fn br_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("br", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/button)
pub fn button(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("button", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/button)
pub fn button_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("button", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/canvas)
pub fn canvas(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("canvas", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/canvas)
pub fn canvas_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("canvas", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/caption)
pub fn caption(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("caption", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/caption)
pub fn caption_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("caption", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/cite)
pub fn cite(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("cite", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/cite)
pub fn cite_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("cite", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/code)
pub fn code(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("code", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/code)
pub fn code_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("code", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/col)
pub fn col(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("col", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/col)
pub fn col_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("col", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/colgroup)
pub fn colgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("colgroup", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/colgroup)
pub fn colgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("colgroup", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/data)
pub fn data(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("data", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/data)
pub fn data_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("data", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/datalist)
pub fn datalist(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("datalist", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/datalist)
pub fn datalist_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("datalist", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dd)
pub fn dd(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dd", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dd)
pub fn dd_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dd", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/del)
pub fn del(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("del", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/del)
pub fn del_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("del", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/details)
pub fn details(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("details", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/details)
pub fn details_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("details", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dfn)
pub fn dfn(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dfn", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dfn)
pub fn dfn_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dfn", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dialog)
pub fn dialog(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dialog", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dialog)
pub fn dialog_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dialog", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/div)
pub fn div(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("div", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/div)
pub fn div_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("div", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dl)
pub fn dl(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dl", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dl)
pub fn dl_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dl", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dt)
pub fn dt(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dt", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dt)
pub fn dt_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dt", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/em)
pub fn em(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("em", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/em)
pub fn em_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("em", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/embed)
pub fn embed(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("embed", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/embed)
pub fn embed_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("embed", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/fieldset)
pub fn fieldset(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("fieldset", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/fieldset)
pub fn fieldset_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("fieldset", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figcaption)
pub fn figcaption(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("figcaption", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figcaption)
pub fn figcaption_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("figcaption", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figure)
pub fn figure(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("figure", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figure)
pub fn figure_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("figure", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/footer)
pub fn footer(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("footer", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/footer)
pub fn footer_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("footer", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/form)
pub fn form(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("form", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/form)
pub fn form_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("form", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h1)
pub fn h1(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h1", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h1)
pub fn h1_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h1", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h2)
pub fn h2(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h2", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h2)
pub fn h2_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h2", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h3)
pub fn h3(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h3", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h3)
pub fn h3_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h3", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h4)
pub fn h4(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h4", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h4)
pub fn h4_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h4", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h5)
pub fn h5(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h5", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h5)
pub fn h5_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h5", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h6)
pub fn h6(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h6", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h6)
pub fn h6_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h6", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/header)
pub fn header(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("header", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/header)
pub fn header_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("header", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hgroup)
pub fn hgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("hgroup", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hgroup)
pub fn hgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("hgroup", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hr)
pub fn hr(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("hr", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hr)
pub fn hr_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("hr", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/i)
pub fn i(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("i", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/i)
pub fn i_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("i", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/iframe)
pub fn iframe(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("iframe", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/iframe)
pub fn iframe_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("iframe", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/img)
pub fn img(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("img", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/img)
pub fn img_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("img", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/input)
pub fn input(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("input", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/input)
pub fn input_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("input", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ins)
pub fn ins(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("ins", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ins)
pub fn ins_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("ins", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/kbd)
pub fn kbd(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("kbd", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/kbd)
pub fn kbd_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("kbd", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/label)
pub fn label(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("label", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/label)
pub fn label_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("label", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/legend)
pub fn legend(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("legend", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/legend)
pub fn legend_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("legend", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/li)
pub fn li(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("li", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/li)
pub fn li_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("li", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/main)
pub fn main(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("main", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/main)
pub fn main_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("main", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/map)
pub fn map(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("map", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/map)
pub fn map_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("map", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/mark)
pub fn mark(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("mark", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/mark)
pub fn mark_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("mark", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/math)
pub fn math(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("math", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/math)
pub fn math_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("math", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/menu)
pub fn menu(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("menu", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/menu)
pub fn menu_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("menu", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/meter)
pub fn meter(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("meter", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/meter)
pub fn meter_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("meter", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/nav)
pub fn nav(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("nav", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/nav)
pub fn nav_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("nav", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/noscript)
pub fn noscript(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("noscript", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/noscript)
pub fn noscript_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("noscript", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/object)
pub fn object(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("object", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/object)
pub fn object_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("object", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ol)
pub fn ol(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("ol", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ol)
pub fn ol_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("ol", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/optgroup)
pub fn optgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("optgroup", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/optgroup)
pub fn optgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("optgroup", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/option)
pub fn option(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("option", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/option)
pub fn option_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("option", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/output)
pub fn output(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("output", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/output)
pub fn output_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("output", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/p)
pub fn p(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("p", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/p)
pub fn p_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("p", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/picture)
pub fn picture(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("picture", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/picture)
pub fn picture_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("picture", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/portal)
pub fn portal(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("portal", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/portal)
pub fn portal_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("portal", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/pre)
pub fn pre(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("pre", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/pre)
pub fn pre_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("pre", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/progress)
pub fn progress(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("progress", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/progress)
pub fn progress_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("progress", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/q)
pub fn q(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("q", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/q)
pub fn q_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("q", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rp)
pub fn rp(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("rp", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rp)
pub fn rp_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("rp", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rt)
pub fn rt(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("rt", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rt)
pub fn rt_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("rt", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ruby)
pub fn ruby(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("ruby", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ruby)
pub fn ruby_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("ruby", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/s)
pub fn s(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("s", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/s)
pub fn s_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("s", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/samp)
pub fn samp(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("samp", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/samp)
pub fn samp_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("samp", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/script)
pub fn script(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("script", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/script)
pub fn script_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("script", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/search)
pub fn search(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("search", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/search)
pub fn search_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("search", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/section)
pub fn section(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("section", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/section)
pub fn section_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("section", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/select)
pub fn select(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("select", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/select)
pub fn select_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("select", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/slot)
pub fn slot(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("slot", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/slot)
pub fn slot_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("slot", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/small)
pub fn small(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("small", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/small)
pub fn small_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("small", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/source)
pub fn source(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("source", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/source)
pub fn source_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("source", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/span)
pub fn span(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("span", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/span)
pub fn span_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("span", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/strong)
pub fn strong(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("strong", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/strong)
pub fn strong_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("strong", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sub)
pub fn sub(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("sub", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sub)
pub fn sub_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("sub", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/summary)
pub fn summary(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("summary", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/summary)
pub fn summary_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("summary", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sup)
pub fn sup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("sup", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sup)
pub fn sup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("sup", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/svg)
pub fn svg(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.namespaced(
    "http://www.w3.org/2000/svg",
    "svg",
    class,
    attributes,
    children,
  )
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/svg)
pub fn svg_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.namespaced_("http://www.w3.org/2000/svg", "svg", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/table)
pub fn table(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("table", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/table)
pub fn table_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("table", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tbody)
pub fn tbody(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("tbody", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tbody)
pub fn tbody_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("tbody", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/td)
pub fn td(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("td", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/td)
pub fn td_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("td", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/template)
pub fn template(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("template", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/template)
pub fn template_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("template", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/textarea)
pub fn textarea(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("textarea", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/textarea)
pub fn textarea_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("textarea", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tfoot)
pub fn tfoot(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("tfoot", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tfoot)
pub fn tfoot_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("tfoot", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/th)
pub fn th(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("th", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/th)
pub fn th_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("th", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/thead)
pub fn thead(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("thead", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/thead)
pub fn thead_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("thead", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/time)
pub fn time(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("time", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/time)
pub fn time_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("time", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tr)
pub fn tr(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("tr", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tr)
pub fn tr_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("tr", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/track)
pub fn track(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("track", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/track)
pub fn track_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("track", attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/u)
pub fn u(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("u", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/u)
pub fn u_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("u", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ul)
pub fn ul(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("ul", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ul)
pub fn ul_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("ul", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/var)
pub fn var(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("var", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/var)
pub fn var_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("var", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/video)
pub fn video(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("video", class, attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/video)
pub fn video_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("video", attributes, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/wbr)
pub fn wbr(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("wbr", class, attributes, [])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/wbr)
pub fn wbr_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("wbr", attributes, [])
}
