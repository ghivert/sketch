import redraw.{type Component} as _
import redraw/dom/attribute.{type Attribute}
import redraw/dom/html
import redraw/internals/coerce.{coerce}
import sketch/css.{type Class}
import sketch/redraw.{styled}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/html)
pub fn html(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("html", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/link)
pub fn link(styles: Class, attrs: List(Attribute)) -> Component {
  styled("link", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/meta)
pub fn meta(styles: Class, attrs: List(Attribute)) -> Component {
  styled("meta", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/script)
pub fn script(styles: Class, attrs: List(Attribute)) -> Component {
  styled("script", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/style)
pub fn style(
  styles: Class,
  attrs: List(Attribute),
  content: String,
) -> Component {
  styled("style", styles, attrs, [html.text(content)])
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/title)
pub fn title(
  styles: Class,
  attrs: List(Attribute),
  content: String,
) -> Component {
  styled("title", styles, attrs, [html.text(content)])
}

// DOM nodes

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/a)
pub fn a(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("a", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/abbr)
pub fn abbr(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("abbr", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/address)
pub fn address(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("address", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/area)
pub fn area(styles: Class, attrs: List(Attribute)) -> Component {
  styled("area", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/article)
pub fn article(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("article", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/aside)
pub fn aside(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("aside", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/audio)
pub fn audio(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("audio", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/b)
pub fn b(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("b", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/base)
pub fn base(styles: Class, attrs: List(Attribute)) -> Component {
  styled("base", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdi)
pub fn bdi(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("bdi", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdo)
pub fn bdo(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("bdo", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/blockquote)
pub fn blockquote(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("blockquote", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/body)
pub fn body(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("body", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/br)
pub fn br(styles: Class, attrs: List(Attribute)) -> Component {
  styled("br", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/button)
pub fn button(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("button", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/canvas)
pub fn canvas(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("canvas", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/caption)
pub fn caption(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("caption", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/cite)
pub fn cite(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("cite", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/code)
pub fn code(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("code", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/col)
pub fn col(styles: Class, attrs: List(Attribute)) -> Component {
  styled("col", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/colgroup)
pub fn colgroup(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("colgroup", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/data)
pub fn data(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("data", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/datalist)
pub fn datalist(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("datalist", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dd)
pub fn dd(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("dd", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/del)
pub fn del(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("del", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/details)
pub fn details(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("details", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dfn)
pub fn dfn(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("dfn", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dialog)
pub fn dialog(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("dialog", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/div)
pub fn div(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("div", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dl)
pub fn dl(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("dl", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dt)
pub fn dt(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("dt", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/em)
pub fn em(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("em", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/embed)
pub fn embed(styles: Class, attrs: List(Attribute)) -> Component {
  styled("embed", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/fieldset)
pub fn fieldset(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("fieldset", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figcaption)
pub fn figcaption(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("figcaption", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figure)
pub fn figure(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("figure", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/footer)
pub fn footer(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("footer", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/form)
pub fn form(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("form", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h1)
pub fn h1(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("h1", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h2)
pub fn h2(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("h2", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h3)
pub fn h3(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("h3", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h4)
pub fn h4(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("h4", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h5)
pub fn h5(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("h5", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h6)
pub fn h6(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("h6", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/head)
pub fn head(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("head", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/header)
pub fn header(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("header", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hgroup)
pub fn hgroup(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("hgroup", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hr)
pub fn hr(styles: Class, attrs: List(Attribute)) -> Component {
  styled("hr", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/i)
pub fn i(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("i", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/iframe)
pub fn iframe(styles: Class, attrs: List(Attribute)) -> Component {
  styled("iframe", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/img)
pub fn img(styles: Class, attrs: List(Attribute)) -> Component {
  styled("img", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/input)
pub fn input(styles: Class, attrs: List(Attribute)) -> Component {
  styled("input", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ins)
pub fn ins(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("ins", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/kbd)
pub fn kbd(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("kbd", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/label)
pub fn label(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("label", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/legend)
pub fn legend(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("legend", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/li)
pub fn li(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("li", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/main)
pub fn main(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("main", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/map)
pub fn map(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("map", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/mark)
pub fn mark(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("mark", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/menu)
pub fn menu(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("menu", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/meter)
pub fn meter(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("meter", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/nav)
pub fn nav(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("nav", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/noscript)
pub fn noscript(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("noscript", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/object)
pub fn object(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("object", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ol)
pub fn ol(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("ol", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/optgroup)
pub fn optgroup(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("optgroup", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/option)
pub fn option(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("option", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/output)
pub fn output(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("output", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/p)
pub fn p(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("p", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/picture)
pub fn picture(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("picture", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/pre)
pub fn pre(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("pre", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/progress)
pub fn progress(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("progress", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/q)
pub fn q(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("q", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rp)
pub fn rp(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("rp", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rt)
pub fn rt(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("rt", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ruby)
pub fn ruby(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("ruby", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/s)
pub fn s(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("s", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/samp)
pub fn samp(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("samp", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/section)
pub fn section(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("section", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/select)
pub fn select(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("select", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/slot)
pub fn slot(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("slot", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/small)
pub fn small(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("small", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/source)
pub fn source(styles: Class, attrs: List(Attribute)) -> Component {
  styled("source", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/span)
pub fn span(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("span", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/strong)
pub fn strong(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("strong", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sub)
pub fn sub(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("sub", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/summary)
pub fn summary(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("summary", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sup)
pub fn sup(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("sup", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/table)
pub fn table(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("table", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tbody)
pub fn tbody(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("tbody", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/td)
pub fn td(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("td", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/template)
pub fn template(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("template", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/textarea)
pub fn textarea(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("textarea", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tfoot)
pub fn tfoot(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("tfoot", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/th)
pub fn th(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("th", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/thead)
pub fn thead(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("thead", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/time)
pub fn time(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("time", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tr)
pub fn tr(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("tr", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/track)
pub fn track(styles: Class, attrs: List(Attribute)) -> Component {
  styled("track", styles, attrs, coerce(Nil))
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/u)
pub fn u(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("u", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ul)
pub fn ul(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("ul", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/var)
pub fn var(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("var", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/video)
pub fn video(
  styles: Class,
  attrs: List(Attribute),
  children: List(Component),
) -> Component {
  styled("video", styles, attrs, children)
}

/// [Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/wbr)
pub fn wbr(styles: Class, attrs: List(Attribute)) -> Component {
  styled("wbr", styles, attrs, coerce(Nil))
}
