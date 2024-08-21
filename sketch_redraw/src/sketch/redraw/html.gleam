import redraw/html
import redraw/internals/coerce.{coerce}
import sketch/redraw.{styled}

pub fn html(styles, attrs, children) {
  styled("html", styles, attrs, children)
}

pub fn link(styles, attrs) {
  styled("link", styles, attrs, coerce(Nil))
}

pub fn meta(styles, attrs) {
  styled("meta", styles, attrs, coerce(Nil))
}

pub fn script(styles, attrs) {
  styled("script", styles, attrs, coerce(Nil))
}

pub fn style(styles, attrs, content) {
  styled("style", styles, attrs, [html.text(content)])
}

pub fn title(styles, attrs, content) {
  styled("title", styles, attrs, [html.text(content)])
}

// DOM nodes

pub fn a(styles, attrs, children) {
  styled("a", styles, attrs, children)
}

pub fn abbr(styles, attrs, children) {
  styled("abbr", styles, attrs, children)
}

pub fn address(styles, attrs, children) {
  styled("address", styles, attrs, children)
}

pub fn area(styles, attrs) {
  styled("area", styles, attrs, coerce(Nil))
}

pub fn article(styles, attrs, children) {
  styled("article", styles, attrs, children)
}

pub fn aside(styles, attrs, children) {
  styled("aside", styles, attrs, children)
}

pub fn audio(styles, attrs, children) {
  styled("audio", styles, attrs, children)
}

pub fn b(styles, attrs, children) {
  styled("b", styles, attrs, children)
}

pub fn base(styles, attrs) {
  styled("base", styles, attrs, coerce(Nil))
}

pub fn bdi(styles, attrs, children) {
  styled("bdi", styles, attrs, children)
}

pub fn bdo(styles, attrs, children) {
  styled("bdo", styles, attrs, children)
}

pub fn blockquote(styles, attrs, children) {
  styled("blockquote", styles, attrs, children)
}

pub fn body(styles, attrs, children) {
  styled("body", styles, attrs, children)
}

pub fn br(styles, attrs) {
  styled("br", styles, attrs, coerce(Nil))
}

pub fn button(styles, attrs, children) {
  styled("button", styles, attrs, children)
}

pub fn canvas(styles, attrs, children) {
  styled("canvas", styles, attrs, children)
}

pub fn caption(styles, attrs, children) {
  styled("caption", styles, attrs, children)
}

pub fn cite(styles, attrs, children) {
  styled("cite", styles, attrs, children)
}

pub fn code(styles, attrs, children) {
  styled("code", styles, attrs, children)
}

pub fn col(styles, attrs) {
  styled("col", styles, attrs, coerce(Nil))
}

pub fn colgroup(styles, attrs, children) {
  styled("colgroup", styles, attrs, children)
}

pub fn data(styles, attrs, children) {
  styled("data", styles, attrs, children)
}

pub fn datalist(styles, attrs, children) {
  styled("datalist", styles, attrs, children)
}

pub fn dd(styles, attrs, children) {
  styled("dd", styles, attrs, children)
}

pub fn del(styles, attrs, children) {
  styled("del", styles, attrs, children)
}

pub fn details(styles, attrs, children) {
  styled("details", styles, attrs, children)
}

pub fn dfn(styles, attrs, children) {
  styled("dfn", styles, attrs, children)
}

pub fn dialog(styles, attrs, children) {
  styled("dialog", styles, attrs, children)
}

pub fn div(styles, attrs, children) {
  styled("div", styles, attrs, children)
}

pub fn dl(styles, attrs, children) {
  styled("dl", styles, attrs, children)
}

pub fn dt(styles, attrs, children) {
  styled("dt", styles, attrs, children)
}

pub fn em(styles, attrs, children) {
  styled("em", styles, attrs, children)
}

pub fn embed(styles, attrs) {
  styled("embed", styles, attrs, coerce(Nil))
}

pub fn fieldset(styles, attrs, children) {
  styled("fieldset", styles, attrs, children)
}

pub fn figcaption(styles, attrs, children) {
  styled("figcaption", styles, attrs, children)
}

pub fn figure(styles, attrs, children) {
  styled("figure", styles, attrs, children)
}

pub fn footer(styles, attrs, children) {
  styled("footer", styles, attrs, children)
}

pub fn form(styles, attrs, children) {
  styled("form", styles, attrs, children)
}

pub fn h1(styles, attrs, children) {
  styled("h1", styles, attrs, children)
}

pub fn h2(styles, attrs, children) {
  styled("h2", styles, attrs, children)
}

pub fn h3(styles, attrs, children) {
  styled("h3", styles, attrs, children)
}

pub fn h4(styles, attrs, children) {
  styled("h4", styles, attrs, children)
}

pub fn h5(styles, attrs, children) {
  styled("h5", styles, attrs, children)
}

pub fn h6(styles, attrs, children) {
  styled("h6", styles, attrs, children)
}

pub fn head(styles, attrs, children) {
  styled("head", styles, attrs, children)
}

pub fn header(styles, attrs, children) {
  styled("header", styles, attrs, children)
}

pub fn hgroup(styles, attrs, children) {
  styled("hgroup", styles, attrs, children)
}

pub fn hr(styles, attrs) {
  styled("hr", styles, attrs, coerce(Nil))
}

pub fn i(styles, attrs, children) {
  styled("i", styles, attrs, children)
}

pub fn iframe(styles, attrs) {
  styled("iframe", styles, attrs, coerce(Nil))
}

pub fn img(styles, attrs) {
  styled("img", styles, attrs, coerce(Nil))
}

pub fn input(styles, attrs) {
  styled("input", styles, attrs, coerce(Nil))
}

pub fn ins(styles, attrs, children) {
  styled("ins", styles, attrs, children)
}

pub fn kbd(styles, attrs, children) {
  styled("kbd", styles, attrs, children)
}

pub fn label(styles, attrs, children) {
  styled("label", styles, attrs, children)
}

pub fn legend(styles, attrs, children) {
  styled("legend", styles, attrs, children)
}

pub fn li(styles, attrs, children) {
  styled("li", styles, attrs, children)
}

pub fn main(styles, attrs, children) {
  styled("main", styles, attrs, children)
}

pub fn map(styles, attrs, children) {
  styled("map", styles, attrs, children)
}

pub fn mark(styles, attrs, children) {
  styled("mark", styles, attrs, children)
}

pub fn menu(styles, attrs, children) {
  styled("menu", styles, attrs, children)
}

pub fn meter(styles, attrs, children) {
  styled("meter", styles, attrs, children)
}

pub fn nav(styles, attrs, children) {
  styled("nav", styles, attrs, children)
}

pub fn noscript(styles, attrs, children) {
  styled("noscript", styles, attrs, children)
}

pub fn object(styles, attrs, children) {
  styled("object", styles, attrs, children)
}

pub fn ol(styles, attrs, children) {
  styled("ol", styles, attrs, children)
}

pub fn optgroup(styles, attrs, children) {
  styled("optgroup", styles, attrs, children)
}

pub fn option(styles, attrs, children) {
  styled("option", styles, attrs, children)
}

pub fn output(styles, attrs, children) {
  styled("output", styles, attrs, children)
}

pub fn p(styles, attrs, children) {
  styled("p", styles, attrs, children)
}

pub fn picture(styles, attrs, children) {
  styled("picture", styles, attrs, children)
}

pub fn pre(styles, attrs, children) {
  styled("pre", styles, attrs, children)
}

pub fn progress(styles, attrs, children) {
  styled("progress", styles, attrs, children)
}

pub fn q(styles, attrs, children) {
  styled("q", styles, attrs, children)
}

pub fn rp(styles, attrs, children) {
  styled("rp", styles, attrs, children)
}

pub fn rt(styles, attrs, children) {
  styled("rt", styles, attrs, children)
}

pub fn ruby(styles, attrs, children) {
  styled("ruby", styles, attrs, children)
}

pub fn s(styles, attrs, children) {
  styled("s", styles, attrs, children)
}

pub fn samp(styles, attrs, children) {
  styled("samp", styles, attrs, children)
}

pub fn section(styles, attrs, children) {
  styled("section", styles, attrs, children)
}

pub fn select(styles, attrs, children) {
  styled("select", styles, attrs, children)
}

pub fn slot(styles, attrs, children) {
  styled("slot", styles, attrs, children)
}

pub fn small(styles, attrs, children) {
  styled("small", styles, attrs, children)
}

pub fn source(styles, attrs) {
  styled("source", styles, attrs, coerce(Nil))
}

pub fn span(styles, attrs, children) {
  styled("span", styles, attrs, children)
}

pub fn strong(styles, attrs, children) {
  styled("strong", styles, attrs, children)
}

pub fn sub(styles, attrs, children) {
  styled("sub", styles, attrs, children)
}

pub fn summary(styles, attrs, children) {
  styled("summary", styles, attrs, children)
}

pub fn sup(styles, attrs, children) {
  styled("sup", styles, attrs, children)
}

pub fn table(styles, attrs, children) {
  styled("table", styles, attrs, children)
}

pub fn tbody(styles, attrs, children) {
  styled("tbody", styles, attrs, children)
}

pub fn td(styles, attrs, children) {
  styled("td", styles, attrs, children)
}

pub fn template(styles, attrs, children) {
  styled("template", styles, attrs, children)
}

pub fn textarea(styles, attrs, children) {
  styled("textarea", styles, attrs, children)
}

pub fn tfoot(styles, attrs, children) {
  styled("tfoot", styles, attrs, children)
}

pub fn th(styles, attrs, children) {
  styled("th", styles, attrs, children)
}

pub fn thead(styles, attrs, children) {
  styled("thead", styles, attrs, children)
}

pub fn time(styles, attrs, children) {
  styled("time", styles, attrs, children)
}

pub fn tr(styles, attrs, children) {
  styled("tr", styles, attrs, children)
}

pub fn track(styles, attrs) {
  styled("track", styles, attrs, coerce(Nil))
}

pub fn u(styles, attrs, children) {
  styled("u", styles, attrs, children)
}

pub fn ul(styles, attrs, children) {
  styled("ul", styles, attrs, children)
}

pub fn var(styles, attrs, children) {
  styled("var", styles, attrs, children)
}

pub fn video(styles, attrs, children) {
  styled("video", styles, attrs, children)
}

pub fn wbr(styles, attrs) {
  styled("wbr", styles, attrs, coerce(Nil))
}
