import sketch/lustre/element.{element}

//

pub fn html(attributes, children) {
  element("html", attributes, children, [])
}

pub fn base(attributes) {
  element("base", attributes, [], [])
}

pub fn head(attributes, children) {
  element("head", attributes, children, [])
}

pub fn link(attributes) {
  element("link", attributes, [], [])
}

pub fn meta(attributes) {
  element("meta", attributes, [], [])
}

pub fn style(attributes, child) {
  element("style", attributes, [text(child)], [])
}

pub fn title(attributes, title: String) {
  element("title", attributes, [text(title)], [])
}

//

pub fn text(content) {
  element.text(content)
}

//

pub fn a(attributes, children, styles) {
  element("a", attributes, children, styles)
}

pub fn a_(attributes, children) {
  element("a", attributes, children, [])
}

pub fn abbr(attributes, children, styles) {
  element("abbr", attributes, children, styles)
}

pub fn abbr_(attributes, children) {
  element("abbr", attributes, children, [])
}

pub fn address(attributes, children, styles) {
  element("address", attributes, children, styles)
}

pub fn address_(attributes, children) {
  element("address", attributes, children, [])
}

pub fn area(attributes, styles) {
  element("area", attributes, [], styles)
}

pub fn area_(attributes) {
  element("area", attributes, [], [])
}

pub fn article(attributes, children, styles) {
  element("article", attributes, children, styles)
}

pub fn article_(attributes, children) {
  element("article", attributes, children, [])
}

pub fn aside(attributes, children, styles) {
  element("aside", attributes, children, styles)
}

pub fn aside_(attributes, children) {
  element("aside", attributes, children, [])
}

pub fn audio(attributes, children, styles) {
  element("audio", attributes, children, styles)
}

pub fn audio_(attributes, children) {
  element("audio", attributes, children, [])
}

pub fn b(attributes, children, styles) {
  element("b", attributes, children, styles)
}

pub fn b_(attributes, children) {
  element("b", attributes, children, [])
}

pub fn bdi(attributes, children, styles) {
  element("bdi", attributes, children, styles)
}

pub fn bdi_(attributes, children) {
  element("bdi", attributes, children, [])
}

pub fn bdo(attributes, children, styles) {
  element("bdo", attributes, children, styles)
}

pub fn bdo_(attributes, children) {
  element("bdo", attributes, children, [])
}

pub fn blockquote(attributes, children, styles) {
  element("blockquote", attributes, children, styles)
}

pub fn blockquote_(attributes, children) {
  element("blockquote", attributes, children, [])
}

pub fn body(attributes, children, styles) {
  element("body", attributes, children, styles)
}

pub fn body_(attributes, children) {
  element("body", attributes, children, [])
}

pub fn br(attributes, styles) {
  element("br", attributes, [], styles)
}

pub fn br_(attributes) {
  element("br", attributes, [], [])
}

pub fn button(attributes, children, styles) {
  element("button", attributes, children, styles)
}

pub fn button_(attributes, children) {
  element("button", attributes, children, [])
}

pub fn canvas(attributes, children, styles) {
  element("canvas", attributes, children, styles)
}

pub fn canvas_(attributes, children) {
  element("canvas", attributes, children, [])
}

pub fn caption(attributes, children, styles) {
  element("caption", attributes, children, styles)
}

pub fn caption_(attributes, children) {
  element("caption", attributes, children, [])
}

pub fn cite(attributes, children, styles) {
  element("cite", attributes, children, styles)
}

pub fn cite_(attributes, children) {
  element("cite", attributes, children, [])
}

pub fn code(attributes, children, styles) {
  element("code", attributes, children, styles)
}

pub fn code_(attributes, children) {
  element("code", attributes, children, [])
}

pub fn col(attributes, styles) {
  element("col", attributes, [], styles)
}

pub fn col_(attributes) {
  element("col", attributes, [], [])
}

pub fn colgroup(attributes, children, styles) {
  element("colgroup", attributes, children, styles)
}

pub fn colgroup_(attributes, children) {
  element("colgroup", attributes, children, [])
}

pub fn data(attributes, children, styles) {
  element("data", attributes, children, styles)
}

pub fn data_(attributes, children) {
  element("data", attributes, children, [])
}

pub fn datalist(attributes, children, styles) {
  element("datalist", attributes, children, styles)
}

pub fn datalist_(attributes, children) {
  element("datalist", attributes, children, [])
}

pub fn dd(attributes, children, styles) {
  element("dd", attributes, children, styles)
}

pub fn dd_(attributes, children) {
  element("dd", attributes, children, [])
}

pub fn del(attributes, children, styles) {
  element("del", attributes, children, styles)
}

pub fn del_(attributes, children) {
  element("del", attributes, children, [])
}

pub fn details(attributes, children, styles) {
  element("details", attributes, children, styles)
}

pub fn details_(attributes, children) {
  element("details", attributes, children, [])
}

pub fn dfn(attributes, children, styles) {
  element("dfn", attributes, children, styles)
}

pub fn dfn_(attributes, children) {
  element("dfn", attributes, children, [])
}

pub fn dialog(attributes, children, styles) {
  element("dialog", attributes, children, styles)
}

pub fn dialog_(attributes, children) {
  element("dialog", attributes, children, [])
}

pub fn div(attributes, children, styles) {
  element("div", attributes, children, styles)
}

pub fn div_(attributes, children) {
  element("div", attributes, children, [])
}

pub fn dl(attributes, children, styles) {
  element("dl", attributes, children, styles)
}

pub fn dl_(attributes, children) {
  element("dl", attributes, children, [])
}

pub fn dt(attributes, children, styles) {
  element("dt", attributes, children, styles)
}

pub fn dt_(attributes, children) {
  element("dt", attributes, children, [])
}

pub fn em(attributes, children, styles) {
  element("em", attributes, children, styles)
}

pub fn em_(attributes, children) {
  element("em", attributes, children, [])
}

pub fn embed(attributes, styles) {
  element("embed", attributes, [], styles)
}

pub fn embed_(attributes) {
  element("embed", attributes, [], [])
}

pub fn fieldset(attributes, children, styles) {
  element("fieldset", attributes, children, styles)
}

pub fn fieldset_(attributes, children) {
  element("fieldset", attributes, children, [])
}

pub fn figcaption(attributes, children, styles) {
  element("figcaption", attributes, children, styles)
}

pub fn figcaption_(attributes, children) {
  element("figcaption", attributes, children, [])
}

pub fn figure(attributes, children, styles) {
  element("figure", attributes, children, styles)
}

pub fn figure_(attributes, children) {
  element("figure", attributes, children, [])
}

pub fn footer(attributes, children, styles) {
  element("footer", attributes, children, styles)
}

pub fn footer_(attributes, children) {
  element("footer", attributes, children, [])
}

pub fn form(attributes, children, styles) {
  element("form", attributes, children, styles)
}

pub fn form_(attributes, children) {
  element("form", attributes, children, [])
}

pub fn h1(attributes, children, styles) {
  element("h1", attributes, children, styles)
}

pub fn h1_(attributes, children) {
  element("h1", attributes, children, [])
}

pub fn h2(attributes, children, styles) {
  element("h2", attributes, children, styles)
}

pub fn h2_(attributes, children) {
  element("h2", attributes, children, [])
}

pub fn h3(attributes, children, styles) {
  element("h3", attributes, children, styles)
}

pub fn h3_(attributes, children) {
  element("h3", attributes, children, [])
}

pub fn h4(attributes, children, styles) {
  element("h4", attributes, children, styles)
}

pub fn h4_(attributes, children) {
  element("h4", attributes, children, [])
}

pub fn h5(attributes, children, styles) {
  element("h5", attributes, children, styles)
}

pub fn h5_(attributes, children) {
  element("h5", attributes, children, [])
}

pub fn h6(attributes, children, styles) {
  element("h6", attributes, children, styles)
}

pub fn h6_(attributes, children) {
  element("h6", attributes, children, [])
}

pub fn header(attributes, children, styles) {
  element("header", attributes, children, styles)
}

pub fn header_(attributes, children) {
  element("header", attributes, children, [])
}

pub fn hgroup(attributes, children, styles) {
  element("hgroup", attributes, children, styles)
}

pub fn hgroup_(attributes, children) {
  element("hgroup", attributes, children, [])
}

pub fn hr(attributes, styles) {
  element("hr", attributes, [], styles)
}

pub fn hr_(attributes) {
  element("hr", attributes, [], [])
}

pub fn i(attributes, children, styles) {
  element("i", attributes, children, styles)
}

pub fn i_(attributes, children) {
  element("i", attributes, children, [])
}

pub fn iframe(attributes, children, styles) {
  element("iframe", attributes, children, styles)
}

pub fn iframe_(attributes, children) {
  element("iframe", attributes, children, [])
}

pub fn img(attributes, styles) {
  element("img", attributes, [], styles)
}

pub fn img_(attributes) {
  element("img", attributes, [], [])
}

pub fn input(attributes, styles) {
  element("input", attributes, [], styles)
}

pub fn input_(attributes) {
  element("input", attributes, [], [])
}

pub fn ins(attributes, children, styles) {
  element("ins", attributes, children, styles)
}

pub fn ins_(attributes, children) {
  element("ins", attributes, children, [])
}

pub fn kbd(attributes, children, styles) {
  element("kbd", attributes, children, styles)
}

pub fn kbd_(attributes, children) {
  element("kbd", attributes, children, [])
}

pub fn label(attributes, children, styles) {
  element("label", attributes, children, styles)
}

pub fn label_(attributes, children) {
  element("label", attributes, children, [])
}

pub fn legend(attributes, children, styles) {
  element("legend", attributes, children, styles)
}

pub fn legend_(attributes, children) {
  element("legend", attributes, children, [])
}

pub fn li(attributes, children, styles) {
  element("li", attributes, children, styles)
}

pub fn li_(attributes, children) {
  element("li", attributes, children, [])
}

pub fn main(attributes, children, styles) {
  element("main", attributes, children, styles)
}

pub fn main_(attributes, children) {
  element("main", attributes, children, [])
}

pub fn map(attributes, children, styles) {
  element("map", attributes, children, styles)
}

pub fn map_(attributes, children) {
  element("map", attributes, children, [])
}

pub fn mark(attributes, children, styles) {
  element("mark", attributes, children, styles)
}

pub fn mark_(attributes, children) {
  element("mark", attributes, children, [])
}

pub fn math(attributes, children, styles) {
  element("math", attributes, children, styles)
}

pub fn math_(attributes, children) {
  element("math", attributes, children, [])
}

pub fn menu(attributes, children, styles) {
  element("menu", attributes, children, styles)
}

pub fn menu_(attributes, children) {
  element("menu", attributes, children, [])
}

pub fn meter(attributes, children, styles) {
  element("meter", attributes, children, styles)
}

pub fn meter_(attributes, children) {
  element("meter", attributes, children, [])
}

pub fn nav(attributes, children, styles) {
  element("nav", attributes, children, styles)
}

pub fn nav_(attributes, children) {
  element("nav", attributes, children, [])
}

pub fn noscript(attributes, children, styles) {
  element("noscript", attributes, children, styles)
}

pub fn noscript_(attributes, children) {
  element("noscript", attributes, children, [])
}

pub fn object(attributes, children, styles) {
  element("object", attributes, children, styles)
}

pub fn object_(attributes, children) {
  element("object", attributes, children, [])
}

pub fn ol(attributes, children, styles) {
  element("ol", attributes, children, styles)
}

pub fn ol_(attributes, children) {
  element("ol", attributes, children, [])
}

pub fn optgroup(attributes, children, styles) {
  element("optgroup", attributes, children, styles)
}

pub fn optgroup_(attributes, children) {
  element("optgroup", attributes, children, [])
}

pub fn option(attributes, children, styles) {
  element("option", attributes, children, styles)
}

pub fn option_(attributes, children) {
  element("option", attributes, children, [])
}

pub fn output(attributes, children, styles) {
  element("output", attributes, children, styles)
}

pub fn output_(attributes, children) {
  element("output", attributes, children, [])
}

pub fn p(attributes, children, styles) {
  element("p", attributes, children, styles)
}

pub fn p_(attributes, children) {
  element("p", attributes, children, [])
}

pub fn picture(attributes, children, styles) {
  element("picture", attributes, children, styles)
}

pub fn picture_(attributes, children) {
  element("picture", attributes, children, [])
}

pub fn portal(attributes, children, styles) {
  element("portal", attributes, children, styles)
}

pub fn portal_(attributes, children) {
  element("portal", attributes, children, [])
}

pub fn pre(attributes, children, styles) {
  element("pre", attributes, children, styles)
}

pub fn pre_(attributes, children) {
  element("pre", attributes, children, [])
}

pub fn progress(attributes, children, styles) {
  element("progress", attributes, children, styles)
}

pub fn progress_(attributes, children) {
  element("progress", attributes, children, [])
}

pub fn q(attributes, children, styles) {
  element("q", attributes, children, styles)
}

pub fn q_(attributes, children) {
  element("q", attributes, children, [])
}

pub fn rp(attributes, children, styles) {
  element("rp", attributes, children, styles)
}

pub fn rp_(attributes, children) {
  element("rp", attributes, children, [])
}

pub fn rt(attributes, children, styles) {
  element("rt", attributes, children, styles)
}

pub fn rt_(attributes, children) {
  element("rt", attributes, children, [])
}

pub fn ruby(attributes, children, styles) {
  element("ruby", attributes, children, styles)
}

pub fn ruby_(attributes, children) {
  element("ruby", attributes, children, [])
}

pub fn s(attributes, children, styles) {
  element("s", attributes, children, styles)
}

pub fn s_(attributes, children) {
  element("s", attributes, children, [])
}

pub fn samp(attributes, children, styles) {
  element("samp", attributes, children, styles)
}

pub fn samp_(attributes, children) {
  element("samp", attributes, children, [])
}

pub fn script(attributes, children, styles) {
  element("script", attributes, children, styles)
}

pub fn script_(attributes, children) {
  element("script", attributes, children, [])
}

pub fn search(attributes, children, styles) {
  element("search", attributes, children, styles)
}

pub fn search_(attributes, children) {
  element("search", attributes, children, [])
}

pub fn section(attributes, children, styles) {
  element("section", attributes, children, styles)
}

pub fn section_(attributes, children) {
  element("section", attributes, children, [])
}

pub fn select(attributes, children, styles) {
  element("select", attributes, children, styles)
}

pub fn select_(attributes, children) {
  element("select", attributes, children, [])
}

pub fn slot(attributes, children, styles) {
  element("slot", attributes, children, styles)
}

pub fn slot_(attributes, children) {
  element("slot", attributes, children, [])
}

pub fn small(attributes, children, styles) {
  element("small", attributes, children, styles)
}

pub fn small_(attributes, children) {
  element("small", attributes, children, [])
}

pub fn source(attributes, styles) {
  element("source", attributes, [], styles)
}

pub fn source_(attributes) {
  element("source", attributes, [], [])
}

pub fn span(attributes, children, styles) {
  element("span", attributes, children, styles)
}

pub fn span_(attributes, children) {
  element("span", attributes, children, [])
}

pub fn strong(attributes, children, styles) {
  element("strong", attributes, children, styles)
}

pub fn strong_(attributes, children) {
  element("strong", attributes, children, [])
}

pub fn sub(attributes, children, styles) {
  element("sub", attributes, children, styles)
}

pub fn sub_(attributes, children) {
  element("sub", attributes, children, [])
}

pub fn summary(attributes, children, styles) {
  element("summary", attributes, children, styles)
}

pub fn summary_(attributes, children) {
  element("summary", attributes, children, [])
}

pub fn sup(attributes, children, styles) {
  element("sup", attributes, children, styles)
}

pub fn sup_(attributes, children) {
  element("sup", attributes, children, [])
}

pub fn svg(attributes, children, styles) {
  element("svg", attributes, children, styles)
}

pub fn svg_(attributes, children) {
  element("svg", attributes, children, [])
}

pub fn table(attributes, children, styles) {
  element("table", attributes, children, styles)
}

pub fn table_(attributes, children) {
  element("table", attributes, children, [])
}

pub fn tbody(attributes, children, styles) {
  element("tbody", attributes, children, styles)
}

pub fn tbody_(attributes, children) {
  element("tbody", attributes, children, [])
}

pub fn td(attributes, children, styles) {
  element("td", attributes, children, styles)
}

pub fn td_(attributes, children) {
  element("td", attributes, children, [])
}

pub fn template(attributes, children, styles) {
  element("template", attributes, children, styles)
}

pub fn template_(attributes, children) {
  element("template", attributes, children, [])
}

pub fn textarea(attributes, children, styles) {
  element("textarea", attributes, children, styles)
}

pub fn textarea_(attributes, children) {
  element("textarea", attributes, children, [])
}

pub fn tfoot(attributes, children, styles) {
  element("tfoot", attributes, children, styles)
}

pub fn tfoot_(attributes, children) {
  element("tfoot", attributes, children, [])
}

pub fn th(attributes, children, styles) {
  element("th", attributes, children, styles)
}

pub fn th_(attributes, children) {
  element("th", attributes, children, [])
}

pub fn thead(attributes, children, styles) {
  element("thead", attributes, children, styles)
}

pub fn thead_(attributes, children) {
  element("thead", attributes, children, [])
}

pub fn time(attributes, children, styles) {
  element("time", attributes, children, styles)
}

pub fn time_(attributes, children) {
  element("time", attributes, children, [])
}

pub fn tr(attributes, children, styles) {
  element("tr", attributes, children, styles)
}

pub fn tr_(attributes, children) {
  element("tr", attributes, children, [])
}

pub fn track(attributes, styles) {
  element("track", attributes, [], styles)
}

pub fn track_(attributes) {
  element("track", attributes, [], [])
}

pub fn u(attributes, children, styles) {
  element("u", attributes, children, styles)
}

pub fn u_(attributes, children) {
  element("u", attributes, children, [])
}

pub fn ul(attributes, children, styles) {
  element("ul", attributes, children, styles)
}

pub fn ul_(attributes, children) {
  element("ul", attributes, children, [])
}

pub fn var(attributes, children, styles) {
  element("var", attributes, children, styles)
}

pub fn var_(attributes, children) {
  element("var", attributes, children, [])
}

pub fn video(attributes, children, styles) {
  element("video", attributes, children, styles)
}

pub fn video_(attributes, children) {
  element("video", attributes, children, [])
}

pub fn wbr(attributes, styles) {
  element("wbr", attributes, [], styles)
}

pub fn wbr_(attributes) {
  element("wbr", attributes, [], [])
}
