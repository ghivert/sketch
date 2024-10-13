import sketch/lustre/element.{element, element_, namespaced, namespaced_}

//

pub fn html(attributes, children) {
  element_("html", attributes, children)
}

pub fn base(attributes) {
  element_("base", attributes, [])
}

pub fn head(attributes, children) {
  element_("head", attributes, children)
}

pub fn link(attributes) {
  element_("link", attributes, [])
}

pub fn meta(attributes) {
  element_("meta", attributes, [])
}

pub fn style(attributes, child) {
  element_("style", attributes, [text(child)])
}

pub fn title(attributes, title: String) {
  element_("title", attributes, [text(title)])
}

//

pub fn text(content) {
  element.text(content)
}

//

pub fn a(class, attributes, children) {
  element("a", class, attributes, children)
}

pub fn a_(attributes, children) {
  element_("a", attributes, children)
}

pub fn abbr(class, attributes, children) {
  element("abbr", class, attributes, children)
}

pub fn abbr_(attributes, children) {
  element_("abbr", attributes, children)
}

pub fn address(class, attributes, children) {
  element("address", class, attributes, children)
}

pub fn address_(attributes, children) {
  element_("address", attributes, children)
}

pub fn area(class, attributes) {
  element("area", class, attributes, [])
}

pub fn area_(attributes) {
  element_("area", attributes, [])
}

pub fn article(class, attributes, children) {
  element("article", class, attributes, children)
}

pub fn article_(attributes, children) {
  element_("article", attributes, children)
}

pub fn aside(class, attributes, children) {
  element("aside", class, attributes, children)
}

pub fn aside_(attributes, children) {
  element_("aside", attributes, children)
}

pub fn audio(class, attributes, children) {
  element("audio", class, attributes, children)
}

pub fn audio_(attributes, children) {
  element_("audio", attributes, children)
}

pub fn b(class, attributes, children) {
  element("b", class, attributes, children)
}

pub fn b_(attributes, children) {
  element_("b", attributes, children)
}

pub fn bdi(class, attributes, children) {
  element("bdi", class, attributes, children)
}

pub fn bdi_(attributes, children) {
  element_("bdi", attributes, children)
}

pub fn bdo(class, attributes, children) {
  element("bdo", class, attributes, children)
}

pub fn bdo_(attributes, children) {
  element_("bdo", attributes, children)
}

pub fn blockquote(class, attributes, children) {
  element("blockquote", class, attributes, children)
}

pub fn blockquote_(attributes, children) {
  element_("blockquote", attributes, children)
}

pub fn body(class, attributes, children) {
  element("body", class, attributes, children)
}

pub fn body_(attributes, children) {
  element_("body", attributes, children)
}

pub fn br(class, attributes) {
  element("br", class, attributes, [])
}

pub fn br_(attributes) {
  element_("br", attributes, [])
}

pub fn button(class, attributes, children) {
  element("button", class, attributes, children)
}

pub fn button_(attributes, children) {
  element_("button", attributes, children)
}

pub fn canvas(class, attributes, children) {
  element("canvas", class, attributes, children)
}

pub fn canvas_(attributes, children) {
  element_("canvas", attributes, children)
}

pub fn caption(class, attributes, children) {
  element("caption", class, attributes, children)
}

pub fn caption_(attributes, children) {
  element_("caption", attributes, children)
}

pub fn cite(class, attributes, children) {
  element("cite", class, attributes, children)
}

pub fn cite_(attributes, children) {
  element_("cite", attributes, children)
}

pub fn code(class, attributes, children) {
  element("code", class, attributes, children)
}

pub fn code_(attributes, children) {
  element_("code", attributes, children)
}

pub fn col(class, attributes) {
  element("col", class, attributes, [])
}

pub fn col_(attributes) {
  element_("col", attributes, [])
}

pub fn colgroup(class, attributes, children) {
  element("colgroup", class, attributes, children)
}

pub fn colgroup_(attributes, children) {
  element_("colgroup", attributes, children)
}

pub fn data(class, attributes, children) {
  element("data", class, attributes, children)
}

pub fn data_(attributes, children) {
  element_("data", attributes, children)
}

pub fn datalist(class, attributes, children) {
  element("datalist", class, attributes, children)
}

pub fn datalist_(attributes, children) {
  element_("datalist", attributes, children)
}

pub fn dd(class, attributes, children) {
  element("dd", class, attributes, children)
}

pub fn dd_(attributes, children) {
  element_("dd", attributes, children)
}

pub fn del(class, attributes, children) {
  element("del", class, attributes, children)
}

pub fn del_(attributes, children) {
  element_("del", attributes, children)
}

pub fn details(class, attributes, children) {
  element("details", class, attributes, children)
}

pub fn details_(attributes, children) {
  element_("details", attributes, children)
}

pub fn dfn(class, attributes, children) {
  element("dfn", class, attributes, children)
}

pub fn dfn_(attributes, children) {
  element_("dfn", attributes, children)
}

pub fn dialog(class, attributes, children) {
  element("dialog", class, attributes, children)
}

pub fn dialog_(attributes, children) {
  element_("dialog", attributes, children)
}

pub fn div(class, attributes, children) {
  element("div", class, attributes, children)
}

pub fn div_(attributes, children) {
  element_("div", attributes, children)
}

pub fn dl(class, attributes, children) {
  element("dl", class, attributes, children)
}

pub fn dl_(attributes, children) {
  element_("dl", attributes, children)
}

pub fn dt(class, attributes, children) {
  element("dt", class, attributes, children)
}

pub fn dt_(attributes, children) {
  element_("dt", attributes, children)
}

pub fn em(class, attributes, children) {
  element("em", class, attributes, children)
}

pub fn em_(attributes, children) {
  element_("em", attributes, children)
}

pub fn embed(class, attributes) {
  element("embed", class, attributes, [])
}

pub fn embed_(attributes) {
  element_("embed", attributes, [])
}

pub fn fieldset(class, attributes, children) {
  element("fieldset", class, attributes, children)
}

pub fn fieldset_(attributes, children) {
  element_("fieldset", attributes, children)
}

pub fn figcaption(class, attributes, children) {
  element("figcaption", class, attributes, children)
}

pub fn figcaption_(attributes, children) {
  element_("figcaption", attributes, children)
}

pub fn figure(class, attributes, children) {
  element("figure", class, attributes, children)
}

pub fn figure_(attributes, children) {
  element_("figure", attributes, children)
}

pub fn footer(class, attributes, children) {
  element("footer", class, attributes, children)
}

pub fn footer_(attributes, children) {
  element_("footer", attributes, children)
}

pub fn form(class, attributes, children) {
  element("form", class, attributes, children)
}

pub fn form_(attributes, children) {
  element_("form", attributes, children)
}

pub fn h1(class, attributes, children) {
  element("h1", class, attributes, children)
}

pub fn h1_(attributes, children) {
  element_("h1", attributes, children)
}

pub fn h2(class, attributes, children) {
  element("h2", class, attributes, children)
}

pub fn h2_(attributes, children) {
  element_("h2", attributes, children)
}

pub fn h3(class, attributes, children) {
  element("h3", class, attributes, children)
}

pub fn h3_(attributes, children) {
  element_("h3", attributes, children)
}

pub fn h4(class, attributes, children) {
  element("h4", class, attributes, children)
}

pub fn h4_(attributes, children) {
  element_("h4", attributes, children)
}

pub fn h5(class, attributes, children) {
  element("h5", class, attributes, children)
}

pub fn h5_(attributes, children) {
  element_("h5", attributes, children)
}

pub fn h6(class, attributes, children) {
  element("h6", class, attributes, children)
}

pub fn h6_(attributes, children) {
  element_("h6", attributes, children)
}

pub fn header(class, attributes, children) {
  element("header", class, attributes, children)
}

pub fn header_(attributes, children) {
  element_("header", attributes, children)
}

pub fn hgroup(class, attributes, children) {
  element("hgroup", class, attributes, children)
}

pub fn hgroup_(attributes, children) {
  element_("hgroup", attributes, children)
}

pub fn hr(class, attributes) {
  element("hr", class, attributes, [])
}

pub fn hr_(attributes) {
  element_("hr", attributes, [])
}

pub fn i(class, attributes, children) {
  element("i", class, attributes, children)
}

pub fn i_(attributes, children) {
  element_("i", attributes, children)
}

pub fn iframe(class, attributes, children) {
  element("iframe", class, attributes, children)
}

pub fn iframe_(attributes, children) {
  element_("iframe", attributes, children)
}

pub fn img(class, attributes) {
  element("img", class, attributes, [])
}

pub fn img_(attributes) {
  element_("img", attributes, [])
}

pub fn input(class, attributes) {
  element("input", class, attributes, [])
}

pub fn input_(attributes) {
  element_("input", attributes, [])
}

pub fn ins(class, attributes, children) {
  element("ins", class, attributes, children)
}

pub fn ins_(attributes, children) {
  element_("ins", attributes, children)
}

pub fn kbd(class, attributes, children) {
  element("kbd", class, attributes, children)
}

pub fn kbd_(attributes, children) {
  element_("kbd", attributes, children)
}

pub fn label(class, attributes, children) {
  element("label", class, attributes, children)
}

pub fn label_(attributes, children) {
  element_("label", attributes, children)
}

pub fn legend(class, attributes, children) {
  element("legend", class, attributes, children)
}

pub fn legend_(attributes, children) {
  element_("legend", attributes, children)
}

pub fn li(class, attributes, children) {
  element("li", class, attributes, children)
}

pub fn li_(attributes, children) {
  element_("li", attributes, children)
}

pub fn main(class, attributes, children) {
  element("main", class, attributes, children)
}

pub fn main_(attributes, children) {
  element_("main", attributes, children)
}

pub fn map(class, attributes, children) {
  element("map", class, attributes, children)
}

pub fn map_(attributes, children) {
  element_("map", attributes, children)
}

pub fn mark(class, attributes, children) {
  element("mark", class, attributes, children)
}

pub fn mark_(attributes, children) {
  element_("mark", attributes, children)
}

pub fn math(class, attributes, children) {
  element("math", class, attributes, children)
}

pub fn math_(attributes, children) {
  element_("math", attributes, children)
}

pub fn menu(class, attributes, children) {
  element("menu", class, attributes, children)
}

pub fn menu_(attributes, children) {
  element_("menu", attributes, children)
}

pub fn meter(class, attributes, children) {
  element("meter", class, attributes, children)
}

pub fn meter_(attributes, children) {
  element_("meter", attributes, children)
}

pub fn nav(class, attributes, children) {
  element("nav", class, attributes, children)
}

pub fn nav_(attributes, children) {
  element_("nav", attributes, children)
}

pub fn noscript(class, attributes, children) {
  element("noscript", class, attributes, children)
}

pub fn noscript_(attributes, children) {
  element_("noscript", attributes, children)
}

pub fn object(class, attributes, children) {
  element("object", class, attributes, children)
}

pub fn object_(attributes, children) {
  element_("object", attributes, children)
}

pub fn ol(class, attributes, children) {
  element("ol", class, attributes, children)
}

pub fn ol_(attributes, children) {
  element_("ol", attributes, children)
}

pub fn optgroup(class, attributes, children) {
  element("optgroup", class, attributes, children)
}

pub fn optgroup_(attributes, children) {
  element_("optgroup", attributes, children)
}

pub fn option(class, attributes, children) {
  element("option", class, attributes, children)
}

pub fn option_(attributes, children) {
  element_("option", attributes, children)
}

pub fn output(class, attributes, children) {
  element("output", class, attributes, children)
}

pub fn output_(attributes, children) {
  element_("output", attributes, children)
}

pub fn p(class, attributes, children) {
  element("p", class, attributes, children)
}

pub fn p_(attributes, children) {
  element_("p", attributes, children)
}

pub fn picture(class, attributes, children) {
  element("picture", class, attributes, children)
}

pub fn picture_(attributes, children) {
  element_("picture", attributes, children)
}

pub fn portal(class, attributes, children) {
  element("portal", class, attributes, children)
}

pub fn portal_(attributes, children) {
  element_("portal", attributes, children)
}

pub fn pre(class, attributes, children) {
  element("pre", class, attributes, children)
}

pub fn pre_(attributes, children) {
  element_("pre", attributes, children)
}

pub fn progress(class, attributes, children) {
  element("progress", class, attributes, children)
}

pub fn progress_(attributes, children) {
  element_("progress", attributes, children)
}

pub fn q(class, attributes, children) {
  element("q", class, attributes, children)
}

pub fn q_(attributes, children) {
  element_("q", attributes, children)
}

pub fn rp(class, attributes, children) {
  element("rp", class, attributes, children)
}

pub fn rp_(attributes, children) {
  element_("rp", attributes, children)
}

pub fn rt(class, attributes, children) {
  element("rt", class, attributes, children)
}

pub fn rt_(attributes, children) {
  element_("rt", attributes, children)
}

pub fn ruby(class, attributes, children) {
  element("ruby", class, attributes, children)
}

pub fn ruby_(attributes, children) {
  element_("ruby", attributes, children)
}

pub fn s(class, attributes, children) {
  element("s", class, attributes, children)
}

pub fn s_(attributes, children) {
  element_("s", attributes, children)
}

pub fn samp(class, attributes, children) {
  element("samp", class, attributes, children)
}

pub fn samp_(attributes, children) {
  element_("samp", attributes, children)
}

pub fn script(class, attributes, children) {
  element("script", class, attributes, children)
}

pub fn script_(attributes, children) {
  element_("script", attributes, children)
}

pub fn search(class, attributes, children) {
  element("search", class, attributes, children)
}

pub fn search_(attributes, children) {
  element_("search", attributes, children)
}

pub fn section(class, attributes, children) {
  element("section", class, attributes, children)
}

pub fn section_(attributes, children) {
  element_("section", attributes, children)
}

pub fn select(class, attributes, children) {
  element("select", class, attributes, children)
}

pub fn select_(attributes, children) {
  element_("select", attributes, children)
}

pub fn slot(class, attributes, children) {
  element("slot", class, attributes, children)
}

pub fn slot_(attributes, children) {
  element_("slot", attributes, children)
}

pub fn small(class, attributes, children) {
  element("small", class, attributes, children)
}

pub fn small_(attributes, children) {
  element_("small", attributes, children)
}

pub fn source(class, attributes) {
  element("source", class, attributes, [])
}

pub fn source_(attributes) {
  element_("source", attributes, [])
}

pub fn span(class, attributes, children) {
  element("span", class, attributes, children)
}

pub fn span_(attributes, children) {
  element_("span", attributes, children)
}

pub fn strong(class, attributes, children) {
  element("strong", class, attributes, children)
}

pub fn strong_(attributes, children) {
  element_("strong", attributes, children)
}

pub fn sub(class, attributes, children) {
  element("sub", class, attributes, children)
}

pub fn sub_(attributes, children) {
  element_("sub", attributes, children)
}

pub fn summary(class, attributes, children) {
  element("summary", class, attributes, children)
}

pub fn summary_(attributes, children) {
  element_("summary", attributes, children)
}

pub fn sup(class, attributes, children) {
  element("sup", class, attributes, children)
}

pub fn sup_(attributes, children) {
  element_("sup", attributes, children)
}

pub fn svg(class, attributes, children) {
  namespaced("http://www.w3.org/2000/svg", "svg", class, attributes, children)
}

pub fn svg_(attributes, children) {
  namespaced_("http://www.w3.org/2000/svg", "svg", attributes, children)
}

pub fn table(class, attributes, children) {
  element("table", class, attributes, children)
}

pub fn table_(attributes, children) {
  element_("table", attributes, children)
}

pub fn tbody(class, attributes, children) {
  element("tbody", class, attributes, children)
}

pub fn tbody_(attributes, children) {
  element_("tbody", attributes, children)
}

pub fn td(class, attributes, children) {
  element("td", class, attributes, children)
}

pub fn td_(attributes, children) {
  element_("td", attributes, children)
}

pub fn template(class, attributes, children) {
  element("template", class, attributes, children)
}

pub fn template_(attributes, children) {
  element_("template", attributes, children)
}

pub fn textarea(class, attributes, children) {
  element("textarea", class, attributes, children)
}

pub fn textarea_(attributes, children) {
  element_("textarea", attributes, children)
}

pub fn tfoot(class, attributes, children) {
  element("tfoot", class, attributes, children)
}

pub fn tfoot_(attributes, children) {
  element_("tfoot", attributes, children)
}

pub fn th(class, attributes, children) {
  element("th", class, attributes, children)
}

pub fn th_(attributes, children) {
  element_("th", attributes, children)
}

pub fn thead(class, attributes, children) {
  element("thead", class, attributes, children)
}

pub fn thead_(attributes, children) {
  element_("thead", attributes, children)
}

pub fn time(class, attributes, children) {
  element("time", class, attributes, children)
}

pub fn time_(attributes, children) {
  element_("time", attributes, children)
}

pub fn tr(class, attributes, children) {
  element("tr", class, attributes, children)
}

pub fn tr_(attributes, children) {
  element_("tr", attributes, children)
}

pub fn track(class, attributes) {
  element("track", class, attributes, [])
}

pub fn track_(attributes) {
  element_("track", attributes, [])
}

pub fn u(class, attributes, children) {
  element("u", class, attributes, children)
}

pub fn u_(attributes, children) {
  element_("u", attributes, children)
}

pub fn ul(class, attributes, children) {
  element("ul", class, attributes, children)
}

pub fn ul_(attributes, children) {
  element_("ul", attributes, children)
}

pub fn var(class, attributes, children) {
  element("var", class, attributes, children)
}

pub fn var_(attributes, children) {
  element_("var", attributes, children)
}

pub fn video(class, attributes, children) {
  element("video", class, attributes, children)
}

pub fn video_(attributes, children) {
  element_("video", attributes, children)
}

pub fn wbr(class, attributes) {
  element("wbr", class, attributes, [])
}

pub fn wbr_(attributes) {
  element_("wbr", attributes, [])
}
