//// This module is a drop-in replacement for `lustre/element/html`. Just
//// use the new functions, and everything will automagically be styled.
//// Every stylable node has two functions: `[node]` and `[node]_`, the former
//// applying a style, while the latter does not accept style, in case you don't
//// need to style a node.

import lustre/attribute.{type Attribute}
import sketch/css.{type Class}
import sketch/lustre/experimental/element.{type Element} as el

/// The `<html>` HTML element represents the root (top-level element) of an
/// HTML document, so it is also referred to as the root element. All other
/// elements must be descendants of this element. There can be only one `<html>`
/// element in a document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/html)
pub fn html(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("html", attributes, children)
}

/// The `<base>` HTML element specifies the base URL to use for all relative
/// URLs in a document. There can be only one `<base>` element in a document.
///
/// A document's used base URL can be accessed by scripts with `Node.baseURI`.
/// If the document has no `<base>` elements, then `baseURI` defaults to
/// `location.href`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/base)
pub fn base(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("base", attributes, [])
}

/// The `<head>` HTML element contains machine-readable information (metadata)
/// about the document, like its title, scripts, and style sheets. There can be
/// only one `<head>` element in an HTML document.
///
/// > `<head>` primarily holds information for machine processing, not
/// > human-readability. For human-visible information, like top-level headings
/// > and listed authors, see the `<header>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/head)
pub fn head(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("head", attributes, children)
}

/// The `<link>` HTML element specifies relationships between the current
/// document and an external resource. This element is most commonly used to
/// link to stylesheets, but is also used to establish site icons (both "favicon"
/// style icons and icons for the home screen and apps on mobile devices) among
/// other things.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/link)
pub fn link(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("link", attributes, [])
}

/// The `<meta>` HTML element represents metadata that cannot be represented
/// by other HTML meta-related elements, like `<base>`, `<link>`, `<script>`,
/// `<style>` or `<title>`.
///
/// The type of metadata provided by the `<meta>` element can be one of the
/// following:
/// - If the `name` attribute is set, the `<meta>` element provides
///   document-level metadata, applying to the whole page.
/// - If the `http-equiv` attribute is set, the `<meta>` element is a pragma
///   directive, providing information equivalent to what can be given by a
///   similarly-named HTTP header.
/// - If the `charset` attribute is set, the `<meta>` element is a charset
///   declaration, giving the character encoding in which the document is encoded.
/// - If the `itemprop` attribute is set, the `<meta>` element provides
///   user-defined metadata.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/meta)
pub fn meta(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("meta", attributes, [])
}

/// The `<style>` HTML element contains style information for a document, or
/// part of a document. It contains CSS, which is applied to the contents of
/// the document containing the `<style>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/style)
pub fn style(attributes: List(Attribute(a)), child: String) -> Element(a) {
  el.element_("style", attributes, [text(child)])
}

/// The `<title>` HTML element defines the document's title that is shown in a
/// browser's title bar or a page's tab. It only contains text; tags within
/// the element are ignored.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/title)
pub fn title(attributes: List(Attribute(a)), title: String) -> Element(a) {
  el.element_("title", attributes, [text(title)])
}

/// The `Text` interface represents a text node in a DOM tree.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/API/Text)
pub fn text(content: String) -> Element(a) {
  el.text(content)
}

/// The `<a>` HTML element (or anchor element), with [its `href` attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#href),
/// creates a hyperlink to web pages, files, email addresses, locations in the
/// same page, or anything else a URL can address.
///
/// Content within each `<a>` should indicate the link's destination. If the
/// `href` attribute is present, pressing the enter key while focused on the
/// `<a>` element will activate it.
///
/// You can use [`modem`](https://hexdocs.pm/modem/) to manage internal linking
/// in your Lustre application.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/a)
pub fn a(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("a", class, attributes, children)
}

/// The `<a>` HTML element (or anchor element), with [its `href` attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#href),
/// creates a hyperlink to web pages, files, email addresses, locations in the
/// same page, or anything else a URL can address.
///
/// Content within each `<a>` should indicate the link's destination. If the
/// `href` attribute is present, pressing the enter key while focused on the
/// `<a>` element will activate it.
///
/// You can use [`modem`](https://hexdocs.pm/modem/) to manage internal linking
/// in your Lustre application.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/a)
pub fn a_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("a", attributes, children)
}

/// The `<abbr>` HTML element represents an abbreviation or acronym.
///
/// When including an abbreviation or acronym, provide a full expansion of the
/// term in plain text on first use, along with the `<abbr>` to mark up the
/// abbreviation. This informs the user what the abbreviation or acronym means.
///
/// The optional `title` attribute can provide an expansion for the abbreviation
/// or acronym when a full expansion is not present. This provides a hint to
/// user agents on how to announce/display the content while informing all users
/// what the abbreviation means. If present, `title` must contain this full
/// description and nothing else.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/abbr)
pub fn abbr(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("abbr", class, attributes, children)
}

/// The `<abbr>` HTML element represents an abbreviation or acronym.
///
/// When including an abbreviation or acronym, provide a full expansion of the
/// term in plain text on first use, along with the `<abbr>` to mark up the
/// abbreviation. This informs the user what the abbreviation or acronym means.
///
/// The optional `title` attribute can provide an expansion for the abbreviation
/// or acronym when a full expansion is not present. This provides a hint to
/// user agents on how to announce/display the content while informing all users
/// what the abbreviation means. If present, `title` must contain this full
/// description and nothing else.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/abbr)
pub fn abbr_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("abbr", attributes, children)
}

/// The `<address>` HTML element indicates that the enclosed HTML provides
/// contact information for a person or people, or for an organization.
///
/// The contact information provided by an `<address>` element's contents can
/// take whatever form is appropriate for the context, and may include any type
///  of contact information that is needed, such as a physical address, URL,
///  email address, phone number, social media handle, geographic coordinates,
/// and so forth. The `<address>` element should include the name of the person,
/// people, or organization to which the contact information refers.
///
/// `<address>` can be used in a variety of contexts, such as providing a
/// business's contact information in the page header, or indicating the author
/// of an article by including an `<address>` element within the `<article>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/address)
pub fn address(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("address", class, attributes, children)
}

/// The `<address>` HTML element indicates that the enclosed HTML provides
/// contact information for a person or people, or for an organization.
///
/// The contact information provided by an `<address>` element's contents can
/// take whatever form is appropriate for the context, and may include any type
///  of contact information that is needed, such as a physical address, URL,
///  email address, phone number, social media handle, geographic coordinates,
/// and so forth. The `<address>` element should include the name of the person,
/// people, or organization to which the contact information refers.
///
/// `<address>` can be used in a variety of contexts, such as providing a
/// business's contact information in the page header, or indicating the author
/// of an article by including an `<address>` element within the `<article>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/address)
pub fn address_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("address", attributes, children)
}

/// The `<area>` HTML element defines an area inside an image map that has
/// predefined clickable areas. An image map allows geometric areas on an
/// image to be associated with hypertext links.
///
/// This element is used only within a `<map>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/area)
pub fn area(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("area", class, attributes, [])
}

/// The `<area>` HTML element defines an area inside an image map that has
/// predefined clickable areas. An image map allows geometric areas on an
/// image to be associated with hypertext links.
///
/// This element is used only within a `<map>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/area)
pub fn area_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("area", attributes, [])
}

/// The `<article>` HTML element represents a self-contained composition in a
/// document, page, application, or site, which is intended to be independently
/// distributable or reusable (e.g., in syndication). Examples include: a forum
/// post, a magazine or newspaper article, or a blog entry, a product card, a
/// user-submitted comment, an interactive widget or gadget, or any other
/// independent item of content.
///
/// A given document can have multiple articles in it; for example, on a blog
/// that shows the text of each article one after another as the reader scrolls,
/// each post would be contained in an `<article>` element, possibly with one
/// or more `<section>`s within.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/article)
pub fn article(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("article", class, attributes, children)
}

/// The `<article>` HTML element represents a self-contained composition in a
/// document, page, application, or site, which is intended to be independently
/// distributable or reusable (e.g., in syndication). Examples include: a forum
/// post, a magazine or newspaper article, or a blog entry, a product card, a
/// user-submitted comment, an interactive widget or gadget, or any other
/// independent item of content.
///
/// A given document can have multiple articles in it; for example, on a blog
/// that shows the text of each article one after another as the reader scrolls,
/// each post would be contained in an `<article>` element, possibly with one
/// or more `<section>`s within.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/article)
pub fn article_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("article", attributes, children)
}

/// The `<aside>` HTML element represents a portion of a document whose content
/// is only indirectly related to the document's main content. Asides are f
/// requently presented as sidebars or call-out boxes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/aside)
pub fn aside(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("aside", class, attributes, children)
}

/// The `<aside>` HTML element represents a portion of a document whose content
/// is only indirectly related to the document's main content. Asides are f
/// requently presented as sidebars or call-out boxes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/aside)
pub fn aside_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("aside", attributes, children)
}

/// The `<audio>` HTML element is used to embed sound content in documents. It
/// may contain one or more audio sources, represented using the src attribute
/// or the `<source>` element: the browser will choose the most suitable one. It
/// can also be the destination for streamed media, using a `MediaStream`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/audio)
pub fn audio(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("audio", class, attributes, children)
}

/// The `<audio>` HTML element is used to embed sound content in documents. It
/// may contain one or more audio sources, represented using the src attribute
/// or the `<source>` element: the browser will choose the most suitable one. It
/// can also be the destination for streamed media, using a `MediaStream`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/audio)
pub fn audio_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("audio", attributes, children)
}

/// The `<b>` HTML element is used to draw the reader's attention to the
/// element's contents, which are not otherwise granted special importance.
/// This was formerly known as the Boldface element, and most browsers still
/// draw the text in boldface. However, you should not use `<b>` for styling text
/// or granting importance. If you wish to create boldface text, you should
/// use the CSS `font-weight` property. If you wish to indicate an element is
/// of special importance, you should use the `<strong>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/b)
pub fn b(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("b", class, attributes, children)
}

/// The `<b>` HTML element is used to draw the reader's attention to the
/// element's contents, which are not otherwise granted special importance.
/// This was formerly known as the Boldface element, and most browsers still
/// draw the text in boldface. However, you should not use `<b>` for styling text
/// or granting importance. If you wish to create boldface text, you should
/// use the CSS `font-weight` property. If you wish to indicate an element is
/// of special importance, you should use the `<strong>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/b)
pub fn b_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("b", attributes, children)
}

/// The `<bdi>` HTML element tells the browser's bidirectional algorithm to
/// treat the text it contains in isolation from its surrounding text. It's
/// particularly useful when a website dynamically inserts some text and
/// doesn't know the directionality of the text being inserted.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdi)
pub fn bdi(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("bdi", class, attributes, children)
}

/// The `<bdi>` HTML element tells the browser's bidirectional algorithm to
/// treat the text it contains in isolation from its surrounding text. It's
/// particularly useful when a website dynamically inserts some text and
/// doesn't know the directionality of the text being inserted.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdi)
pub fn bdi_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("bdi", attributes, children)
}

/// The `<bdo>` HTML element overrides the current directionality of text, so
/// that the text within is rendered in a different direction.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdo)
pub fn bdo(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("bdo", class, attributes, children)
}

/// The `<bdo>` HTML element overrides the current directionality of text, so
/// that the text within is rendered in a different direction.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/bdo)
pub fn bdo_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("bdo", attributes, children)
}

/// The `<blockquote>` HTML element indicates that the enclosed text is an
/// extended quotation. Usually, this is rendered visually by indentation.
/// A URL for the source of the quotation may be given using the cite attribute,
/// while a text representation of the source can be given using the `<cite>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/blockquote)
pub fn blockquote(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("blockquote", class, attributes, children)
}

/// The `<blockquote>` HTML element indicates that the enclosed text is an
/// extended quotation. Usually, this is rendered visually by indentation.
/// A URL for the source of the quotation may be given using the cite attribute,
/// while a text representation of the source can be given using the `<cite>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/blockquote)
pub fn blockquote_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("blockquote", attributes, children)
}

/// The `<body>` HTML element represents the content of an HTML document.
/// There can be only one `<body>` element in a document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/body)
pub fn body(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("body", class, attributes, children)
}

/// The `<body>` HTML element represents the content of an HTML document.
/// There can be only one `<body>` element in a document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/body)
pub fn body_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("body", attributes, children)
}

/// The `<br>` HTML element produces a line break in text (carriage-return).
/// It is useful for writing a poem or an address, where the division of
/// lines is significant.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/br)
pub fn br(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("br", class, attributes, [])
}

/// The `<br>` HTML element produces a line break in text (carriage-return).
/// It is useful for writing a poem or an address, where the division of
/// lines is significant.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/br)
pub fn br_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("br", attributes, [])
}

/// The `<button>` HTML element is an interactive element activated by a user
/// with a mouse, keyboard, finger, voice command, or other assistive technology.
/// Once activated, it then performs an action, such as submitting a form or
/// opening a dialog.
///
/// By default, HTML buttons are presented in a style resembling the platform
/// the user agent runs on, but you can change buttons' appearance with CSS.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/button)
pub fn button(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("button", class, attributes, children)
}

/// The `<button>` HTML element is an interactive element activated by a user
/// with a mouse, keyboard, finger, voice command, or other assistive technology.
/// Once activated, it then performs an action, such as submitting a form or
/// opening a dialog.
///
/// By default, HTML buttons are presented in a style resembling the platform
/// the user agent runs on, but you can change buttons' appearance with CSS.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/button)
pub fn button_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("button", attributes, children)
}

/// Use the HTML `<canvas>` element with either the
/// [canvas scripting API](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API) or the
/// [WebGL API](https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API)
/// to draw graphics and animations.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/canvas)
pub fn canvas(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("canvas", class, attributes, children)
}

/// Use the HTML `<canvas>` element with either the
/// [canvas scripting API](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API) or the
/// [WebGL API](https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API)
/// to draw graphics and animations.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/canvas)
pub fn canvas_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("canvas", attributes, children)
}

/// The `<caption>` HTML element specifies the caption (or title) of a table, p
/// roviding the table an [accessible description](https://developer.mozilla.org/en-US/docs/Glossary/Accessible_description).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/caption)
pub fn caption(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("caption", class, attributes, children)
}

/// The `<caption>` HTML element specifies the caption (or title) of a table, p
/// roviding the table an [accessible description](https://developer.mozilla.org/en-US/docs/Glossary/Accessible_description).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/caption)
pub fn caption_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("caption", attributes, children)
}

/// The `<cite>` HTML element is used to mark up the title of a cited creative
/// work. The reference may be in an abbreviated form according to
/// context-appropriate conventions related to citation metadata.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/cite)
pub fn cite(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("cite", class, attributes, children)
}

/// The `<cite>` HTML element is used to mark up the title of a cited creative
/// work. The reference may be in an abbreviated form according to
/// context-appropriate conventions related to citation metadata.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/cite)
pub fn cite_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("cite", attributes, children)
}

/// The `<code>` HTML element displays its contents styled in a fashion intended
/// to indicate that the text is a short fragment of computer code. By default,
/// the content text is displayed using the user agent's default monospace font.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/code)
pub fn code(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("code", class, attributes, children)
}

/// The `<code>` HTML element displays its contents styled in a fashion intended
/// to indicate that the text is a short fragment of computer code. By default,
/// the content text is displayed using the user agent's default monospace font.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/code)
pub fn code_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("code", attributes, children)
}

/// The `<col>` HTML element defines one or more columns in a column group
/// represented by its parent `<colgroup>` element. The `<col>` element is
/// only valid as a child of a `<colgroup>` element that has no `span`
/// attribute defined.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/col)
pub fn col(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("col", class, attributes, [])
}

/// The `<col>` HTML element defines one or more columns in a column group
/// represented by its parent `<colgroup>` element. The `<col>` element is
/// only valid as a child of a `<colgroup>` element that has no `span`
/// attribute defined.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/col)
pub fn col_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("col", attributes, [])
}

/// The `<colgroup>` HTML element defines a group of columns within a table.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/colgroup)
pub fn colgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("colgroup", class, attributes, children)
}

/// The `<colgroup>` HTML element defines a group of columns within a table.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/colgroup)
pub fn colgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("colgroup", attributes, children)
}

/// The `<data>` HTML element links a given piece of content with a
/// machine-readable translation. If the content is time- or date-related,
/// the `<time>` element must be used.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/data)
pub fn data(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("data", class, attributes, children)
}

/// The `<data>` HTML element links a given piece of content with a
/// machine-readable translation. If the content is time- or date-related,
/// the `<time>` element must be used.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/data)
pub fn data_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("data", attributes, children)
}

/// The `<datalist>` HTML element contains a set of `<option>` elements that
/// represent the permissible or recommended options available to choose from
/// within other controls.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/datalist)
pub fn datalist(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("datalist", class, attributes, children)
}

/// The `<datalist>` HTML element contains a set of `<option>` elements that
/// represent the permissible or recommended options available to choose from
/// within other controls.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/datalist)
pub fn datalist_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("datalist", attributes, children)
}

/// The `<dd>` HTML element provides the description, definition, or value for
/// the preceding term (`<dt>`) in a description list (`<dl>`).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dd)
pub fn dd(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dd", class, attributes, children)
}

/// The `<dd>` HTML element provides the description, definition, or value for
/// the preceding term (`<dt>`) in a description list (`<dl>`).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dd)
pub fn dd_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dd", attributes, children)
}

/// The `<del>` HTML element represents a range of text that has been deleted
/// from a document. This can be used when rendering "track changes" or source
/// code diff information, for example. The `<ins>` element can be used for the
/// opposite purpose: to indicate text that has been added to the document.
///
/// This element is often (but need not be) rendered by applying a
/// strike-through style to the text.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/del)
pub fn del(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("del", class, attributes, children)
}

/// The `<del>` HTML element represents a range of text that has been deleted
/// from a document. This can be used when rendering "track changes" or source
/// code diff information, for example. The `<ins>` element can be used for the
/// opposite purpose: to indicate text that has been added to the document.
///
/// This element is often (but need not be) rendered by applying a
/// strike-through style to the text.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/del)
pub fn del_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("del", attributes, children)
}

/// The `<details>` HTML element creates a disclosure widget in which
/// information is visible only when the widget is toggled into an "open" state.
/// A summary or label must be provided using the `<summary>` element.
///
/// A disclosure widget is typically presented onscreen using a small triangle
/// that rotates (or twists) to indicate open/closed status, with a label next
/// to the triangle. The contents of the `<summary>` element are used as the
/// label for the disclosure widget. The contents of the `<details>` provide
/// the accessible description for the `<summary>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/details)
pub fn details(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("details", class, attributes, children)
}

/// The `<details>` HTML element creates a disclosure widget in which
/// information is visible only when the widget is toggled into an "open" state.
/// A summary or label must be provided using the `<summary>` element.
///
/// A disclosure widget is typically presented onscreen using a small triangle
/// that rotates (or twists) to indicate open/closed status, with a label next
/// to the triangle. The contents of the `<summary>` element are used as the
/// label for the disclosure widget. The contents of the `<details>` provide
/// the accessible description for the `<summary>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/details)
pub fn details_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("details", attributes, children)
}

/// The `<dfn>` HTML element indicates a term to be defined. The `<dfn>` element
/// should be used in a complete definition statement, where the full definition
/// of the term can be one of the following:
/// - The ancestor paragraph (a block of text, sometimes marked by a `<p>`
///   element)
/// - The `<dt>`/`<dd>` pairing
/// - The nearest section ancestor of the `<dfn>` element
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dfn)
pub fn dfn(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dfn", class, attributes, children)
}

/// The `<dfn>` HTML element indicates a term to be defined. The `<dfn>` element
/// should be used in a complete definition statement, where the full definition
/// of the term can be one of the following:
/// - The ancestor paragraph (a block of text, sometimes marked by a `<p>`
///   element)
/// - The `<dt>`/`<dd>` pairing
/// - The nearest section ancestor of the `<dfn>` element
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dfn)
pub fn dfn_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dfn", attributes, children)
}

/// The `<dialog>` HTML element represents a modal or non-modal dialog box or
/// other interactive component, such as a dismissible alert, inspector, or
/// subwindow.
///
/// The HTML `<dialog>` element is used to create both modal and non-modal
/// dialog boxes. Modal dialog boxes interrupt interaction with the rest of the
/// page being inert, while non-modal dialog boxes allow interaction with the
/// rest of the page.
///
/// JavaScript should be used to display the `<dialog>` element. Use the
/// `.showModal()` method to display a modal dialog and the `.show()` method
/// to display a non-modal dialog. The dialog box can be closed using the
/// `.close()` method or using the `dialog` method when submitting a `<form>`
/// that is nested within the `<dialog>` element. Modal dialogs can also be
/// closed by pressing the Esc key.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dialog)
pub fn dialog(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dialog", class, attributes, children)
}

/// The `<dialog>` HTML element represents a modal or non-modal dialog box or
/// other interactive component, such as a dismissible alert, inspector, or
/// subwindow.
///
/// The HTML `<dialog>` element is used to create both modal and non-modal
/// dialog boxes. Modal dialog boxes interrupt interaction with the rest of the
/// page being inert, while non-modal dialog boxes allow interaction with the
/// rest of the page.
///
/// JavaScript should be used to display the `<dialog>` element. Use the
/// `.showModal()` method to display a modal dialog and the `.show()` method
/// to display a non-modal dialog. The dialog box can be closed using the
/// `.close()` method or using the `dialog` method when submitting a `<form>`
/// that is nested within the `<dialog>` element. Modal dialogs can also be
/// closed by pressing the Esc key.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dialog)
pub fn dialog_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dialog", attributes, children)
}

/// The `<div>` HTML element is the generic container for flow content. It has
/// no effect on the content or layout until styled in some way using CSS (e.g.
/// styling is directly applied to it, or some kind of layout model like Flexbox
/// is applied to its parent element).
///
/// As a "pure" container, the `<div>` element does not inherently represent
/// anything. Instead, it's used to group content so it can be easily styled
/// using the `class` or `id` attributes, marking a section of a document as
/// being written in a different language (using the `lang` attribute),
/// and so on.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/div)
pub fn div(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("div", class, attributes, children)
}

/// The `<div>` HTML element is the generic container for flow content. It has
/// no effect on the content or layout until styled in some way using CSS (e.g.
/// styling is directly applied to it, or some kind of layout model like Flexbox
/// is applied to its parent element).
///
/// As a "pure" container, the `<div>` element does not inherently represent
/// anything. Instead, it's used to group content so it can be easily styled
/// using the `class` or `id` attributes, marking a section of a document as
/// being written in a different language (using the `lang` attribute),
/// and so on.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/div)
pub fn div_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("div", attributes, children)
}

/// The `<dl>` HTML element represents a description list. The element encloses
/// a list of groups of terms (specified using the `<dt>` element) and
/// descriptions (provided by `<dd>` elements). Common uses for this element are
/// to implement a glossary or to display metadata (a list of key-value pairs).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dl)
pub fn dl(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dl", class, attributes, children)
}

/// The `<dl>` HTML element represents a description list. The element encloses
/// a list of groups of terms (specified using the `<dt>` element) and
/// descriptions (provided by `<dd>` elements). Common uses for this element are
/// to implement a glossary or to display metadata (a list of key-value pairs).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dl)
pub fn dl_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dl", attributes, children)
}

/// The `<dt>` HTML element specifies a term in a description or definition list,
/// and as such must be used inside a `<dl>` element. It is usually followed by
/// a `<dd>` element; however, multiple `<dt>` elements in a row indicate several
/// terms that are all defined by the immediate next `<dd>` element.
///
/// The subsequent `<dd>` (Description Details) element provides the definition
/// or other related text associated with the term specified using `<dt>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dt)
pub fn dt(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("dt", class, attributes, children)
}

/// The `<dt>` HTML element specifies a term in a description or definition list,
/// and as such must be used inside a `<dl>` element. It is usually followed by
/// a `<dd>` element; however, multiple `<dt>` elements in a row indicate several
/// terms that are all defined by the immediate next `<dd>` element.
///
/// The subsequent `<dd>` (Description Details) element provides the definition
/// or other related text associated with the term specified using `<dt>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/dt)
pub fn dt_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("dt", attributes, children)
}

/// The `<em>` HTML element marks text that has stress emphasis. The `<em>`
/// element can be nested, with each level of nesting indicating a greater
/// degree of emphasis.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/em)
pub fn em(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("em", class, attributes, children)
}

/// The `<em>` HTML element marks text that has stress emphasis. The `<em>`
/// element can be nested, with each level of nesting indicating a greater
/// degree of emphasis.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/em)
pub fn em_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("em", attributes, children)
}

/// The `<embed>` HTML element embeds external content at the specified point in
/// the document. This content is provided by an external application or other
/// source of interactive content such as a browser plug-in.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/embed)
pub fn embed(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("embed", class, attributes, [])
}

/// The `<embed>` HTML element embeds external content at the specified point in
/// the document. This content is provided by an external application or other
/// source of interactive content such as a browser plug-in.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/embed)
pub fn embed_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("embed", attributes, [])
}

/// The `<fieldset>` HTML element is used to group several controls as well as
/// labels (`<label>`) within a web form.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/fieldset)
pub fn fieldset(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("fieldset", class, attributes, children)
}

/// The `<fieldset>` HTML element is used to group several controls as well as
/// labels (`<label>`) within a web form.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/fieldset)
pub fn fieldset_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("fieldset", attributes, children)
}

/// The `<figcaption>` HTML element represents a caption or legend describing
/// the rest of the contents of its parent `<figure>` element, providing the
/// `<figure>` an [accessible description](https://developer.mozilla.org/en-US/docs/Glossary/Accessible_description).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figcaption)
pub fn figcaption(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("figcaption", class, attributes, children)
}

/// The `<figcaption>` HTML element represents a caption or legend describing
/// the rest of the contents of its parent `<figure>` element, providing the
/// `<figure>` an [accessible description](https://developer.mozilla.org/en-US/docs/Glossary/Accessible_description).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figcaption)
pub fn figcaption_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("figcaption", attributes, children)
}

/// The `<figure>` HTML element represents self-contained content, potentially
/// with an optional caption, which is specified using the `<figcaption>`
/// element. The figure, its caption, and its contents are referenced as a
/// single unit.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figure)
pub fn figure(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("figure", class, attributes, children)
}

/// The `<figure>` HTML element represents self-contained content, potentially
/// with an optional caption, which is specified using the `<figcaption>`
/// element. The figure, its caption, and its contents are referenced as a
/// single unit.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/figure)
pub fn figure_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("figure", attributes, children)
}

/// The `<footer>` HTML element represents a footer for its nearest ancestor
/// [sectioning content](https://developer.mozilla.org/en-US/docs/Web/HTML/Content_categories#sectioning_content)
/// or [sectioning root](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements#labeling_section_content)
/// element. A `<footer>` typically contains information about the author of
/// the section, copyright data or links to related documents.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/footer)
pub fn footer(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("footer", class, attributes, children)
}

/// The `<footer>` HTML element represents a footer for its nearest ancestor
/// [sectioning content](https://developer.mozilla.org/en-US/docs/Web/HTML/Content_categories#sectioning_content)
/// or [sectioning root](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements#labeling_section_content)
/// element. A `<footer>` typically contains information about the author of
/// the section, copyright data or links to related documents.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/footer)
pub fn footer_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("footer", attributes, children)
}

/// The `<form>` HTML element represents a document section containing
/// interactive controls for submitting information.
///
/// It is possible to use the `:valid` and `:invalid` CSS pseudo-classes to
/// style a `<form>` element based on whether the elements inside the form
/// are valid.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/form)
pub fn form(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("form", class, attributes, children)
}

/// The `<form>` HTML element represents a document section containing
/// interactive controls for submitting information.
///
/// It is possible to use the `:valid` and `:invalid` CSS pseudo-classes to
/// style a `<form>` element based on whether the elements inside the form
/// are valid.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/form)
pub fn form_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("form", attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h1)
pub fn h1(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h1", class, attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h1)
pub fn h1_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h1", attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h2)
pub fn h2(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h2", class, attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h2)
pub fn h2_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h2", attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h3)
pub fn h3(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h3", class, attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h3)
pub fn h3_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h3", attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h4)
pub fn h4(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h4", class, attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h4)
pub fn h4_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h4", attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h5)
pub fn h5(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h5", class, attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h5)
pub fn h5_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h5", attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h6)
pub fn h6(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("h6", class, attributes, children)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/h6)
pub fn h6_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("h6", attributes, children)
}

/// The `<header>` HTML element represents introductory content, typically a
/// group of introductory or navigational aids. It may contain some heading
/// elements but also a logo, a search form, an author name, and other elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/header)
pub fn header(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("header", class, attributes, children)
}

/// The `<header>` HTML element represents introductory content, typically a
/// group of introductory or navigational aids. It may contain some heading
/// elements but also a logo, a search form, an author name, and other elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/header)
pub fn header_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("header", attributes, children)
}

/// The `<hgroup>` HTML element represents a heading and related content. It
/// groups a single `<h1>`–`<h6>` element with one or more `<p>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hgroup)
pub fn hgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("hgroup", class, attributes, children)
}

/// The `<hgroup>` HTML element represents a heading and related content. It
/// groups a single `<h1>`–`<h6>` element with one or more `<p>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hgroup)
pub fn hgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("hgroup", attributes, children)
}

/// The `<hr>` HTML element represents a thematic break between paragraph-level
/// elements: for example, a change of scene in a story, or a shift of topic
/// within a section.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hr)
pub fn hr(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("hr", class, attributes, [])
}

/// The `<hr>` HTML element represents a thematic break between paragraph-level
/// elements: for example, a change of scene in a story, or a shift of topic
/// within a section.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/hr)
pub fn hr_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("hr", attributes, [])
}

/// The `<i>` HTML element represents a range of text that is set off from the
/// normal text for some reason, such as idiomatic text, technical terms,
/// taxonomical designations, among others. Historically, these have been
/// presented using italicized type, which is the original source of the `<i>`
/// naming of this element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/i)
pub fn i(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("i", class, attributes, children)
}

/// The `<i>` HTML element represents a range of text that is set off from the
/// normal text for some reason, such as idiomatic text, technical terms,
/// taxonomical designations, among others. Historically, these have been
/// presented using italicized type, which is the original source of the `<i>`
/// naming of this element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/i)
pub fn i_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("i", attributes, children)
}

/// The `<iframe>` HTML element represents a nested browsing context, embedding
/// another HTML page into the current one.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/iframe)
pub fn iframe(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("iframe", class, attributes, children)
}

/// The `<iframe>` HTML element represents a nested browsing context, embedding
/// another HTML page into the current one.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/iframe)
pub fn iframe_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("iframe", attributes, children)
}

/// The `<img>` HTML element embeds an image into the document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/img)
pub fn img(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("img", class, attributes, [])
}

/// The `<img>` HTML element embeds an image into the document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/img)
pub fn img_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("img", attributes, [])
}

/// The `<input>` HTML element is used to create interactive controls for
/// web-based forms in order to accept data from the user; a wide variety of
/// types of input data and control widgets are available, depending on the
/// device and user agent. The `<input>` element is one of the most powerful and
/// complex in all of HTML due to the sheer number of combinations of input
/// types and attributes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/input)
pub fn input(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("input", class, attributes, [])
}

/// The `<input>` HTML element is used to create interactive controls for
/// web-based forms in order to accept data from the user; a wide variety of
/// types of input data and control widgets are available, depending on the
/// device and user agent. The `<input>` element is one of the most powerful and
/// complex in all of HTML due to the sheer number of combinations of input
/// types and attributes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/input)
pub fn input_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("input", attributes, [])
}

/// The `<ins>` HTML element represents a range of text that has been added to
/// a document. You can use the `<del>` element to similarly represent a range
/// of text that has been deleted from the document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ins)
pub fn ins(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("ins", class, attributes, children)
}

/// The `<ins>` HTML element represents a range of text that has been added to
/// a document. You can use the `<del>` element to similarly represent a range
/// of text that has been deleted from the document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ins)
pub fn ins_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("ins", attributes, children)
}

/// The `<kbd>` HTML element represents a span of inline text denoting textual
/// user input from a keyboard, voice input, or any other text entry device. By
/// convention, the user agent defaults to rendering the contents of a `<kbd>`
/// element using its default monospace font, although this is not mandated by
/// the HTML standard.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/kbd)
pub fn kbd(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("kbd", class, attributes, children)
}

/// The `<kbd>` HTML element represents a span of inline text denoting textual
/// user input from a keyboard, voice input, or any other text entry device. By
/// convention, the user agent defaults to rendering the contents of a `<kbd>`
/// element using its default monospace font, although this is not mandated by
/// the HTML standard.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/kbd)
pub fn kbd_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("kbd", attributes, children)
}

/// The `<label>` HTML element represents a caption for an item in a user
/// interface.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/label)
pub fn label(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("label", class, attributes, children)
}

/// The `<label>` HTML element represents a caption for an item in a user
/// interface.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/label)
pub fn label_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("label", attributes, children)
}

/// The `<legend>` HTML element represents a caption for the content of
/// its parent `<fieldset>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/legend)
pub fn legend(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("legend", class, attributes, children)
}

/// The `<legend>` HTML element represents a caption for the content of
/// its parent `<fieldset>`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/legend)
pub fn legend_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("legend", attributes, children)
}

/// The `<li>` HTML element is used to represent an item in a list. It must be
/// contained in a parent element: an ordered list (`<ol>`), an unordered list
/// (`<ul>`), or a menu (`<menu>`). In menus and unordered lists, list items are
/// usually displayed using bullet points. In ordered lists, they are usually
/// displayed with an ascending counter on the left, such as a number or letter.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/li)
pub fn li(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("li", class, attributes, children)
}

/// The `<li>` HTML element is used to represent an item in a list. It must be
/// contained in a parent element: an ordered list (`<ol>`), an unordered list
/// (`<ul>`), or a menu (`<menu>`). In menus and unordered lists, list items are
/// usually displayed using bullet points. In ordered lists, they are usually
/// displayed with an ascending counter on the left, such as a number or letter.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/li)
pub fn li_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("li", attributes, children)
}

/// The `<main>` HTML element represents the dominant content of the `<body>` of
/// a document. The main content area consists of content that is directly
/// related to or expands upon the central topic of a document, or the central
/// functionality of an application.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/main)
pub fn main(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("main", class, attributes, children)
}

/// The `<main>` HTML element represents the dominant content of the `<body>` of
/// a document. The main content area consists of content that is directly
/// related to or expands upon the central topic of a document, or the central
/// functionality of an application.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/main)
pub fn main_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("main", attributes, children)
}

/// The `<map>` HTML element is used with `<area>` elements to define an
/// image map (a clickable link area).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/map)
pub fn map(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("map", class, attributes, children)
}

/// The `<map>` HTML element is used with `<area>` elements to define an
/// image map (a clickable link area).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/map)
pub fn map_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("map", attributes, children)
}

/// The `<mark>` HTML element represents text which is marked or highlighted for
/// reference or notation purposes due to the marked passage's relevance in the
/// enclosing context.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/mark)
pub fn mark(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("mark", class, attributes, children)
}

/// The `<mark>` HTML element represents text which is marked or highlighted for
/// reference or notation purposes due to the marked passage's relevance in the
/// enclosing context.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/mark)
pub fn mark_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("mark", attributes, children)
}

/// The `<math>` MathML element is the top-level MathML element, used to write
/// a single mathematical formula. It can be placed in HTML content where flow
/// content is permitted.
///
/// > See the [Authoring MathML page](https://developer.mozilla.org/en-US/docs/Web/MathML/Authoring#using_mathml)
/// > for tips to properly integrate MathML formulas in your web pages and the
/// > [Examples](https://developer.mozilla.org/en-US/docs/Web/MathML/Examples)
/// > page for more demos.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/MathML/Element/math)
pub fn math(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("math", class, attributes, children)
}

/// The `<math>` MathML element is the top-level MathML element, used to write
/// a single mathematical formula. It can be placed in HTML content where flow
/// content is permitted.
///
/// > See the [Authoring MathML page](https://developer.mozilla.org/en-US/docs/Web/MathML/Authoring#using_mathml)
/// > for tips to properly integrate MathML formulas in your web pages and the
/// > [Examples](https://developer.mozilla.org/en-US/docs/Web/MathML/Examples)
/// > page for more demos.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/MathML/Element/math)
pub fn math_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("math", attributes, children)
}

/// The `<menu>` HTML element is described in the HTML specification as a
/// semantic alternative to `<ul>`, but treated by browsers (and exposed through
/// the accessibility tree) as no different than `<ul>`. It represents an
/// unordered list of items (which are represented by `<li>` elements).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/menu)
pub fn menu(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("menu", class, attributes, children)
}

/// The `<menu>` HTML element is described in the HTML specification as a
/// semantic alternative to `<ul>`, but treated by browsers (and exposed through
/// the accessibility tree) as no different than `<ul>`. It represents an
/// unordered list of items (which are represented by `<li>` elements).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/menu)
pub fn menu_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("menu", attributes, children)
}

/// The `<meter>` HTML element represents either a scalar value within a
/// known range or a fractional value.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/meter)
pub fn meter(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("meter", class, attributes, children)
}

/// The `<meter>` HTML element represents either a scalar value within a
/// known range or a fractional value.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/meter)
pub fn meter_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("meter", attributes, children)
}

/// The `<nav>` HTML element represents a section of a page whose purpose is to
/// provide navigation links, either within the current document or to other
/// documents. Common examples of navigation sections are menus, tables of
/// contents, and indexes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/nav)
pub fn nav(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("nav", class, attributes, children)
}

/// The `<nav>` HTML element represents a section of a page whose purpose is to
/// provide navigation links, either within the current document or to other
/// documents. Common examples of navigation sections are menus, tables of
/// contents, and indexes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/nav)
pub fn nav_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("nav", attributes, children)
}

/// The `<noscript>` HTML element defines a section of HTML to be inserted if a
/// script type on the page is unsupported or if scripting is currently turned
/// off in the browser.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/noscript)
pub fn noscript(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("noscript", class, attributes, children)
}

/// The `<noscript>` HTML element defines a section of HTML to be inserted if a
/// script type on the page is unsupported or if scripting is currently turned
/// off in the browser.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/noscript)
pub fn noscript_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("noscript", attributes, children)
}

/// The `<object>` HTML element represents an external resource, which can be
/// treated as an image, a nested browsing context, or a resource to be handled
/// by a plugin.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/object)
pub fn object(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("object", class, attributes, children)
}

/// The `<object>` HTML element represents an external resource, which can be
/// treated as an image, a nested browsing context, or a resource to be handled
/// by a plugin.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/object)
pub fn object_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("object", attributes, children)
}

/// The `<ol>` HTML element represents an ordered list of items — typically
/// rendered as a numbered list.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ol)
pub fn ol(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("ol", class, attributes, children)
}

/// The `<ol>` HTML element represents an ordered list of items — typically
/// rendered as a numbered list.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ol)
pub fn ol_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("ol", attributes, children)
}

/// The `<optgroup>` HTML element creates a grouping of options within a
/// `<select>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/optgroup)
pub fn optgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("optgroup", class, attributes, children)
}

/// The `<optgroup>` HTML element creates a grouping of options within a
/// `<select>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/optgroup)
pub fn optgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("optgroup", attributes, children)
}

/// The `<option>` HTML element is used to define an item contained in a
/// `<select>`, an `<optgroup>`, or a `<datalist>` element. As such, `<option>`
/// can represent menu items in popups and other lists of items in an HTML
/// document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/option)
pub fn option(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("option", class, attributes, children)
}

/// The `<option>` HTML element is used to define an item contained in a
/// `<select>`, an `<optgroup>`, or a `<datalist>` element. As such, `<option>`
/// can represent menu items in popups and other lists of items in an HTML
/// document.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/option)
pub fn option_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("option", attributes, children)
}

/// The `<output>` HTML element is a container element into which a site or
/// app can inject the results of a calculation or the outcome of a user action.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/output)
pub fn output(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("output", class, attributes, children)
}

/// The `<output>` HTML element is a container element into which a site or
/// app can inject the results of a calculation or the outcome of a user action.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/output)
pub fn output_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("output", attributes, children)
}

/// The `<p>` HTML element represents a paragraph. Paragraphs are usually
/// represented in visual media as blocks of text separated from adjacent
/// blocks by blank lines and/or first-line indentation, but HTML paragraphs
/// can be any structural grouping of related content, such as images or form
/// fields.
///
/// Paragraphs are block-level elements, and notably will automatically close
/// if another block-level element is parsed before the closing `</p>` tag.
/// See "Tag omission" below.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/p)
pub fn p(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("p", class, attributes, children)
}

/// The `<p>` HTML element represents a paragraph. Paragraphs are usually
/// represented in visual media as blocks of text separated from adjacent
/// blocks by blank lines and/or first-line indentation, but HTML paragraphs
/// can be any structural grouping of related content, such as images or form
/// fields.
///
/// Paragraphs are block-level elements, and notably will automatically close
/// if another block-level element is parsed before the closing `</p>` tag.
/// See "Tag omission" below.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/p)
pub fn p_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("p", attributes, children)
}

/// The `<picture>` HTML element contains zero or more `<source>` elements and
/// one `<img>` element to offer alternative versions of an image for different
/// display/device scenarios.
///
/// The browser will consider each child `<source>` element and choose the best
/// match among them. If no matches are found—or the browser doesn't support
/// the `<picture>` element—the URL of the `<img>` element's `src` attribute is
/// selected. The selected image is then presented in the space occupied by
/// the `<img>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/picture)
pub fn picture(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("picture", class, attributes, children)
}

/// The `<picture>` HTML element contains zero or more `<source>` elements and
/// one `<img>` element to offer alternative versions of an image for different
/// display/device scenarios.
///
/// The browser will consider each child `<source>` element and choose the best
/// match among them. If no matches are found—or the browser doesn't support
/// the `<picture>` element—the URL of the `<img>` element's `src` attribute is
/// selected. The selected image is then presented in the space occupied by
/// the `<img>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/picture)
pub fn picture_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("picture", attributes, children)
}

/// The `<portal>` HTML element enables the embedding of another HTML page into
/// the current one for the purposes of allowing smoother navigation into new pages.
///
/// A `<portal>` is similar to an `<iframe>`. An `<iframe>` allows a separate
/// browsing context to be embedded. However, the embedded content of a
/// `<portal>` is more limited than that of an `<iframe>`. It cannot be
/// interacted with, and therefore is not suitable for embedding widgets into a
/// document. Instead, the <portal> acts as a preview of the content of another
/// page. It can be navigated into therefore allowing for seamless transition
/// to the embedded content.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/portal)
pub fn portal(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("portal", class, attributes, children)
}

/// The `<portal>` HTML element enables the embedding of another HTML page into
/// the current one for the purposes of allowing smoother navigation into new pages.
///
/// A `<portal>` is similar to an `<iframe>`. An `<iframe>` allows a separate
/// browsing context to be embedded. However, the embedded content of a
/// `<portal>` is more limited than that of an `<iframe>`. It cannot be
/// interacted with, and therefore is not suitable for embedding widgets into a
/// document. Instead, the <portal> acts as a preview of the content of another
/// page. It can be navigated into therefore allowing for seamless transition
/// to the embedded content.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/portal)
pub fn portal_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("portal", attributes, children)
}

/// The `<pre>` HTML element represents preformatted text which is to be
/// presented exactly as written in the HTML file. The text is typically
/// rendered using a non-proportional, or monospaced, font. Whitespace inside
/// this element is displayed as written.
///
/// By default, `<pre>` is a block-level element, i.e. its default `display`
/// value is `block`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/pre)
pub fn pre(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("pre", class, attributes, children)
}

/// The `<pre>` HTML element represents preformatted text which is to be
/// presented exactly as written in the HTML file. The text is typically
/// rendered using a non-proportional, or monospaced, font. Whitespace inside
/// this element is displayed as written.
///
/// By default, `<pre>` is a block-level element, i.e. its default `display`
/// value is `block`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/pre)
pub fn pre_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("pre", attributes, children)
}

/// The `<progress>` HTML element displays an indicator showing the completion
/// progress of a task, typically displayed as a progress bar.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/progress)
pub fn progress(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("progress", class, attributes, children)
}

/// The `<progress>` HTML element displays an indicator showing the completion
/// progress of a task, typically displayed as a progress bar.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/progress)
pub fn progress_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("progress", attributes, children)
}

/// The `<q>` HTML element indicates that the enclosed text is a short inline
/// quotation. Most modern browsers implement this by surrounding the text in
/// quotation marks. This element is intended for short quotations that don't
/// require paragraph breaks; for long quotations use the `<blockquote>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/q)
pub fn q(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("q", class, attributes, children)
}

/// The `<q>` HTML element indicates that the enclosed text is a short inline
/// quotation. Most modern browsers implement this by surrounding the text in
/// quotation marks. This element is intended for short quotations that don't
/// require paragraph breaks; for long quotations use the `<blockquote>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/q)
pub fn q_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("q", attributes, children)
}

/// The `<rp>` HTML element is used to provide fall-back parentheses for
/// browsers that do not support display of ruby annotations using the `<ruby>`
/// element. One `<rp>` element should enclose each of the opening and closing
/// parentheses that wrap the `<rt>` element that contains the annotation's text.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rp)
pub fn rp(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("rp", class, attributes, children)
}

/// The `<rp>` HTML element is used to provide fall-back parentheses for
/// browsers that do not support display of ruby annotations using the `<ruby>`
/// element. One `<rp>` element should enclose each of the opening and closing
/// parentheses that wrap the `<rt>` element that contains the annotation's text.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rp)
pub fn rp_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("rp", attributes, children)
}

/// The `<rt>` HTML element specifies the ruby text component of a ruby
/// annotation, which is used to provide pronunciation, translation, or
/// transliteration information for East Asian typography. The `<rt>` element
/// must always be contained within a `<ruby>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rt)
pub fn rt(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("rt", class, attributes, children)
}

/// The `<rt>` HTML element specifies the ruby text component of a ruby
/// annotation, which is used to provide pronunciation, translation, or
/// transliteration information for East Asian typography. The `<rt>` element
/// must always be contained within a `<ruby>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/rt)
pub fn rt_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("rt", attributes, children)
}

/// The `<ruby>` HTML element represents small annotations that are rendered
/// above, below, or next to base text, usually used for showing the
/// pronunciation of East Asian characters. It can also be used for annotating
/// other kinds of text, but this usage is less common.
///
/// The term ruby originated as a
/// [unit of measurement used by typesetters](https://en.wikipedia.org/wiki/Agate_(typography)),
/// representing the smallest size that text can be printed on newsprint while
/// remaining legible.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ruby)
pub fn ruby(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("ruby", class, attributes, children)
}

/// The `<ruby>` HTML element represents small annotations that are rendered
/// above, below, or next to base text, usually used for showing the
/// pronunciation of East Asian characters. It can also be used for annotating
/// other kinds of text, but this usage is less common.
///
/// The term ruby originated as a
/// [unit of measurement used by typesetters](https://en.wikipedia.org/wiki/Agate_(typography)),
/// representing the smallest size that text can be printed on newsprint while
/// remaining legible.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ruby)
pub fn ruby_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("ruby", attributes, children)
}

/// The `<s>` HTML element renders text with a strikethrough, or a line through
/// it. Use the `<s>` element to represent things that are no longer relevant or
/// no longer accurate. However, `<s>` is not appropriate when indicating
/// document edits; for that, use the `<del>` and `<ins>` elements, as
/// appropriate.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/s)
pub fn s(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("s", class, attributes, children)
}

/// The `<s>` HTML element renders text with a strikethrough, or a line through
/// it. Use the `<s>` element to represent things that are no longer relevant or
/// no longer accurate. However, `<s>` is not appropriate when indicating
/// document edits; for that, use the `<del>` and `<ins>` elements, as
/// appropriate.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/s)
pub fn s_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("s", attributes, children)
}

/// The `<samp>` HTML element is used to enclose inline text which represents
/// sample (or quoted) output from a computer program. Its contents are
/// typically rendered using the browser's default monospaced font (such as
/// Courier or Lucida Console).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/samp)
pub fn samp(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("samp", class, attributes, children)
}

/// The `<samp>` HTML element is used to enclose inline text which represents
/// sample (or quoted) output from a computer program. Its contents are
/// typically rendered using the browser's default monospaced font (such as
/// Courier or Lucida Console).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/samp)
pub fn samp_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("samp", attributes, children)
}

/// The `<script>` HTML element is used to embed executable code or data; this
/// is typically used to embed or refer to JavaScript code. The `<script>`
/// element can also be used with other languages, such as WebGL's GLSL shader
/// programming language and JSON.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/script)
pub fn script(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("script", class, attributes, children)
}

/// The `<script>` HTML element is used to embed executable code or data; this
/// is typically used to embed or refer to JavaScript code. The `<script>`
/// element can also be used with other languages, such as WebGL's GLSL shader
/// programming language and JSON.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/script)
pub fn script_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("script", attributes, children)
}

/// The `<search>` HTML element is a container representing the parts of the
/// document or application with form controls or other content related to
/// performing a search or filtering operation. The <search> element
/// semantically identifies the purpose of the element's contents as having
/// search or filtering capabilities. The search or filtering functionality can
/// be for the website or application, the current web page or document, or the
/// entire Internet or subsection thereof.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/search)
pub fn search(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("search", class, attributes, children)
}

/// The `<search>` HTML element is a container representing the parts of the
/// document or application with form controls or other content related to
/// performing a search or filtering operation. The <search> element
/// semantically identifies the purpose of the element's contents as having
/// search or filtering capabilities. The search or filtering functionality can
/// be for the website or application, the current web page or document, or the
/// entire Internet or subsection thereof.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/search)
pub fn search_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("search", attributes, children)
}

/// The `<section>` HTML element represents a generic standalone section of a
/// document, which doesn't have a more specific semantic element to represent
/// it. Sections should always have a heading, with very few exceptions.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/section)
pub fn section(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("section", class, attributes, children)
}

/// The `<section>` HTML element represents a generic standalone section of a
/// document, which doesn't have a more specific semantic element to represent
/// it. Sections should always have a heading, with very few exceptions.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/section)
pub fn section_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("section", attributes, children)
}

/// The `<select>` HTML element represents a control that provides a menu of options.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/select)
pub fn select(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("select", class, attributes, children)
}

/// The `<select>` HTML element represents a control that provides a menu of options.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/select)
pub fn select_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("select", attributes, children)
}

/// The `<slot>` HTML element—part of the Web Components technology suite—is a
/// placeholder inside a web component that you can fill with your own markup,
/// which lets you create separate DOM trees and present them together.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/slot)
pub fn slot(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("slot", class, attributes, children)
}

/// The `<slot>` HTML element—part of the Web Components technology suite—is a
/// placeholder inside a web component that you can fill with your own markup,
/// which lets you create separate DOM trees and present them together.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/slot)
pub fn slot_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("slot", attributes, children)
}

/// The `<small>` HTML element represents side-comments and small print, like
/// `copyright and legal text, independent of its styled presentation. By
/// default, it renders text within it one font-size smaller, such as from
/// `small` to `x-small`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/small)
pub fn small(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("small", class, attributes, children)
}

/// The `<small>` HTML element represents side-comments and small print, like
/// `copyright and legal text, independent of its styled presentation. By
/// default, it renders text within it one font-size smaller, such as from
/// `small` to `x-small`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/small)
pub fn small_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("small", attributes, children)
}

/// The `<source>` HTML element specifies one or more media resources for the
/// `<picture>,` `<audio>,` and `<video>` elements. It is a
/// [void element](https://developer.mozilla.org/en-US/docs/Glossary/Void_element),
/// which means that it has no content and does not require a closing tag. This
/// element is commonly used to offer the same media content in multiple file
/// formats in order to provide compatibility with a broad range of browsers
/// given their differing support for image file formats and media file formats.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/source)
pub fn source(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("source", class, attributes, [])
}

/// The `<source>` HTML element specifies one or more media resources for the
/// `<picture>,` `<audio>,` and `<video>` elements. It is a
/// [void element](https://developer.mozilla.org/en-US/docs/Glossary/Void_element),
/// which means that it has no content and does not require a closing tag. This
/// element is commonly used to offer the same media content in multiple file
/// formats in order to provide compatibility with a broad range of browsers
/// given their differing support for image file formats and media file formats.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/source)
pub fn source_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("source", attributes, [])
}

/// The `<span>` HTML element is a generic inline container for phrasing content,
/// which does not inherently represent anything. It can be used to group
/// elements for styling purposes (using the class or id attributes), or because
/// they share attribute values, such as lang. It should be used only when no
/// other semantic element is appropriate. `<span>` is very much like a `<div>`
/// element, but `<div>` is a block-level element whereas a `<span>` is an
/// inline-level element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/span)
pub fn span(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("span", class, attributes, children)
}

/// The `<span>` HTML element is a generic inline container for phrasing content,
/// which does not inherently represent anything. It can be used to group
/// elements for styling purposes (using the class or id attributes), or because
/// they share attribute values, such as lang. It should be used only when no
/// other semantic element is appropriate. `<span>` is very much like a `<div>`
/// element, but `<div>` is a block-level element whereas a `<span>` is an
/// inline-level element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/span)
pub fn span_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("span", attributes, children)
}

/// The `<strong>` HTML element indicates that its contents have strong
/// importance, seriousness, or urgency. Browsers typically render the
/// contents in bold type.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/strong)
pub fn strong(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("strong", class, attributes, children)
}

/// The `<strong>` HTML element indicates that its contents have strong
/// importance, seriousness, or urgency. Browsers typically render the
/// contents in bold type.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/strong)
pub fn strong_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("strong", attributes, children)
}

/// The `<sub>` HTML element specifies inline text which should be displayed as
/// subscript for solely typographical reasons. Subscripts are typically
/// rendered with a lowered baseline using smaller text.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sub)
pub fn sub(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("sub", class, attributes, children)
}

/// The `<sub>` HTML element specifies inline text which should be displayed as
/// subscript for solely typographical reasons. Subscripts are typically
/// rendered with a lowered baseline using smaller text.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sub)
pub fn sub_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("sub", attributes, children)
}

/// The `<summary>` HTML element specifies a summary, caption, or legend for a
/// `<details>` element's disclosure box. Clicking the `<summary>` element
/// toggles the state of the parent `<details>` element open and closed.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/summary)
pub fn summary(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("summary", class, attributes, children)
}

/// The `<summary>` HTML element specifies a summary, caption, or legend for a
/// `<details>` element's disclosure box. Clicking the `<summary>` element
/// toggles the state of the parent `<details>` element open and closed.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/summary)
pub fn summary_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("summary", attributes, children)
}

/// The `<sup>` HTML element specifies inline text which is to be displayed as
/// superscript for solely typographical reasons. Superscripts are usually
/// rendered with a raised baseline using smaller text.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sup)
pub fn sup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("sup", class, attributes, children)
}

/// The `<sup>` HTML element specifies inline text which is to be displayed as
/// superscript for solely typographical reasons. Superscripts are usually
/// rendered with a raised baseline using smaller text.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/sup)
pub fn sup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("sup", attributes, children)
}

/// The `<svg>` SVG element is a container that defines a new coordinate system
/// and viewport. It is used as the outermost element of SVG documents, but it
/// can also be used to embed an SVG fragment inside an SVG or HTML document.
///
/// > The `xmlns` attribute is only required on the outermost `svg` element of
/// > SVG documents, or inside HTML documents with XML serialization. It is
/// > unnecessary for inner `svg` elements or inside HTML documents with
/// > HTML serialization.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/svg)
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

/// The `<svg>` SVG element is a container that defines a new coordinate system
/// and viewport. It is used as the outermost element of SVG documents, but it
/// can also be used to embed an SVG fragment inside an SVG or HTML document.
///
/// > The `xmlns` attribute is only required on the outermost `svg` element of
/// > SVG documents, or inside HTML documents with XML serialization. It is
/// > unnecessary for inner `svg` elements or inside HTML documents with
/// > HTML serialization.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/svg)
pub fn svg_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.namespaced_("http://www.w3.org/2000/svg", "svg", attributes, children)
}

/// The `<table>` HTML element represents tabular data—that is, information
/// presented in a two-dimensional table comprised of rows and columns of
/// cells containing data.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/table)
pub fn table(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("table", class, attributes, children)
}

/// The `<table>` HTML element represents tabular data—that is, information
/// presented in a two-dimensional table comprised of rows and columns of
/// cells containing data.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/table)
pub fn table_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("table", attributes, children)
}

/// The `<tbody>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the body of a table's (main) data.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tbody)
pub fn tbody(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("tbody", class, attributes, children)
}

/// The `<tbody>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the body of a table's (main) data.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tbody)
pub fn tbody_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("tbody", attributes, children)
}

/// The `<td>` HTML element defines a cell of a table that contains data and
/// may be used as a child of the `<tr>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/td)
pub fn td(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("td", class, attributes, children)
}

/// The `<td>` HTML element defines a cell of a table that contains data and
/// may be used as a child of the `<tr>` element.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/td)
pub fn td_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("td", attributes, children)
}

/// The `<template>` HTML element serves as a mechanism for holding HTML
/// fragments, which can either be used later via JavaScript or generated
/// immediately into shadow DOM.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/template)
pub fn template(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("template", class, attributes, children)
}

/// The `<template>` HTML element serves as a mechanism for holding HTML
/// fragments, which can either be used later via JavaScript or generated
/// immediately into shadow DOM.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/template)
pub fn template_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("template", attributes, children)
}

/// The `<textarea>` HTML element represents a multi-line plain-text editing
/// control, useful when you want to allow users to enter a sizeable amount of
/// free-form text, for example a comment on a review or feedback form.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/textarea)
pub fn textarea(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("textarea", class, attributes, children)
}

/// The `<textarea>` HTML element represents a multi-line plain-text editing
/// control, useful when you want to allow users to enter a sizeable amount of
/// free-form text, for example a comment on a review or feedback form.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/textarea)
pub fn textarea_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("textarea", attributes, children)
}

/// The `<tfoot>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the foot of a table with
/// information about the table's columns. This is usually a summary of the
/// columns, e.g., a sum of the given numbers in a column.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tfoot)
pub fn tfoot(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("tfoot", class, attributes, children)
}

/// The `<tfoot>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the foot of a table with
/// information about the table's columns. This is usually a summary of the
/// columns, e.g., a sum of the given numbers in a column.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tfoot)
pub fn tfoot_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("tfoot", attributes, children)
}

/// The `<th>` HTML element defines a cell as the header of a group of table
/// cells and may be used as a child of the `<tr>` element. The exact nature
/// of this group is defined by the `scope` and `headers` attributes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/th)
pub fn th(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("th", class, attributes, children)
}

/// The `<th>` HTML element defines a cell as the header of a group of table
/// cells and may be used as a child of the `<tr>` element. The exact nature
/// of this group is defined by the `scope` and `headers` attributes.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/th)
pub fn th_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("th", attributes, children)
}

/// The `<thead>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the head of a table with
/// information about the table's columns. This is usually in the form of
/// column headers (`<th>` elements).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/thead)
pub fn thead(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("thead", class, attributes, children)
}

/// The `<thead>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the head of a table with
/// information about the table's columns. This is usually in the form of
/// column headers (`<th>` elements).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/thead)
pub fn thead_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("thead", attributes, children)
}

/// The `<time>` HTML element represents a specific period in time. It may
/// include the datetime attribute to translate dates into machine-readable
/// format, allowing for better search engine results or custom features such
/// as reminders.
///
/// It may represent one of the following:
/// - A time on a 24-hour clock.
/// - A precise date in the [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar)
///   (with optional time and timezone information).
/// - [A valid time duration](https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#valid-duration-string).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/time)
pub fn time(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("time", class, attributes, children)
}

/// The `<time>` HTML element represents a specific period in time. It may
/// include the datetime attribute to translate dates into machine-readable
/// format, allowing for better search engine results or custom features such
/// as reminders.
///
/// It may represent one of the following:
/// - A time on a 24-hour clock.
/// - A precise date in the [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar)
///   (with optional time and timezone information).
/// - [A valid time duration](https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#valid-duration-string).
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/time)
pub fn time_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("time", attributes, children)
}

/// The `<tr>` HTML element defines a row of cells in a table. The row's cells
/// can then be established using a mix of `<td>` (data cell) and `<th>`
/// (header cell) elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tr)
pub fn tr(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("tr", class, attributes, children)
}

/// The `<tr>` HTML element defines a row of cells in a table. The row's cells
/// can then be established using a mix of `<td>` (data cell) and `<th>`
/// (header cell) elements.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/tr)
pub fn tr_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("tr", attributes, children)
}

/// The `<track>` HTML element is used as a child of the media elements,
/// `<audio>` and `<video>`. Each track element lets you specify a timed text
/// track (or time-based data) that can be displayed in parallel with the media
/// element, for example to overlay subtitles or closed captions on top of a
/// video or alongside audio tracks.
///
/// Multiple tracks can be specified for a media element, containing different
/// kinds of timed text data, or timed text data that has been translated for
/// different locales. The data that is used will either be the track that has
/// been set to be the default, or a kind and translation based on user
/// preferences.
///
/// The tracks are formatted in [WebVTT format](https://developer.mozilla.org/en-US/docs/Web/API/WebVTT_API)
/// (`.vtt` files) — Web Video Text Tracks.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/track)
pub fn track(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("track", class, attributes, [])
}

/// The `<track>` HTML element is used as a child of the media elements,
/// `<audio>` and `<video>`. Each track element lets you specify a timed text
/// track (or time-based data) that can be displayed in parallel with the media
/// element, for example to overlay subtitles or closed captions on top of a
/// video or alongside audio tracks.
///
/// Multiple tracks can be specified for a media element, containing different
/// kinds of timed text data, or timed text data that has been translated for
/// different locales. The data that is used will either be the track that has
/// been set to be the default, or a kind and translation based on user
/// preferences.
///
/// The tracks are formatted in [WebVTT format](https://developer.mozilla.org/en-US/docs/Web/API/WebVTT_API)
/// (`.vtt` files) — Web Video Text Tracks.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/track)
pub fn track_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("track", attributes, [])
}

/// The `<u>` HTML element represents a span of inline text which should be
/// rendered in a way that indicates that it has a non-textual annotation. This
/// is rendered by default as a single solid underline, but may be altered
/// using CSS.
///
/// > This element used to be called the "Underline" element in older versions
/// > of HTML, and is still sometimes misused in this way. To underline text,
/// > you should instead apply a style that includes the CSS `text-decoration`
/// > property set to `underline`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/u)
pub fn u(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("u", class, attributes, children)
}

/// The `<u>` HTML element represents a span of inline text which should be
/// rendered in a way that indicates that it has a non-textual annotation. This
/// is rendered by default as a single solid underline, but may be altered
/// using CSS.
///
/// > This element used to be called the "Underline" element in older versions
/// > of HTML, and is still sometimes misused in this way. To underline text,
/// > you should instead apply a style that includes the CSS `text-decoration`
/// > property set to `underline`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/u)
pub fn u_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("u", attributes, children)
}

/// The `<ul>` HTML element represents an unordered list of items, typically
/// rendered as a bulleted list.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ul)
pub fn ul(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("ul", class, attributes, children)
}

/// The `<ul>` HTML element represents an unordered list of items, typically
/// rendered as a bulleted list.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/ul)
pub fn ul_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("ul", attributes, children)
}

/// The `<var>` HTML element represents the name of a variable in a mathematical
/// expression or a programming context. It's typically presented using an
/// italicized version of the current typeface, although that behavior is
/// browser-dependent.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/var)
pub fn var(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("var", class, attributes, children)
}

/// The `<var>` HTML element represents the name of a variable in a mathematical
/// expression or a programming context. It's typically presented using an
/// italicized version of the current typeface, although that behavior is
/// browser-dependent.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/var)
pub fn var_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("var", attributes, children)
}

/// The `<video>` HTML element embeds a media player which supports video
/// playback into the document. You can use `<video>` for audio content as
/// well, but the `<audio>` element may provide a more appropriate user experience.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/video)
pub fn video(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element("video", class, attributes, children)
}

/// The `<video>` HTML element embeds a media player which supports video
/// playback into the document. You can use `<video>` for audio content as
/// well, but the `<audio>` element may provide a more appropriate user experience.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/video)
pub fn video_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  el.element_("video", attributes, children)
}

/// The `<wbr>` HTML element represents a word break opportunity—a position
/// within text where the browser may optionally break a line, though its
/// line-breaking rules would not otherwise create a break at that location.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/wbr)
pub fn wbr(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  el.element("wbr", class, attributes, [])
}

/// The `<wbr>` HTML element represents a word break opportunity—a position
/// within text where the browser may optionally break a line, though its
/// line-breaking rules would not otherwise create a break at that location.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/HTML/Element/wbr)
pub fn wbr_(attributes: List(Attribute(a))) -> Element(a) {
  el.element_("wbr", attributes, [])
}
