//// This module is a replacement for `sketch/redraw/dom/html` with styles
//// lazily computed, in the hook context of the component.
////
//// Contrarily to functions in `sketch/redraw/dom/html`, functions in this
//// module accepts  a function that generates the styles, and runs directly
//// in the component.
//// This lets you use hooks or any custom code you need directly in your style
//// function!
////
//// For most usage, `sketch/redraw/dom/html` should be enough, but when you
//// need more flexibility in some nodes, you can try its equivalent in this
//// module, to generate the exact styles you need!

import redraw.{type Element} as _
import redraw/dom/attribute.{type Attribute}
import redraw/dom/html
import sketch/css.{type Class}
import sketch/redraw.{hooked as styled}

/// The `<html>` HTML element represents the root (top-level element) of an
/// HTML document, so it is also referred to as the root element. All other
/// elements must be descendants of this element. There can be only one `<html>`
/// element in a document.
///
/// > `html` is a variant of the base `sketch/redraw/dom/html.html` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/html)
pub fn html(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("html", attributes, children, class)
}

/// The `<base>` HTML element specifies the base URL to use for all relative
/// URLs in a document. There can be only one `<base>` element in a document.
///
/// A document's used base URL can be accessed by scripts with `Node.baseURI`.
/// If the document has no `<base>` elements, then `baseURI` defaults to
/// `location.href`.
///
/// > `base` is a variant of the base `sketch/redraw/dom/html.base` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/base)
pub fn base(class: fn() -> Class, attributes: List(Attribute)) -> Element {
  styled("base", attributes, Nil, class)
}

/// The `<head>` HTML element contains machine-readable information (metadata)
/// about the document, like its title, scripts, and style sheets. There can be
/// only one `<head>` element in an HTML document.
///
/// > `<head>` primarily holds information for machine processing, not
/// > human-readability. For human-visible information, like top-level headings
/// > and listed authors, see the `<header>` element.
///
/// > `head` is a variant of the base `sketch/redraw/dom/html.head` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/head)
pub fn head(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("head", attributes, children, class)
}

/// The `<link>` HTML element specifies relationships between the current
/// document and an external resource. This element is most commonly used to
/// link to stylesheets, but is also used to establish site icons (both "favicon"
/// style icons and icons for the home screen and apps on mobile devices) among
/// other things.
///
/// > `link` is a variant of the base `sketch/redraw/dom/html.link` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/link)
pub fn link(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("link", attributes, Nil, class)
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
/// > `meta` is a variant of the base `sketch/redraw/dom/html.meta` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/meta)
pub fn meta(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("meta", attributes, Nil, class)
}

/// The `<style>` HTML element contains style information for a document, or
/// part of a document. It contains CSS, which is applied to the contents of
/// the document containing the `<style>` element.
///
/// > `style` is a variant of the base `sketch/redraw/dom/html.style` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/style)
pub fn style(
  attributes: List(Attribute),
  child: String,
  class: fn() -> Class,
) -> Element {
  styled("style", attributes, [text(child)], class)
}

/// The `<title>` HTML element defines the document's title that is shown in a
/// browser's title bar or a page's tab. It only contains text; tags within
/// the element are ignored.
///
/// > `title` is a variant of the base `sketch/redraw/dom/html.title` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/title)
pub fn title(
  attributes: List(Attribute),
  title: String,
  class: fn() -> Class,
) -> Element {
  styled("title", attributes, [text(title)], class)
}

/// The `Text` interface represents a text node in a DOM tree.
///
/// > `text` is a variant of the base `sketch/redraw/dom/html.text` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/API/Text)
pub const text: fn(String) -> Element = html.text

/// Does not render anything in the DOM.
pub const none: fn() -> Element = html.none

/// The `<a>` HTML element (or anchor element), with [its `href` attribute](https://developer.mozilla.org/docs/Web/HTML/Element/a#href),
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
/// > `a` is a variant of the base `sketch/redraw/dom/html.a` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/a)
pub fn a(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("a", attributes, children, class)
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
/// > `abbr` is a variant of the base `sketch/redraw/dom/html.abbr` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/abbr)
pub fn abbr(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("abbr", attributes, children, class)
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
/// > `address` is a variant of the base `sketch/redraw/dom/html.address` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/address)
pub fn address(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("address", attributes, children, class)
}

/// The `<area>` HTML element defines an area inside an image map that has
/// predefined clickable areas. An image map allows geometric areas on an
/// image to be associated with hypertext links.
///
/// This element is used only within a `<map>` element.
///
/// > `area` is a variant of the base `sketch/redraw/dom/html.area` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/area)
pub fn area(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("area", attributes, Nil, class)
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
/// > `article` is a variant of the base `sketch/redraw/dom/html.article` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/article)
pub fn article(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("article", attributes, children, class)
}

/// The `<aside>` HTML element represents a portion of a document whose content
/// is only indirectly related to the document's main content. Asides are f
/// requently presented as sidebars or call-out boxes.
///
/// > `aside` is a variant of the base `sketch/redraw/dom/html.aside` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/aside)
pub fn aside(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("aside", attributes, children, class)
}

/// The `<audio>` HTML element is used to embed sound content in documents. It
/// may contain one or more audio sources, represented using the src attribute
/// or the `<source>` element: the browser will choose the most suitable one. It
/// can also be the destination for streamed media, using a `MediaStream`.
///
/// > `audio` is a variant of the base `sketch/redraw/dom/html.audio` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/audio)
pub fn audio(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("audio", attributes, children, class)
}

/// The `<b>` HTML element is used to draw the reader's attention to the
/// element's contents, which are not otherwise granted special importance.
/// This was formerly known as the Boldface element, and most browsers still
/// draw the text in boldface. However, you should not use `<b>` for styling text
/// or granting importance. If you wish to create boldface text, you should
/// use the CSS `font-weight` property. If you wish to indicate an element is
/// of special importance, you should use the `<strong>` element.
///
/// > `b` is a variant of the base `sketch/redraw/dom/html.b` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/b)
pub fn b(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("b", attributes, children, class)
}

/// The `<bdi>` HTML element tells the browser's bidirectional algorithm to
/// treat the text it contains in isolation from its surrounding text. It's
/// particularly useful when a website dynamically inserts some text and
/// doesn't know the directionality of the text being inserted.
///
/// > `bdi` is a variant of the base `sketch/redraw/dom/html.bdi` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/bdi)
pub fn bdi(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("bdi", attributes, children, class)
}

/// The `<bdo>` HTML element overrides the current directionality of text, so
/// that the text within is rendered in a different direction.
///
/// > `bdo` is a variant of the base `sketch/redraw/dom/html.bdo` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/bdo)
pub fn bdo(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("bdo", attributes, children, class)
}

/// The `<blockquote>` HTML element indicates that the enclosed text is an
/// extended quotation. Usually, this is rendered visually by indentation.
/// A URL for the source of the quotation may be given using the cite attribute,
/// while a text representation of the source can be given using the `<cite>` element.
///
/// > `blockquote` is a variant of the base `sketch/redraw/dom/html.blockquote` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/blockquote)
pub fn blockquote(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("blockquote", attributes, children, class)
}

/// The `<body>` HTML element represents the content of an HTML document.
/// There can be only one `<body>` element in a document.
///
/// > `body` is a variant of the base `sketch/redraw/dom/html.body` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/body)
pub fn body(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("body", attributes, children, class)
}

/// The `<br>` HTML element produces a line break in text (carriage-return).
/// It is useful for writing a poem or an address, where the division of
/// lines is significant.
///
/// > `br` is a variant of the base `sketch/redraw/dom/html.br` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/br)
pub fn br(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("br", attributes, Nil, class)
}

/// The `<button>` HTML element is an interactive element activated by a user
/// with a mouse, keyboard, finger, voice command, or other assistive technology.
/// Once activated, it then performs an action, such as submitting a form or
/// opening a dialog.
///
/// By default, HTML buttons are presented in a style resembling the platform
/// the user agent runs on, but you can change buttons' appearance with CSS.
///
/// > `button` is a variant of the base `sketch/redraw/dom/html.button` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/button)
pub fn button(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("button", attributes, children, class)
}

/// Use the HTML `<canvas>` element with either the
/// [canvas scripting API](https://developer.mozilla.org/docs/Web/API/Canvas_API) or the
/// [WebGL API](https://developer.mozilla.org/docs/Web/API/WebGL_API)
/// to draw graphics and animations.
///
/// > `canvas` is a variant of the base `sketch/redraw/dom/html.canvas` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/canvas)
pub fn canvas(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("canvas", attributes, children, class)
}

/// The `<caption>` HTML element specifies the caption (or title) of a table, p
/// roviding the table an [accessible description](https://developer.mozilla.org/docs/Glossary/Accessible_description).
///
/// > `caption` is a variant of the base `sketch/redraw/dom/html.caption` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/caption)
pub fn caption(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("caption", attributes, children, class)
}

/// The `<cite>` HTML element is used to mark up the title of a cited creative
/// work. The reference may be in an abbreviated form according to
/// context-appropriate conventions related to citation metadata.
///
/// > `cite` is a variant of the base `sketch/redraw/dom/html.cite` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/cite)
pub fn cite(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("cite", attributes, children, class)
}

/// The `<code>` HTML element displays its contents styled in a fashion intended
/// to indicate that the text is a short fragment of computer code. By default,
/// the content text is displayed using the user agent's default monospace font.
///
/// > `code` is a variant of the base `sketch/redraw/dom/html.code` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/code)
pub fn code(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("code", attributes, children, class)
}

/// The `<col>` HTML element defines one or more columns in a column group
/// represented by its parent `<colgroup>` element. The `<col>` element is
/// only valid as a child of a `<colgroup>` element that has no `span`
/// attribute defined.
///
/// > `col` is a variant of the base `sketch/redraw/dom/html.col` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/col)
pub fn col(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("col", attributes, Nil, class)
}

/// The `<colgroup>` HTML element defines a group of columns within a table.
///
/// > `colgroup` is a variant of the base `sketch/redraw/dom/html.colgroup` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/colgroup)
pub fn colgroup(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("colgroup", attributes, children, class)
}

/// The `<data>` HTML element links a given piece of content with a
/// machine-readable translation. If the content is time- or date-related,
/// the `<time>` element must be used.
///
/// > `data` is a variant of the base `sketch/redraw/dom/html.data` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/data)
pub fn data(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("data", attributes, children, class)
}

/// The `<datalist>` HTML element contains a set of `<option>` elements that
/// represent the permissible or recommended options available to choose from
/// within other controls.
///
/// > `datalist` is a variant of the base `sketch/redraw/dom/html.datalist` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/datalist)
pub fn datalist(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("datalist", attributes, children, class)
}

/// The `<dd>` HTML element provides the description, definition, or value for
/// the preceding term (`<dt>`) in a description list (`<dl>`).
///
/// > `dd` is a variant of the base `sketch/redraw/dom/html.dd` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/dd)
pub fn dd(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("dd", attributes, children, class)
}

/// The `<del>` HTML element represents a range of text that has been deleted
/// from a document. This can be used when rendering "track changes" or source
/// code diff information, for example. The `<ins>` element can be used for the
/// opposite purpose: to indicate text that has been added to the document.
///
/// This element is often (but need not be) rendered by applying a
/// strike-through style to the text.
///
/// > `del` is a variant of the base `sketch/redraw/dom/html.del` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/del)
pub fn del(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("del", attributes, children, class)
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
/// > `details` is a variant of the base `sketch/redraw/dom/html.details` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/details)
pub fn details(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("details", attributes, children, class)
}

/// The `<dfn>` HTML element indicates a term to be defined. The `<dfn>` element
/// should be used in a complete definition statement, where the full definition
/// of the term can be one of the following:
/// - The ancestor paragraph (a block of text, sometimes marked by a `<p>`
///   element)
/// - The `<dt>`/`<dd>` pairing
/// - The nearest section ancestor of the `<dfn>` element
///
/// > `dfn` is a variant of the base `sketch/redraw/dom/html.dfn` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/dfn)
pub fn dfn(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("dfn", attributes, children, class)
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
/// > `dialog` is a variant of the base `sketch/redraw/dom/html.dialog` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/dialog)
pub fn dialog(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("dialog", attributes, children, class)
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
/// > `div` is a variant of the base `sketch/redraw/dom/html.div` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/div)
pub fn div(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("div", attributes, children, class)
}

/// The `<dl>` HTML element represents a description list. The element encloses
/// a list of groups of terms (specified using the `<dt>` element) and
/// descriptions (provided by `<dd>` elements). Common uses for this element are
/// to implement a glossary or to display metadata (a list of key-value pairs).
///
/// > `dl` is a variant of the base `sketch/redraw/dom/html.dl` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/dl)
pub fn dl(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("dl", attributes, children, class)
}

/// The `<dt>` HTML element specifies a term in a description or definition list,
/// and as such must be used inside a `<dl>` element. It is usually followed by
/// a `<dd>` element; however, multiple `<dt>` elements in a row indicate several
/// terms that are all defined by the immediate next `<dd>` element.
///
/// The subsequent `<dd>` (Description Details) element provides the definition
/// or other related text associated with the term specified using `<dt>`.
///
/// > `dt` is a variant of the base `sketch/redraw/dom/html.dt` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/dt)
pub fn dt(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("dt", attributes, children, class)
}

/// The `<em>` HTML element marks text that has stress emphasis. The `<em>`
/// element can be nested, with each level of nesting indicating a greater
/// degree of emphasis.
///
/// > `em` is a variant of the base `sketch/redraw/dom/html.em` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/em)
pub fn em(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("em", attributes, children, class)
}

/// The `<embed>` HTML element embeds external content at the specified point in
/// the document. This content is provided by an external application or other
/// source of interactive content such as a browser plug-in.
///
/// > `embed` is a variant of the base `sketch/redraw/dom/html.embed` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/embed)
pub fn embed(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("embed", attributes, Nil, class)
}

/// The `<fieldset>` HTML element is used to group several controls as well as
/// labels (`<label>`) within a web form.
///
/// > `fieldset` is a variant of the base `sketch/redraw/dom/html.fieldset` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/fieldset)
pub fn fieldset(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("fieldset", attributes, children, class)
}

/// The `<figcaption>` HTML element represents a caption or legend describing
/// the rest of the contents of its parent `<figure>` element, providing the
/// `<figure>` an [accessible description](https://developer.mozilla.org/docs/Glossary/Accessible_description).
///
/// > `figcaption` is a variant of the base `sketch/redraw/dom/html.figcaption` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/figcaption)
pub fn figcaption(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("figcaption", attributes, children, class)
}

/// The `<figure>` HTML element represents self-contained content, potentially
/// with an optional caption, which is specified using the `<figcaption>`
/// element. The figure, its caption, and its contents are referenced as a
/// single unit.
///
/// > `figure` is a variant of the base `sketch/redraw/dom/html.figure` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/figure)
pub fn figure(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("figure", attributes, children, class)
}

/// The `<footer>` HTML element represents a footer for its nearest ancestor
/// [sectioning content](https://developer.mozilla.org/docs/Web/HTML/Content_categories#sectioning_content)
/// or [sectioning root](https://developer.mozilla.org/docs/Web/HTML/Element/Heading_Elements#labeling_section_content)
/// element. A `<footer>` typically contains information about the author of
/// the section, copyright data or links to related documents.
///
/// > `footer` is a variant of the base `sketch/redraw/dom/html.footer` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/footer)
pub fn footer(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("footer", attributes, children, class)
}

/// The `<form>` HTML element represents a document section containing
/// interactive controls for submitting information.
///
/// It is possible to use the `:valid` and `:invalid` CSS pseudo-classes to
/// style a `<form>` element based on whether the elements inside the form
/// are valid.
///
/// > `form` is a variant of the base `sketch/redraw/dom/html.form` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/form)
pub fn form(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("form", attributes, children, class)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// > `h1` is a variant of the base `sketch/redraw/dom/html.h1` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/h1)
pub fn h1(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("h1", attributes, children, class)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// > `h2` is a variant of the base `sketch/redraw/dom/html.h2` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/h2)
pub fn h2(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("h2", attributes, children, class)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// > `h3` is a variant of the base `sketch/redraw/dom/html.h3` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/h3)
pub fn h3(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("h3", attributes, children, class)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// > `h4` is a variant of the base `sketch/redraw/dom/html.h4` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/h4)
pub fn h4(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("h4", attributes, children, class)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// > `h5` is a variant of the base `sketch/redraw/dom/html.h5` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/h5)
pub fn h5(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("h5", attributes, children, class)
}

/// The `<h1>` to `<h6>` HTML elements represent six levels of section headings.
/// `<h1>` is the highest section level and `<h6>` is the lowest. By default,
/// all heading elements create a block-level box in the layout, starting on a
/// new line and taking up the full width available in their containing block.
///
/// > `h6` is a variant of the base `sketch/redraw/dom/html.h6` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/h6)
pub fn h6(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("h6", attributes, children, class)
}

/// The `<header>` HTML element represents introductory content, typically a
/// group of introductory or navigational aids. It may contain some heading
/// elements but also a logo, a search form, an author name, and other elements.
///
/// > `header` is a variant of the base `sketch/redraw/dom/html.header` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/header)
pub fn header(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("header", attributes, children, class)
}

/// The `<hgroup>` HTML element represents a heading and related content. It
/// groups a single `<h1>`–`<h6>` element with one or more `<p>`.
///
/// > `hgroup` is a variant of the base `sketch/redraw/dom/html.hgroup` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/hgroup)
pub fn hgroup(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("hgroup", attributes, children, class)
}

/// The `<hr>` HTML element represents a thematic break between paragraph-level
/// elements: for example, a change of scene in a story, or a shift of topic
/// within a section.
///
/// > `hr` is a variant of the base `sketch/redraw/dom/html.hr` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/hr)
pub fn hr(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("hr", attributes, Nil, class)
}

/// The `<i>` HTML element represents a range of text that is set off from the
/// normal text for some reason, such as idiomatic text, technical terms,
/// taxonomical designations, among others. Historically, these have been
/// presented using italicized type, which is the original source of the `<i>`
/// naming of this element.
///
/// > `i` is a variant of the base `sketch/redraw/dom/html.i` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/i)
pub fn i(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("i", attributes, children, class)
}

/// The `<iframe>` HTML element represents a nested browsing context, embedding
/// another HTML page into the current one.
///
/// > `iframe` is a variant of the base `sketch/redraw/dom/html.iframe` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/iframe)
pub fn iframe(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("iframe", attributes, children, class)
}

/// The `<img>` HTML element embeds an image into the document.
///
/// > `img` is a variant of the base `sketch/redraw/dom/html.img` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/img)
pub fn img(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("img", attributes, Nil, class)
}

/// The `<input>` HTML element is used to create interactive controls for
/// web-based forms in order to accept data from the user; a wide variety of
/// types of input data and control widgets are available, depending on the
/// device and user agent. The `<input>` element is one of the most powerful and
/// complex in all of HTML due to the sheer number of combinations of input
/// types and attributes.
///
/// > `input` is a variant of the base `sketch/redraw/dom/html.input` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/input)
pub fn input(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("input", attributes, Nil, class)
}

/// The `<ins>` HTML element represents a range of text that has been added to
/// a document. You can use the `<del>` element to similarly represent a range
/// of text that has been deleted from the document.
///
/// > `ins` is a variant of the base `sketch/redraw/dom/html.ins` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/ins)
pub fn ins(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("ins", attributes, children, class)
}

/// The `<kbd>` HTML element represents a span of inline text denoting textual
/// user input from a keyboard, voice input, or any other text entry device. By
/// convention, the user agent defaults to rendering the contents of a `<kbd>`
/// element using its default monospace font, although this is not mandated by
/// the HTML standard.
///
/// > `kbd` is a variant of the base `sketch/redraw/dom/html.kbd` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/kbd)
pub fn kbd(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("kbd", attributes, children, class)
}

/// The `<label>` HTML element represents a caption for an item in a user
/// interface.
///
/// > `label` is a variant of the base `sketch/redraw/dom/html.label` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/label)
pub fn label(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("label", attributes, children, class)
}

/// The `<legend>` HTML element represents a caption for the content of
/// its parent `<fieldset>`.
///
/// > `legend` is a variant of the base `sketch/redraw/dom/html.legend` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/legend)
pub fn legend(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("legend", attributes, children, class)
}

/// The `<li>` HTML element is used to represent an item in a list. It must be
/// contained in a parent element: an ordered list (`<ol>`), an unordered list
/// (`<ul>`), or a menu (`<menu>`). In menus and unordered lists, list items are
/// usually displayed using bullet points. In ordered lists, they are usually
/// displayed with an ascending counter on the left, such as a number or letter.
///
/// > `li` is a variant of the base `sketch/redraw/dom/html.li` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/li)
pub fn li(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("li", attributes, children, class)
}

/// The `<main>` HTML element represents the dominant content of the `<body>` of
/// a document. The main content area consists of content that is directly
/// related to or expands upon the central topic of a document, or the central
/// functionality of an application.
///
/// > `main` is a variant of the base `sketch/redraw/dom/html.main` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/main)
pub fn main(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("main", attributes, children, class)
}

/// The `<map>` HTML element is used with `<area>` elements to define an
/// image map (a clickable link area).
///
/// > `map` is a variant of the base `sketch/redraw/dom/html.map` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/map)
pub fn map(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("map", attributes, children, class)
}

/// The `<mark>` HTML element represents text which is marked or highlighted for
/// reference or notation purposes due to the marked passage's relevance in the
/// enclosing context.
///
/// > `mark` is a variant of the base `sketch/redraw/dom/html.mark` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/mark)
pub fn mark(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("mark", attributes, children, class)
}

/// The `<math>` MathML element is the top-level MathML element, used to write
/// a single mathematical formula. It can be placed in HTML content where flow
/// content is permitted.
///
/// > See the [Authoring MathML page](https://developer.mozilla.org/docs/Web/MathML/Authoring#using_mathml)
/// > for tips to properly integrate MathML formulas in your web pages and the
/// > [Examples](https://developer.mozilla.org/docs/Web/MathML/Examples)
/// > page for more demos.
///
/// > `math` is a variant of the base `sketch/redraw/dom/html.math` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/MathML/Element/math)
pub fn math(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("math", attributes, children, class)
}

/// The `<menu>` HTML element is described in the HTML specification as a
/// semantic alternative to `<ul>`, but treated by browsers (and exposed through
/// the accessibility tree) as no different than `<ul>`. It represents an
/// unordered list of items (which are represented by `<li>` elements).
///
/// > `menu` is a variant of the base `sketch/redraw/dom/html.menu` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/menu)
pub fn menu(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("menu", attributes, children, class)
}

/// The `<meter>` HTML element represents either a scalar value within a
/// known range or a fractional value.
///
/// > `meter` is a variant of the base `sketch/redraw/dom/html.meter` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/meter)
pub fn meter(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("meter", attributes, children, class)
}

/// The `<nav>` HTML element represents a section of a page whose purpose is to
/// provide navigation links, either within the current document or to other
/// documents. Common examples of navigation sections are menus, tables of
/// contents, and indexes.
///
/// > `nav` is a variant of the base `sketch/redraw/dom/html.nav` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/nav)
pub fn nav(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("nav", attributes, children, class)
}

/// The `<noscript>` HTML element defines a section of HTML to be inserted if a
/// script type on the page is unsupported or if scripting is currently turned
/// off in the browser.
///
/// > `noscript` is a variant of the base `sketch/redraw/dom/html.noscript` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/noscript)
pub fn noscript(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("noscript", attributes, children, class)
}

/// The `<object>` HTML element represents an external resource, which can be
/// treated as an image, a nested browsing context, or a resource to be handled
/// by a plugin.
///
/// > `object` is a variant of the base `sketch/redraw/dom/html.object` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/object)
pub fn object(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("object", attributes, children, class)
}

/// The `<ol>` HTML element represents an ordered list of items — typically
/// rendered as a numbered list.
///
/// > `ol` is a variant of the base `sketch/redraw/dom/html.ol` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/ol)
pub fn ol(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("ol", attributes, children, class)
}

/// The `<optgroup>` HTML element creates a grouping of options within a
/// `<select>` element.
///
/// > `optgroup` is a variant of the base `sketch/redraw/dom/html.optgroup` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/optgroup)
pub fn optgroup(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("optgroup", attributes, children, class)
}

/// The `<option>` HTML element is used to define an item contained in a
/// `<select>`, an `<optgroup>`, or a `<datalist>` element. As such, `<option>`
/// can represent menu items in popups and other lists of items in an HTML
/// document.
///
/// > `option` is a variant of the base `sketch/redraw/dom/html.option` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/option)
pub fn option(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("option", attributes, children, class)
}

/// The `<output>` HTML element is a container element into which a site or
/// app can inject the results of a calculation or the outcome of a user action.
///
/// > `output` is a variant of the base `sketch/redraw/dom/html.output` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/output)
pub fn output(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("output", attributes, children, class)
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
/// > `p` is a variant of the base `sketch/redraw/dom/html.p` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/p)
pub fn p(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("p", attributes, children, class)
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
/// > `picture` is a variant of the base `sketch/redraw/dom/html.picture` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/picture)
pub fn picture(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("picture", attributes, children, class)
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
/// > `portal` is a variant of the base `sketch/redraw/dom/html.portal` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/portal)
pub fn portal(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("portal", attributes, children, class)
}

/// The `<pre>` HTML element represents preformatted text which is to be
/// presented exactly as written in the HTML file. The text is typically
/// rendered using a non-proportional, or monospaced, font. Whitespace inside
/// this element is displayed as written.
///
/// By default, `<pre>` is a block-level element, i.e. its default `display`
/// value is `block`.
///
/// > `pre` is a variant of the base `sketch/redraw/dom/html.pre` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/pre)
pub fn pre(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("pre", attributes, children, class)
}

/// The `<progress>` HTML element displays an indicator showing the completion
/// progress of a task, typically displayed as a progress bar.
///
/// > `progress` is a variant of the base `sketch/redraw/dom/html.progress` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/progress)
pub fn progress(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("progress", attributes, children, class)
}

/// The `<q>` HTML element indicates that the enclosed text is a short inline
/// quotation. Most modern browsers implement this by surrounding the text in
/// quotation marks. This element is intended for short quotations that don't
/// require paragraph breaks; for long quotations use the `<blockquote>` element.
///
/// > `q` is a variant of the base `sketch/redraw/dom/html.q` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/q)
pub fn q(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("q", attributes, children, class)
}

/// The `<rp>` HTML element is used to provide fall-back parentheses for
/// browsers that do not support display of ruby annotations using the `<ruby>`
/// element. One `<rp>` element should enclose each of the opening and closing
/// parentheses that wrap the `<rt>` element that contains the annotation's text.
///
/// > `rp` is a variant of the base `sketch/redraw/dom/html.rp` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/rp)
pub fn rp(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("rp", attributes, children, class)
}

/// The `<rt>` HTML element specifies the ruby text component of a ruby
/// annotation, which is used to provide pronunciation, translation, or
/// transliteration information for East Asian typography. The `<rt>` element
/// must always be contained within a `<ruby>` element.
///
/// > `rt` is a variant of the base `sketch/redraw/dom/html.rt` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/rt)
pub fn rt(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("rt", attributes, children, class)
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
/// > `ruby` is a variant of the base `sketch/redraw/dom/html.ruby` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/ruby)
pub fn ruby(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("ruby", attributes, children, class)
}

/// The `<s>` HTML element renders text with a strikethrough, or a line through
/// it. Use the `<s>` element to represent things that are no longer relevant or
/// no longer accurate. However, `<s>` is not appropriate when indicating
/// document edits; for that, use the `<del>` and `<ins>` elements, as
/// appropriate.
///
/// > `s` is a variant of the base `sketch/redraw/dom/html.s` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/s)
pub fn s(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("s", attributes, children, class)
}

/// The `<samp>` HTML element is used to enclose inline text which represents
/// sample (or quoted) output from a computer program. Its contents are
/// typically rendered using the browser's default monospaced font (such as
/// Courier or Lucida Console).
///
/// > `samp` is a variant of the base `sketch/redraw/dom/html.samp` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/samp)
pub fn samp(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("samp", attributes, children, class)
}

/// The `<script>` HTML element is used to embed executable code or data; this
/// is typically used to embed or refer to JavaScript code. The `<script>`
/// element can also be used with other languages, such as WebGL's GLSL shader
/// programming language and JSON.
///
/// > `script` is a variant of the base `sketch/redraw/dom/html.script` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/script)
pub fn script(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("script", attributes, children, class)
}

/// The `<search>` HTML element is a container representing the parts of the
/// document or application with form controls or other content related to
/// performing a search or filtering operation. The <search> element
/// semantically identifies the purpose of the element's contents as having
/// search or filtering capabilities. The search or filtering functionality can
/// be for the website or application, the current web page or document, or the
/// entire Internet or subsection thereof.
///
/// > `search` is a variant of the base `sketch/redraw/dom/html.search` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/search)
pub fn search(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("search", attributes, children, class)
}

/// The `<section>` HTML element represents a generic standalone section of a
/// document, which doesn't have a more specific semantic element to represent
/// it. Sections should always have a heading, with very few exceptions.
///
/// > `section` is a variant of the base `sketch/redraw/dom/html.section` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/section)
pub fn section(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("section", attributes, children, class)
}

/// The `<select>` HTML element represents a control that provides a menu of options.
///
/// > `select` is a variant of the base `sketch/redraw/dom/html.select` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/select)
pub fn select(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("select", attributes, children, class)
}

/// The `<slot>` HTML element—part of the Web Components technology suite—is a
/// placeholder inside a web component that you can fill with your own markup,
/// which lets you create separate DOM trees and present them together.
///
/// > `slot` is a variant of the base `sketch/redraw/dom/html.slot` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/slot)
pub fn slot(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("slot", attributes, children, class)
}

/// The `<small>` HTML element represents side-comments and small print, like
/// `copyright and legal text, independent of its styled presentation. By
/// default, it renders text within it one font-size smaller, such as from
/// `small` to `x-small`.
///
/// > `small` is a variant of the base `sketch/redraw/dom/html.small` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/small)
pub fn small(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("small", attributes, children, class)
}

/// The `<source>` HTML element specifies one or more media resources for the
/// `<picture>,` `<audio>,` and `<video>` elements. It is a
/// [void element](https://developer.mozilla.org/docs/Glossary/Void_element),
/// which means that it has no content and does not require a closing tag. This
/// element is commonly used to offer the same media content in multiple file
/// formats in order to provide compatibility with a broad range of browsers
/// given their differing support for image file formats and media file formats.
///
/// > `source` is a variant of the base `sketch/redraw/dom/html.source` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/source)
pub fn source(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("source", attributes, Nil, class)
}

/// The `<span>` HTML element is a generic inline container for phrasing content,
/// which does not inherently represent anything. It can be used to group
/// elements for styling purposes (using the class or id attributes), or because
/// they share attribute values, such as lang. It should be used only when no
/// other semantic element is appropriate. `<span>` is very much like a `<div>`
/// element, but `<div>` is a block-level element whereas a `<span>` is an
/// inline-level element.
///
/// > `span` is a variant of the base `sketch/redraw/dom/html.span` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/span)
pub fn span(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("span", attributes, children, class)
}

/// The `<strong>` HTML element indicates that its contents have strong
/// importance, seriousness, or urgency. Browsers typically render the
/// contents in bold type.
///
/// > `strong` is a variant of the base `sketch/redraw/dom/html.strong` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/strong)
pub fn strong(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("strong", attributes, children, class)
}

/// The `<sub>` HTML element specifies inline text which should be displayed as
/// subscript for solely typographical reasons. Subscripts are typically
/// rendered with a lowered baseline using smaller text.
///
/// > `sub` is a variant of the base `sketch/redraw/dom/html.sub` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/sub)
pub fn sub(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("sub", attributes, children, class)
}

/// The `<summary>` HTML element specifies a summary, caption, or legend for a
/// `<details>` element's disclosure box. Clicking the `<summary>` element
/// toggles the state of the parent `<details>` element open and closed.
///
/// > `summary` is a variant of the base `sketch/redraw/dom/html.summary` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/summary)
pub fn summary(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("summary", attributes, children, class)
}

/// The `<sup>` HTML element specifies inline text which is to be displayed as
/// superscript for solely typographical reasons. Superscripts are usually
/// rendered with a raised baseline using smaller text.
///
/// > `sup` is a variant of the base `sketch/redraw/dom/html.sup` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/sup)
pub fn sup(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("sup", attributes, children, class)
}

/// The `<table>` HTML element represents tabular data—that is, information
/// presented in a two-dimensional table comprised of rows and columns of
/// cells containing data.
///
/// > `table` is a variant of the base `sketch/redraw/dom/html.table` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/table)
pub fn table(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("table", attributes, children, class)
}

/// The `<tbody>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the body of a table's (main) data.
///
/// > `tbody` is a variant of the base `sketch/redraw/dom/html.tbody` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/tbody)
pub fn tbody(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("tbody", attributes, children, class)
}

/// The `<td>` HTML element defines a cell of a table that contains data and
/// may be used as a child of the `<tr>` element.
///
/// > `td` is a variant of the base `sketch/redraw/dom/html.td` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/td)
pub fn td(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("td", attributes, children, class)
}

/// The `<template>` HTML element serves as a mechanism for holding HTML
/// fragments, which can either be used later via JavaScript or generated
/// immediately into shadow DOM.
///
/// > `template` is a variant of the base `sketch/redraw/dom/html.template` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/template)
pub fn template(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("template", attributes, children, class)
}

/// The `<textarea>` HTML element represents a multi-line plain-text editing
/// control, useful when you want to allow users to enter a sizeable amount of
/// free-form text, for example a comment on a review or feedback form.
///
/// > `textarea` is a variant of the base `sketch/redraw/dom/html.textarea` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/textarea)
pub fn textarea(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("textarea", attributes, children, class)
}

/// The `<tfoot>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the foot of a table with
/// information about the table's columns. This is usually a summary of the
/// columns, e.g., a sum of the given numbers in a column.
///
/// > `tfoot` is a variant of the base `sketch/redraw/dom/html.tfoot` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/tfoot)
pub fn tfoot(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("tfoot", attributes, children, class)
}

/// The `<th>` HTML element defines a cell as the header of a group of table
/// cells and may be used as a child of the `<tr>` element. The exact nature
/// of this group is defined by the `scope` and `headers` attributes.
///
/// > `th` is a variant of the base `sketch/redraw/dom/html.th` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/th)
pub fn th(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("th", attributes, children, class)
}

/// The `<thead>` HTML element encapsulates a set of table rows (`<tr>`
/// elements), indicating that they comprise the head of a table with
/// information about the table's columns. This is usually in the form of
/// column headers (`<th>` elements).
///
/// > `thead` is a variant of the base `sketch/redraw/dom/html.thead` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/thead)
pub fn thead(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("thead", attributes, children, class)
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
/// > `time` is a variant of the base `sketch/redraw/dom/html.time` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/time)
pub fn time(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("time", attributes, children, class)
}

/// The `<tr>` HTML element defines a row of cells in a table. The row's cells
/// can then be established using a mix of `<td>` (data cell) and `<th>`
/// (header cell) elements.
///
/// > `tr` is a variant of the base `sketch/redraw/dom/html.tr` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/tr)
pub fn tr(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("tr", attributes, children, class)
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
/// The tracks are formatted in [WebVTT format](https://developer.mozilla.org/docs/Web/API/WebVTT_API)
/// (`.vtt` files) — Web Video Text Tracks.
///
/// > `track` is a variant of the base `sketch/redraw/dom/html.track` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/track)
pub fn track(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("track", attributes, Nil, class)
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
/// > `u` is a variant of the base `sketch/redraw/dom/html.u` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/u)
pub fn u(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("u", attributes, children, class)
}

/// The `<ul>` HTML element represents an unordered list of items, typically
/// rendered as a bulleted list.
///
/// > `ul` is a variant of the base `sketch/redraw/dom/html.ul` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/ul)
pub fn ul(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("ul", attributes, children, class)
}

/// The `<var>` HTML element represents the name of a variable in a mathematical
/// expression or a programming context. It's typically presented using an
/// italicized version of the current typeface, although that behavior is
/// browser-dependent.
///
/// > `var` is a variant of the base `sketch/redraw/dom/html.var` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/var)
pub fn var(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("var", attributes, children, class)
}

/// The `<video>` HTML element embeds a media player which supports video
/// playback into the document. You can use `<video>` for audio content as
/// well, but the `<audio>` element may provide a more appropriate user experience.
///
/// > `video` is a variant of the base `sketch/redraw/dom/html.video` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/video)
pub fn video(
  attributes: List(Attribute),
  children: List(Element),
  class: fn() -> Class,
) -> Element {
  styled("video", attributes, children, class)
}

/// The `<wbr>` HTML element represents a word break opportunity—a position
/// within text where the browser may optionally break a line, though its
/// line-breaking rules would not otherwise create a break at that location.
///
/// > `wbr` is a variant of the base `sketch/redraw/dom/html.wbr` which accepts a function
/// > that generates a `css.Class`, and not a `css.Class` directly. That
/// > function will run in the hook context of the component, directly at
/// > the top-level of the component. It means you can use any hook you need
/// > in that function directly, like `use_memo` if the `css.Class` is too
/// > costly to compute, or `use_context` to access, for example, a theme
/// > inserted in your React components-tree!
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/HTML/Element/wbr)
pub fn wbr(attributes: List(Attribute), class: fn() -> Class) -> Element {
  styled("wbr", attributes, Nil, class)
}
