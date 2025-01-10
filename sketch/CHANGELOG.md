## v4.0.0 - Unreleased

v4.0.0 marks a major release for Sketch! It bundles new improvements, new
features, and improve namespacing! Six months after the initial release of v3,
v4 marks a new milestone as Sketch gains in maturity, usability and usage. More
and more people are using it, and as such, Sketch has to evolve in the right
path!

Thanks to everyone using Sketch and helping me maintaining it as a package,
always pushing the boundaries of what can be achieved with it!

### Breaking Changes

- `size` module is renamed `length`, to follow CSS specifications. All lengths
  are also now implemented and supported.

### Features

- Most selectors have been implemented. Some selectors are still missing, when
  they were overlapping with Gleam, hard to implement, or simply useless to
  implement (like `:host` that should be used in plain CSS stylesheets mostly).
- Most combinators have been implemented. They can be used exclusively with
  other classes and I have no will to support anything else. If you need
  specific CSS, then you probably need to write CSS.

### Improvements

- Every CSS property, selectors & combinators now live in `sketch/css`.
- Every other modules, like `media`, `size` or `angle` now live in `sketch/css`.
- Every CSS property now display the headline of the documentation, and points
  to its MDN documentation.
- Every CSS pseudo-classes & pseudo-elements now points to its MDN documentation
  and have a fragment of the page to simplify understanding.
- `to_string` functions have been hidden from user in documentation. While they
  remain publicly accessible, they are of small utility for the daily users.
  Frontend authors can still use them if they need it.
- Code has been simplified as a whole. Readability is better, codebase is easier
  to understand.
- Few tests are now implemented to ensure no regression across versions!
- `sketch.Cache` has been renamed `sketch.StyleSheet`, and related function have
  been updated accordingly.
- Support for `only`, `print`, `screen` & `all` media queries has been aded.
- Add support for every transform functions.

### Bugfixes

- Nested selectors/combinators are now supported.
- Persistent class name generation strategy in caches have been removed to only
  keep Ephemeral class name generation strategy. Persistent vs Ephemeral caches
  are still there, but they now don't lead to different class names. One class
  == one class name no matter the cache they're in.
