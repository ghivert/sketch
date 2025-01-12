## v4.0.0 - 2025-01-12

v4.0.0 marks a major release for Sketch! It bundles new improvements, new
features, and improved namespacing! Six months after the initial release of v3,
v4 marks a new milestone as Sketch gains in maturity, usability and usage. More
and more people are using it, and as such, Sketch has to evolve in the right
path!

Every functions now display documentation, and points to according MDN
documentation, with no defaults for a language. This means Sketch tries to help
you with precise, up to date documentation extracts, and let you deep dive in
the documentation _in your favourite language_ with one click!

Thanks to everyone using Sketch and helping me maintaining it as a package,
always pushing the boundaries of what can be achieved with it!

### Features

- Most selectors have been implemented. Some selectors are still missing, when
  they were overlapping with Gleam, hard to implement, or simply useless to
  implement (like `:host` that should be used in plain CSS stylesheets mostly).
- Most combinators have been implemented. They can be used exclusively with
  other classes and I have no will to support anything else. If you need
  specific CSS, then you probably need to write CSS.
- Support for `only`, `print`, `screen` & `all` media queries has been added.
- CSS transform functions are now fully supported, with all functions supported.
- CSS Length are now fully supported, with all length types supported.
- Keyframes @rule are now supported.
- Font Face @rule are now supported.

### Improvements

- Every CSS property, selectors & combinators now live in `sketch/css`.
- Every other modules, like `media`, `size` or `angle` now live in `sketch/css`.
- Every CSS property now display the headline of the documentation, and points
  to its MDN Reference.
- Every CSS length now display the headline of the documentation, and points to
  its MDN Reference.
- Every CSS angle now display the headline of the documentation, and points to
  its MDN Reference.
- Every CSS media queries now display the headline of the documentation, and
  points to its MDN Reference.
- Every CSS transform now display the headline of the documentation, and points
  to its MDN Reference.
- Every CSS pseudo-classes & pseudo-elements now points to its MDN Reference and
  have a fragment of the page to simplify understanding.
- SVG exclusives properties has moved to `sketch/css/svg`.
- `to_string` functions have been hidden from user in documentation. While they
  remain publicly accessible, they are of small utility for the daily users.
  Frontend authors can still use them if they need it.
- Code has been simplified as a whole. Readability is better, codebase is easier
  to understand.
- Few tests are now implemented to ensure no regression across versions!
- `sketch.Cache` has been renamed `sketch.StyleSheet`, and related function have
  been updated accordingly.
- `size` module is renamed `length`, to follow CSS specifications. All lengths
  are also now implemented and supported.
- `transform.translate2` and `transform.scale2` are deleted.

### Bugfixes

- Nested selectors/combinators are now supported.
- Persistent class name generation strategy in caches have been removed to only
  keep Ephemeral class name generation strategy. Persistent vs Ephemeral caches
  are still there, but they now don't lead to different class names. One class
  == one class name no matter the cache they're in.
