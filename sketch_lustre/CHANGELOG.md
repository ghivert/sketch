## v2.0.0 - Unreleased

v2.0.0 marks a breaking change with the new Sketch release (i.e. v4.0.0). Sketch
Lustre now uses a new, more idiomatic `render` and `ssr` API, and simplifies as
much a possible the exposed API.

Sketch Lustre has been one of the principal package used in combination with
Lustre. Thanks for your investment!

### Improvements

- Adapt to new Sketch API v4.0.0. `Cache` has been renamed to `StyleSheet`, and
  as such, can cause some inconsistencies bugs with old code.
- Use a new middleware-render API. The old `compose` function is now not
  supported anymore.
- Remove the old `Options` in favour of `Container`.
- Shadow root management has been changed from `plinth` to simple `Dynamic`.
  This can't type-check it, but it avoid having to depend on `plinth` in
  production, to let every user handle `ShadowRoot` as they want.
- Every HMTL element now has MDN Reference links & fragment of description to
  explain how to use them.

## v1.0.3 - 2024-11-06

- Conform to Lustre v4.6.0. Lustre v4.6.0 includes a fragment change internally,
  which is breaking Sketch Lustre. v1.0.3 adapts to that change.

## v1.0.2 - 2024-10-13

- Use correct SVG namespace for SVG elements. SVG elements were previously
  generated as HTML elements. They're now generated as
  `http://www.w3.org/2000/svg` namespaced elements.

## v1.0.1 - 2024-09-17

- Fix a bug where `fold_right` was producing a stack overflow on some JavaScript
  runtimes.

## v1.0.0 - 2024-08-05

Initial release of Sketch Lustre! That first version marks the first separation
of Sketch from a single, huge package, to a core package and companion packages,
specialized for various use cases.

Sketch Lustre includes a complete framework to make Sketch interacts with
Lustre, and provides an easy way to style any HTML node.
