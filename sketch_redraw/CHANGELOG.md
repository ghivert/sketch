## v3.3.2 - 2025-12-26

- Fix JS error.

## v3.3.1 - 2025-12-24

- Use latest Gleam external API.

## v3.3.0 - 2025-09-22

- Add `hooks` namespace to allow more flexibility in component generation.

## v3.2.1 - 2025-09-11

- Fix `sketch_redraw` overriding existing CSS class.

## v3.2.0 - 2025-09-08

- Exposes `use_class_name` to generate a class name dynamically.

## v3.1.1 - 2025-09-02

- Correctly handle void elements.

## v3.1.0 - 2025-09-01

- Exposes `initialise_cache` to provide a way to modify the Sketch stylesheet
  before insertion in the provider.

## v3.0.0 - 2025-08-31

- Upgrades to Redraw 19.
- Change initialisation with `create_cache`.

## v2.0.0 - 2025-01-12

v2.0.0 marks a breaking change with the new Sketch release (i.e. v4.0.0). Sketch
Redraw should now be used in conjuction with `redraw_dom` exclusively.

## Improvements

- Every HMTL element now has MDN Reference links & fragment of description to
  explain how to use them.
