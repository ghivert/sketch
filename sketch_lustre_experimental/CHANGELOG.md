## v0.1.0 - Unreleased

Initial release of Sketch Lustre Experimental! That experimental version tries a
new approach for Sketch Lustre, by hiding completely the `sketch.StyleSheet`,
while still creating plain `lustre/element.Element`, and no
`sketch/lustre/element.Element`. This should solve some problems of relying on
Lustre internals to make `sketch_lustre` viable, and help to integrate Sketch in
any Lustre application.

As an experimental package, this should be tried with caution, and kept in mind
that a bug could be hiding inside if used in production.
