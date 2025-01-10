//// `Angle` defines a [CSS Unit](https://developer.mozilla.org/docs/Web/CSS/CSS_Values_and_Units).
//// It represents an angle value expressed in degrees, gradians, radians, or turns.
//// It is used, for example, in `<gradient>`s and in some `transform` functions.
////
//// ---
////
//// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/angle)

import gleam/float

/// `Angle` defines a [CSS Unit](https://developer.mozilla.org/docs/Web/CSS/CSS_Values_and_Units).
/// It represents an angle value expressed in degrees, gradians, radians, or turns.
/// It is used, for example, in `<gradient>`s and in some `transform` functions.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/angle)
pub opaque type Angle {
  Deg(Float)
  Rad(Float)
  Grad(Float)
  Turn(Float)
}

/// Represents an angle in [degrees](https://en.wikipedia.org/wiki/Degree_%28angle%29).
/// One full circle is `360deg`. Examples: `0deg`, `90deg`, `14.23deg`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/angle#deg)
pub fn deg(value: Float) -> Angle {
  Deg(value)
}

/// Represents an angle in [radians](https://en.wikipedia.org/wiki/Radian).
/// One full circle is 2π radians which approximates to `6.2832rad`. `1rad` is
/// 180/π degrees. Examples: `0rad`, 1`.0708rad`, `6.2832rad`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/angle#rad)
pub fn rad(value: Float) -> Angle {
  Rad(value)
}

/// Represents an angle in [gradians](https://en.wikipedia.org/wiki/Gradian).
/// One full circle is `400grad`. Examples: `0grad`, `100grad`, `38.8grad`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/angle#grad)
pub fn grad(value: Float) -> Angle {
  Grad(value)
}

/// Represents an angle in a number of turns. One full circle is `1turn`.
/// Examples: `0turn`, `0.25turn`, `1.2turn`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/angle#turn)
pub fn turn(value: Float) -> Angle {
  Turn(value)
}

@internal
pub fn to_string(angle: Angle) -> String {
  case angle {
    Deg(value) -> float.to_string(value) <> "deg"
    Rad(value) -> float.to_string(value) <> "rad"
    Grad(value) -> float.to_string(value) <> "grad"
    Turn(value) -> float.to_string(value) <> "turn"
  }
}
