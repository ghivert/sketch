//// Defines standard error returned by sketch.

import gleam/otp/actor

/// Standard Sketch Error. Used on BEAM target. JS target never fails.
pub type SketchError {
  OtpError(actor.StartError)
}
