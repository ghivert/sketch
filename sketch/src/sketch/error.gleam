//// Defines standard error returned by sketch.

import gleam/otp/actor

pub type SketchError {
  OtpError(actor.StartError)
}
