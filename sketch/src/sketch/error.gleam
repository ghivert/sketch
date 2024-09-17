//// Defines standard error returned by sketch.

import gleam/otp/actor

/// NotABrowser is unused nowadays. It will be removed in v4.
pub type SketchError {
  NotABrowser
  OtpError(actor.StartError)
}
