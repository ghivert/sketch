import gleam/float

pub opaque type Angle {
  Deg(Float)
  Rad(Float)
  Grad(Float)
  Turn(Float)
}

pub fn deg(value: Float) {
  Deg(value)
}

pub fn rad(value: Float) {
  Rad(value)
}

pub fn grad(value: Float) {
  Grad(value)
}

pub fn turn(value: Float) {
  Turn(value)
}

pub fn to_string(angle: Angle) {
  case angle {
    Deg(value) -> float.to_string(value) <> "deg"
    Rad(value) -> float.to_string(value) <> "rad"
    Grad(value) -> float.to_string(value) <> "grad"
    Turn(value) -> float.to_string(value) <> "turn"
  }
}
