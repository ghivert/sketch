import gleam/float

pub opaque type Angle {
  Deg(Float)
  Rad(Float)
  Grad(Float)
  Turn(Float)
}

pub fn deg(value: Float) -> Angle {
  Deg(value)
}

pub fn rad(value: Float) -> Angle {
  Rad(value)
}

pub fn grad(value: Float) -> Angle {
  Grad(value)
}

pub fn turn(value: Float) -> Angle {
  Turn(value)
}

pub fn to_string(angle: Angle) -> String {
  case angle {
    Deg(value) -> float.to_string(value) <> "deg"
    Rad(value) -> float.to_string(value) <> "rad"
    Grad(value) -> float.to_string(value) <> "grad"
    Turn(value) -> float.to_string(value) <> "turn"
  }
}
