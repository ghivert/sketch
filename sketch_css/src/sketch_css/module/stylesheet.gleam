import glance as g
import sketch/css
import sketch/size

pub type Value {
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  SizeValue(size.Size)
}

pub type CSS {
  CSS(environment: List(#(String, Value)))
}

pub fn convert(
  modules: List(#(String, CSS)),
  module: #(String, g.Module),
) -> List(#(String, CSS)) {
  todo
}
