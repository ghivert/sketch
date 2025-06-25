import sketch/css/length

pub fn sketch_test() {
  // describe("Sketch", [
  //   describe("CSS dimensions", [
  //     describe("length", [
  //       it("should handle px", fn() {
  length.px(10)
  |> expect_length("10.0px")
  // }),
  // it("should handle px_", fn() {
  length.px_(10.0)
  |> expect_length("10.0px")
  // }),
  // it("should handle cm", fn() {
  length.cm(10)
  |> expect_length("10.0cm")
  // }),
  // it("should handle cm_", fn() {
  length.cm_(10.0)
  |> expect_length("10.0cm")
  // }),
  // it("should handle mm", fn() {
  length.mm(10)
  |> expect_length("10.0mm")
  // }),
  // it("should handle mm_", fn() {
  length.mm_(10.0)
  |> expect_length("10.0mm")
  // }),
  // it("should handle q", fn() {
  length.q(10)
  |> expect_length("10.0q")
  // }),
  // it("should handle q_", fn() {
  length.q_(10.0)
  |> expect_length("10.0q")
  // }),
  // it("should handle in", fn() {
  length.in(10)
  |> expect_length("10.0in")
  // }),
  // it("should handle in_", fn() {
  length.in_(10.0)
  |> expect_length("10.0in")
  // }),
  // it("should handle pc", fn() {
  length.pc(10)
  |> expect_length("10.0pc")
  // }),
  // it("should handle pc_", fn() {
  length.pc_(10.0)
  |> expect_length("10.0pc")
  // }),
  // it("should handle pt", fn() {
  length.pt(10)
  |> expect_length("10.0pt")
  // }),
  // it("should handle pt_", fn() {
  length.pt_(10.0)
  |> expect_length("10.0pt")
  // }),
  // it("should handle vh", fn() {
  length.vh(10)
  |> expect_length("10.0vh")
  // }),
  // it("should handle vh_", fn() {
  length.vh_(10.0)
  |> expect_length("10.0vh")
  // }),
  // it("should handle vw", fn() {
  length.vw(10)
  |> expect_length("10.0vw")
  // }),
  // it("should handle vw_", fn() {
  length.vw_(10.0)
  |> expect_length("10.0vw")
  // }),
  // it("should handle em", fn() {
  length.em(10.0)
  |> expect_length("10.0em")
  // }),
  // it("should handle rem", fn() {
  length.rem(10.0)
  |> expect_length("10.0rem")
  // }),
  // it("should handle lh", fn() {
  length.lh(10.0)
  |> expect_length("10.0lh")
  // }),
  // it("should handle rlh", fn() {
  length.rlh(10.0)
  |> expect_length("10.0rlh")
  // }),
  // it("should handle ch", fn() {
  length.ch(10.0)
  |> expect_length("10.0ch")
  // }),
  // it("should handle pct", fn() {
  length.percent(10)
  |> expect_length("10.0%")
  // }),
  // it("should handle cap", fn() {
  length.cap(10.0)
  |> expect_length("10.0cap")
  // }),
  // it("should handle ex", fn() {
  length.ex(10.0)
  |> expect_length("10.0ex")
  // }),
  // it("should handle ic", fn() {
  length.ic(10.0)
  |> expect_length("10.0ic")
  // }),
  // it("should handle rcap", fn() {
  length.rcap(10.0)
  |> expect_length("10.0rcap")
  // }),
  // it("should handle rch", fn() {
  length.rch(10.0)
  |> expect_length("10.0rch")
  // }),
  // it("should handle rex", fn() {
  length.rex(10.0)
  |> expect_length("10.0rex")
  // }),
  // it("should handle ric", fn() {
  length.ric(10.0)
  |> expect_length("10.0ric")
  // }),
  // it("should handle vmax", fn() {
  length.vmax(10)
  |> expect_length("10.0vmax")
  // }),
  // it("should handle vmin", fn() {
  length.vmin(10)
  |> expect_length("10.0vmin")
  // }),
  // it("should handle vb", fn() {
  length.vb(10)
  |> expect_length("10.0vb")
  // }),
  // it("should handle vi", fn() {
  length.vi(10)
  |> expect_length("10.0vi")
  // }),
  // it("should handle cqw", fn() {
  length.cqw(10)
  |> expect_length("10.0cqw")
  // }),
  // it("should handle cqh", fn() {
  length.cqh(10)
  |> expect_length("10.0cqh")
  // }),
  // it("should handle cqi", fn() {
  length.cqi(10)
  |> expect_length("10.0cqi")
  // }),
  // it("should handle cqb", fn() {
  length.cqb(10)
  |> expect_length("10.0cqb")
  // }),
  // it("should handle cqmin", fn() {
  length.cqmin(10)
  |> expect_length("10.0cqmin")
  // }),
  // it("should handle cqmax", fn() {
  length.cqmax(10)
  |> expect_length("10.0cqmax")
  // }),
  // ]),
  //   ]),
  // ])
}

fn expect_length(length: length.Length, result: String) {
  assert length.to_string(length) == result
}
