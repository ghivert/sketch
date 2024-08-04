pub const content = "pub const main = \"main\"

pub const body = \"card body\"

pub const block = \"card block\"

pub const card_body = \"card-body\"

pub const card = \"card\""

pub const css = ".card {
  background: #ddd;
  background: red;
  display: block;
  padding: 12pt;
  padding: 12px;
  grid-template-areas:
    \"muf\"
    \"muf\";
  muf: mumuf;
}

.card:hover {
  background: var(--custom);
}

@media (max-width: 700px) {
  .card {
    background: blue;
  }
}

@media (max-width: 700px) or (min-width: 600px) {
  .card {
    background: blue;
  }
}

@media (max-width: 700px) or (min-width: 400px) {
  .card {
    background: blue;
  }

  .card:hover {
    background: red;
  }
}

.card-body {
  background: #ddd;
  background: red;
  display: block;
}

.block {
  background: #ccc;
  display: flex;
}

.body {

}

.main {
  background: #ddd;
}"
