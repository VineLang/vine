
let files = (ls vine/examples/*.vi tests/programs/*.vi tests/aoc_2024/*.vi | get name)

$files | each {path basename | str replace ".vi" "" | $"#($in)::main\(&io);"} | str join ' ' | $"pub fn main\(&io: &IO) { ($in) }" | save -f omnivy.vi

(vine build
  omnivy.vi
  ...($files | each {[--lib $in]} | flatten)
  --target none
)

rm omnivy.vi

