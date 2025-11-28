
tree-sitter generate

let success = (
  ls
    ../../root/**/*.vi
    ../../vine/examples/*.vi
    ../../tests/programs/*.vi
    ../../tests/programs/aoc_2024/*.vi
  | each {
    let path = $in.name
    let success = tree-sitter parse $path | complete | $in.exit_code == 0
    let status = if $success { $'(ansi green)success(ansi reset)' } else { $'(ansi red)failure(ansi reset)' }
    print $'($status) - ($path)'
    $success
  }
  | collect
  | all { $in }
)

if not $success {
  exit 1
}
 
