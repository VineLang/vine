
export def main [snaps, dir, --write] {
  mut success = true
  let $snaps = open --raw $snaps | from json
  for entry in ($snaps | transpose path expected) {
    let path = $dir | path join $entry.path
    if ($path | path exists) {
      if (open $path) != (open $entry.expected) {
        if $write {
          open $entry.expected | save -f $path
          print $"(ansi green)updated(ansi reset) ($path)"
        } else {
          $success = false
          print $"(ansi red)invalid(ansi reset) ($path)"
        }
      }
    } else {
      if $write {
        mkdir ($path | path dirname)
        open $entry.expected | save -f $path
        print $"(ansi green)created(ansi reset) ($entry.path)"
      } else {
        $success = false
        print $"(ansi yellow)missing(ansi reset) ($entry.path)"
      }
    }
  }

  for path in (glob $"($dir)/**") {
    if ($path | path type) == "file" {
      let $path = $path | path relative-to ($dir | path expand)
      if not ($path in $snaps) {
        if $write {
          rm ($dir | path join $path)
          print $"(ansi green)removed(ansi reset) ($path)"
        } else {
          $success = false
          print $"(ansi yellow)unknown(ansi reset) ($path)"
        }
      }
    }
  }

  if not $success {
    exit 1
  }
}

