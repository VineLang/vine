
export def main [--vine: path, --test: path, --root: path, --timeout = 5sec, --debug] {
  let root = $root | default { $vine | path dirname | path dirname | path join lib root } | path expand
  let tmp = mktemp -d
  let entrypoints = [($tmp | path join '*.vi')]
  let replace_uri = { |method| replace_uri $method $root $tmp }

  let main_id = job id
  let buffer_id = spawn {
    mut buf = ""
    loop {
      let parsed = $buf | parse --regex '(?s)^Content-Length: (?<len>\d+)\r\n\r\n(?<rest>.*)$' | get 0?
      let len = $parsed.len?  | default { -1 } | into int
      let rest = $parsed.rest? | default ""
      if $len >= 0 and ($rest | str length) >= $len {
        let msg = $rest | str substring 0..<$len
        $"Content-Length: ($len)\r\n\r\n($msg)" | peek $debug | job send $main_id
        $buf = ($rest | str substring $len..)
      } else {
        $buf += job recv --timeout $timeout
      }
    }
  }
  let lsp_id = spawn {
    0..
    | each { job recv --timeout $timeout | peek $debug }
    | str join ''
    | ^$vine lsp ...$entrypoints
    | each { job send $buffer_id }
  }

  let send = { do $replace_uri send | jsonrpc encode | job send $lsp_id }
  let recv = { job recv --timeout $timeout | jsonrpc decode | do $replace_uri recv }

  mut log = []
  for step in (open $test) {
    if send in $step {
      $step.send | do $send
      $log ++= [{ send: $step.send }]
    }
    if recv in $step {
      mut got = do $recv
      $log ++= [{ recv: $got }]
      if not (matches $got $step.recv) {
        error make { msg: $"expected:\n($step.recv | to yml)\n\ngot:\n($got | to yml)" }
      }
    }
    if wait in $step {
      loop {
        let got = try {
          do $recv
        } catch {
          error make { msg: $"waited for but never arrived:\n($step.wait | to yml)" }
        }
        $log ++= [{ recv: $got }]
        if (matches $got $step.wait) {
          break
        }
      }
    }
    if write in $step {
      $step.write | transpose path content | each {|entry| $entry.content | save -f ($tmp | path join $entry.path)}
    }
    if run in $step {
      tmp=$tmp ^bash -c $step.run 
    }
  }

  let got = try { do $recv }
  if $got != null {
    error make { msg: $"unexpected:\n($got | to yml)" }
  }

  $log | to yml
}

def 'jsonrpc encode' [] : record -> string {
  let json = $in | insert jsonrpc "2.0" | to json
  let len = $json | str length
  $"Content-Length: ($len)\r\n\r\n($json)"
}

def 'jsonrpc decode' [] : string -> record {
  # TODO(enricozb): check length
  $in | split row "\r\n\r\n" | get 1 | from json | reject jsonrpc
}

def spawn [closure] {
  job spawn { do --ignore-errors $closure }
}

def matches [got: record, expected: record] {
  if "$exact" in $expected {
    $got == ($expected | reject "$exact")
  } else {
    $got == ($got | merge deep $expected)
  }
}

def replace_uri [method: string, root: path, tmp: path]: record -> record {
  let record = $in | to json
  match $method {
    "send" => (
      $record
      | str replace "<root>" $root
      | str replace "<tmp>" $tmp
    )
    "recv" => (
      $record
      | str replace $root "<root>"
      | str replace $tmp "<tmp>"
    )
  }
  | from json
}

def peek [debug:bool] : any -> any {
  if ($debug) {
    print $"peek: ($in)"
  }
  $in
}
