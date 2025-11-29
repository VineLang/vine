#import "/lib.typ": *

= Debug Mode <debug>

Vine programs can be run in debug mode by passing the `--debug` flag.

In debug mode, any function can log to the console by calling #expr[`debug::log`].

```vi
// add.vi

pub fn main(&io: &IO) {
  io.println("12 + 34 = {add(12, 34)}");
}

fn add(x: N32, y: N32) -> N32 {
  debug::log("adding {x} and {y}");
  let result = x + y;
  debug::log("calculated {result}");
  result
}
```

```
$ vine run add.vi --debug
[add.vi:8:3] adding 12 and 34
[add.vi:10:3] calculated 46
12 + 34 = 46
```

When not running in debug mode, #expr[`debug::log`] is a no-op:
```
$ vine run add.vi
12 + 34 = 46
```


Errors can be reported with #expr[`debug::error`], which will print the error message along with a backtrace:

```vi
// divide.vi

pub fn main(&io: &IO) {
  io.println("1 / 0 = {divide(1, 0)}")
}

fn divide(x: N32, y: N32) -> N32 {
  if y == 0 {
    return debug::error("attempted to divide by zero");
  }
  x / y
}
```
```
$ vine run divide.vi --debug
ERROR: attempted to divide by zero
  @ #divide::divide (divide.vi:9:12)
  @ #divide::main (divide.vi:4:24)

Error: the net did not return its `IO` handle
```

When not running in debug mode, only the runtime's error will be printed:
```
$ vine run divide.vi

Error: the net did not return its `IO` handle
```
