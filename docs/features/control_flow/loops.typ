#import "/lib.typ": *

= Loops

== `while`

The #vi[`while`] loop repeatedly executes its block
  as long as its @conditions[condition] is #vi[`true`].

```vi
let i = 0;
while i < 6 {
  io.println("{i}");
  i += 1;
}
```
```
0
1
2
3
4
5
```

== `for`

The #vi[`for`] loop iterates over the values of an iterator.
Ranges (written with #op[`..`]) are one type of iterator.

```vi
for i in 0..6 {
  io.println("{i}");
}
```
```
0
1
2
3
4
5
```

Many collections, such as @list[lists], can also be iterated over using their #fn[`.iter()`] method.
```vi
let messages = ["hello", "how are you?", "goodbye"];
for message in messages.iter() {
  io.println(message);
}
```
```
hello
how are you?
goodbye
```

== `break` & `continue`

Loops can have their execution stopped early with #vi[`break`]:

```vi
for i in 0..6 {
  io.println("{i}");
  if i == 3 {
    break;
  }
}
```
```
0
1
2
3
```

#vi[`continue`] causes execution to skip to the next iteration:
```vi
for i in 0..6 {
  // skip even numbers
  if i % 2 == 0 {
    continue;
  }
  io.println("{i}");
}
```
```
1
3
5
```

== `loop`

#vi[`loop`] creates a loop without specifying a condition to repeat it.
By default, it will execute only once;
  to repeat the loop, #vi[`continue`] must be used.

```vi
let i = 0;
loop {
  io.println("{i}");
  i += 1;
  if i < 6 {
    continue;
  }
}
```
```
0
1
2
3
4
5
```

