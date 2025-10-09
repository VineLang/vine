#import "/lib.typ": *

= `Hello, world!` <hello-world>

Let's run our first Vine program!

```vi
// vine/examples/hello_world.vi

pub fn main(&io: &IO) {
  io.println("Hello, world!");
}
```

We can run this with:

```nu
vine run vine/examples/hello_world.vi
```

You should see something like the following:

```iv
Hello, world!

Interactions
  Total                  358
  Annihilate             185
  Commute                  0
  Copy                    27
  Erase                   54
  Expand                  37
  Call                    41
  Branch                  14

Memory
  Heap                   720 B
  Allocated            7_440 B
  Freed                7_440 B

Performance
  Time                     0 ms
  Speed            6_979_645 IPS
```

At the top, notice that it successfully printed `Hello, world!`.

At the bottom, you'll also see several @statistics[_statistics_] printed out.
You don't need to worry about them for now.
If you want to disable them, you can use `--no-stats`.

Let's break down what just happened.

#let p0 = vi_.with("", "() {}")
#let p1 = vi_.with("", " fn main() {}")
#let p2 = vi_.with("fn main", "{}")
#let p3 = vi_.with("fn main()", "")
#let p4 = vi_.with("", "()")
#let p5 = vi_.with("io.println", "")

- #p0[`fn main`]: declares a function, named #fn[`main`]
  - #p1[`pub`]: makes it public
  - #p2[`(&io: &IO)`]: it takes a single parameter:
    - #ty[`&IO`]: of type #ty[`&IO`]
    - #pat[`&io`]: to be dereferenced and bound to the variable #pat[`io`]
  - #p3[`{ ... }`]: in the function body
    - #p4[`io.println`]: uses the #fn[`println`] method on #ty[`IO`]
    - #p5[`("Hello, world!")`]: calls it with the string #vstr[`Hello, world!`]

We'll go into this in more detail in future chapters.

Every Vine program must contain a #vi[`pub fn main(&io: &IO) { ... }`]
  ("a public function `main` that takes a reference to IO").
