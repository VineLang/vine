#import "/lib.typ": *

= Enums <enums>

An enum is a type with a fixed set of _variants_. An enum value is one of those
variants.

```vi
enum Weekday {
  Monday,
  Tuesday,
  Wednesday,
  Thursday,
  Friday,
}

let day = Weekday::Friday; // day: Weekday

day is Weekday::Monday // false
day is Weekday::Friday // true

let mood = match day {
  Weekday::Monday { ":(" }
  Weekday::Friday { ":)" }
  _ { ":|" }
};
mood // ":)"
```

Like structs, enum variants can wrap content:

```vi
enum Shape {
  Point,
  Circle(F32),
  Rect({ width: F32, height: F32 }),
}

let x = Shape::Point; // x: Shape
let y = Shape::Circle(1.0); // y: Shape
let z = Shape::Rect({ width: 4.0, height: 6.0 }); // z: Shape
```

Pattern matching can be used to branch on the variant of the enum and access its
content.

```vi
fn perimeter(shape: Shape) -> F32 {
  match shape {
    Shape::Point {
      0.0
    }
    Shape::Circle(radius) {
      6.28 * radius
    }
    Shape::Rect({ width, height }) {
      2.0 * (width + height)
    }
  }
}

perimeter(Shape::Point) // 0.0
perimeter(Shape::Circle(1.0)) // 6.28
perimeter(Shape::Rect({ width: 4.0, height: 6.0 })) // 20.0
```

Pattern matching is discussed further in the @patterns[Patterns] section.

== Standard Enums

The standard library defines the enum types #ty[`Option[T]`] and #ty[`Result[T, E]`]:

```vi
enum Option[T] {
  Some(T),
  None,
}

enum Result[T, E] {
  Ok(T),
  Err(E),
}
```
