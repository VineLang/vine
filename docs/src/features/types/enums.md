# Enums

An enum is a type with a fixed set of *variants*. An enum value is one of those
variants.

```rs
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

Enum variants can have associated fields in the form of a tuple or an object.

```rs
enum Shape {
  Point,
  Circle(F32),
  Rect { width: F32, height: F32 },
}

let x = Shape::Point; // x: Shape
let y = Shape::Circle(1.0); // y: Shape
let z = Shape::Rect({ width: 4.0, height: 6.0 }); // z: Shape
```

Pattern matching can be used to branch on the variant of the enum and access its
fields.

```rs
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

Pattern matching is discussed further in the [Patterns](../patterns.md) section.

## Standard Enums

The standard library defines the enum types `Option[T]` and `Result[T, E]`:

```rs
enum Option[T] {
  Some(T),
  None,
}

enum Result[T, E] {
  Ok(T),
  Err(E),
}
```
