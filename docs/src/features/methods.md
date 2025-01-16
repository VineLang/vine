# Methods

User-defined types can have *methods*, functions defined in the type's module
that take the type as their first parameter.

```rs
enum Shape {
  Circle(F32),
  Rect { width: F32, height: F32 },
}

mod Shape {
  pub fn perimeter(shape: Shape) -> F32 {
    match shape {
      Shape::Circle(radius) {
        6.28 * radius
      }
      Shape::Rect({ width, height }) {
        2.0 * (width + height)
      }
    }
  }
}

let shape = Shape::Circle(1.0);
shape.perimeter() // 6.28
```

The first parameter of a methods can also be a [reference](./references.md),
allowing the method to mutate the value it is called upon.

```rs
mod Shape {
  pub fn scale(&shape: &Shape, factor: F32) {
    match &shape {
      &Shape::Circle(radius) {
        radius *= factor;
      }
      &Shape::Rect({ width, height }) {
        width *= factor;
        height *= factor;
      }
    }
  }
}

let shape = Shape::Rect({ width: 4.0, height: 6.0 });
shape.perimeter() // 20.0
shape.scale(2.3);
shape.perimeter() // 46.0
```
