# Methods

Any function can be marked as a *method* by writing a `.` before the name in the
definition:

```rs
fn .sum(list: List[N32]) -> N32 {
  let sum = 0;
  while list.pop_front() is Some(value) {
    sum += value;
  }
  sum
}
```

Methods must take at least one argument, called the *receiver*. Methods can
still be called like normal functions, but they can also be called using method
syntax:

```rs
[1, 2, 3].sum() // 6
```

A method call can refer to any *method candidate* with the appropriate receiver
types. Method candidates include:

- any method that is in scope
- any method defined in the module of the receiver type

This means that if you define a custom type, and declare methods in its module,
anything using that type can call those methods, without needing to explicitly
import them:

```rs
enum Shape {
  Circle(F32),
  Rect({ width: F32, height: F32 }),
}

mod Shape {
  pub fn .perimeter(shape: Shape) -> F32 {
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
  pub fn .scale(&shape: &Shape, factor: F32) {
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
