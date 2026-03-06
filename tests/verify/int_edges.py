#!/usr/bin/env python3

# @snap programs/int_edges/output.txt
# @reproduce

edges = [0, 1, 2**32 - 1]

for a_sign in [False, True]:
  for a0 in edges:
    for a1 in edges:
      a = a0 | a1 << 32
      if a_sign:
        a = -a

      for b_sign in [False, True]:
        for b0 in edges:
          for b1 in edges:
            b = b0 | b1 << 32
            if b_sign:
              b = -b

            print(f"{a} + {b} = {a + b}")
            print(f"{a} - {b} = {a - b}")
            print(f"{a} * {b} = {a * b}")
            if a < b:
              print(f"{a} < {b}")
            elif a > b:
              print(f"{a} > {b}")
            else:
              print(f"{a} = {b}")

      for b in [-0x7fffffff, -1, +1, +2, 0x7fffffff]:
        q, r = divmod(a, b)
        if (a < 0) != (b < 0) and r != 0:
          q += 1
          r -= b
        print(f"{a} / {b} = {q}")
        print(f"{a} % {b} = {r}")
        q_show = f"+{q}" if q > 0 else f"{q}"
        r_show = f"+{r}" if r > 0 else f"{r}"
        print(f"{a}.div_rem_i32({b}) = ({q_show}, {r_show})")
