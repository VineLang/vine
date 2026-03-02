#!/usr/bin/env python3

# @snap programs/nat_edges/output.txt
# @reproduce

edges = [0, 1, 2**32 - 2, 2**32 - 1]

for a0 in edges:
  for a1 in edges:
    for a2 in edges:
      a = a2 << 64 | a1 << 32 | a0

      for b0 in edges:
        if b0 != 0:
          print(f"{a}.div_rem_n32({b0}) = ({a // b0}, {a % b0})")

        for b1 in edges:
          for b2 in edges:
            b = b2 << 64 | b1 << 32 | b0
            print(f"{a} + {b} = {a + b}")
            print(f"{a} - {b} = {max(a - b, 0)}")
            print(f"{a} * {b} = {a * b}")
            if b != 0:
              print(f"{a}.div_rem({b}) = {divmod(a, b)}")

edges = [0, 2**32 - 1]
for a0 in edges:
  for a1 in edges:
    a = a1 << 32 | a0
    for b0 in edges:
      for b1 in edges:
        b = b1 << 32 | b0
        print(f"{a} & {b} = {a & b}")
        print(f"{a} | {b} = {a | b}")
        print(f"{a} ^ {b} = {a ^ b}")
