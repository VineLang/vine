# Statistics

The IVM collects various statistics during execution. The Vine and Ivy CLIs
display these statistics after running a program.

Let's take a look at a sample stat block, section by section.

### Interactions

```
Interactions
  Total           11_106_946
  Annihilate       4_319_083
  Commute             33_750
  Copy             2_378_685
  Erase              315_132
  Expand             556_910
  Call             3_227_767
  Branch             275_619
```

`Total` shows the total number of interactions performed in the execution of the
program. This measures how much "work" was done, in a deterministic and
universal way; the same program will always have the exact same interaction
count, no matter what machine it is run on.

The other numbers break down the
[various types of interactions](./ivy/ivm-system.md).

### Memory

```
Memory
  Heap                   784 B
  Allocated      242_850_688 B
  Freed          242_850_688 B
```

`Heap` shows the greatest size of the runtime's heap over the course of the
program, measured in bytes. In this case, it never needed to use more than a
kilobyte of heap space throughout the entire program.

`Allocated` shows the total number of bytes allocated through the program, and
`Freed` shows the total number of bytes freed. These numbers should always
match; if they differ, that indicates that there was a vicious circle in the
interaction net.

In this case, `Allocated` and `Freed` are much greater than `Heap`; this shows
that the evaluator was reusing its memory buffer very effectively. (This is very
common for the IVM.)

These numbers are deterministic when the program is executed sequentially, but
can vary when executed in parallel (since the OS's thread scheduling is
non-deterministic).

### Performance

```
Performance
  Time                   175 ms
  Speed           63_375_794 IPS
```

`Time` is the amount of time elapsed over the execution of the program.

`Speed` is the speed of the execution, measured in IPS (interactions per
second), and equal to `Interactions / Time`.

## Parallel Statistics

Some statistics only apply to parallel execution.

### Workload

```
Workload
  Workers                 16
  Active                  16
  Minimum            312_096
  Average            694_184
  Maximum            855_071
  Moved                  111
```

`Workers` is the number of worker threads available to the IVM.

`Active` is the number of worker threads that were used.

`Minimum`, `Average`, and `Maximum` describe the statistics of the number of
interactions performed by each active worker.

`Moved` is the number of active pairs that were moved between workers.

### Performance

```
Performance
  Time                    19 ms
  Speed          569_167_395 IPS
  Working                230 ms
  Rate            48_201_985 IPS
```

`Time` and `Speed` are the same as in sequential execution.

`Working` is the total of the amounts of time each worker was active.

`Rate` is the average speed of an individual worker, and equal to
`Interactions / Working`.
