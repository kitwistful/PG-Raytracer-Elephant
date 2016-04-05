USAGE:
====
ABSTRACT:
----
Write a script in haskell using library. Allows to set up scene width & height
and populate with objects. Then, can print out the information.

EXAMPLE OUTPUT:
====
DIAGRAM:
-----
y
.       │  ii │
.       │────(2)─────
.       │  iii│
.       │ *   │
.──────(1)───────────
.  i    │
.       │
.       │
0 . . . . . . . . . x

FOUND OBJECTS:
-----
Quadrant 1.2.3 x[2.000, 3.500), y[1.000, 3.000)
*              x: 2.100 y: 2.900
Object iii     x: 3.100 y: 0.750

QUADTREE:
-----
(1)
├─ x[2.000, 5.000), y[0.000, 3.000)
│                 └ (2)
│                   ├─ x[2.000, 3.500), y[0.000, 1.000)
│                   │                   └─ Object ii
│                   └─ x[2.000, 3.500), y[1.000, 3.000)
│                                       └─ Object iii
└─ x[0.000, 2.000), y[3.000, 5.000)
                    └─ Object i

OBJECTS:
----
*             x: 2.100 y: 2.900
Object i      x: 0.500 y: 3.250
Object ii     x: 3.000 y: 0.500
Object iii    x: 3.100 y: 0.750