Ray Tracer Demonstration in Haskell
====
This is a Programming Languages final project raytracer in Haskell.

Author:
----
Kathryn McKay

Motivation:
----
- Implement octree/BSP/Adaptive space partitioning
- Take opprotunity to represent algorithms and mathematical expressions in a
    recognizable --> concise syntax.
- Compare the implementation of a ray tracer in C, C++, to that of a pure
    functional language.

Problem Description
----
The components of this application form a sequence of (1) the setup of the
scene parameters, (2) the calculations to trace rays, (3) the visual
representation of those calculations. Importantly, the last two steps of the
sequence concern the interpretation of data, suggesting a functional solution.

The situation being modelled is that of a scene containing objects being 
projected onto a two-dimensional screen, with an eye behind it. For each pixel
(x,y) on the screen, a ray is sent from the eye through that pixel to check
what object in the scene that pixel will be shown, if any, and to evaluate the
properies of that object at the point of the ray's intersection. This process
yields a colour value for the tested pixel, and when every pixel on the screen
has a colour an image has been successfully produced.

In order to demonstrate the calculations involved in tracing a ray, the
application will focus on presenting the process of finding the color of one
single pixel on the screen. This will involve:
  - The x,y position of the screen being tested.
  - What parts of the scene are being checked
  - What object is intersected by a ray, and where that intersection occurs
  - Shadow, Reflecting, and Refracting rays sent out at that intersection

To illustrate these interactions, the entire scene will be shown from an
orthogonal view, including the positions of the eye and screen.

Initially, a default scene will be loaded. The user will be able to adjust the
properties of the objects and lights, as well select the pixel to test. A
preview will be shown of the direction of the ray through the scene as a 
reference before the actual raytracing begins. When the scene is defined to
the user's satisfaction, the calculations will be performed and the results
displayed.
  
Language Selection Decision:
----
The language selected for this project is Haskell.
- Purely functional language
  - This project's focus is on the analysis of the known data - the scene
    and it's properties - to produce new data. This is well complemented by
    the functional style of describing rather than managing data.
- Syntax suitability
  - Mathematical expressions translate well/concisely to Haskell.
  --> ability to code the algorithms in a way that is recognizable when
    compared to the original expressions.
  - Ability to traverse list/sequences without for loop.

Design concept of object properties to display
----
- Rays
  - origin
  - direction
  - intersection point (could be none)
  - colour yielded
  - type of ray (shadow, root, reflection, refraction)
    - (visually distingush types using colours)
  - energy of ray
    - (starts at 1.00 and decays with each recursive call)
- Root ray
  - x,y coordinate of corresponding pixel on screen
- Refractive ray
  - whether it has broken in or out of the object
  - index of refraction before intersection
- Shadow ray
  - light object being tested
- Intersection Point
  - Object intersected
  - Intersection position
  - Rays generated
    - (could be true/false values, or point towards generated rays)
  - surface normal
    - (of side of object hit by ray)
- Objects
  - (all spheres)
  - radius
  - material properties
  - position
  - whether this object is hidden, i.e. is included in the scene/turned
      off/removed from calculations.
- Partition
  - contained objects
  - whether or not it was checked
    - (visually, the order of the partitions checked should be evident by the
       direction of the ray. Probably.)
- Light
  - (Point Light)
  - Position
  - Color
- Material
  - strength of ambience, diffuse, specular, reflection, refraction
  - Index of refraction
  - Color
  - name
