# Warning!

This repository uses git submodule, please clone with the --recursive option:

```shell
git clone --recursive https://******insert github path here**********
```

1. Introduction
---------------



2. License
----------

All files are provided under a 3-clause Berkeley Software Distribution (BSD)
license. As such, and within the conditions required by the license, the files
are available both for proprietary ("commercial") and non-proprietary use.

For details, see the "LICENSE" file in the root directory.


3. Requirements
---------------

The software is written in Ada 2012 and uses, for example, preconditions,
postconditions, and the high-level iterator form of for-loops.

In addition, a GNAT implementation-defined pragma is used extensively. This
pragma makes it possible to avoid explicit temporary copies when assigning
components of types representing hardware registers requiring full word or full
half-word accesses. The pragma is named Volatile_Full_Access. Those persons
wishing to submit additions to the library should see the GNAT Reference Manual
for details.

Therefore, building with the sources requires a compiler supporting both Ada
2012 and the GNAT-defined pragma Volatile_Full_Access. The "GNAT GPL 2016"
compiler for ARM ELF is one such compiler [(Download it
here)](http://libre.adacore.com/download/configurations). A recent GNAT Pro
compiler for that target will also suffice.


4. Content
----------


5. Roadmap
----------


6. Project using the Ada_Drivers_Library
----------------------------------------

 
(Add yours to the list!)