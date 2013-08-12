toy-interface
=============

This is a simple, generic interface for making applications which use the
mouse and keyboard.  These events are transformed into nicer datatypes,
making it easy to ignore the details of the backend's event datatypes.
Coupled with a backend-independent rendering system such as diagrams, this
allows for backend-independent applications to be written.

Backends
--------

1. [GTK backend](https://github.com/mgsloan/toy-gtk)

Why "toy"?
----------

The name \"toy\" comes from the \"toy framework\", a part of the
[lib2geom](http://lib2geom.sourceforge.net) library. It's used in building
"toys" demonstrating the features of the library.  This is a different variety
of "TDD" - but instead of tests, it's toys! We found that building little demos
to be a nice way to drive initial design / development.

Installation
------------

If you want to build the latest repository versions of the toy packages, use
[toy-sources](https://github.com/mgsloan/toy-sources), or manually fetch /
build the repositories.
