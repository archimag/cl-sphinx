.. _installation:

Installation
============

Implementation-specific notes
-----------------------------

cl-libxml2 tested with sbcl and clisp on Gentoo Linux (with use gentoo-lisp-overlay).

Requirements
------------

cl-libxml2 needs:

* libxml2
* libxslt (optional, necessary for xslt support)
* cffi (=> 0.10.2)
* lift (optional, for tests)
* puri
* iterate
* flexi-streams
* metabang-bind
* garbage-pools (=> 0.1.1)

Download
--------

| Download tarballs 
| Or get it from git:
::

  $ git clone git://github.com/archimag/cl-libxml2.git

Build C helper library
----------------------

libxslt for error handling require callback functions that called with a varying
number of arguments of varying types (see xsltSetGenericErrorFunc). cffi is do not
support it. Thus, C helper library are required for translate libxslt errors to lisp
conditions. The helper library reside in the directory foreign.
::

  $ cd /path/to/cl-libxml2
  $ make -C foreign
  $ make -C foreign install

Compilation and loading
-----------------------

Register the .asd file, e.g. by symlinking it:
::

  $ ln -sf `pwd`/cl-libxml2/cl-libxml2.asd /path/to/your/registry/
  $ ln -sf `pwd`/cl-libxml2/cl-libxslt.asd /path/to/your/registry/
  $ ln -sf `pwd`/cl-libxml2/xfactory.asd /path/to/your/registry/

The compile cl-libxlm2 without xslt using:

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :cl-libxml2)

Or with xslt using

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :cl-libxslt)

The compile xfactory system:

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :xfactory)

Run test suite (optional)
-------------------------

The test suite can be executed using the asdf test-op operator. If cl-libxml2
has not been loaded with asdf:load-op, the asdf:test-op operator will automatically
load cl-libxml2.

Run tests without xslt:

.. code-block:: common-lisp

  (asdf:operate 'asdf:test-op :cl-libxml2)

With xslt:

.. code-block:: common-lisp

  (asdf:operate 'asdf:test-op :cl-libxslt)

Run tests for xfactory system:

.. code-block:: common-lisp

  (asdf:operate 'asdf:test-op :xfactory)

