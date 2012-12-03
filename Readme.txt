To clone:
---------

0/ 

    git clone git@github.com:OCamlPro/typerex.git
    cd typerex
    git checkout typerex2

To build:
---------

1/ configure

   ./configure -prefix /usr/local

2/ Build

    make

4/ Install

   make install

5/ Build and read the documentation

   make doc
   cd docs/user-manual
   evince user-manual.pdf


To use it
---------

    ocp-edit-mode install -emacs

If you want to customize your typerex config:

    emacs ~/.ocp/ocp-edit-mode.conf
   
