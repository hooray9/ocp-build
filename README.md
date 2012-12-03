## To clone

    git clone git@github.com:OCamlPro/typerex.git
    cd typerex
    git checkout typerex2

## To build

Configure:

    ./configure -prefix /usr/local

Build:

    make

Install:

    make install

Build and read the documentation:

    make doc
    cd docs/user-manual
    evince user-manual.pdf


## To use it

    ocp-edit-mode install -emacs

If you want to customize your typerex config:

    emacs ~/.ocp/ocp-edit-mode.conf

