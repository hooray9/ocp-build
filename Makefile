# This Makefile will build all OCaml code, for development.
# Sub-projects may have their own Makefiles internally to build
# non-ocaml files and targets.

include Makefile.config

BOOTSTRAP_OCPBUILD=./boot/ocp-build.boot

OCPBUILD=./boot/ocp-build
OCPBUILD_FLAGS= -no-ocamlfind

all: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -scan
init: $(OCPBUILD)
	$(OCPBUILD) root
	$(OCPBUILD) configure -scan
verbose: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -v 5
byte: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -byte
opt: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -asm
noscan: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -no-scan

WIN32_FILES= \
  src/win32/win32_waitpids_c.c \
  src/win32/win32_fileinfo_c.c

boot/win32_c.c: $(WIN32_FILES)
	cat $(WIN32_FILES) > boot/win32_c.c

boot/ocp-build: boot/ocp-build.boot boot/win32_c.c
	$(MAKE) -C boot

scan: $(OCPBUILD)
	$(OCPBUILD) build -scan
sanitize: $(OCPBUILD)
	$(OCPBUILD) -sanitize
ocpbuild: $(OCPBUILD)
	$(OCPBUILD) ocp-build

clean-temps:

clean: clean-temps $(OCPBUILD)
	$(OCPBUILD) -clean
distclean: clean
	(cd boot; $(MAKE) clean)
	rm -f Makefile.config config.log
	rm -rf autom4te.cache ocp-build.root*

TO_INSTALL = ocp-build  ocp-build-infer-env

uninstall:
	$(OCPBUILD) -uninstall

installed:
	$(OCPBUILD) -installed

install:
	$(OCPBUILD) -install \
	  -install-bundle ocp-build-bundle \
          -install-lib $(LIBDIR) \
          -install-bin $(BINDIR) \
          -install-data $(TYPEREXDIR)

# Only for tests
install-destdir:
	$(OCPBUILD) install \
	  -install-destdir $(HOME)/typerex-root \
          -install-lib $(LIBDIR) \
          -install-bin $(BINDIR) \
          -install-data $(TYPEREXDIR)

uninstall-destdir:
	$(OCPBUILD) uninstall \
	  -install-destdir $(HOME)/typerex-root \
          -install-lib $(LIBDIR) 

install-ocpbuild:
	sudo cp _obuild/ocp-build/ocp-build.asm /usr/local/bin/ocp-build


doc:
	cd docs/user-manual; $(MAKE)



configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf
	./configure $(CONFIGURE_ARGS)

###########################################################################
#
#
#                           For OPAM
#
#
###########################################################################

## We need this tool installed to opamize ocp-build

OCP_OPAMER=ocp-opamer

push-tag:
	git push -f origin ocp-build.$(VERSION)

tag:
	git tag ocp-build.$(VERSION)
	$(MAKE) push-tag

force_tag:
	git tag -f ocp-build.$(VERSION)
	$(MAKE) push-tag

opamize:
	$(MAKE) opamize-ocp-build
opamize-ocp-build:
	$(OCP_OPAMER) \
	 	-descr opam/ocp-build.descr \
		-opam opam/ocp-build.opam  \
		ocp-build $(VERSION) \
		https://github.com/OCamlPro/ocp-build/tarball/ocp-build.$(VERSION)




###########################################################################
#
#
#                           For bootstrap
#
#
###########################################################################
OCP_BYTECODE=ocp-bytecode

#
#  Building $(BOOTSTRAP_OCPBUILD) is difficult, because it must run
# with any version of ocamlrun. Currently, we remove dynamic dependencies
# from ocp-build and remove all unused primitives, using ocp-bytecode.
#

_obuild/ocp-build/ocp-build.byte:
	$(OCPBUILD) -byte ocp-build

# We are happy with what we have generated, we just want it to be compiled
# with ocaml-3.12.1
upgrade-ocp-build:
	mv _obuild/ocp-build/ocp-build.asm boot/
	ocaml-manager -set ocaml-3.12.1
	./boot/ocp-build.asm -clean
	./boot/ocp-build.asm -byte ocp-build 

# update boot/ with and check it works
bootstrap-ready: \
   _obuild/ocp-build/ocp-build.byte

old-ocp-build:
	OCAML_VERSION=ocaml-3.12.1 ocp-build -no-color -arch 3.12.1 ocp-build

bootstrap: old-ocp-build
	$(OCP_BYTECODE) _obuild/3.12.1/ocp-build/ocp-build.byte \
	   -make-static \
	   -filter-unused-prims \
	   -remove-prims boot/remove-primitives.txt \
	   -o $(BOOTSTRAP_OCPBUILD)
	$(MAKE) clean
	$(MAKE) byte

# restore saved version of boot/
restore:
	mv boot/Saved Current
	mv boot boot-to-remove
	mv Current boot
	rm -rf boot-to-remove

# clean all old versions of boot/./
bootclean:
	rm -rf boot/Saved

make-boot:
	ocamlc -o $(BOOTSTRAP_OCPBUILD) -use-runtime  boot/ocp-build-runner \
	   -use-prims boot/prims_needed.txt \
	   unix.cma \
           ./_obuild/ocplib-lang/ocplib-lang.cma \
           ./_obuild/ocplib-system/ocplib-system.cma \
           ./_obuild/ocp-build-engine/ocp-build-engine.cma \
           ./_obuild/ocp-build-lib/ocp-build-lib.cma \
           ./_obuild/ocp-build/buildMain.cmo
