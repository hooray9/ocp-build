# This Makefile will build all OCaml code, for development.
# Sub-projects may have their own Makefiles internally to build
# non-ocaml files and targets.

include Makefile.config

BOOTSTRAP_OCPBUILD=./boot/ocp-build.boot

OCPBUILD=./boot/ocp-build
OCPBUILD_FLAGS=

all: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -scan
init: $(OCPBUILD)
	$(OCPBUILD) root
	$(OCPBUILD_FLAGS) configure -scan
verbose: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -v 5
byte: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -byte
opt: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -asm
noscan: $(OCPBUILD)
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -no-scan

WIN32_FILES= \
  libs/ocplib-win32/win32_waitpids_c.c \
  libs/ocplib-win32/win32_fileinfo_c.c

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
	  -install-bundle typerex \
          -install-lib $(LIBDIR) \
          -install-bin $(BINDIR) \
          -install-data $(TYPEREXDIR)

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

#	mkdir -p $(TYPEREXDIR)
#	mkdir -p $(BINDIR)
#	$(foreach i,$(TO_INSTALL),cp _obuild/$(i)/$(i).asm $(BINDIR)/$(i);)
#	rm -rf $(TYPEREXDIR)/ocp-edit-mode
#	cp -PpR tools/ocp-edit-mode/files $(TYPEREXDIR)/ocp-edit-mode

#install-emacs:
#	cp tools/ocp-fix-errors/emacs/ocp-fix-errors.el $(HOME)/.emacs.d/

install-manager:
	sudo cp _obuild/ocaml-manager/ocaml-manager.asm /usr/bin/ocaml-manager
	sudo ocaml-manager -update

install-ocpbuild:
	sudo cp _obuild/ocp-build/ocp-build.asm /usr/local/bin/ocp-build

#
#  Building $(BOOTSTRAP_OCPBUILD) is difficult, because it must run
# with any version of ocamlrun. Currently, we remove dynamic dependencies
# from ocp-build and remove all unused primitives, using ocp-bytecode.
#

_obuild/ocp-bytecode/ocp-bytecode.byte \
_obuild/ocp-build/ocp-build.byte:
	$(OCPBUILD) -byte ocp-bytecode ocp-build

# We are happy with what we have generated, we just want it to be compiled
# with ocaml-3.12.1
upgrade-ocp-build:
	mv _obuild/ocp-build/ocp-build.asm boot/
	ocaml-manager -set ocaml-3.12.1
	./boot/ocp-build.asm -clean
	./boot/ocp-build.asm -byte ocp-build ocp-bytecode

# update boot/ with and check it works
bootstrap-ready: \
   _obuild/ocp-bytecode/ocp-bytecode.byte \
   _obuild/ocp-build/ocp-build.byte

old-ocp-build:
	OCAML_VERSION=ocaml-3.12.1 ocp-build -no-color -arch 3.12.1 ocp-build

bootstrap: old-ocp-build
	rm -rf Saved
	mv boot Saved
	mkdir boot
	mv Saved boot/Saved
	_obuild/ocp-bytecode/ocp-bytecode.byte _obuild/3.12.1/ocp-build/ocp-build.byte \
	   -make-static \
	   -filter-unused-prims \
	   -remove-prims tools/ocp-build/remove-primitives.txt \
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

fabrice-upload:
	git checkout fabrice-typerex
	git push origin fabrice-typerex
	git push ocamlpro fabrice-typerex

doc:
	cd docs/user-manual; $(MAKE)



configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf
	./configure $(CONFIGURE_ARGS)

tag:
	git tag typerex.$(VERSION)
	git push ocamlpro typerex.$(VERSION)

force_tag:
	git tag -f typerex.$(VERSION)
	git push -f ocamlpro typerex.$(VERSION)

opamize:
	$(MAKE) opamize-typerex
	$(MAKE) opamize-ocp-build
opamize-typerex:
	./_obuild/ocp-opamer/ocp-opamer.asm \
	 	-descr packages/opam/typerex.descr \
		-opam packages/opam/typerex.opam  \
		typerex $(VERSION) \
		https://github.com/OCamlPro/typerex/tarball/typerex.$(VERSION)

opamize-ocp-build:
	./_obuild/ocp-opamer/ocp-opamer.asm -branch typerex \
		-descr packages/opam/ocp-build.descr \
		-opam packages/opam/ocp-build.opam \
		 ocp-build $(VERSION) -no-url

