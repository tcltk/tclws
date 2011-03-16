#
# "make install" to copy all the stuff into a dir where Tcl can find it
#
# $Id$
#

TARGETDIR=/usr/local/lib/tclws

all:
	@echo "Use \"make install\" to deploy files."

install:
	mkdir -p $(TARGETDIR)
	cp -v *.tcl $(TARGETDIR)

