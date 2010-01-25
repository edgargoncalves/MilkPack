# Define required macros here
SHELL = /bin/sh

SETUP_FORM = "(load \"./MilkPack.lisp\")"
ARGS = -b -e ${SETUP_FORM}

all: app

app:
	${CCL} ${ARGS}

clean:
	-rm -f *.app
