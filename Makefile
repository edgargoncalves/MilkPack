# Define required macros here
SHELL = /bin/sh

SETUP_FORM = "(load \"./MilkPack.lisp\")"
ARGS = -b -e ${SETUP_FORM}

all: app

app: resources
	${CCL} ${ARGS}

resources:
	cp ./resources/Info.plist MilkPack.app/Contents/
	cp ./resources/english/* MilkPack.app/Contents/Resources/English.lproj/
	cp ./resources/img/* MilkPack.app/Contents/Resources/
	touch MilkPack.app

clean:
	-rm -f *.app
