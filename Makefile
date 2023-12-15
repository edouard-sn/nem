NAME	=	nem

ODINC	=	$(which odin)

all:
	odin build .

debug:
	odin build . -o:none -debug
