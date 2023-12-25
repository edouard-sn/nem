NAME	=	nem

ODINC	=	$(which odin)

all:
	odin build . -strict-style -vet -vet-style -vet-semicolon -error-pos-style:unix

debug:
	odin build . -o:none -debug
