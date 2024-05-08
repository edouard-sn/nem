NAME	=	nem

ODINC	=	`which odin`

VET_FLAGS	=	-strict-style -vet -vet-style -vet-semicolon
ERROR_FLAGS	=	-error-pos-style:unix

RELEASE_FLAGS =	-disable-assert -no-bounds-check

DEBUG_FLAGS	=	-debug -o:none

all: release
all: debug

release:
	$(ODINC) build . $(VET_FLAGS) $(ERROR_FLAGS) $(RELEASE_FLAGS)  -out:$(NAME) 

debug:
	$(ODINC) build . $(DEBUG_FLAGS) $(ERROR_FLAGS) -out:$(NAME)-debug

update_test_files:
	# Get/Update test roms
	git submodule update --init --recursive


tests: update_test_files
	# Run tests
	$(ODINC) test . -all-packages 

clean:
	rm -rf $(NAME) $(NAME)-debug