NAME	=	nem

ODINC	=	`which odin`

VET_FLAGS	=	-strict-style -vet -vet-style -vet-semicolon
ERROR_FLAGS	=	-error-pos-style:unix

RELEASE_FLAGS =	-disable-assert -no-bounds-check

DEBUG_FLAGS	=	-debug -o:none

all: format tests release debug

release:
	$(ODINC) build . $(VET_FLAGS) $(ERROR_FLAGS) $(RELEASE_FLAGS) -out:$(NAME) 

debug:
	$(ODINC) build . $(DEBUG_FLAGS) $(ERROR_FLAGS) -out:$(NAME)-debug

update_test_files:
	$(info Get/Update test roms)
	git submodule update --init --recursive

format:
	$(info Formatting files)
	@which odinfmt &> /dev/null && odinfmt -w . || echo "No odinfmt found in PATH, consider installing it for proper formatting."

tests: update_test_files
	$(info Run tests)
	$(ODINC) test . -all-packages

clean:
	rm -rf $(NAME) $(NAME)-debug