-include Makefile.config

SCRIPTFLAGS?=-verbose
DEFAULT_FLAGS?=-default-type int64 -default-type word64

.PHONY: all clean realclean phony

default:
	@echo "usage: make <bench>.<config>.bin"

clean:
	rm -f bin/*.bin* bin/*.log bin/*.c bin/*.s

phony:

%.bin: phony
	@mkdir -p bin
	@./scripts/compile-benchmark \
		-benchmark $(shell echo $* | cut -d. -f1) \
		-config $(shell echo $* | cut -d. -f2) \
		-output "bin/$@" \
		-log "bin/$@.log" \
		$(SCRIPTFLAGS) \
		-- $(DEFAULT_FLAGS) $(EXTRA_FLAGS)
