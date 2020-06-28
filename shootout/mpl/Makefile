.PHONY: msort.mpl msort.mplcc

all: msort.mpl msort.mplcc

msort.mplcc:
	mpl-cc -mlb-path-var 'COMPAT mpl-cc' -default-type int64 -default-type word64 -output $@ msort.mlb

msort.mpl:
	mpl -mlb-path-var 'COMPAT mpl' -default-type int64 -default-type word64 -output $@ msort.mlb
