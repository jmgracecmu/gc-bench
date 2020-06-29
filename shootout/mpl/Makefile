.PHONY: msort.mpl msort.mpl-cc msort.mlton

all: msort.mpl msort.mpl-cc msort.mlton

msort.mpl-cc:
	mpl-cc -mlb-path-var 'COMPAT mpl-cc' -default-type int64 -default-type word64 -output $@ msort.mlb

msort.mpl:
	mpl -mlb-path-var 'COMPAT mpl' -default-type int64 -default-type word64 -output $@ msort.mlb

msort.mlton:
	mlton -mlb-path-var 'COMPAT mlton' -default-type int64 -default-type word64 -output $@ msort.mlb
