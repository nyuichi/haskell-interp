all: interp

interp: interp.hs
	ghc -o interp interp.hs
