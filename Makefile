proglet.pdf: proglet.ps
	ps2pdf proglet.ps
proglet.hp: recompile
	cabal run proglet
proglet.ps: proglet.hp
	hp2ps -e8in -c proglet.hp
