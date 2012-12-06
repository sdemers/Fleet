redo-ifchange *.hs

exec >&2

ghc --make FleetServ
