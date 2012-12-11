redo-ifchange *.hs

exec >&2

GHC=/usr/local/bin/ghc

PACKAGES="-package json"

${GHC} ${PACKAGES} --make FleetServ
