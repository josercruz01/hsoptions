#!/bin/bash
cabal configure && cabal build && cabal haddock --hyperlink-source \
                                    --html-location='http://hackage.haskell.org/package/$pkg/docs' \
                                    --contents-location='http://hackage.haskell.org/package/$pkg'

version=${2}
name=hsoptions

if [ -z "$version" ]; then 
  version=$(cabal info hsoptions.cabal | grep -Eow "\d\.\d\.\d\.\d")
fi

S=$?
if [ "${S}" -eq "0" ]; then
    cd "dist/doc/html"
    DDIR="$name-$version-docs"
    echo $DDIR
    cp -r "$name" "${DDIR}" && tar -czvf "${DDIR}.tar.gz" "${DDIR}"
    CS=$?
    if [ "${CS}" -eq "0" ]; then
        echo "Uploading to Hackage…"
        curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@${DDIR}.tar.gz" -u josercruz01 "http://hackage.haskell.org/package/$name-$version/docs"
        exit $?
    else
        echo "Error when packaging the documentation"
        exit $CS
    fi
else
    echo "Error when trying to build the package."
    exit $S
fi
#!/bin/sh
