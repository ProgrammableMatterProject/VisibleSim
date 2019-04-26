#! /bin/sh

defaultBaudRate="38900"
defaultSpeed="1.881119320574239116075"

confDir="evaluation/confs/"
subConfDir="car/ flag/ magnet/ pyramid/"

resDir="evaluation/results/"

export=".confCheck.xml"

bak="evaluation/results-bak"
rm -rf "$bak"
mv "$resDir" "$bak"
mkdir "$resDir"

for sub in $subConfDir; do
    dir="$confDir$sub"
    subResDir="$resDir$sub"
    echo "=== $dir ==="
    mkdir "$subResDir"
    for xml in $(ls $dir*.xml); do
	echo "$(basename $xml)"
	xmlBaseName="$(basename $xml .xml)"
	mkdir "$subResDir$xmlBaseName"
	for i in $(seq 1 10); do
	    rad="$subResDir$xmlBaseName/$i"
	    #echo "$xml => $out"
	    ./c2sr -c $xml -R -t -i -a -1 -g -B $defaultBaudRate -M $defaultSpeed > "$rad.out"  2>&1
	    cp "$export" "$rad.xml"
	    cp "moves.dat" "$subResDir$xmlBaseName/$i.moves"
	    cp "messages.dat" "$subResDir$xmlBaseName/$i.messages"
	done
    done
done
