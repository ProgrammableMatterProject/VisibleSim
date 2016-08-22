#! /bin/sh

defaultBaudRate="38900"
defaultSpeed="1.881119320574239116075"

confDir="evaluation/confs/"
subConfDir="car/ pyramid/ square/"
resDir="evaluation/results/"

mv "$resDir" "evaluation/results-bak"
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
	    out="$subResDir$xmlBaseName/$i.out"
	    #echo "$xml => $out"
	    ./catom2D1 -c $xml -R -t -i -a -1 -B $defaultBaudRate -M $defaultSpeed > $out  2>&1
	done
    done
done
