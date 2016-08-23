#! /bin/sh

baudrates="75 110 300 1200 2400 4800 9600 19200 38400 57600 115200" 
# revolution "in 1.67 seconds or 3.35 seconds"
# revolution, distance = (2*pi*R)/T_rev.
# (2*3.14159265359*0.5)/(3.35)
speeds="0.93778885181791044776 1.881119320574239116075"

xml="evaluation/confs/car/car-x1-1073.xml"
resDir="evaluation/results-rate/"
export=".confCheck.xml"

mv "$resDir" "evaluation/results-rate-bak"
mkdir "$resDir"

for speed in $speeds; do
    echo "=== $speed ==="
    for baudrate in $baudrates; do
	echo "$baudrate"
	s="$(echo $speed | cut -c -4)"
	outDir="$resDir$s-$baudrate/"
	mkdir "$outDir"
	for i in $(seq 1 10); do
	    out="$outDir$i"
	    ./catom2D1 -c $xml -R -t -i -a -1 -g -B $baudrate -M $speed > "$out.out"  2>&1
	    cp "$export" "$out.xml"
	done
    done
done
