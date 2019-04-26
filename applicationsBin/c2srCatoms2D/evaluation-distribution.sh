
defaultBaudRate="38900"
defaultSpeed="1.881119320574239116075"

export=".confCheck.xml"
moves="moves.dat"
messages="messages.dat"

#xml="evaluation/confs/car/car-120.xml"
#xml="evaluation/confs/car/car-x1-1073.xml"
xml="evaluation/confs/car/car-x2-9644.xml"

rad="$(basename $xml .xml)"
resDir="evaluation/results-dis/"

rm -rf $resDir
mkdir $resDir

resDir="$resDir$rad"
mkdir $resDir

echo $resDir

./c2sr -c $xml -R -t -i -a -1 -g -B $defaultBaudRate -M $defaultSpeed > "$rad.out"  2>&1

echo "$resDir/sim.out"
cp "$rad.out" "$resDir/sim.out"
cp "$export"  "$resDir/conf.xml"
cp "$messages" "$resDir/msg.out"
cp "$moves" "$resDir/moves.out"

