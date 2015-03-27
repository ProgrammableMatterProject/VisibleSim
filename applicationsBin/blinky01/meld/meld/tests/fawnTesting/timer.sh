

EXEC="./mpiTest"
WRITETO=$1
prog=$2

ONE="192.168.100.208"
TWO="192.168.100.208,192.168.100.209"
THREE="192.168.100.208,192.168.100.209,192.168.100.210"
FOUR="192.168.100.208,192.168.100.209,192.168.100.210,192.168.100.211"
FIVE="192.168.100.208,192.168.100.209,192.168.100.210,192.168.100.211,192.168.100.212"
SIX="192.168.100.208,192.168.100.209,192.168.100.210,192.168.100.211,192.168.100.212,192.168.100.213"
SEVEN="192.168.100.208,192.168.100.209,192.168.100.210,192.168.100.211,192.168.100.212,192.168.100.213,192.168.100.214"
EIGHT="192.168.100.208,192.168.100.209,192.168.100.210,192.168.100.211,192.168.100.212,192.168.100.213,192.168.100.214,192.168.100.215"

TEXT=".txt"
RDVS=0
UNDER="_"

run_timer()
{

	rm -rf $WRITETO
	mkdir $WRITETO

	local NUM=1
	OUTPUT="$WRITETO/$prog.txt"
		for number in $ONE $TWO $THREE $FOUR $FIVE $SIX $SEVEN $EIGHT; do
			for i in 1 2 3 4; do
					if  [ "$number" = $ONE ]; then
        	                        	NUM=1
                	        	elif [ "$number" = $TWO ]; then
						NUM=2
					elif [ "$number" = $THREE ]; then
						NUM=3
					elif [ "$number" = $FOUR ]; then
						NUM=4
					elif [ "$number" = $FIVE ]; then
						NUM=5
					elif [ "$number" = $SIX ]; then
						NUM=6
					elif [ "$number" = $SEVEN ]; then
						NUM=7
					elif [ "$number" = $EIGHT ]; then
						NUM=8
					fi 
        	               	 	echo -n ' ' >> $OUTPUT
					echo -n -e '\t' Timing $i processes on $NUM Nodes...
					
					echo -n $NUM >> $OUTPUT
					echo -n ' ' >> $OUTPUT
        	              		echo -n $i >> $OUTPUT
					echo -n ' ' >> $OUTPUT
					echo -n $(($NUM*$i)) >> $OUTPUT
					echo -n ' ' >> $OUTPUT
					/usr/bin/time -f "%e %U %S" -o $OUTPUT -a mpiexec -H $number -npernode $i ../meld -f $prog.ml -c sl
					echo DONE
				done
			done
}

#echo $TWO	
#mpiexec -H $TWO -npernode 3 $EXEC 2 1 0
run_timer
