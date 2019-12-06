timeout=300
find . -size  0 -print0 | xargs -0 rm --
touch cc_${timeout}.lst
for f in `ls -1 *.graph`;
do
    g=`basename $f .graph`
    if test -f "$g.cc"; then
	echo $g 'already processed'
    else
	if grep -Fxq "$g" cc_${timeout}.lst
	then
	    echo $g 'will not finish within the present timeout'
	else
	    echo 'processing' $g 'for' $timeout's max'	
	    gtimeout ${timeout}s python3 layers.py $f > $g.cc # .layers
	    code=$?
	    if [ "$code" -eq "124" ]; then
		echo "interrupted" $g
		rm $g.cc
		echo $g >> cc_${timeout}.lst
	    fi
	fi
    fi
done 
