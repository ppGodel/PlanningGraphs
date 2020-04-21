while read p; do
    g=`echo "$p"`
    if test -f "$g.graph"; then
        echo $g 'has already been extracted'
    else
        echo 'Extracting' $g
        awk -v graph=$g '{if ($1 == graph) { for (i = 3; i <= NF; i++) { printf $i" " }; printf "\n" } }' < nodes2.csv > $g.graph
        awk -v graph=$g '{if ($1 == graph) { for (i = 2; i <= NF; i++) { printf $i" " }; printf "\n" } }' < edges2.csv >> $g.graph
        echo $g 'extracted'
    fi
done <graphs2.csv
# these graphs are available for download at http://elisa.dyndns-web.com/PlanningGraphs/
