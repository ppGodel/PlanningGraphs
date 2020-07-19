echo 'IPC results incl. header'
wc -l IPCResults.csv
Rscript analyze.R
cp model.txt /Users/elisa/Dropbox/Research/Topics/Planning
cp *.png /Users/elisa/Dropbox/Research/Topics/Planning
cp *.pdf /Users/elisa/Dropbox/Research/Topics/Planning
