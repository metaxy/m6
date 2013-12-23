ssh -t -t git_lightplanke@5.9.58.75 << ENDHERE
    killall m6 
    cd /var/www/vhosts/ecg-berlin.de/m6
    LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/var/www/vhosts/ecg-berlin.de/m6" nohup ./m6 Production &
    exit
ENDHERE

