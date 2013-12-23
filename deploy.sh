echo "Building Binary\n#########################\n"
cabal configure
cabal build
echo "Copying to Remote\n#######################\n"
rsync -avz dist/build/m6/m6 git_lightplanke@5.9.58.75:/var/www/vhosts/ecg-berlin.de/m6
rsync -avz config git_lightplanke@5.9.58.75:/var/www/vhosts/ecg-berlin.de/m6
rsync -avz static git_lightplanke@5.9.58.75:/var/www/vhosts/ecg-berlin.de/m6
echo "starting webserver"
sh run_remote.sh

