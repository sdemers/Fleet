rm -rf tags
hasktags -x -c -o tags `find | grep .hs | xargs`
