make
./dist/build/main/main
cat feature/20120123_hmi.txt feature/20120307_hmi.txt > training.txt
./libsvm/easy.py ./training.txt ./feature/20130515_11745_hmi.txt
