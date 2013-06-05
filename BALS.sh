ps ax | grep libsvm | grep python | awk '{print "kill -9 " $1}'
