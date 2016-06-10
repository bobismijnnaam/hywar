all: plow_test

plow_test: plow_test.cpp
	g++ -L/opt/AMDAPPSDK-3.0/lib/x86_64 -I/opt/AMDAPPSDK-3.0/include plow_test.cpp -lOpenCL -lm -o plow_test -std=c++14 -g
