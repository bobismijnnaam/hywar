kernels = test6003/test6003.cpp test6004/test6004.cpp test6005/test6005.cpp test6006/test6006.cpp test6007/test6007.cpp

all: kernel_test

kernel_test: TestKernels.cpp ${kernels} utils.cpp
	g++ -L/opt/AMDAPPSDK-3.0/lib/x86_64 -I/opt/AMDAPPSDK-3.0/include TestKernels.cpp utils.cpp ${kernels} -lOpenCL -lm -o kernel_test -std=c++14 -g -Wall
