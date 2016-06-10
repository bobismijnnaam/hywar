#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <memory>
#include <cmath>
#include "test6004.hpp"
#include "utils.hpp"
#include "CL/cl.hpp"
int test6004(std::vector<int> xs) {
int workGroupSize = 64;
std::string kernelSource = R"Fancy0penCL(


int mod(int a, int b) {
    return a % b;
}


int myMax(int, int);
int lfwN1xVSPUCz(int, int);

__constant int xs[{!xs_length}]  = {!xs_contents};
int myMax(int v1, int v2) {
    return lfwN1xVSPUCz(v1, v2);
};
int lfwN1xVSPUCz(int x, int y) {
    return ((x) > (y)) ? (x) : (y);
}
kernel void GPUMonad(__global int *gOut, __global int *gCountWhere, __global int *gCountDone, __local int *lOut) {
lOut[get_local_id(0)] = xs[get_global_id(0)];
barrier(CLK_LOCAL_MEM_FENCE);
if (!get_local_id(0)) {
for (int i = 1; i < get_local_size(0); ++i) {
 lOut[0] = myMax(lOut[0], lOut[i]);
}
gOut[atomic_inc(gCountWhere)] = lOut[0];
barrier(CLK_GLOBAL_MEM_FENCE);
int myTurn = atomic_inc(gCountDone);
if (myTurn == get_num_groups(0) - 1) {
for (int i = 1; i < get_num_groups(0); ++i) {
gOut[0] = myMax(gOut[0], gOut[i]);
}
}
}
}

)Fancy0penCL";
auto xs_strings = map([](int i){return std::to_string(i);}, xs);
kernelSource = replace(kernelSource, "{!xs_length}", std::to_string(xs.size()));
kernelSource = replace(kernelSource, "{!xs_contents}", braces(params(xs_strings)));

int n = xs.size();
if ((xs.size()) % workGroupSize != 0) {
throw std::invalid_argument("Job size should be multiple of " + std::to_string(workGroupSize));
}
cl::Buffer d_gOut;

cl::Buffer d_gCountWhere;

cl::Buffer d_gCountDone;



auto h_out = std::make_unique<int[]>(n / workGroupSize);
std::fill_n(h_out.get(), n / workGroupSize, 0);
auto h_count = std::make_unique<int[]>(1);
h_count[0] = 0;
cl_int err = CL_SUCCESS;
try {

    // Query platforms
    std::vector<cl::Platform> platforms;
    cl::Platform::get(&platforms);
    if (platforms.size() == 0) {
        std::cout << "Platform size 0\n";
        throw std::domain_error("Platform size is zero, cannot run.");
        }

    // Get list of devices on default platform and create context
    cl_context_properties properties[] = { CL_CONTEXT_PLATFORM, (cl_context_properties)(platforms[0])(), 0};
    cl::Context context(CL_DEVICE_TYPE_GPU, properties);
    std::vector<cl::Device> devices = context.getInfo<CL_CONTEXT_DEVICES>();

    // Create command queue for first device
    cl::CommandQueue queue(context, devices[0], 0, &err);

d_gOut = cl::Buffer(context, CL_MEM_WRITE_ONLY, (xs.size()/workGroupSize) * sizeof(int));
d_gCountWhere = cl::Buffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, (1) * sizeof(int), h_count.get());

d_gCountDone = cl::Buffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, (1) * sizeof(int), h_count.get());




    //Build kernel from source string
    cl::Program::Sources source(1, std::make_pair(kernelSource.c_str(),kernelSource.size()));
    cl::Program program = cl::Program(context, source);

    std::cout << program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(devices[0]);

    try {
        program.build(devices);
    } catch (cl::Error& e) {
        if (e.err() == CL_BUILD_PROGRAM_FAILURE) {
            // Check the build status
            cl_build_status status = program.getBuildInfo<CL_PROGRAM_BUILD_STATUS>(devices[0]);

            // Get the build log
            std::string name     = devices[0].getInfo<CL_DEVICE_NAME>();
            std::string buildlog = program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(devices[0]);
            std::cout << "Build log for " << name << ":" << "\n" << buildlog << "\n";
            std::cout << "Program options: " << program.getBuildInfo<CL_PROGRAM_BUILD_OPTIONS>(devices[0]) << "\n";
        } else {
            throw e;
        }
    }

cl::Kernel kernel(program, "GPUMonad", &err);
kernel.setArg(0, d_gOut);
kernel.setArg(1, d_gCountWhere);
kernel.setArg(2, d_gCountDone);
kernel.setArg(3, (workGroupSize) * sizeof(int), nullptr);


    // Number of work items in each local work group
    cl::NDRange localSize(workGroupSize);
    // Number of total work items - localSize must be devisor
    cl::NDRange globalSize((int) (ceil(n / (float)workGroupSize) * workGroupSize));

    // Enqueue kernel
    cl::Event event;
    queue.enqueueNDRangeKernel(
        kernel,
        cl::NullRange,
        globalSize,
        localSize,
        NULL,
        &event);

    // Block until kernel completion
    event.wait();

queue.enqueueReadBuffer(d_gOut, CL_TRUE, 0, sizeof(int), h_out.get());
} catch (cl::Error err) {

    std::cerr << "ERROR: "<<err.what()<<"("<<err.err()<<")"<< "\n";
    return {};

}
return h_out[0];
}
