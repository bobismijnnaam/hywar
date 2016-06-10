#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <memory>
#include <cmath>
#include "test6007.hpp"
#include "utils.hpp"
#include "CL/cl.hpp"
std::vector<int> test6007(std::vector<int> xs, std::vector<int> ys) {
int workGroupSize = 64;
std::string kernelSource = R"Fancy0penCL(


int mod(int a, int b) {
    return a % b;
}


int myMin(int, int);
int lfwM1xZTP4FJ(int, int);

__constant int xs[{!xs_length}]  = {!xs_contents};
__constant int ys[{!ys_length}]  = {!ys_contents};
int myMin(int v1, int v2) {
    return lfwM1xZTP4FJ(v1, v2);
};
int lfwM1xZTP4FJ(int x, int y) {
    return ((x) > (y)) ? (y) : (x);
}
kernel void GPUMonad(__global int *out) {
out[get_global_id(0)] = myMin(xs[get_global_id(0)], ys[get_global_id(0)]);
}

)Fancy0penCL";
auto xs_strings = map([](int i){return std::to_string(i);}, xs);
kernelSource = replace(kernelSource, "{!xs_length}", std::to_string(xs.size()));
kernelSource = replace(kernelSource, "{!xs_contents}", braces(params(xs_strings)));
auto ys_strings = map([](int i){return std::to_string(i);}, ys);
kernelSource = replace(kernelSource, "{!ys_length}", std::to_string(ys.size()));
kernelSource = replace(kernelSource, "{!ys_contents}", braces(params(ys_strings)));

int n = std::min(xs.size(), ys.size());
if ((n) % workGroupSize != 0) {
throw std::invalid_argument("Job size should be multiple of " + std::to_string(workGroupSize));
}
cl::Buffer d_out;


auto h_out = std::make_unique<int[]>(n);
std::fill_n(h_out.get(), n, 0);
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

d_out = cl::Buffer(context, CL_MEM_WRITE_ONLY, (n) * sizeof(int));


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
kernel.setArg(0, d_out);


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

queue.enqueueReadBuffer(d_out, CL_TRUE, 0, n * sizeof(int), h_out.get());
} catch (cl::Error err) {

    std::cerr << "ERROR: "<<err.what()<<"("<<err.err()<<")"<< "\n";
    return {};

}
return std::vector<int>(h_out.get(), h_out.get() + n);
}
