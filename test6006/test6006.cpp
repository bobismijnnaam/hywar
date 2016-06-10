#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <memory>
#include <cmath>
#include "test6006.hpp"
#include "utils.hpp"
#include "CL/cl.hpp"
std::vector<int> test6006(std::vector<int> xs) {
int workGroupSize = 64;
std::string kernelSource = R"Fancy0penCL(


int mod(int a, int b) {
    return a % b;
}


int lf_173en_xtP2(int, int);

__constant int xs[{!xs_length}]  = {!xs_contents};
__constant int a = 5;
int lf_173en_xtP2(int x, int z) {
    return (x) + (z);
}
kernel void GPUMonad(__global int *out, __global int *count) {
out[get_global_id(0)] = xs[get_global_id(0)];
barrier(CLK_GLOBAL_MEM_FENCE);
if (!get_local_id(0)) {
int offset = get_local_size(0) * get_group_id(0);
for (int i = 1; i < get_local_size(0); ++i) {
out[offset + i] = lf_173en_xtP2(out[offset + i - 1], out[offset + i]);
}
int myTurn = atomic_inc(count);
if (myTurn == get_num_groups(0) - 1) {
    for (int group = 1; group < get_num_groups(0); ++group) {
        offset = group * get_local_size(0);
        for (int workItem = 0; workItem < get_local_size(0); ++workItem) {
            out[offset + workItem] = lf_173en_xtP2(out[offset - 1], out[offset + workItem]);
        }
    }
}
}

}

)Fancy0penCL";
auto xs_strings = map([](int i){return std::to_string(i);}, xs);
kernelSource = replace(kernelSource, "{!xs_length}", std::to_string(xs.size()));
kernelSource = replace(kernelSource, "{!xs_contents}", braces(params(xs_strings)));

int n = xs.size();
if ((n) % workGroupSize != 0) {
throw std::invalid_argument("Job size should be multiple of " + std::to_string(workGroupSize));
}
cl::Buffer d_out;

cl::Buffer d_count;


auto h_out = std::make_unique<int[]>(n);
std::fill_n(h_out.get(), n, 0);
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

d_out = cl::Buffer(context, CL_MEM_READ_WRITE, (n) * sizeof(int));
d_count = cl::Buffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, (1) * sizeof(int), h_count.get());



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
kernel.setArg(1, d_count);


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
