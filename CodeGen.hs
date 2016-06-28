{-# LANGUAGE RecordWildCards, QuasiQuotes#-} -- , OverloadedStrings #-}

module CodeGen where

import Compiler
import Data.Char
import System.Process
import System.Directory
import System.FilePath
import Types
import Text.RawString.QQ
import Text.Format

-- Code generation begins here
spaced :: String -> String
spaced t = " " ++ t ++ " "
parens :: String -> String
parens t = "(" ++ t ++ ")"
brackets :: String -> String
brackets t = "[" ++ t ++ "]"
separate :: String -> [String] -> String
separate x ys = foldl1 (\l r -> l ++ x ++ r) ys

kernelBody :: Expr -> String
kernelBody (FuncCall f args) = format temp [
    kernelArgs,
    body
    ]
    where
        temp = [r|
kernel void GPUMonad({0}) {
    {1}
}
|]
        kernelArgs = case f of
            "filter" -> "__global int *out, __global int *count"
            "reduce" -> "__global int *gOut, __global int *gCountWhere, __global int *gCountDone, __local int *lOut"
            "map" -> "__global int *out"
            "plow" -> "__global int *out, __global int *count"
            "zipWith" -> "__global int *out"
            
        body = case f of
            "filter" -> format temp [
                funcName,
                listArg
                ]
                where
                    [TIdf funcName _, TIdf listArg _] = args
                    temp = [r|
                        if ({0}({1}[get_global_id(0)])) {
                            out[atomic_inc(count)] = {1}[get_global_id(0)];
                        }
                        |]

            "reduce" -> format temp [
                funcName,
                listArg
                ]
                where
                    [TIdf funcName _, TIdf listArg _] = args
                    temp = [r|
                        lOut[get_local_id(0)] = {1}[get_global_id(0)];
                        barrier(CLK_LOCAL_MEM_FENCE);
                        if (!get_local_id(0)) {
                            for (int i = 1; i < get_local_size(0); ++i) {
                                lOut[0] = {0}(lOut[0], lOut[i]);
                            }
                            gOut[atomic_inc(gCountWhere)] = lOut[0];
                            barrier(CLK_GLOBAL_MEM_FENCE);
                            int myTurn = atomic_inc(gCountDone);
                            if (myTurn == get_num_groups(0) - 1) {
                                for (int i = 1; i < get_num_groups(0); ++i) {
                                    gOut[0] = {0}(gOut[0], gOut[i]);
                                }
                            }
                        }
                        |]

            "map" -> format temp [
                funcName,
                listArg
                ]
                where
                    [TIdf funcName _, TIdf listArg _] = args
                    temp = [r|
                        out[get_global_id(0)] = {0}({1}[get_global_id(0)]);
                        |]

            "plow" -> format temp [
                funcName,
                listArg
                ]
                where
                    [TIdf funcName _, TIdf listArg _] = args
                    temp = [r|
                        out[get_global_id(0)] = {1}[get_global_id(0)];
                        barrier(CLK_GLOBAL_MEM_FENCE);
                        if (!get_local_id(0)) {
                            int offset = get_local_size(0) * get_group_id(0);
                            for (int i = 1; i < get_local_size(0); ++i) {
                                out[offset + i] = {0}(out[offset + i - 1], out[offset + i]);
                            }
                            int myTurn = atomic_inc(count);
                            if (myTurn == get_num_groups(0) - 1) {
                                for (int group = 1; group < get_num_groups(0); ++group) {
                                    offset = group * get_local_size(0);
                                    for (int workItem = 0; workItem < get_local_size(0); ++workItem) {
                                        out[offset + workItem] = {0}(out[offset - 1], out[offset + workItem]);
                                    }
                                }
                            }
                        }
                        |]
                
            "zipWith" -> format temp [
                funcName,
                listArg1,
                listArg2
                ]
                where
                    [TIdf funcName _, TIdf listArg1 _, TIdf listArg2 _] = args
                    temp = [r|
                        out[get_global_id(0)] = {0}({1}[get_global_id(0)], {2}[get_global_id(0)]);
                        |]

generateDef :: Expr -> String
generateDef t = case t of
    Type (TIdf varName _) (Idf "Int") -> "__constant int " ++ varName ++ " = {!" ++ varName ++ "_contents}"
    Type (TIdf varName _) (List (Idf "Int")) -> "__constant int " ++ varName ++ "[{!" ++ varName ++ "_length}] " ++ " = {!" ++ varName ++ "_contents}"
    Def (TIdf varName _) (Num num) -> "__constant int " ++ varName ++ " = " ++ show num
    Def (TIdf varName _) (TIdf funcName (FuncSig n)) -> 
            "int " 
            ++ varName
            ++ (parens argList)
            ++ " {\n    return "
            ++ funcName ++ "(" ++ callList ++ ");\n"
            ++ "}"
        where
            argList = separate ", " (map (\x -> "int v" ++ show x) [1..n])
            callList = separate ", " (map (\x -> "v" ++ show x) [1..n])
    FuncDef funcName args body -> "int "
        ++ funcName
        ++ (parens argList)
        ++ " {\n    return "
        ++ bodyText
        ++ ";\n}"
        where
            argList = separate ", " (map (\x -> "int " ++ x) args)
            bodyText = toOpenCLC body
    _ -> ""

generateSig :: Expr -> String
generateSig t = case t of
    FuncDef funcName args _ -> "int "
        ++ funcName
        ++ "("
        ++ (separate ", " (replicate (length args) "int"))
        ++ ")"
    Def (TIdf funcName (FuncSig n)) _ -> "int " 
        ++ funcName
        ++ "("
        ++ (separate ", " (replicate n "int"))
        ++ ")"
    _ -> ""

base :: String
base = [r|

int mod(int a, int b) {
    return a % b;
}

|]

toOpenCLC :: Expr -> String
toOpenCLC t = case t of
    Let exprs body -> unlines [base, sigs, globals, kernel]
        where
            sigs = (separate ";\n" (filter (/="") (map generateSig exprs))) ++ ";\n"
            globals = (foldl1 (\x y -> x ++ ";\n" ++ y) $ map generateDef exprs)
            kernel = kernelBody body


    TIdf varName _ -> varName
    Num num -> show num
    Bln True -> show 1
    Bln False -> show 0

    Numeric op l r -> (parens $ toOpenCLC l) ++ spaced op ++ (parens $ toOpenCLC r)
    Boolean op l r -> (parens $ toOpenCLC l) ++ spaced op ++ (parens $ toOpenCLC r)
    Compose _ _ _ -> error "compose not implemented"
    IfE c t f -> (parens $ toOpenCLC c) ++ " ? " ++ (parens $ toOpenCLC t) ++ " : " ++ (parens $ toOpenCLC f)

    FuncCall funcName args -> funcName ++ (parens $ separate ", " $ map parens $ map toOpenCLC args)

    _ -> error $ "Unsupported: " ++ show t

-- Library generation

getInput (Type (TIdf varName varType) _) = [(varName, varType)]
getInput _ = []
getInputs = concat . map getInput

generateCppSignature :: Expr -> String -> String
generateCppSignature (Let exprs body) kernelName = returnType ++ " " ++ kernelName ++ parens argList
    where
        inputs = getInputs exprs

        convertInput (List (Idf "Int")) = "std::vector<int>"
        convertInput (Idf "Int") = "int"
        convertedInputs = map (\(varName, varType) -> (varName, convertInput varType)) inputs
        
        argList = separate ", " parts
            where
                parts = map (\(varName, varType) -> varType ++ " " ++ varName) convertedInputs
        
        FuncCall primitive _ = body
        returnType = case primitive of
            "filter" -> "std::vector<int>"
            "reduce" -> "int"
            "map" -> "std::vector<int>"
            "plow" -> "std::vector<int>"
            "zipWith" -> "std::vector<int>"
        
generateKernelReplacementCode :: Expr -> String
generateKernelReplacementCode (Let exprs body) = unlines $ map makeReplaceCode inputs
    where
        inputs = getInputs exprs

        makeReplaceCode (varName, Idf "Int") = format temp [varName]
            where
                temp = [r|
                    kernelSource = replace(kernelSource, "{!{0}_contents}", std::to_string({0}));
                    |]

        makeReplaceCode (varName, List (Idf "Int")) = format temp [varName]
            where
                temp = [r|
                    auto {0}_strings = map([](int i){return std::to_string(i);}, {0});
                    kernelSource = replace(kernelSource, "{!{0}_length}", std::to_string({0}.size()));
                    kernelSource = replace(kernelSource, "{!{0}_contents}", braces(params({0}_strings)));
                    |]

-- TODO: The actual purpose of the function is unclear. What does n do in the 
-- generated programs? This needs to be looked at.
generateWorkerSizeDiscoveryCode :: Expr -> String    
generateWorkerSizeDiscoveryCode (Let exprs body) = case primitive of
    "filter" -> 
        "int n = " ++ arg ++ ".size();"
        ++ mod64Check "n"
        where
            FuncCall _ [_, TIdf arg _] = body
    
    "reduce" ->
        "int n = " ++ arg ++ ".size();\n"
        ++ mod64Check (arg ++ ".size()")
        where
            FuncCall _ [_, TIdf arg _] = body

    "map" ->
        "int n = " ++ arg ++ ".size();\n"
        ++ mod64Check "n"
        where
            FuncCall _ [_, TIdf arg _] = body
    
    "plow" ->
        "int n = " ++ arg ++ ".size();\n"
        ++ mod64Check "n"
        where
            FuncCall _ [_, TIdf arg _] = body

    "zipWith" ->
        "int n = std::min(" ++ l ++ ".size(), " ++ r ++ ".size());\n"
        ++ mod64Check "n"
        where
            FuncCall _ [_, TIdf l _, TIdf r _] = body

    where
        FuncCall primitive _ = body

        mod64Check count = format temp [count]
            where
                temp = [r|
                    if (({0}) % workGroupSize != 0) {
                        throw std::invalid_argument("Job size should be multiple of" + std::to_string(workGroupSize));
                    }
                    |] 

              -- Name, Flags (read/write), size (integers)
data Buffer = Buffer String String String
              -- Or just size (in integers)
            | Local String
            deriving (Show)

generateBuffers :: Expr -> [Buffer]
generateBuffers (Let exprs body) = case primitive of
    "filter" -> [
        Buffer "out" "CL_MEM_WRITE_ONLY" "n",
        Buffer "count" "CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR" "1"
        ]
    
    "reduce" -> [
        Buffer "gOut" "CL_MEM_WRITE_ONLY" "xs.size()/workGroupSize",
        Buffer "gCountWhere" "CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR" "1", 
        Buffer "gCountDone" "CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR" "1",
        Local "workGroupSize"
        ]

    "map" -> [
        Buffer "out" "CL_MEM_WRITE_ONLY" "n"
        ]

    "plow" -> [
        Buffer "out" "CL_MEM_READ_WRITE" "n",
        Buffer "count" "CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR" "1"
        ]

    "zipWith" -> [
        Buffer "out" "CL_MEM_WRITE_ONLY" "n"
        ]

    where
        FuncCall primitive _ = body

generateOutputArray :: Expr -> String
generateOutputArray (Let exprs body) = case primitive of
    "filter" -> withCountVar
    "plow" -> withCountVar
    "reduce" -> [r|
        auto h_out = std::make_unique<int[]>(n / workGroupSize);
        std::fill_n(h_out.get(), n / workGroupSize, 0);
        auto h_count = std::make_unique<int[]>(1);
        h_count[0] = 0;
        |]
    _ -> [r|
        auto h_out = std::make_unique<int[]>(n);
        std::fill_n(h_out.get(), n, 0);
        |]
    where
        FuncCall primitive _ = body
        
        withCountVar = [r|
            auto h_out = std::make_unique<int[]>(n);
            std::fill_n(h_out.get(), n, 0);
            auto h_count = std::make_unique<int[]>(1);
            h_count[0] = 0;
            |]

generateKernelLiteral :: Expr -> String
generateKernelLiteral program = format ([r|
    std::string kernelSource = R"Fancy0penCL({0})Fancy0penCL";
    |]) [toOpenCLC program]

generateExtractionCode :: Expr -> String
generateExtractionCode (Let exprs body) = case primitive of
    "filter" -> fewInts
    "reduce" -> singleInt
    _ -> allInts
    
    where
        FuncCall primitive _ = body
        
        singleInt = "queue.enqueueReadBuffer(d_gOut, CL_TRUE, 0, sizeof(int), h_out.get());"
        allInts = "queue.enqueueReadBuffer(d_out, CL_TRUE, 0, n * sizeof(int), h_out.get());"
        fewInts = [r|
            queue.enqueueReadBuffer(d_out, CL_TRUE, 0, n * sizeof(int), h_out.get());
            queue.enqueueReadBuffer(d_count, CL_TRUE, 0, sizeof(int), h_count.get());
            |]

generateReturnCode :: Expr -> String
generateReturnCode (Let exprs body) = case primitive of
    "filter" -> fewInts
    "reduce" -> singleInt
    _ -> allInts
    where
        FuncCall primitive _ = body
        
        singleInt = "return h_out[0];"
        allInts = "return std::vector<int>(h_out.get(), h_out.get() + n);"
        fewInts = "return std::vector<int>(h_out.get(), h_out.get() + h_count[0]);"

deviceDiscoveryCode = [r|
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
|]

programBuildCode = [r|
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
|]

kernelRunCode = [r|
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
|]

errorPrintCode = [r|
    std::cerr << "ERROR: "<<err.what()<<"("<<err.err()<<")"<< "\n";
    return {};
|]

-- Generates the C++ source file containing the function
generateLibrary :: Expr -> String -> String
generateLibrary program name = unlines [
    -- Enables exceptions in OpenCL API
    "#define __CL_ENABLE_EXCEPTIONS\n" 
    -- Includes for writing to stdout in case of error
    , "#include <iostream>"
    -- For std::unique_ptr
    , "#include <memory>"
    -- For std::ceil() and other math functions
    , "#include <cmath>"
    -- Here we include the header file we generate
    , "#include \"" ++ name ++ ".hpp\""
    -- Contains general purpose functions like string replacement
    , "#include \"utils.hpp\""
    -- Include the OpenCL API
    , "#include \"CL/cl.hpp\""
    -- Generate the CPP signature
    , generateCppSignature program name ++ " {"
    -- Pick our favourite workgroup size. At the moment this is hardcoded,
    -- however this magic number is specific to graphics cards. Can impact
    -- performance.
    , "int workGroupSize = 64;"
    -- Generates the actual OpenCL program
    , generateKernelLiteral program
    -- Generates code that specializes the OpenCL program for the arguments
    , generateKernelReplacementCode program
    -- Generates code that checks how much workers are needed based on the
    -- input arguments. For example, when filtering you want the same
    -- amount as workers as the amount of elements. But when zipWith'ing
    -- you only want the amount of workers equal to the shortest of the two
    -- lists. Additionally, this amount should be divisible by workgroupsize.
    , generateWorkerSizeDiscoveryCode program
    -- Generate code that creates buffers
    , unlines (map generateBufferDefinition buffers)
    -- Creates an array in which the result of the kernel will be stored
    , generateOutputArray program
    , "cl_int err = CL_SUCCESS;"
    , "try {"
    -- Code that picks the first usable device for kernel execution
    , deviceDiscoveryCode
    -- Initialize the created buffers
    , unlines (map generateBufferInitialization buffers)
    -- Insert code that compiles the program;
    , programBuildCode
    -- Create a kernel object from the program, and tell it which
    -- function to execute. OpenCL programs generated by Hywar
    -- are always called GPUMonad
    , "cl::Kernel kernel(program, \"GPUMonad\", &err);"
    -- Generate code that sets the arguments of the kernel
    , unlines (zipWith generateKernelBufferArgSet [0..length buffers - 1] buffers)
    -- Insert code that runs the kernel
    , kernelRunCode
    -- Extract the results
    , generateExtractionCode program
    -- Error handling
    , "} catch (cl::Error err) {"
    , errorPrintCode
    , "}"
    -- Code that returns the result in the proper container (int or std::vector<int>)
    , generateReturnCode program
    , "}"
    ]
    where
        -- Finds out how many buffers are needed
        buffers = generateBuffers program

        -- Generates a buffer definition if it is needed
        -- Local buffers do not need to be defined and initialized.
        generateBufferDefinition (Buffer name  _ _) = "cl::Buffer d_" ++ name ++ ";\n"
        generateBufferDefinition (Local _) = ""
        
        -- Count buffers need to be initialized to zero for proper counting
        -- Generates buffer initialization code.
        generateBufferInitialization (Buffer "gCountWhere" flags size) = format [r|
            d_gCountWhere = cl::Buffer(context, {0}, ({1}) * sizeof(int), h_count.get());
            |] [flags, size]
        generateBufferInitialization (Buffer "gCountDone" flags size) = format [r|
            d_gCountDone = cl::Buffer(context, {0}, ({1}) * sizeof(int), h_count.get());
            |] [flags, size]
        generateBufferInitialization (Buffer "count" flags size) = format [r|
            d_count = cl::Buffer(context, {0} , ({1}) * sizeof(int), h_count.get());
            |] [flags, size]
        generateBufferInitialization (Buffer name flags size) =
            "d_" ++ name ++ " = cl::Buffer(context, " ++ flags ++ ", (" ++ size ++ ") * sizeof(int));"
        generateBufferInitialization (Local _) = ""

        generateKernelBufferArgSet i (Buffer name _ size) = "kernel.setArg(" ++ show i ++ ", d_" ++ name ++ ");"
        generateKernelBufferArgSet i (Local size) = "kernel.setArg(" ++ show i ++ ", (" ++ size ++ ") * sizeof(int), nullptr);"

-- Generates the C++ header file containing the function declaration
generateHeader :: Expr -> String -> String
generateHeader program name = unlines [
    "#ifndef " ++ (map toUpper name) ++ "_H", 
    "#define " ++ (map toUpper name) ++ "_H",
    "#include <vector>",
    generateCppSignature program name ++ ";",
    "#endif"
    ]
    where

-- Generates source and header files and saves them in the directory named the given name
generateAndSaveLibrary :: Expr -> String -> IO()
generateAndSaveLibrary program name = do
    createDirectoryIfMissing False name
    copyFile "utils.cpp" (name ++ [pathSeparator] ++ "utils.cpp")
    copyFile "utils.hpp" (name ++ [pathSeparator] ++ "utils.hpp")
    writeFile (name ++ [pathSeparator] ++ name ++ ".cpp") $ generateLibrary program name
    writeFile (name ++ [pathSeparator] ++ name ++ ".hpp") $ generateHeader program name

-- Generates all example programs (6003-6007)
generateAllExamples = do
    sequence_ $ map (\x -> generateAndSaveLibrary (doTaal x) ("test" ++ show x)) [6003..6007]

-- Executes the test
doTest = do
    generateAllExamples
    callCommand "make -j4"
    callCommand "./kernel_test"

