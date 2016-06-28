#include <iostream>
#include <vector>
#include <random>
#include <set>
#include <chrono>
#include <functional>

#include "test6003/test6003.hpp"
#include "test6004/test6004.hpp"
#include "test6005/test6005.hpp"
#include "test6006/test6006.hpp"
#include "test6007/test6007.hpp"

std::chrono::duration<double> benchmark(std::function<std::vector<int>(std::vector<int>)> exec6003,
        std::function<int(std::vector<int>)> exec6004,
        std::function<std::vector<int>(std::vector<int>, int)> exec6005,
        std::function<std::vector<int>(std::vector<int>)> exec6006,
        std::function<std::vector<int>(std::vector<int>, std::vector<int>)> exec6007,
        int jobSize) {

    std::random_device rd;
    std::mt19937 rng(rd()); 
    std::uniform_int_distribution<int> dist(0, 1000);

    std::vector<int> testDataLeft, testDataRight;

    for (int i = 0; i < jobSize; i++) {
        testDataLeft.push_back(dist(rng));
        testDataRight.push_back(dist(rng));
    }
    
    std::chrono::time_point<std::chrono::system_clock> start, end;
    start = std::chrono::system_clock::now();

    auto result6003 = exec6003(testDataLeft);
    auto result6004 = exec6004(testDataLeft);
    auto result6005 = exec6005(testDataLeft, 5);
    auto result6006 = exec6006(testDataLeft);
    auto result6007 = exec6007(testDataLeft, testDataRight);

    std::vector<int> aggr;
    for (int i = 0; i < jobSize; ++i) {
        aggr.push_back(
            result6003[i]
            * result6004
            * result6005[i]
            * result6006[i]
            * result6007[i]);
    }
 
    end = std::chrono::system_clock::now();
    return end-start;
}

auto isEven(auto x) {
    return x % 2 == 0;
}

std::vector<int> seq6003(std::vector<int> ps) {
    std::vector<int> result;
    for (auto p : ps) {
        if (isEven(p)) result.push_back(p);
    }

    return result;
}

auto myMax(auto x, auto y) {
    if (x > y) return x; else return y;
}

int seq6004(std::vector<int> xs) {
    int theMax;

    if (xs.empty()) return theMax;

    theMax = xs.front();
    
    for (int i = 1; i < xs.size(); i++) {
        theMax = myMax(theMax, xs[i]);
    }

    return theMax;
}

auto sum(auto x, auto add) {
    return x + add + 5;
}

std::vector<int> seq6005(std::vector<int> xs, int add) {
    std::vector<int> result;

    for (auto x : xs) {
        result.push_back(sum(x, add));
    }

    return result;
}

auto secret_lambda(auto x, auto z) {
    return x + z;
}

std::vector<int> seq6006(std::vector<int> xs) {
    std::vector<int> result;

    result.push_back(xs.front());

    for (int i = 1; i < xs.size(); i++) {
        result.push_back(secret_lambda(result.back(), xs[i]));
    }

    return result;
}

auto myMin(auto x, auto y) {
    if (x > y) return y; else return x;
}

std::vector<int> seq6007(std::vector<int> left, std::vector<int> right) {
    std::vector<int> result;

    for (int i = 0; i < std::min(left.size(), right.size()); ++i) {
        result.push_back(myMin(left[i], right[i]));
    }

    return result;
}
    
int doTest() {
    std::random_device rd;
    std::mt19937 rng(rd()); 
    std::uniform_int_distribution<int> dist(0, 1000);

    int const jobSize = 1024;
    int const groupSize = 64;

    bool const do6003 = true;
    bool const do6004 = true;
    bool const do6005 = true;
    bool const do6006 = true;
    bool const do6007 = true;

    std::vector<int> correct;
    std::vector<int> incorrect;

    std::cout << "Hywar Test Suite\n";

    std::vector<int> testDataLeft, testDataRight;

    for (int i = 0; i < jobSize; i++) {
        testDataLeft.push_back(dist(rng));
        testDataRight.push_back(dist(rng));
    }

    if (do6003) {
        std::cout << "-- test6003 --\n";
        std::vector<int> testData;
        std::vector<int> checkData;
        for (int i = 0; i < jobSize; i++) {
            testData.push_back(dist(rng));

            if (testData.back() % 2 == 0) {
                checkData.push_back(testData.back());
            }
        }

        auto result6003 = test6003(testData);

        for (auto i : result6003) {
            std::cout << i << "\n";
        }

        std::cout << "Correct data: \n";

        for (auto i : checkData) {
            std::cout << i << "\n";
        }

        if (std::set<int>(result6003.begin(), result6003.end()) == std::set<int>(checkData.begin(), checkData.end())) {
            std::cout << "=> Correct!\n";
            correct.push_back(6003);
        } else {
            std::cout << "=> Incorrect!\n";
            incorrect.push_back(6003);
        }
    }

    if (do6004) {
        std::cout << "-- test6004 --\n";
        std::vector<int> testData;
        int maxValue = -1;
        for (int i = 0; i < jobSize; i++) {
            testData.push_back(dist(rng));
            if (testData.back() > maxValue) {
                maxValue = testData.back();
            }
        }

        auto result6004 = test6004(testData);

        std::cout << result6004 << "\n";
        std::cout << "Actual max value: " << maxValue << "\n";
        if (result6004 == maxValue) {
            std::cout << "=> Correct!\n";
            correct.push_back(6004);
        } else {
            std::cout << "=> Incorrect!\n";
            incorrect.push_back(6004);

            for (int i = 0; i < jobSize / groupSize; ++i) {
                int offset = i * groupSize;
                int walkingMax = -1;
                for (int i = offset; i < offset + groupSize; ++i) {
                    if (testData[i] > walkingMax) {
                        walkingMax = testData[i];
                    }
                }
                std::cout << "Max of group " << i << ": " << walkingMax << "\n";
            }
        }
    }

    if (do6005) {
        std::cout << "-- test6005 --\n";
        std::vector<int> testData;
        std::vector<int> checkData;
        int addValue = 7;
        for (int i = 0; i < jobSize; ++i) {
            testData.push_back(dist(rng));
            checkData.push_back(testData.back() + addValue + 5);
        }

        auto result6005 = test6005(testData, addValue);

        int correctCount = 0;

        for (int i = 0; i < jobSize; i++) {
            std::cout << testData[i] << " -> " << result6005[i] << " (" << checkData[i] << ")\n";
            if (result6005[i] == checkData[i]) {
                correctCount++;
            }
        }

        if (correctCount == jobSize) {
            std::cout << "=> Correct!\n";
            correct.push_back(6005);
        } else {
            std::cout << "=> Incorrect!\n";
            incorrect.push_back(6005);


        }
    }

    if (do6006) {
        std::cout << "-- test6006 --\n";
        std::vector<int> testData;
        std::vector<int> checkData;
        for (int i = 0; i < jobSize; ++i) {
            testData.push_back(dist(rng));

            if (i > 0) {
                checkData.push_back(testData.back() + checkData.back());
            } else {
                checkData.push_back(testData.back());
            }
        }

        auto result6006 = test6006(testData);
        
        int correctCount = 0;

        for (int i = 0; i < jobSize; ++i) {
            std::cout << testData[i] << " -v> " << result6006[i] << " (" << checkData[i] << ")\n";
            if (result6006[i] == checkData[i]) {
                correctCount++;
            }
        }

        if (correctCount == jobSize) {
            std::cout << "=> Correct!\n";
            correct.push_back(6006);
        } else {
            std::cout << "=> Incorrect!\n";
            incorrect.push_back(6006);
        }
    }

    if (do6007) {
        std::cout << "-- test6007--\n";
        std::vector<int> testDataLeft;
        std::vector<int> testDataRight;
        std::vector<int> checkData;
        for (int i = 0; i < jobSize; i++) {
            testDataLeft.push_back(dist(rng));
            testDataRight.push_back(dist(rng));

            checkData.push_back(std::min(testDataLeft.back(), testDataRight.back()));
        }

        auto result6007 = test6007(testDataLeft, testDataRight);

        int correctCount = 0;
        
        for (int i = 0; i < jobSize; ++i) {
            std::cout << testDataLeft[i] << ", " << testDataRight[i] << " -> " << result6007[i] << " (" << checkData[i] << ")\n";
            if (result6007[i] == checkData[i]) {
                correctCount++;
            }
        }

        if (correctCount == jobSize) {
            std::cout << "=> Correct!\n";
            correct.push_back(6007);
        } else {
            std::cout << "=> Incorrect!\n";
            incorrect.push_back(6007);
        }
    }

    std::cout << "-- Correct tests: ";
    if (correct.size() > 0) {
        std::cout << correct[0];
    } else {
        std::cout << "None.";
    }
    for (int i = 1; i < correct.size(); ++i) {
        std::cout << ", " << correct[i];
    }
    std::cout << "\n";

    std::cout << "-- Incorrect tests: ";
    if (incorrect.size() > 0) {
        std::cout << incorrect[0];
    } else {
        std::cout << "None.";
    }
    for (int i = 1; i < incorrect.size(); ++i) {
        std::cout << ", " << incorrect[i];
    }
    std::cout << "\n";

    if (incorrect.size() == 0) {
        std::cout << "All's good!\n";
    } else {
        std::cout << "Test failed!\n";
    }

    std::cout << "Hywar Test Suite finished!\n";

    return 0;

}

int main() {
    doTest();

    std::cout << "Hywar:\n";

	int jobStep = 64;

    for (int i = 1; i < 11; ++i) {
        // int size = 64 + i * 12288;
		int size = 64 + i * jobStep;
        std::cout << "job size = " << std::to_string(size) << " time = " << benchmark(
                test6003,
                test6004,
                test6005,
                test6006,
                test6007,
                size
                ).count() << "\n";
    }

    std::cout << "Naive sequential:\n";
    for (int i = 1; i < 11; ++i) {
        // int size = 64 + i * 12288;
		int size = 64 + i * jobStep;
        std::cout << "job size = " << std::to_string(size) << " time = " << benchmark(
                seq6003,
                seq6004,
                seq6005,
                seq6006,
                seq6007,
                size
                ).count() << "\n";
    }
}
