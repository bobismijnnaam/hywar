#ifndef UTILS_HPP
#define UTILS_HPP

#include <string>
#include <vector>
#include <algorithm>

std::string replace(std::string source, std::string target, std::string replacement);
std::string params(std::vector<std::string> params_);
std::string braces(std::string inner);

template <typename U, typename Fun>
auto map(Fun f, std::vector<U> in) -> std::vector<decltype(f(*in.begin()))> {
    std::vector<decltype(f(*in.begin()))> results;
    std::transform(in.begin(),
            in.end(),
            std::back_inserter(results),
            f);
    return results;
}

#endif // UTILS_HPP
