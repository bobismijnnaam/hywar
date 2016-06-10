#include "utils.hpp"

std::string replace(std::string source, std::string target, std::string replacement) {
    size_t start = source.find(target);

    if (start != std::string::npos) {
        size_t end = start + target.size();
        std::string left = source.substr(0, start);
        std::string right = source.substr(end, source.size() - end);
        return left + replacement + right;
    }

    return source;
}

std::string params(std::vector<std::string> params_) {
    if (params_.size() == 0)
        return "";

    std::string source = params_[0];

    for (size_t i = 1; i < params_.size(); ++i) {
        source += ", " + params_[i];
    }
    
    return source;
}

std::string braces(std::string inner) {
    return "{" + inner + "}";
}
