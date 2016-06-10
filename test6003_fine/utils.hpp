#ifndef UTILS_HPP
#define UTILS_HPP

#include <string>
#include <memory>
#include <map>
#include <algorithm>

struct bracesb {
    bracesb(std::string &source, std::string end = "");
    ~bracesb();

    std::string &source;
    std::string initial;
    std::string end;
} ;

extern std::string tab;

std::string replace(std::string source, std::string target, std::string replacement);
std::string replace_once(std::string source, std::string target, std::string replacement);
std::vector<std::string> split(std::string source, std::string op);

// #TODO
// Posible optimization: cache the {!} regex, since it is created on every call to replace_once
std::string interpolate(std::string source, std::vector<std::string> args);
std::string params(std::vector<std::string> params_);
std::string braces(std::string inner);
std::string parentheses(std::string inner);
std::string quotes(std::string inner);
void nl(std::string &source);
std::string nl(std::vector<std::string> sources);
std::string indent(std::string block, std::string front);
std::string include(std::string header, bool quotes = true);

// (not so) Strangely enough this doubles the amount of hacks.
// (And yes, a backwards slash is called a hack. Right, Walnoot?)
std::string doubleHacks(std::string p);
std::string initlist(std::vector<std::string> parts);

void log(std::string msg);
void log(int msg);
void writeToFile(std::string file, std::string contents);

namespace std {
    std::string to_string(std::string);
}

std::string readFile(std::string filename);

template <typename T, typename U>
std::vector<T> extract(std::vector<U> units, std::map<U, T> mapping) {
    std::vector<T> results;
    std::transform(units.begin(),
            units.end(),
            std::back_inserter(results),
            [&](U unit) {
        return mapping[unit];
    });
    return results;
}

template <typename U, typename Fun>
auto map(Fun f, std::vector<U> in) -> std::vector<decltype(f(*in.begin()))> {
    std::vector<decltype(f(*in.begin()))> results;
    std::transform(in.begin(),
            in.end(),
            std::back_inserter(results),
            f);
    return results;
}

template <typename U, typename Fun>
std::vector<U> filter(Fun f, std::vector<U> in) {
    std::vector<U> results;
    std::copy_if(in.begin(),
            in.end(),
            std::back_inserter(results),
            f);
    return results;
}

int system(std::string command);

#endif // UTILS_HPP
