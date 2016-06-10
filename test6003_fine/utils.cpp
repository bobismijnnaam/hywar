#include <string>
#include <functional>
#include <regex>
#include <fstream>
#include <iostream>
#include <vector>

#include "utils.hpp"

namespace std {
    string to_string(string str) {
        return str;
    }
}


bracesb::bracesb(std::string &source, std::string end) : source{source}, end{end} {
    initial = source;
    source = "";
}

bracesb::~bracesb() {
    source = indent(source, tab);
    source = initial + "{\n" + source + "\n}" + end;
}

std::string tab = "    ";

std::string replace(std::string source, std::string target, std::string replacement) {
    std::regex re(target);

    return std::regex_replace(source, re, replacement);
}

std::string replace_once(std::string source, std::string target, std::string replacement) {
    std::regex re(target);

    return std::regex_replace(source, re, replacement, std::regex_constants::format_first_only);
}

std::vector<std::string> split(std::string source, std::string op) {
    std::vector<std::string> parts;
    size_t pointer = 0;
    
    while (pointer != std::string::npos) {
        size_t occurrence = source.find(op, pointer);

        if (occurrence == std::string::npos) {
            pointer = occurrence;
            continue;
        } 
        
        if (pointer == occurrence) {
            pointer += op.size();
            continue;
        }

        parts.push_back(source.substr(pointer, occurrence - pointer));

        pointer = occurrence + op.size();
    }

    return parts;
}

// #OPTIMIZEME
// Posible optimization: cache the {!} regex, since it is created on every call to replace_once
std::string interpolate(std::string source, std::vector<std::string> args) {
    for (auto arg : args) {
        source = replace_once(source, "\\{!\\}", arg);
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

std::string parentheses(std::string inner) {
    return "(" + inner + ")";
}

std::string quotes(std::string inner) {
    return "\"" + inner + "\"";
}

void nl(std::string &source) {
    source += "\n";
}

std::string nl(std::vector<std::string> sources) {
    if (sources.size() == 0)
        return "";

    std::string source = sources[0];

    for (size_t i = 1; i < sources.size(); ++i) {
        source += "\n" + sources[i];
    }
    
    return source;
}

std::string indent(std::string block, std::string front = tab) {
    return front + replace(block, "\n", "\n" + front);
}

std::string include(std::string header, bool quotes) {
    if (quotes) {
        return "#include \"" + header + "\"\n";
    } else {
        return "#include <" + header + ">\n";
    }
}

// (not so) Strangely enough this doubles the amount of hacks.
// (And yes, a backwards slash is called a hack. Right, Walnoot?)
std::string doubleHacks(std::string p) {
    return replace(p, "\\\\", "\\\\");
}

std::string initlist(std::vector<std::string> parts) {
    return braces(params(parts));
}

void log(std::string msg) {
	std::cout << msg << "\n";
}

void log(int msg) {
	log(std::to_string(msg));
}

void writeToFile(std::string file, std::string contents) {
    std::ofstream fout(file);
    fout << contents;
}

std::string readFile(std::string filename) {
	std::ifstream is(filename);
	return std::string((std::istreambuf_iterator<char>(is)), std::istreambuf_iterator<char>());
}

int system(std::string command) {
    return system(command.c_str());
}
