#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

// Utility function to strip all spaces and '&' symbols from a string
std::string stripSpaces(const std::string& input) {
    std::string result;
    for (char ch : input) {
        if (ch != ' ') {
            result += ch;
        }
    }
    return result;
}

// Utility function to strip all spaces and '&' symbols from a string
std::string stripAmpersands(const std::string& input) {
    std::string result;
    for (char ch : input) {
        if (ch != '&') {
            result += ch;
        }
    }
    return result;
}

// Utility function to tokenize a string based on a given delimiter
std::vector<std::string> tokenize(const std::string& str, char delimiter) {
    std::vector<std::string> tokens;
    std::stringstream ss(str);
    std::string token;
    while (std::getline(ss, token, delimiter)) {
        tokens.push_back(token);
    }
    return tokens;
}

int main(int argc, char* argv[]) {
    // Check for the correct number of arguments
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>" << std::endl;
        return 1;
    }

    // Open the file passed as a command-line argument
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << argv[1] << std::endl;
        return 1;
    }

    std::string line, concatenatedLine;
    std::regex startPattern(R"(^\s*PP_THREAD_SAFE\s+)");
    std::regex newFramePattern(R"(^\s*PP_DEBUG_PUSH_FRAME\(\)$)");
    std::vector<std::string> dummy_args;
    bool readingMultiLine = false;
    bool searchForNewFrame = false;

    while (std::getline(file, line)) {
        std::cout << line << std::endl;

        // Check if the line starts with 'PP_THREAD_SAFE'
        if (std::regex_search(line, startPattern)) {
            concatenatedLine.clear();  // Start reading a new line
            dummy_args.clear();
            readingMultiLine = true;
        }
        const std::string lineWithoutSpaces = stripSpaces(line);

        if (readingMultiLine) {
            concatenatedLine += stripAmpersands(lineWithoutSpaces);

            // Check if the concatenated line ends without '&' symbol (after stripping)
            if (lineWithoutSpaces.back() != '&') {
                readingMultiLine = false;

                // Now that we have the full concatenated line, process it
                std::regex pattern(R"(^.*\(([^)]*)\)RESULT.*$)");
                std::smatch match;
                if (std::regex_search(concatenatedLine, match, pattern)) {
                    // Extract the content within the first set of parentheses
                    std::string params = match[1].str();

                    // Tokenize the parameters using ',' as the separator
                    std::vector<std::string> tokens = tokenize(params, ',');

                    // Print the tokens
                    std::cout << "Extracted Tokens:\n";
                    for (const auto& token : tokens) {
                        dummy_args.push_back(token);
                        // std::cout << token << std::endl;
                    }
                    searchForNewFrame = true;
                }
                else {
                    std::cerr << "Pattern not matched in the line: " << concatenatedLine << std::endl;
                }
            }
        }


        if (searchForNewFrame) {
            if (std::regex_search(line, newFramePattern)) {
                // std::cout << "Found new frame\n";
                // std::cout << "Arguments:\n";
                std::cout << std::endl;
                std::cout << "    ! Add Dummy arguments to the current error frame" << std::endl;
                int cnt = 0;
                for (const auto& arg : dummy_args) {
                    std::cout << "    PP_DEBUG_PUSH_DUMMY_TO_FRAME( " << ++cnt << ", \"" << arg << "\", " << arg << " )"
                              << std::endl;
                }
                searchForNewFrame = false;
            }
        }
    }

    file.close();
    return 0;
}
