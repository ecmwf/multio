
#include "sandbox/MultioThreadTool.h"

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multio::sandbox::MultioThreadTool tool(argc, argv);
    return tool.start();
}
