// File: main.cpp

#include <iostream>

#include <sys/resource.h>

#include "maze.hpp"

int main(int argc, char *argv[])
{
    rlimit rlp;
    getrlimit(RLIMIT_STACK, &rlp);
    rlp.rlim_cur = rlp.rlim_max;
    setrlimit(RLIMIT_STACK, &rlp);

    Maze<11> maze(argv[1]);

    Maze<11>::path_type path;

    maze.findPath(1,5,1, 7,3,7, path);

    for (auto i : path) {
        std::cout << std::get<0>(i) << ","
                  << std::get<1>(i) << ","
                  << std::get<2>(i) << std::endl;
    }

    return 0;
}
