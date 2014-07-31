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


    Maze<9> maze(argv[1]);

    Maze<9>::path_type path;

    maze.findPath(0,4,0, 6,2,6, path);

    for (auto i : path) {
        std::cout << std::get<0>(i) << ","
                  << std::get<1>(i) << ","
                  << std::get<2>(i) << std::endl;
    }

    return 0;
}
