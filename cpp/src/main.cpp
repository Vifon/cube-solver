// File: main.cpp

#include <iostream>

#include "maze.hpp"

int main(int argc, char *argv[])
{
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
