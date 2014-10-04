// File: maze.hpp
#ifndef _h_MAZE_
#define _h_MAZE_

#include <deque>
#include <fstream>
#include <tuple>
#include <utility>
#include <cassert>

#include "matrix3d.hpp"

template <size_t N>
class Maze
{
  public:
    typedef std::deque<std::tuple<size_t, size_t, size_t> > path_type;


    Maze(const char* filepath)
    {
        std::ifstream file(filepath);

        bool walls[3][N][N];

        for (int i = 0; i < 3; ++i) {
            for (int y = 0; y < N; ++y) {
                for (int x = 0; x < N; ++x) {
                    file >> walls[i][y][x];
                }
            }
        }

        for (size_t z = 0; z < N; ++z) {
            for (size_t y = 0; y < N; ++y) {
                for (size_t x = 0; x < N; ++x) {
                    layout_(x,y,z) =
                        walls[0][y][x] ||
                        walls[1][y][z] ||
                        walls[2][N-z-1][x];
                }
            }
        }
    }

    bool findPath(size_t x,
                  size_t y,
                  size_t z,
                  const size_t fin_x,
                  const size_t fin_y,
                  const size_t fin_z,
                  path_type& path)
    {
        if (layout_(fin_x, fin_y, fin_z) == true) {
            return false;
        } else {
            return findPath_NO_CHECK(x, y, z,
                                     fin_x, fin_y, fin_z,
                                     path);
        }
    }


  private:
    Matrix3D<bool, N> layout_;

    bool findPath_NO_CHECK(size_t x,
                           size_t y,
                           size_t z,
                           const size_t fin_x,
                           const size_t fin_y,
                           const size_t fin_z,
                           path_type& path)
    {
        // we cannot move in here
        try {
            if (layout_(x,y,z) == true) {
                return false;
            }
        } catch (const std::out_of_range& e) {
            return false;
        }

        if (x == fin_x && y == fin_y && z == fin_z) {
            path.push_front(std::make_tuple(x,y,z));
            return true;
        }

        layout_(x,y,z) = true;  // place a wall to prevent going back

        // check all the adjacent nodes
        if (findPath_NO_CHECK(x+1, y  , z  , fin_x , fin_y , fin_z , path) ||
            findPath_NO_CHECK(x  , y+1, z  , fin_x , fin_y , fin_z , path) ||
            findPath_NO_CHECK(x  , y  , z+1, fin_x , fin_y , fin_z , path) ||
            findPath_NO_CHECK(x-1, y  , z  , fin_x , fin_y , fin_z , path) ||
            findPath_NO_CHECK(x  , y-1, z  , fin_x , fin_y , fin_z , path) ||
            findPath_NO_CHECK(x  , y  , z-1, fin_x , fin_y , fin_z , path)) {

            path.push_front(std::make_tuple(x,y,z));
            return true;
        }

        return false;
    }
};

#endif
