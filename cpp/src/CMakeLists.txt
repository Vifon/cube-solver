set(cube-solver_SOURCES
  main.cpp
  )

set(cube-solver_HEADERS
  matrix3d.hpp
  maze.hpp
  )

add_executable(cube-solver ${cube-solver_SOURCES})
