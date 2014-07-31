// File: matrix3d.hpp
#ifndef _h_MATRIX3D_
#define _h_MATRIX3D_

#include <array>
#include <stdexcept>
#include <memory>

//////////////////////////////////////////////////////////////////////

template <typename T, size_t N>
class Matrix3D
{
  public:
    Matrix3D();
    Matrix3D(const T& initial_value);

    T& operator()(size_t x, size_t y, size_t z);
    const T& operator()(size_t x, size_t y, size_t z) const;

  private:
    std::unique_ptr<std::array<T,N*N*N> > array_;
};

//////////////////////////////////////////////////////////////////////

template <typename T, size_t N>
Matrix3D<T,N>::Matrix3D()
    : array_(new std::array<T,N*N*N>)
{
}

////////////////////////////////////////

template <typename T, size_t N>
Matrix3D<T,N>::Matrix3D(const T& initial_value)
    : array_(new std::array<T,N*N*N>)
{
    array_->fill(initial_value);
}

////////////////////////////////////////

template <typename T, size_t N>
T& Matrix3D<T,N>::operator()(size_t x, size_t y, size_t z)
{
    if (x >= N || y >= N || z >= N) {
        throw std::out_of_range(__func__);
    }

    return array_->at(x*N*N +
                      y*N   +
                      z);
}

////////////////////////////////////////

template <typename T, size_t N>
const T& Matrix3D<T,N>::operator()(size_t x, size_t y, size_t z) const
{
    if (x >= N || y >= N || z >= N) {
        throw std::out_of_range(__func__);
    }

    return array_->at(x*N*N +
                      y*N   +
                      z);
}

//////////////////////////////////////////////////////////////////////

#endif
