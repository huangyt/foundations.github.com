// Software License for MTL
// 
// Copyright (c) 2007 The Trustees of Indiana University.
//               2008 Dresden University of Technology and the Trustees of Indiana University.
//               2010 SimuNova UG (haftungsbeschränkt), www.simunova.com.
// All rights reserved.
// Authors: Peter Gottschling and Andrew Lumsdaine
// 
// This file is part of the Matrix Template Library
// 
// See also license.mtl.txt in the distribution.

#include <iostream>
#include <cmath>
#include <boost/numeric/mtl/mtl.hpp>
 
template <typename Matrix, typename Vector>
void test(const char* A_string, const char* v_string, const Matrix& A, const Vector&x)
{
    std::cout << "\n" << A_string << "ly sized matrix and " << v_string << "ly sized vector\nA is\n" << A;

    // asm("#mat_add begins here!");
    Matrix B(A + A);
    // asm("#mat_add ends here!");
    std::cout << "A+A = \n" << B;
    MTL_THROW_IF(B[0][0] != 4.0, mtl::runtime_error("wrong result in matrix addition."));

    // asm("#mat_mult begins here!");
    B= A * A;
    // asm("#mat_mult ends here!");
    std::cout << "A*A = \n" << B;
    MTL_THROW_IF(B[0][0] != 16.0, mtl::runtime_error("wrong result in matrix product."));

    // asm("#vec_add begins here!");
    Vector w(x + x);
    // asm("#vec_add ends here!");
    std::cout << "x = " << x << "\nw = x+x = " << w << "\n";
    MTL_THROW_IF(w[0] != 6.0, mtl::runtime_error("wrong result in vector addition."));

    // asm("#mat_vec_mult begins here!");
    w= A * x;
    // asm("#mat_vec_mult ends here!");


    std::cout << "A*x = " << w << "\n";
    MTL_THROW_IF(w[0] != 18.0, mtl::runtime_error("wrong result in matrix vector product."));
}


int main(int , char**)
{
    using namespace mtl;
    typedef mtl::vector::parameters<tag::col_major, mtl::vector::fixed::dimension<2>, true> fvec_para;
    typedef matrix::parameters<tag::row_major, mtl::index::c_index, mtl::fixed::dimensions<2, 2>, true> fmat_para;

    float ma[2][2]= {{2., 3.}, {4., 5.}}, va[2]= {3., 4.};
    
    dense2D<float>                   A_dyn(ma);
    dense2D<float, fmat_para>        A_stat(ma);
    dense_vector<float>              v_dyn(va);
    dense_vector<float, fvec_para>   v_stat(va);

    typedef mtl::vector::parameters<tag::col_major, mtl::vector::fixed::dimension<4>, true> fvec_para4;
    dense_vector<double, fvec_para4>   v_stat4, w_stat4;
    v_stat4= 3;
#if 0 // introduce values unknown at compile time
    double x;
    std::cin >> x;
    v_stat4[0]= x;
#endif

    w_stat4= v_stat4 + v_stat4;
    std::cout << "w_stat4 is " << w_stat4 << '\n';

#if 0
    test("dynamic", "dynamic", A_dyn, v_dyn);
    test("dynamic", "static", A_dyn, v_stat);
    test("static", "dynamic", A_stat, v_dyn);
#endif 
    test("static", "static", A_stat, v_stat);

    return 0;
}

