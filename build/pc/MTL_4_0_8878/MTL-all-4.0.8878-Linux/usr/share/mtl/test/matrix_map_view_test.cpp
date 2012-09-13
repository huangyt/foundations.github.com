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
#include <complex>
#include <boost/numeric/mtl/matrix/dense2D.hpp>
#include <boost/numeric/mtl/matrix/morton_dense.hpp> 
#include <boost/numeric/mtl/matrix/compressed2D.hpp> 
#include <boost/numeric/mtl/matrix/map_view.hpp>
#include <boost/numeric/mtl/matrix/hermitian_view.hpp>
#include <boost/numeric/mtl/matrix/inserter.hpp>
#include <boost/numeric/mtl/recursion/predefined_masks.hpp>
#include <boost/numeric/mtl/operation/print.hpp>
#include <boost/numeric/mtl/operation/set_to_zero.hpp>
#include <boost/numeric/mtl/operation/conj.hpp>
#include <boost/numeric/mtl/operation/scale.hpp>
#include <boost/numeric/mtl/operation/hermitian.hpp>
#include <boost/numeric/mtl/operation/operators.hpp>
#include <boost/numeric/mtl/operation/mult_result.hpp>
#include <boost/numeric/mtl/utility/ashape.hpp>


using namespace std;  

typedef complex<double> ct;

double value(double)
{
    return 7.0;
}

complex<double> value(complex<double>)
{
    return ct(7.0, 1.0);
}

// scaled value
double svalue(double)
{
    return 14.0;
}

ct svalue(ct)
{
    return ct(14.0, 2.0);
}

// conjugated value
double cvalue(double)
{
    return 7.0;
}

ct cvalue(ct)
{
    return ct(7.0, -1.0);
}

// complex scaled value
ct csvalue(double)
{
    return ct(0.0, 7.0);
}

ct csvalue(ct)
{
    return ct(-1.0, 7.0);
}


template <typename Matrix>
void test(Matrix& matrix, const char* name)
{
    using mtl::conj;

    set_to_zero(matrix);
    typename Matrix::value_type ref(0);

    {
	mtl::matrix::inserter<Matrix>  ins(matrix);
	ins(2, 3) << value(ref);
	ins(4, 3) << value(ref) + 1.0;
	ins(2, 5) << value(ref) + 2.0;
    }

    cout << "\n\n" << name << "\n";
    cout << "Original matrix:\n" << matrix << "\n";

    mtl::matrix::scaled_view<double, Matrix>  scaled_matrix(2.0, matrix);
    cout << "matrix  scaled with 2.0\n" << scaled_matrix << "\n";
    MTL_THROW_IF(scaled_matrix(2, 3) != svalue(ref), mtl::runtime_error("scaling wrong"));
   
    cout << "matrix  scaled with 2.0 (as operator)\n" << 2.0 * matrix << "\n";
    MTL_THROW_IF((2.0 * matrix)(2, 3) != svalue(ref), mtl::runtime_error("scaling wrong"));

 
    mtl::matrix::conj_view<Matrix>  conj_matrix(matrix);
    cout << "conjugated matrix\n" << conj_matrix << "\n";
    MTL_THROW_IF(conj_matrix(2, 3) != cvalue(ref), mtl::runtime_error(" wrong"));

    mtl::matrix::scaled_view<ct, Matrix>  cscaled_matrix(ct(0.0, 1.0), matrix);
    cout << "matrix scaled with i (complex(0, 1))\n" << cscaled_matrix << "\n";
    MTL_THROW_IF(cscaled_matrix(2, 3) != csvalue(ref), mtl::runtime_error("complex scaling wrong"));

    mtl::matrix::hermitian_view<Matrix>  hermitian_matrix(matrix);
    cout << "Hermitian matrix (conjugate transposed)\n" << hermitian_matrix << "\n";
    MTL_THROW_IF(hermitian_matrix(3, 2) != cvalue(ref), mtl::runtime_error("conjugate transposing  wrong"));

    cout << "matrix  scaled with 2.0 (free function)\n" << scale(2.0, matrix) << "\n";
    MTL_THROW_IF(scale(2.0, matrix)(2, 3) != svalue(ref), mtl::runtime_error("scaling wrong"));

    cout << "matrix  scaled with 2.0 (free function as mtl::scale)\n" << mtl::scale(2.0, matrix) << "\n";

    cout << "conjugated matrix (free function) \n" << conj(matrix) << "\n";
    MTL_THROW_IF(conj(matrix)(2, 3) != cvalue(ref), mtl::runtime_error("conjugating wrong"));

    cout << "matrix scaled with i (complex(0, 1)) (free function)\n" << scale(ct(0.0, 1.0), matrix) << "\n";
    MTL_THROW_IF(scale(ct(0.0, 1.0), matrix)(2, 3) != csvalue(ref), mtl::runtime_error("complex scaling wrong"));

    cout << "Hermitian  matrix (conjugate transposed) (free function)\n" << hermitian(matrix) << "\n";
    MTL_THROW_IF(hermitian(matrix)(3, 2) != cvalue(ref), mtl::runtime_error("conjugate transposing wrong"));
}



int main(int argc, char* argv[])
{
    using namespace mtl;
    unsigned size= 7; 
    if (argc > 1) size= atoi(argv[1]); 

    dense2D<double>                                      dr(size, size);
    dense2D<double, matrix::parameters<col_major> >      dc(size, size);
    morton_dense<double, recursion::morton_z_mask>       mzd(size, size);
    morton_dense<double, recursion::doppled_2_row_mask>  d2r(size, size);
    compressed2D<double>                                 cr(size, size);
    compressed2D<double, matrix::parameters<col_major> > cc(size, size);

    dense2D<complex<double> >                            drc(size, size);
    compressed2D<complex<double> >                       crc(size, size);


    test(dr, "Dense row major");
    test(dc, "Dense column major");
    test(mzd, "Morton Z-order");
    test(d2r, "Hybrid 2 row-major");
    test(cr, "Compressed row major");
    test(cc, "Compressed column major");
    test(drc, "Dense row major complex");
    test(crc, "Compressed row major complex");

    return 0;
}
