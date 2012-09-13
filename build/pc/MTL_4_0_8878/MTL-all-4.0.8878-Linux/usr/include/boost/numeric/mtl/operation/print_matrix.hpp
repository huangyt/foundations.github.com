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

#ifndef MTL_PRINT_MATRIX_INCLUDE
#define MTL_PRINT_MATRIX_INCLUDE

#include <cstddef>
#include <iostream>
#include <boost/numeric/mtl/mtl_fwd.hpp>
#include <boost/numeric/mtl/concept/collection.hpp>
#include <boost/numeric/mtl/utility/tag.hpp>
#include <boost/numeric/mtl/utility/category.hpp>
#include <boost/numeric/mtl/utility/range_generator.hpp>

namespace mtl { namespace matrix {

template <typename Matrix>
std::ostream& print_matrix(Matrix const& matrix, std::ostream& out= std::cout, int width= 3, int precision= 2)
{
    // typedef typename Collection<Matrix>::size_type size_type;
    // all indices will start from 0; otherwise wrong
    for (std::size_t r= 0, nr= num_rows(matrix); r < nr; ++r) {
	out << '[';
	for (std::size_t c= 0, nc= num_cols(matrix); c < nc; ++c) {
	    out.fill (' '); out.width (width); // out.precision (precision); // out.flags (std::ios_base::right);
	    if (precision)
		out.precision(precision); 
	    out << matrix(r, c) << (c + 1 < nc ? " " : "]\n");
	}
    }
    return out;
}

}} // namespace mtl::matrix

#endif // MTL_PRINT_MATRIX_INCLUDE
