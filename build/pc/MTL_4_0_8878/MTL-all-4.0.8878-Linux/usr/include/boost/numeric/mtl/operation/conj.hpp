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

#ifndef MTL_CONJ_INCLUDE
#define MTL_CONJ_INCLUDE

#include <boost/numeric/mtl/mtl_fwd.hpp>
#include <boost/numeric/mtl/utility/enable_if.hpp>
#include <boost/numeric/mtl/utility/tag.hpp>
#include <boost/numeric/mtl/utility/category.hpp>
#include <boost/numeric/linear_algebra/identity.hpp>
#include <boost/numeric/mtl/matrix/map_view.hpp>
#include <boost/numeric/mtl/vector/map_view.hpp>

#include <complex>

namespace mtl {

namespace sfunctor {

    template <typename Value, typename AlgebraicCategory>
    struct conj_aux
    {
	typedef Value result_type;

	static inline result_type apply(const Value& v)
	{
	    return v;
	}

	result_type operator() (const Value& v) const
	{
	    return v;
	}
    };


    template <typename Value, typename AlgebraicCategory>
    struct conj_aux<std::complex<Value>, AlgebraicCategory>
    {
	typedef std::complex<Value> result_type;

	static inline result_type apply(const std::complex<Value>& v)
	{
	    return std::conj(v);
	}

	result_type operator() (const std::complex<Value>& v) const
	{
	    return std::conj(v);
	}
    };

    template <typename Matrix>
    struct conj_aux<Matrix, tag::matrix>
    {
	typedef matrix::conj_view<Matrix> result_type;

	static inline result_type apply(const Matrix& matrix)
	{
	    return result_type(matrix);
	}

	result_type operator() (const Matrix& matrix) const
	{
	    return apply(matrix);
	}
    };

    template <typename Vector>
    struct conj_aux<Vector, tag::vector>
    {
	typedef mtl::vector::conj_view<Vector> result_type;

	static inline result_type apply(const Vector& vector)
	{
	    return result_type(vector);
	}

	result_type operator() (const Vector& vector) const
	{
	    return apply(vector);
	}
    };

    // Short cut for result type
    template <typename Value>
    struct conj
	: public conj_aux<Value, typename mtl::traits::algebraic_category<Value>::type>
    {};

} // namespace sfunctor
    
    namespace vector {

	/// Conjugate of an vector
	template <typename Vector>
	typename mtl::traits::enable_if_vector<Vector, conj_view<Vector> >::type
	inline conj(const Vector& v)
	{
	    return conj_view<Vector>(v);
	}
    } 

    namespace matrix {

	/// Conjugate of a matrix
	template <typename Matrix>
	typename mtl::traits::enable_if_matrix<Matrix, conj_view<Matrix> >::type
	inline conj(const Matrix& v)
	{
	    return conj_view<Matrix>(v);
	}
    } 

    namespace scalar {

	// Only scalar values remain here
	template <typename Value>
	typename mtl::traits::enable_if_scalar<
	    Value
	  , typename sfunctor::conj<Value>::result_type
	>::type
	inline conj(const Value& v)
	{
	    return mtl::sfunctor::conj<Value>::apply(v);
	}

	float inline conj(float v) { return v; }
	double inline conj(double v) { return v; }
	long double inline conj(long double v) { return v; }
    }

    /// Conjugate of vector, matrix, or scalar
    using vector::conj;
    using matrix::conj;
    using scalar::conj;

} // namespace mtl

#endif // MTL_CONJ_INCLUDE
