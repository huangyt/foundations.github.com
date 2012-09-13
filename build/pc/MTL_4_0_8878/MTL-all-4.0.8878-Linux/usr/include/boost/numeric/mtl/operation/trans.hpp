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

#ifndef MTL_TRANS_INCLUDE
#define MTL_TRANS_INCLUDE

#include <boost/numeric/mtl/mtl_fwd.hpp>
#include <boost/numeric/mtl/utility/tag.hpp>
#include <boost/numeric/mtl/utility/category.hpp>
#include <boost/numeric/mtl/utility/transposed_orientation.hpp>
#include <boost/numeric/mtl/matrix/transposed_view.hpp>
#include <boost/numeric/mtl/vector/parameter.hpp>
#include <boost/numeric/mtl/interface/vpt.hpp>

namespace mtl { 

namespace matrix {

    namespace sfunctor {

	// General case is not defined
	template <typename Value, typename AlgebraicCategory>
	struct trans {};

	template <typename Matrix>
	struct trans<Matrix, tag::matrix>
	{
	    typedef transposed_view<Matrix>               result_type;
	
	    static inline result_type apply(Matrix& matrix)
	    {
		return result_type(matrix);
	    }
	};

	// General case is not defined
	template <typename Value, typename AlgebraicCategory>
	struct const_trans {};

	template <typename Matrix>
	struct const_trans<Matrix, tag::matrix>
	{
	    typedef const transposed_view<const Matrix>   result_type;
	
	    static inline result_type apply(const Matrix& matrix)
	    {
		return result_type(matrix);
	    }
	};

    } // namespace sfunctor


    template <typename Value>
    typename sfunctor::const_trans<Value, typename mtl::traits::algebraic_category<Value>::type>::result_type 
    inline trans(const Value& v)
    {
    vampir_trace<3041> tracer;
	return sfunctor::const_trans<const Value, typename mtl::traits::algebraic_category<Value>::type>::apply(v);
    }

    template <typename Value>
    typename sfunctor::trans<Value, typename mtl::traits::algebraic_category<Value>::type>::result_type 
    inline trans(Value& v)
    {
    vampir_trace<3042> tracer;
	return sfunctor::trans<Value, typename mtl::traits::algebraic_category<Value>::type>::apply(v);
    }

} // namespace mtl::matrix


namespace vector {

    template <typename Vector>
    struct transposed_vector {};

    template <typename Parameters>
    struct transposed_parameters
    {
	typedef typename mtl::traits::transposed_orientation<typename Parameters::orientation>::type orientation; // switch
	typedef parameters<orientation, typename Parameters::dimension, false, typename Parameters::size_type>           type;        // not on stack!!!
    };

    template <typename Value, typename Parameters>
    struct transposed_vector<dense_vector<Value, Parameters> >
    {
	typedef dense_vector<Value, typename transposed_parameters<Parameters>::type>           type;
    };

    template <typename Value, typename Parameters>
    struct transposed_vector<strided_vector_ref<Value, Parameters> >
    {
	typedef strided_vector_ref<Value, typename transposed_parameters<Parameters>::type>     type;
    };

///Returns tranposed view of %vector v
    template <typename Vector>
    typename transposed_vector<Vector>::type const
    inline trans(const Vector& v)
    {
    vampir_trace<2037> tracer;
	typedef typename transposed_vector<Vector>::type  type;
	return type(size(v), &const_cast<Vector&>(v)[0]);
    }

    template <typename Vector>
    typename transposed_vector<Vector>::type
    inline trans(Vector& v)
    {
    vampir_trace<2038> tracer;
	typedef typename transposed_vector<Vector>::type  type;
	return type(size(v), &v[0]);
    }
}

} // mtl


#endif // MTL_TRANS_INCLUDE
