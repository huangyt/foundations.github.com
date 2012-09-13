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

#ifndef MTL_MATRIX_MULTI_VECTOR_INCLUDE
#define MTL_MATRIX_MULTI_VECTOR_INCLUDE


#include <boost/numeric/mtl/mtl_fwd.hpp>
#include <boost/numeric/mtl/concept/collection.hpp>
#include <boost/numeric/mtl/matrix/parameter.hpp>
#include <boost/numeric/mtl/matrix/crtp_base_matrix.hpp>
#include <boost/numeric/mtl/matrix/mat_expr.hpp>
#include <boost/numeric/mtl/matrix/multi_vector_range.hpp>
#include <boost/numeric/mtl/vector/parameter.hpp>


// Under development (to be used with caution)

namespace mtl { namespace matrix {

    
// Might need to be defined later
struct multi_vector_key {};

/// Matrix constituting of set of column vectors (under development)
template <typename Vector>
class multi_vector
  : public base_matrix<typename mtl::Collection<Vector>::value_type, parameters<> >,
    public crtp_base_matrix< multi_vector<Vector>, typename Collection<Vector>::value_type, 
			     typename Collection<Vector>::size_type>,
    public mat_expr< multi_vector<Vector> >    
{
    typedef base_matrix<typename Collection<Vector>::value_type, parameters<> >           super;

    // Vector must by column vector
    BOOST_STATIC_ASSERT((boost::is_same<typename OrientedCollection<Vector>::orientation,
			                tag::col_major>::value));
  public:
    typedef multi_vector                             self;
    // typedef mtl::matrix::parameters<>                parameters;
    typedef tag::col_major                           orientation;
    typedef typename Collection<Vector>::value_type  value_type;
    typedef typename Collection<Vector>::size_type   size_type;
    typedef const value_type&                        const_reference;
    typedef value_type&                              reference;
    typedef multi_vector_key                         key_type;
    typedef crtp_matrix_assign< self, value_type, size_type >    assign_base;

    /// Constructor by number of rows and columns
    multi_vector(size_type num_rows, size_type num_cols)
	: super(non_fixed::dimensions(num_rows, num_cols)), 
	  data(num_cols, Vector(num_rows))
    {
	this->my_nnz= num_rows * num_cols;
    }

    /// Constructor column vector and number of columns (for easier initialization)
    multi_vector(const Vector& v, size_type num_cols)
      : super(non_fixed::dimensions(size(v), num_cols)),
	data(num_cols, v)
    {
	this->my_nnz= num_cols * size(v);
    }

    /// Consuming assignment operator
    self& operator=(self src)
    {
	// Self-copy would be an indication of an error
	assert(this != &src);
	
	check_dim(src.num_rows(), src.num_cols());
	swap(*this, src);
	return *this;
    }

    using assign_base::operator=;

    const_reference operator() (size_type i, size_type j) const { return data[j][i]; }
    reference operator() (size_type i, size_type j) { return data[j][i]; }

    Vector& vector(size_type i) { return data[i]; }
    const Vector& vector(size_type i) const { return data[i]; }

    multi_vector_range<Vector> vector(irange const& r) const { return multi_vector_range<Vector>(*this, r); }

  protected:  
	mtl::vector::dense_vector<Vector, mtl::vector::parameters<> >          data;
};

/// Number of rows
template< typename Vector >
typename Collection< Vector >::size_type num_cols(const multi_vector< Vector >& A) { return A.num_cols(); }

/// Number of columns
template< typename Vector >
typename Collection< Vector >::size_type num_rows(const multi_vector< Vector >& A) { return A.num_rows(); }

/// Size as defined by number of rows times columns
template< typename Vector >
typename Collection< Vector >::size_type size(const multi_vector< Vector >& A) { return num_rows(A) * num_cols(A); }
}} // namespace mtl::matrix

namespace mtl {
	using matrix::multi_vector;
}

#endif // MTL_MATRIX_MULTI_VECTOR_INCLUDE
