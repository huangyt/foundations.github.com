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

#ifndef ITL_CGS_INCLUDE
#define ITL_CGS_INCLUDE

#include <boost/numeric/mtl/concept/collection.hpp>
#include <boost/numeric/mtl/operation/resource.hpp>
#include <boost/numeric/mtl/operation/dot.hpp>

namespace itl {

/// Conjugate Gradient Squared
template < typename LinearOperator, typename Vector, 
	   typename Preconditioner, typename Iteration >
int cgs(const LinearOperator &A, Vector &x, const Vector &b,
	const Preconditioner &M, Iteration& iter)
{
    typedef typename mtl::Collection<Vector>::value_type Scalar;
    Scalar     rho_1(0), rho_2(0), alpha(0), beta(0);
    Vector     p(resource(x)), phat(resource(x)), q(resource(x)), qhat(resource(x)), vhat(resource(x)),
	       u(resource(x)), uhat(resource(x)), r(b - A * x), rtilde= r;

    while (! iter.finished(r)) {
	++iter;
	rho_1= dot(rtilde, r);

	if (rho_1 == 0.) iter.fail(2, "cgs breakdown");

	if (iter.first())
	    p= u= r;
	else {
	    beta = rho_1 / rho_2;
	    u= r + beta * q;
	    p= u + beta * (q + beta * p);
	}

        vhat= A * Vector(solve(M, p));
	alpha = rho_1 / dot(rtilde, vhat);
	q= u - alpha * vhat;

	u+= q;
	uhat= solve(M, u);
	
	x+= alpha * uhat;
	qhat= A * uhat;
	r-= alpha * qhat;

	rho_2= rho_1;
    }
    return iter;
}

} // namespace itl

#endif // ITL_CGS_INCLUDE



