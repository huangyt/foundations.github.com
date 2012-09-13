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

#ifndef ITL_BICGSTAB_2_INCLUDE
#define ITL_BICGSTAB_2_INCLUDE

#include <boost/numeric/mtl/concept/collection.hpp>
#include <boost/numeric/mtl/utility/exception.hpp>
#include <boost/numeric/linear_algebra/identity.hpp>
#include <boost/numeric/mtl/operation/resource.hpp>

namespace itl {

/// Bi-Conjugate Gradient Stabilized(2)
template < typename LinearOperator, typename Vector, 
	   typename Preconditioner, typename Iteration >
int bicgstab_2(const LinearOperator &A, Vector &x, const Vector &b,
	       const Preconditioner &, Iteration& iter)
{
    typedef typename mtl::Collection<Vector>::value_type Scalar;
    const Scalar zero= math::zero(Scalar()), one= math::one(Scalar());
    Scalar     alpha(zero), beta, gamma, mu, nu, rho_0(one), rho_1, tau, omega_1, omega_2(one);
    Vector     r(b - A * x), r_0(r), r_i(r), x_i(x), 
	       s(resource(x)), t(resource(x)), u(resource(x), zero), v(resource(x)), w(resource(x));

    if (size(b) == 0) throw mtl::logic_error("empty rhs vector");
    while ( ! iter.finished(r)) {
	++iter;
	rho_0*= -omega_2;
	// z= solve(M, r); z_tilde= solve(M, r_tilde); ???

	rho_1= dot(r_0, r_i);       // or rho_1= dot(z, r_tilde) ???
	beta= alpha * rho_1 / rho_0; rho_0= rho_1;
	u= r_i - beta * u;
	v= A * u;
	gamma= dot(v, r_0); alpha= rho_0 / gamma;
	r= r_i - alpha * v;
	s= A * r;
	x= x_i + alpha * u;

	rho_1= dot(r_0, s); beta= alpha * rho_1 / rho_0; rho_0= rho_1;
	v= s - beta * v;
	w= A * v;
	gamma= dot(w, r_0); alpha= rho_0 / gamma;
	u= r - beta * u;
	r-= alpha * v;
	s-= alpha * w;
	t= A * s;

	omega_1= dot(r, s); mu= dot(s, s); nu= dot(s, t); tau= dot(t, t);
	omega_2= dot(r, t); tau-= nu * nu / mu; omega_2= (omega_2 - nu * omega_1 / mu) / tau;
	omega_1= (omega_1 - nu * omega_2) / mu;
	x_i= x + omega_1 * r + omega_2 * s + alpha * u;
	r_i= r - omega_1 * s - omega_2 * t;
	u-= omega_1 * v + omega_2 * w;
    }
    return iter;
}

} // namespace itl

#endif // ITL_BICGSTAB_2_INCLUDE






