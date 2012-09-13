// MTL4 example: minimize complexity of traversal

#include <iostream>
#include <boost/numeric/mtl/mtl.hpp>

using namespace mtl;
    
template <typename Matrix>
void f(Matrix& m)
{
    using traits::range_generator; using traits::range::min;

    // Set values in diagonal
    m= 7.0;
    
    // Choose between row and column traversal
    typedef typename min<range_generator<tag::row, Matrix>, range_generator<tag::col, Matrix> >::type range_type;
    range_type                                                      my_range;

    // Type of outer cursor
    typedef typename range_type::type                               c_type;
    // Type of inner cursor
    typedef typename traits::range_generator<tag::nz, c_type>::type ic_type;

    // Define the property maps
    typename traits::row<Matrix>::type                              row(m); 
    typename traits::col<Matrix>::type                              col(m);
    typename traits::const_value<Matrix>::type                      value(m); 

    // Now iterate over the matrix    
    for (c_type cursor(my_range.begin(m)), cend(my_range.end(m)); cursor != cend; ++cursor)
       for (ic_type icursor(begin<tag::nz>(cursor)), icend(end<tag::nz>(cursor)); icursor != icend; ++icursor)
	   std::cout << "matrix[" << row(*icursor) << ", " << col(*icursor) << "] = " << value(*icursor) << '\n';    
}


int main(int, char**)
{
    // Define a CRS matrix
    compressed2D<double>                                  A(3, 3);
    // And a CCS matrix
    compressed2D<double, matrix::parameters<col_major> >  B(3, 3);

    f(A);
    f(B);
    
    return 0;
}
