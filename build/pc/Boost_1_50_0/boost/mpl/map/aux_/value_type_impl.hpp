
#ifndef BOOST_MPL_MAP_AUX_VALUE_TYPE_IMPL_HPP_INCLUDED
#define BOOST_MPL_MAP_AUX_VALUE_TYPE_IMPL_HPP_INCLUDED

// Copyright Aleksey Gurtovoy 2003-2004
// Copyright David Abrahams 2003-2004
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/mpl for documentation.

// $Id: value_type_impl.hpp 49267 2008-10-11 06:19:02Z agurtovoy $
// $Date: 2008-10-11 14:19:02 +0800 (星期六, 2008-10-11) $
// $Revision: 49267 $

#include <boost/mpl/value_type_fwd.hpp>
#include <boost/mpl/pair.hpp>
#include <boost/mpl/map/aux_/tag.hpp>

namespace boost {
namespace mpl {

template<>
struct value_type_impl< aux::map_tag >
{
    template< typename Map, typename T > struct apply
        : second<T>
    {
    };
};

}}

#endif // BOOST_MPL_MAP_AUX_VALUE_TYPE_IMPL_HPP_INCLUDED