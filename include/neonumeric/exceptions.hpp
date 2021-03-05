// exceptions.hpp
/*
 *  Copyright (c) 2019 Leigh Johnston.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 *     * Neither the name of Leigh Johnston nor the names of any
 *       other contributors to this software may be used to endorse or
 *       promote products derived from this software without specific prior
 *       written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 *  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 *  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

#include <stdexcept>

namespace neonumeric
{
    struct not_yet_implemented : std::logic_error { not_yet_implemented() : std::logic_error("neonumeric::not_yet_implemented") {} };
    struct number_allocation_failure : std::logic_error { number_allocation_failure() : std::logic_error("neonumeric::number_allocation_failure") {} };
    struct number_too_large : std::domain_error { number_too_large() : std::domain_error("neonumeric::number_too_large") {} };
    struct invalid_string : std::invalid_argument { invalid_string() : std::invalid_argument("neonumeric::invalid_string") {} };
    struct divide_by_zero : std::domain_error { divide_by_zero() : std::domain_error("neonumeric::divide_by_zero") {} };
    struct out_of_range : std::out_of_range { out_of_range() : std::out_of_range("neonumeric::out_of_range") {} };
    struct invalid_partial_value : std::invalid_argument { invalid_partial_value() : std::invalid_argument("neonumeric::invalid_partial_value") {} };
    struct not_a_fraction : std::logic_error { not_a_fraction() : std::logic_error("neonumeric::not_a_fraction") {} };
    struct bad_magnitude : std::logic_error { bad_magnitude() : std::logic_error("neonumeric::bad_magnitude") {} };
    struct not_packed : std::logic_error { not_packed() : std::logic_error("neonumeric::not_packed") {} };
}