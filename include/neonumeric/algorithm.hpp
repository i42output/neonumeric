// algorithm.hpp
/*
 *  Copyright (c) 2021 Leigh Johnston.
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

#include <neonumeric/neonumeric.hpp>

namespace neonumeric
{
    inline constexpr uint32_t min(uint32_t aLhs, uint32_t aRhs)
    {
        return aLhs <= aRhs ? aLhs : aRhs;
    }

    inline constexpr uint32_t max(uint32_t aLhs, uint32_t aRhs)
    {
        return aLhs >= aRhs ? aLhs : aRhs;
    }

    inline constexpr architecture_t::word_t bits(architecture_t::word_t aValue)
    {
        return aValue < architecture_t::Two ? aValue : architecture_t::One + bits(aValue >> architecture_t::One);
    }

    namespace detail
    {
        #define R2(n)    n,        n + 2*64,      n + 1*64,       n + 3*64
        #define R4(n) R2(n),    R2(n + 2*16),  R2(n + 1*16),   R2(n + 3*16)
        #define R6(n) R4(n),    R4(n + 2*4 ),  R4(n + 1*4 ),   R4(n + 3*4 )
        constexpr uint8_t reverse_bits_lookup_table[] =
        {
            R6(0), R6(2), R6(1), R6(3)
        };
    }

    inline constexpr uint8_t reverse_bits(uint8_t aOctet)
    {
        return detail::reverse_bits_lookup_table[aOctet];
    }

    inline constexpr uint16_t reverse_bits(uint16_t aValue)
    {
        return (static_cast<uint16_t>(reverse_bits(static_cast<uint8_t>(aValue))) << 8) | static_cast<uint16_t>(reverse_bits(static_cast<uint8_t>(aValue >> 8)));
    }

    inline constexpr uint32_t reverse_bits(uint32_t aValue)
    {
        return (static_cast<uint32_t>(reverse_bits(static_cast<uint16_t>(aValue))) << 16) | static_cast<uint32_t>(reverse_bits(static_cast<uint16_t>(aValue >> 16)));
    }

    inline constexpr uint64_t reverse_bits(uint64_t aValue)
    {
        return (static_cast<uint64_t>(reverse_bits(static_cast<uint32_t>(aValue))) << 32) | static_cast<uint64_t>(reverse_bits(static_cast<uint32_t>(aValue >> 32)));
    }


    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    class xinteger;

    template <typename XInteger>
    struct as_xinteger;
    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    struct as_xinteger<xinteger<Size, Type, Signalling, SmallBufferSize>>
    {
        typedef xinteger<Size, Type, Signalling, SmallBufferSize> type;
    };
    template <typename XInteger>
    using xinteger_t = typename as_xinteger<XInteger>::type;

    template <typename XInteger>
    struct xinteger_traits
    {
        typedef typename xinteger_t<XInteger>::signed_type signed_type;
        typedef typename xinteger_t<XInteger>::unsigned_type unsigned_type;
        typedef typename xinteger_t<XInteger>::storage_traits_type storage_traits_type;
        typedef typename xinteger_t<XInteger>::word_t word_t;
        typedef typename xinteger_t<XInteger>::words_t words_t;
        typedef typename xinteger_t<XInteger>::word_index_t word_index_t;

        static constexpr bool IsInteger = xinteger_t<XInteger>::IsInteger;
        static constexpr bool IsReal = xinteger_t<XInteger>::IsReal;
        static constexpr bool IsSigned = xinteger_t<XInteger>::IsSigned;
        static constexpr word_t Zero = xinteger_t<XInteger>::Zero;
        static constexpr word_t One = xinteger_t<XInteger>::One;
        static constexpr word_t Two = xinteger_t<XInteger>::Two;
        static constexpr word_index_t MaxSize = xinteger_t<XInteger>::MaxSize;
        static constexpr word_index_t MaxSizeHalfWords = xinteger_t<XInteger>::MaxSizeHalfWords;
        static constexpr bool FixedSize = xinteger_t<XInteger>::FixedSize;
        static constexpr word_t MaxSupportedPower = xinteger_t<XInteger>::MaxSupportedPower;
        static constexpr word_t MaxLeftShift = xinteger_t<XInteger>::MaxLeftShift;
        static constexpr word_t MultiplyAlgorithm1Threshold = xinteger_t<XInteger>::MultiplyAlgorithm1Threshold;
        static constexpr word_t MultiplyAlgorithm2Threshold = xinteger_t<XInteger>::MultiplyAlgorithm2Threshold;
        static constexpr word_t DivideAlgorithm1Threshold = xinteger_t<XInteger>::DivideAlgorithm1Threshold;
    };

    template <typename XInteger>
    struct xinteger_detail
    {
        typedef typename xinteger_t<XInteger>::word_t word_t;
        typedef typename xinteger_t<XInteger>::word_index_t word_index_t;

        static word_t magnitude(XInteger const& aLhs, bool aTruncate = true) { return aLhs.magnitude(aTruncate); }
        static word_t magnitude_in_bits(XInteger const& aLhs) { return aLhs.magnitude_in_bits(); }
        static word_index_t size(XInteger const& aLhs) { return aLhs.size(); }
        static word_index_t max_size(XInteger const& aLhs) { return aLhs.max_size(); }
        static void resize(XInteger& aLhs, word_index_t aSize) { aLhs.resize(aSize); }
        static const typename xinteger_t<XInteger>::words_t& words(XInteger const& aLhs) { return aLhs.words(); }
        static typename xinteger_t<XInteger>::words_t& words(XInteger& aLhs) { return aLhs.words(); }
        static const word_t& cword(XInteger const& aLhs, word_index_t aIndex) { return aLhs.cword(aIndex); }
        static const word_t& cword(XInteger& aLhs, word_index_t aIndex) { return aLhs.cword(aIndex); }
        static const word_t& word(XInteger const& aLhs, word_index_t aIndex) { return aLhs.word(aIndex); }
        static word_t& word(XInteger& aLhs, word_index_t aIndex) { return aLhs.word(aIndex); }
        static word_t add_words(word_t aLhs, word_t aRhs, word_t& aCarry) { return XInteger::add_words(aLhs, aRhs, aCarry); }
        static word_t multiply_words(word_t aLhs, word_t aRhs, word_t& aCarry) { return XInteger::multiply_words(aLhs, aRhs, aCarry); }
        static typename xinteger_t<XInteger>::unsigned_type to_unsigned(XInteger const& aLhs, bool aTakeAbsolute = false) { return aLhs.to_unsigned(aTakeAbsolute); }
        static typename xinteger_t<XInteger>::unsigned_type& to_unsigned(typename xinteger_t<XInteger>::unsigned_type& aLhs, XInteger const& aRhs, bool aTakeAbsolute = false) { return aRhs.to_unsigned(aLhs, aTakeAbsolute); }
        static typename xinteger_t<XInteger>::signed_type to_signed(XInteger const& aLhs, bool aIsAbsolute = false, bool aHasSignBit = true) { return aLhs.to_signed(aIsAbsolute, aHasSignBit); }
        static typename xinteger_t<XInteger>::signed_type& to_signed(typename xinteger_t<XInteger>::signed_type& aLhs, XInteger const& aRhs, bool aIsAbsolute = false, bool aHasSignBit = true) { return aRhs.to_signed(aLhs, aIsAbsolute, aHasSignBit); }
        static word_t bit(XInteger const& aLhs, word_t aBitPosition) { return aLhs.bit(aBitPosition); }
        static void set_bit(XInteger& aLhs, word_t aBitPosition, word_t aValue) { return aLhs.set_bit(aBitPosition, aValue); }
        static bool is_sign_bit_set(XInteger const& aLhs) { return aLhs.is_sign_bit_set(); }
        static void correct_sign(XInteger& aLhs, bool aPreviousLhsSignBit, bool aPreviousRhsSignBit) { aLhs.correct_sign(aPreviousLhsSignBit, aPreviousRhsSignBit);  }
        static void correct_sign(XInteger& aLhs, bool aPreviousLhsSignBit, bool aPreviousRhsSignBit, word_t& aCarry) { aLhs.correct_sign(aPreviousLhsSignBit, aPreviousRhsSignBit, aCarry); }
        static bool is_positive(XInteger const& aLhs) { return aLhs.is_positive(); }
        static bool is_negative(XInteger const& aLhs) { return aLhs.is_negative(); }
        static void set_positive(XInteger& aLhs, bool aPositive = true) { aLhs.set_positive(aPositive); }
        static void set_negative(XInteger& aLhs, bool aNegative = true) { aLhs.set_negative(aNegative); }
        static bool has_repetend(XInteger const& aLhs) { return aLhs.has_repetend(); }
        static const neonumeric::repetend& repetend(XInteger const& aLhs) { return aLhs.repetend(); }
        static neonumeric::repetend& repetend(XInteger& aLhs) { return aLhs.repetend(); }
        static XInteger unpack(XInteger const& aLhs, word_t aMagntitudeInBits) { return aLhs.unpack(aMagntitudeInBits); }
    };

    template <typename Arg>
    struct as_argument_cracker
    {
        typedef Arg type;
    };
    template <typename Arg>
    struct as_argument_cracker<Arg const&>
    {
        typedef Arg type;
    };
    template <typename Arg>
    struct as_argument_cracker<Arg&>
    {
        typedef Arg& type;
    };
    template <typename Arg>
    using as_argument_t = typename as_argument_cracker<Arg>::type;

    template <typename Arg>
    struct argument
    {
        as_argument_t<Arg> arg;
    };

    template <typename Lhs, typename Rhs>
    struct arguments
    {
        as_argument_t<Lhs> lhs;
        as_argument_t<Rhs> rhs;
    };

    template <typename XInteger, typename Arg>
    inline argument<Arg> as_argument(Arg&& aArg)
    {
        thread_local XInteger arg;
        if (!xinteger_detail<XInteger>::has_repetend(aArg))
            return argument<Arg>{ aArg };
        else
        {
            arg = xinteger_detail<XInteger>::unpack(aArg, xinteger_detail<XInteger>::magnitude_in_bits(aArg));
            return argument<Arg>{ arg };
        }
    }

    template <typename XInteger, typename Lhs, typename Rhs>
    inline arguments<Lhs, Rhs> as_arguments(Lhs&& aLhs, Rhs&& aRhs)
    {
        thread_local XInteger lhs;
        thread_local XInteger rhs;
        if (!xinteger_detail<XInteger>::has_repetend(aLhs) && !xinteger_detail<XInteger>::has_repetend(aRhs))
            return arguments<Lhs, Rhs>{ aLhs, aRhs };
        else if (xinteger_detail<XInteger>::has_repetend(aLhs) && !xinteger_detail<XInteger>::has_repetend(aRhs))
        {
            lhs = xinteger_detail<XInteger>::unpack(aLhs, xinteger_detail<XInteger>::magnitude_in_bits(aRhs));
            return arguments<Lhs, Rhs>{ lhs, aRhs };
        }
        else if (!xinteger_detail<XInteger>::has_repetend(aLhs) && xinteger_detail<XInteger>::has_repetend(aRhs))
        {
            rhs = xinteger_detail<XInteger>::unpack(aRhs, xinteger_detail<XInteger>::magnitude_in_bits(aLhs));
            return arguments<Lhs, Rhs>{ aLhs, rhs };
        }
        else
        {
            auto const magBits = std::max(xinteger_detail<XInteger>::repetend(aLhs).cycleEnd - xinteger_detail<XInteger>::repetend(aLhs).cycleStart + xinteger_t<XInteger>::One, xinteger_detail<XInteger>::repetend(aRhs).cycleEnd - xinteger_detail<XInteger>::repetend(aRhs).cycleStart + xinteger_t<XInteger>::One);
            lhs = xinteger_detail<XInteger>::unpack(aLhs, magBits);
            rhs = xinteger_detail<XInteger>::unpack(aRhs, magBits);
            return arguments<Lhs, Rhs>{ lhs, rhs };
        }
    }

    template <typename XInteger>
    inline XInteger add_algorithm_0(const XInteger& aLhs, const XInteger& aRhs, bool& aOverflow)
    {
        XInteger result = aLhs;
        return add_algorithm_0(result, aRhs, aOverflow);
    }
    
    template <typename XInteger>
    inline XInteger& add_algorithm_0(XInteger& aLhs, const XInteger& aRhs, bool& aOverflow)
    {
        auto args = as_arguments<XInteger>(aLhs, aRhs);
        auto& lhs = args.lhs;
        auto const& rhs = args.rhs;
        auto& result = lhs;

        result = add_algorithm_1(lhs, rhs, aOverflow);

        if constexpr (xinteger_t<XInteger>::IsSigned)
        {
            if (xinteger_detail<XInteger>::is_negative(result) & !xinteger_detail<XInteger>::is_sign_bit_set(result))
                xinteger_detail<XInteger>::set_positive(result);
        }

        return result;
    }
    
    template <typename XInteger>
    inline XInteger& add_algorithm_1(XInteger& aLhs, const XInteger& aRhs, bool& aOverflow)
    {
        XInteger& result = aLhs;

        auto const lhsSignBit = xinteger_detail<XInteger>::is_sign_bit_set(aLhs);
        auto const rhsSignBit = xinteger_detail<XInteger>::is_sign_bit_set(aRhs);
        auto const previousMagnitude = std::max(xinteger_detail<XInteger>::magnitude(aLhs), xinteger_detail<XInteger>::magnitude(aRhs));

        architecture_t::word_t carry = xinteger_t<XInteger>::Zero;
        auto const end = previousMagnitude;
        typename xinteger_t<XInteger>::word_index_t index = 0u;
        while (index < end)
        {
            auto lhs = xinteger_detail<XInteger>::cword(aLhs, index);
            auto rhs = xinteger_detail<XInteger>::cword(aRhs, index);
            xinteger_detail<XInteger>::word(result, index++) = xinteger_detail<XInteger>::add_words(lhs, rhs, carry);
        }

        if constexpr (xinteger_t<XInteger>::IsSigned)
        {
            if (lhsSignBit || rhsSignBit)
            {
                xinteger_detail<XInteger>::correct_sign(result, lhsSignBit, rhsSignBit, carry);
                if (carry)
                {
                    xinteger_detail<XInteger>::word(result, index);
                    carry = xinteger_t<XInteger>::Zero;
                }
            }
            else if (xinteger_detail<XInteger>::is_sign_bit_set(result) && carry == xinteger_t<XInteger>::Zero)
                xinteger_detail<XInteger>::word(result, index++) = xinteger_t<XInteger>::Zero;
        }

        while (carry != xinteger_t<XInteger>::Zero && index < xinteger_detail<XInteger>::max_size(result))
        {
            auto& partialResult = xinteger_detail<XInteger>::word(result, index++);
            architecture_t::word_t newcarry = xinteger_t<XInteger>::Zero;
            partialResult = xinteger_detail<XInteger>::add_words(partialResult, carry, newcarry);
            carry = newcarry;
        }
        aOverflow = xinteger_detail<XInteger>::magnitude(result) > previousMagnitude;

        return result;
    }
    
    template <typename XInteger>
    inline XInteger subtract_algorithm_0(const XInteger& aLhs, const XInteger& aRhs, bool& aOverflow)
    {
        XInteger result = aLhs;
        return subtract_algorithm_0(result, aRhs, aOverflow);
    }

    template <typename XInteger>
    inline XInteger& subtract_algorithm_0(XInteger& aLhs, const XInteger& aRhs, bool& aOverflow)
    {
        auto args = as_arguments<XInteger>(aLhs, aRhs);
        auto& lhs = args.lhs;
        auto const& rhs = args.rhs;
        auto& result = args.lhs;

        if constexpr (!xinteger_t<XInteger>::IsSigned)
        {
            if (xinteger_detail<XInteger>::magnitude(lhs) == xinteger_t<XInteger>::One && xinteger_detail<XInteger>::magnitude(rhs) == xinteger_t<XInteger>::One && lhs >= rhs)
                xinteger_detail<XInteger>::word(result, 0u) = integer_cast<architecture_t::word_t>(lhs) - integer_cast<architecture_t::word_t>(rhs);
            else
            {
                typedef typename xinteger_traits<XInteger>::signed_type signed_type;
                thread_local signed_type signedLhs;
                thread_local signed_type signedRhs;
                bool const sameMag = (xinteger_detail<XInteger>::magnitude(lhs) == xinteger_detail<XInteger>::magnitude(rhs));
                xinteger_detail<signed_type>::to_unsigned(result, subtract_algorithm_0(xinteger_detail<XInteger>::to_signed(signedLhs, lhs, true, !sameMag), xinteger_detail<XInteger>::to_signed(signedRhs, rhs, true, !sameMag), aOverflow), false);
            }
            return result;
        }

        if (xinteger_detail<XInteger>::magnitude(lhs, false) == xinteger_t<XInteger>::One && xinteger_detail<XInteger>::magnitude(rhs, false) == xinteger_t<XInteger>::One)
        {
            if constexpr (xinteger_t<XInteger>::IsSigned)
            {
                result = integer_cast<architecture_t::word_t>(lhs) - integer_cast<architecture_t::word_t>(rhs);
                xinteger_detail<XInteger>::set_negative(result, xinteger_detail<XInteger>::is_sign_bit_set(result));
                return result;
            }
        }

        return subtract_algorithm_1(lhs, rhs, aOverflow);
    }

    template <typename XInteger>
    inline XInteger& subtract_algorithm_1(XInteger& aLhs, const XInteger& aRhs, bool& aOverflow)
    {
        auto& lhs = aLhs;
        auto rhs = negate(aRhs);
        XInteger& result = aLhs;

        bool const possiblyLarger = (xinteger_detail<XInteger>::is_positive(lhs) == xinteger_detail<XInteger>::is_positive(rhs));
        auto maxResultSize = std::max(xinteger_detail<XInteger>::size(lhs), xinteger_detail<XInteger>::size(rhs));
        add_algorithm_0(lhs, rhs, aOverflow);
        if (!possiblyLarger && xinteger_detail<XInteger>::size(result) != maxResultSize)
        {
            xinteger_detail<XInteger>::resize(result, maxResultSize);
            aOverflow = false;
        }
        xinteger_detail<XInteger>::resize(result, static_cast<typename xinteger_t<XInteger>::word_index_t>(xinteger_detail<XInteger>::magnitude(result)));

        return result;
    }

    template <typename XInteger>
    inline XInteger multiply_algorithm_0(const XInteger& aLhs, const XInteger& aRhs)
    {
        auto args = as_arguments<XInteger>(aLhs, aRhs);
        auto const& lhs = args.lhs;
        auto const& rhs = args.rhs;

        if constexpr (xinteger_t<XInteger>::IsSigned)
        {
            auto result = xinteger_detail<typename xinteger_traits<XInteger>::unsigned_type>::to_signed(multiply_algorithm_0(xinteger_detail<XInteger>::to_unsigned(lhs, true), xinteger_detail<XInteger>::to_unsigned(rhs, true)), true);
            if (xinteger_detail<XInteger>::is_positive(lhs) != xinteger_detail<XInteger>::is_positive(rhs))
                negate(result);
            return result;
        }

        if (xinteger_detail<XInteger>::magnitude(lhs) == xinteger_t<XInteger>::One && xinteger_detail<XInteger>::magnitude(rhs) != xinteger_t<XInteger>::One)
            return multiply_algorithm_0(rhs, lhs);
        else if (xinteger_detail<XInteger>::magnitude(rhs) == xinteger_t<XInteger>::One)
        {
            switch (xinteger_detail<XInteger>::cword(rhs, 0u))
            {
            case 0x01ull:
                return lhs;
            case 0x02ull:
                return multiply_algorithm_0<XInteger, 0x02ull>(lhs);
            case 0x04ull:
                return multiply_algorithm_0<XInteger, 0x04ull>(lhs);
            case 0x08ull:
                return multiply_algorithm_0<XInteger, 0x08ull>(lhs);
            case 0x10ull:
                return multiply_algorithm_0<XInteger, 0x10ull>(lhs);
            case 0x20ull:
                return multiply_algorithm_0<XInteger, 0x20ull>(lhs);
            case 0x40ull:
                return multiply_algorithm_0<XInteger, 0x40ull>(lhs);
            case 0x80ull:
                return multiply_algorithm_0<XInteger, 0x80ull>(lhs);
            case 0x100ull:
                return multiply_algorithm_0<XInteger, 0x100ull>(lhs);
            case 0x200ull:
                return multiply_algorithm_0<XInteger, 0x200ull>(lhs);
            case 0x400ull:
                return multiply_algorithm_0<XInteger, 0x400ull>(lhs);
            case 0x800ull:
                return multiply_algorithm_0<XInteger, 0x800ull>(lhs);
            case 0x1000ull:
                return multiply_algorithm_0<XInteger, 0x1000ull>(lhs);
            case 0x2000ull:
                return multiply_algorithm_0<XInteger, 0x2000ull>(lhs);
            case 0x4000ull:
                return multiply_algorithm_0<XInteger, 0x4000ull>(lhs);
            case 0x8000ull:
                return multiply_algorithm_0<XInteger, 0x8000ull>(lhs);
            case 0x10000ull:
                return multiply_algorithm_0<XInteger, 0x10000ull>(lhs);
            case 0x20000ull:
                return multiply_algorithm_0<XInteger, 0x20000ull>(lhs);
            case 0x40000ull:
                return multiply_algorithm_0<XInteger, 0x40000ull>(lhs);
            case 0x80000ull:
                return multiply_algorithm_0<XInteger, 0x80000ull>(lhs);
            case 0x100000ull:
                return multiply_algorithm_0<XInteger, 0x100000ull>(lhs);
            case 0x200000ull:
                return multiply_algorithm_0<XInteger, 0x200000ull>(lhs);
            case 0x400000ull:
                return multiply_algorithm_0<XInteger, 0x400000ull>(lhs);
            case 0x800000ull:
                return multiply_algorithm_0<XInteger, 0x800000ull>(lhs);
            case 0x1000000ull:
                return multiply_algorithm_0<XInteger, 0x1000000ull>(lhs);
            case 0x2000000ull:
                return multiply_algorithm_0<XInteger, 0x2000000ull>(lhs);
            case 0x4000000ull:
                return multiply_algorithm_0<XInteger, 0x4000000ull>(lhs);
            case 0x8000000ull:
                return multiply_algorithm_0<XInteger, 0x8000000ull>(lhs);
            case 0x10000000ull:
                return multiply_algorithm_0<XInteger, 0x10000000ull>(lhs);
            case 0x20000000ull:
                return multiply_algorithm_0<XInteger, 0x20000000ull>(lhs);
            case 0x40000000ull:
                return multiply_algorithm_0<XInteger, 0x40000000ull>(lhs);
            case 0x80000000ull:
                return multiply_algorithm_0<XInteger, 0x80000000ull>(lhs);
            case 0x100000000ull:
                return multiply_algorithm_0<XInteger, 0x100000000ull>(lhs);
            case 0x200000000ull:
                return multiply_algorithm_0<XInteger, 0x200000000ull>(lhs);
            case 0x400000000ull:
                return multiply_algorithm_0<XInteger, 0x400000000ull>(lhs);
            case 0x800000000ull:
                return multiply_algorithm_0<XInteger, 0x800000000ull>(lhs);
            case 0x1000000000ull:
                return multiply_algorithm_0<XInteger, 0x1000000000ull>(lhs);
            case 0x2000000000ull:
                return multiply_algorithm_0<XInteger, 0x2000000000ull>(lhs);
            case 0x4000000000ull:
                return multiply_algorithm_0<XInteger, 0x4000000000ull>(lhs);
            case 0x8000000000ull:
                return multiply_algorithm_0<XInteger, 0x8000000000ull>(lhs);
            case 0x10000000000ull:
                return multiply_algorithm_0<XInteger, 0x10000000000ull>(lhs);
            case 0x20000000000ull:
                return multiply_algorithm_0<XInteger, 0x20000000000ull>(lhs);
            case 0x40000000000ull:
                return multiply_algorithm_0<XInteger, 0x40000000000ull>(lhs);
            case 0x80000000000ull:
                return multiply_algorithm_0<XInteger, 0x80000000000ull>(lhs);
            case 0x100000000000ull:
                return multiply_algorithm_0<XInteger, 0x100000000000ull>(lhs);
            case 0x200000000000ull:
                return multiply_algorithm_0<XInteger, 0x200000000000ull>(lhs);
            case 0x400000000000ull:
                return multiply_algorithm_0<XInteger, 0x400000000000ull>(lhs);
            case 0x800000000000ull:
                return multiply_algorithm_0<XInteger, 0x800000000000ull>(lhs);
            case 0x1000000000000ull:
                return multiply_algorithm_0<XInteger, 0x1000000000000ull>(lhs);
            case 0x2000000000000ull:
                return multiply_algorithm_0<XInteger, 0x2000000000000ull>(lhs);
            case 0x4000000000000ull:
                return multiply_algorithm_0<XInteger, 0x4000000000000ull>(lhs);
            case 0x8000000000000ull:
                return multiply_algorithm_0<XInteger, 0x8000000000000ull>(lhs);
            case 0x10000000000000ull:
                return multiply_algorithm_0<XInteger, 0x10000000000000ull>(lhs);
            case 0x20000000000000ull:
                return multiply_algorithm_0<XInteger, 0x20000000000000ull>(lhs);
            case 0x40000000000000ull:
                return multiply_algorithm_0<XInteger, 0x40000000000000ull>(lhs);
            case 0x80000000000000ull:
                return multiply_algorithm_0<XInteger, 0x80000000000000ull>(lhs);
            case 0x100000000000000ull:
                return multiply_algorithm_0<XInteger, 0x100000000000000ull>(lhs);
            case 0x200000000000000ull:
                return multiply_algorithm_0<XInteger, 0x200000000000000ull>(lhs);
            case 0x400000000000000ull:
                return multiply_algorithm_0<XInteger, 0x400000000000000ull>(lhs);
            case 0x800000000000000ull:
                return multiply_algorithm_0<XInteger, 0x800000000000000ull>(lhs);
            case 0x1000000000000000ull:
                return multiply_algorithm_0<XInteger, 0x1000000000000000ull>(lhs);
            case 0x2000000000000000ull:
                return multiply_algorithm_0<XInteger, 0x2000000000000000ull>(lhs);
            case 0x4000000000000000ull:
                return multiply_algorithm_0<XInteger, 0x4000000000000000ull>(lhs);
            case 0x8000000000000000ull:
                return multiply_algorithm_0<XInteger, 0x8000000000000000ull>(lhs);
            default:
                break;
            }
        }

        return multiply_algorithm_1(lhs, rhs);
    }

    template <typename XInteger, architecture_t::word_t Modulo>
    inline XInteger multiply_algorithm_0(const XInteger& aLhs)
    {
        return aLhs << bits(Modulo - 1u);
    }

    template <typename XInteger>
    inline XInteger multiply_algorithm_1(const XInteger& aLhs, const XInteger& aRhs)
    {
        if (xinteger_detail<XInteger>::magnitude(aLhs) + xinteger_detail<XInteger>::magnitude(aRhs) > xinteger_traits<XInteger>::MultiplyAlgorithm1Threshold)
            return multiply_algorithm_2(aLhs, aRhs);

        return multiply_algorithm_naive_long_multiplication(aLhs, aRhs);
    }

    template <typename XInteger>
    inline XInteger multiply_algorithm_naive_long_multiplication(const XInteger& aLhs, const XInteger& aRhs)
    {
        XInteger product;

        auto const& a = aLhs;
        auto const& b = aRhs;
        auto const p = xinteger_detail<XInteger>::size(a);
        auto const q = xinteger_detail<XInteger>::size(b);

        for (typename xinteger_t<XInteger>::word_index_t b_i = 1u; b_i <= q; ++b_i)
        {
            architecture_t::word_t carry = architecture_t::Zero;
            for (typename xinteger_t<XInteger>::word_index_t a_i = 1u; a_i <= p; ++a_i)
            {
                typename xinteger_t<XInteger>::word_index_t const p_i = a_i + b_i - architecture_t::One - architecture_t::One;
                auto const partialProduct = xinteger_detail<XInteger>::multiply_words(xinteger_detail<XInteger>::cword(a, a_i - architecture_t::One), xinteger_detail<XInteger>::cword(b, b_i - architecture_t::One), carry);
                auto& partialResult = xinteger_detail<XInteger>::word(product, p_i);
                architecture_t::word_t carry2 = xinteger_t<XInteger>::Zero;
                partialResult = xinteger_detail<XInteger>::add_words(partialResult, partialProduct, carry2);
                for (typename xinteger_t<XInteger>::word_index_t c_i = p_i + architecture_t::One; (carry != xinteger_t<XInteger>::Zero || carry2 != xinteger_t<XInteger>::Zero) && c_i < xinteger_detail<XInteger>::max_size(product); ++c_i)
                {
                    auto& partialCarryResult = xinteger_detail<XInteger>::word(product, c_i);
                    architecture_t::word_t carry3 = xinteger_t<XInteger>::Zero;
                    partialCarryResult = xinteger_detail<XInteger>::add_words(partialCarryResult, carry, carry3);
                    carry = carry3;
                    carry3 = xinteger_t<XInteger>::Zero;
                    partialCarryResult = xinteger_detail<XInteger>::add_words(partialCarryResult, carry2, carry3);
                    carry2 = xinteger_t<XInteger>::Zero;
                    carry += carry3;
                }
            }
        }

        return product;
    }

    template <typename XInteger>
    inline XInteger multiply_algorithm_2(const XInteger& aLhs, const XInteger& aRhs)
    {
        if (xinteger_detail<XInteger>::magnitude(aLhs) + xinteger_detail<XInteger>::magnitude(aRhs) > xinteger_traits<XInteger>::MultiplyAlgorithm2Threshold)
            return multiply_algorithm_3(aLhs, aRhs);

        return multiply_algorithm_karatsuba(aLhs, aRhs);
    }

    template <typename XInteger>
    inline XInteger multiply_algorithm_karatsuba(const XInteger& aLhs, const XInteger& aRhs)
    {
        auto const lhsMag = xinteger_detail<XInteger>::magnitude(aLhs);
        auto const rhsMag = xinteger_detail<XInteger>::magnitude(aRhs);

        if (lhsMag + rhsMag <= xinteger_traits<XInteger>::MultiplyAlgorithm1Threshold)
            return multiply_algorithm_naive_long_multiplication(aLhs, aRhs);

        auto const m = std::min(lhsMag, rhsMag);
        auto const m2 = m / 2;

        XInteger low1{ xinteger_detail<XInteger>::words(aLhs).begin(), xinteger_detail<XInteger>::words(aLhs).begin() + m2 };
        XInteger high1{ xinteger_detail<XInteger>::words(aLhs).begin() + m2, xinteger_detail<XInteger>::words(aLhs).end() };
        XInteger low2{ xinteger_detail<XInteger>::words(aRhs).begin(), xinteger_detail<XInteger>::words(aRhs).begin() + m2 };
        XInteger high2{ xinteger_detail<XInteger>::words(aRhs).begin() + m2, xinteger_detail<XInteger>::words(aRhs).end() };

        auto const z0 = multiply_algorithm_karatsuba(low1, low2);
        auto const z1 = multiply_algorithm_karatsuba(low1 + high1, low2 + high2);
        auto const z2 = multiply_algorithm_karatsuba(high1, high2);
        auto const term120 = z1 - z2 - z0;

        auto result = (z2 << (m2 * 2u * architecture_t::WordBits)) + (term120 << (m2 * architecture_t::WordBits)) + z0;

        return std::move(result);
    }

    template <typename XInteger>
    inline XInteger multiply_algorithm_3(const XInteger& aLhs, const XInteger& aRhs)
    {
        return multiply_algorithm_fft(aLhs, aRhs);
    }

    template <typename XInteger>
    inline XInteger multiply_algorithm_fft(const XInteger& aLhs, const XInteger& aRhs)
    {
        // todo

        XInteger result;
        result = aLhs;
        return multiply_algorithm_karatsuba(result, aRhs);
    }

    template <typename XInteger>
    inline XInteger divide_algorithm_0(const XInteger& aLhs, const XInteger& aRhs)
    {
        if constexpr (xinteger_t<XInteger>::IsSigned)
        {
            auto result = xinteger_detail<typename xinteger_traits<XInteger>::unsigned_type>::to_signed(divide_algorithm_0(xinteger_detail<XInteger>::to_unsigned(aLhs, true), xinteger_detail<XInteger>::to_unsigned(aRhs, true)), true);
            if (xinteger_detail<XInteger>::is_positive(aLhs) != xinteger_detail<XInteger>::is_positive(aRhs))
                negate(result);
            return std::move(result);
        }

        XInteger remainder;
        return divide_algorithm_0(aLhs, aRhs, remainder);
    }

    template <typename XInteger>
    inline XInteger divide_algorithm_0(const XInteger& aLhs, const XInteger& aRhs, XInteger& aRemainder)
    {
        auto args = as_arguments<XInteger>(aLhs, aRhs);
        auto const& lhs = args.lhs;
        auto const& rhs = args.rhs;

        if constexpr (xinteger_t<XInteger>::IsSigned)
        {
            typename xinteger_traits<XInteger>::unsigned_type unsignedRemainder;
            auto result = xinteger_detail<typename xinteger_traits<XInteger>::unsigned_type>::to_signed(divide_algorithm_0(xinteger_detail<XInteger>::to_unsigned(lhs, true), xinteger_detail<XInteger>::to_unsigned(rhs, true), unsignedRemainder), true);
            aRemainder = xinteger_detail<typename xinteger_traits<XInteger>::unsigned_type>::to_signed(unsignedRemainder, true);
            if (xinteger_detail<XInteger>::is_negative(lhs))
                negate(aRemainder);
            if (xinteger_detail<XInteger>::is_positive(lhs) != xinteger_detail<XInteger>::is_positive(rhs))
                negate(result);
            return std::move(result);
        }

        static XInteger const zero = xinteger_t<XInteger>::Zero;

        if (xinteger_detail<XInteger>::magnitude(rhs) == xinteger_t<XInteger>::One)
        {
            switch (xinteger_detail<XInteger>::cword(rhs, 0))
            {
            case 0x01:
                aRemainder = zero;
                return lhs;
            case 0x02:
                return divide_algorithm_0<XInteger, 0x02>(lhs, aRemainder);
            case 0x04:
                return divide_algorithm_0<XInteger, 0x04>(lhs, aRemainder);
            case 0x08:
                return divide_algorithm_0<XInteger, 0x08>(lhs, aRemainder);
            case 0x10:
                return divide_algorithm_0<XInteger, 0x10>(lhs, aRemainder);
            case 0x20:
                return divide_algorithm_0<XInteger, 0x20>(lhs, aRemainder);
            case 0x40:
                return divide_algorithm_0<XInteger, 0x40>(lhs, aRemainder);
            case 0x80:
                return divide_algorithm_0<XInteger, 0x80>(lhs, aRemainder);
            case 0x100:
                return divide_algorithm_0<XInteger, 0x100>(lhs, aRemainder);
            case 0x200:
                return divide_algorithm_0<XInteger, 0x200>(lhs, aRemainder);
            case 0x400:
                return divide_algorithm_0<XInteger, 0x400>(lhs, aRemainder);
            case 0x800:
                return divide_algorithm_0<XInteger, 0x800>(lhs, aRemainder);
            case 0x1000:
                return divide_algorithm_0<XInteger, 0x1000>(lhs, aRemainder);
            case 0x2000:
                return divide_algorithm_0<XInteger, 0x2000>(lhs, aRemainder);
            case 0x4000:
                return divide_algorithm_0<XInteger, 0x4000>(lhs, aRemainder);
            case 0x8000:
                return divide_algorithm_0<XInteger, 0x8000>(lhs, aRemainder);
            default:
                break;
            }
        }

        if (xinteger_detail<XInteger>::magnitude(lhs) + xinteger_detail<XInteger>::magnitude(rhs) > xinteger_traits<XInteger>::DivideAlgorithm1Threshold)
            return divide_algorithm_2(lhs, rhs, aRemainder);

        return divide_algorithm_1(lhs, rhs, aRemainder);
    }

    template <typename XInteger, architecture_t::word_t Modulo>
    inline XInteger divide_algorithm_0(const XInteger& aLhs, XInteger& aRemainder)
    {
        aRemainder = xinteger_detail<XInteger>::cword(aLhs, 0u) % Modulo;
        return aLhs >> bits(Modulo - 1u);
    }

    template <typename XInteger>
    inline XInteger divide_algorithm_1(const XInteger& aLhs, const XInteger& aRhs, XInteger& aRemainder)
    {
        static XInteger const zero = xinteger_t<XInteger>::Zero;
        static XInteger const one = xinteger_t<XInteger>::One;

        thread_local XInteger numerator;
        thread_local XInteger denominator;
        thread_local XInteger quotient;
        thread_local XInteger remainder;
        numerator = aLhs;
        denominator = aRhs;
        quotient = zero;
        remainder = zero;

        if (denominator == zero)
            return quotient.template raise_signal<divide_by_zero>();
        if (denominator > numerator)
            remainder = numerator;
        else
        {
            architecture_t::word_t start = xinteger_detail<XInteger>::magnitude_in_bits(numerator);
            if (start > xinteger_t<XInteger>::Zero)
                for (architecture_t::word_t i = start; i --> xinteger_t<XInteger>::Zero;)
                {
                    remainder <<= one;
                    xinteger_detail<XInteger>::set_bit(remainder, 0u, xinteger_detail<XInteger>::bit(numerator, i));
                    if (remainder >= denominator)
                    {
                        remainder -= denominator;
                        xinteger_detail<XInteger>::set_bit(quotient, i, xinteger_t<XInteger>::One);
                    }
                }
        }
        aRemainder = remainder;
        return quotient;
    }

    template <typename XInteger>
    inline XInteger divide_algorithm_2(const XInteger& aLhs, const XInteger& aRhs, XInteger& aRemainder)
    {
        thread_local bool alreadyInHere = false;
        if (alreadyInHere)
            return divide_algorithm_1(aLhs, aRhs, aRemainder);
        scoped_flag sf{ alreadyInHere };
        auto q = typename xinteger_t<XInteger>::real_type{ aLhs } / typename xinteger_t<XInteger>::real_type{ aRhs };
        aRemainder = aLhs - (aRhs * q);
        return q;
    }

    template <typename XInteger>
    inline XInteger modulo_algorithm_1(const XInteger& aLhs, const XInteger& aRhs)
    {
        XInteger result;
        divide_algorithm_0(aLhs, aRhs, result);
        return result;
    }

    template <typename XInteger>
    inline XInteger negate(const XInteger& aLhs)
    {
        auto arg = as_argument<XInteger>(aLhs);
        auto const& lhs = arg.arg;

        XInteger result = lhs;
        negate(result);
        return std::move(result);
    }

    template <typename XInteger>
    XInteger& negate(XInteger& aLhs)
    {
        static XInteger const zero = xinteger_traits<XInteger>::Zero;
        static XInteger const one = xinteger_traits<XInteger>::One;

        bool const lhsWasNegative = xinteger_detail<XInteger>::is_negative(aLhs);
        auto& result = aLhs;

        auto const end = xinteger_detail<XInteger>::size(result);
        bool isZero = true;
        for (typename xinteger_t<XInteger>::word_index_t index = 0; index < end; ++index)
        {
            auto& next = xinteger_detail<XInteger>::word(result, index);
            if (next != xinteger_traits<XInteger>::Zero)
                isZero = false;
            next = ~next;
        }
        if (isZero)
        {
            result = zero;
            xinteger_detail<XInteger>::set_positive(result);
        }
        else
        {
            bool overflow;
            add_algorithm_0(result, one, overflow);
            xinteger_detail<XInteger>::set_positive(result, lhsWasNegative);
        }

        return result;
    }

    template <typename XInteger>
    inline XInteger pow_algorithm_0(const XInteger& aBase, const XInteger& aExponent)
    {
        if (xinteger_detail<XInteger>::magnitude(aBase) == xinteger_t<XInteger>::One && false)
        {
            switch (xinteger_detail<XInteger>::cword(aBase, 0u))
            {
            case 0x01:
                return aBase;
            case 0x02:
                return pow_algorithm_0<XInteger, 0x02>(aExponent);
            case 0x04:
                return pow_algorithm_0<XInteger, 0x04>(aExponent);
            case 0x08:
                return pow_algorithm_0<XInteger, 0x08>(aExponent);
            case 0x10:
                return pow_algorithm_0<XInteger, 0x10>(aExponent);
            default:
                break;
            }
        }

        return pow_algorithm_1(aBase, aExponent);
    }

    template <typename XInteger, architecture_t::word_t Modulo>
    inline XInteger pow_algorithm_0(const XInteger& aExponent)
    {
        if (aExponent == xinteger_t<XInteger>::Zero)
            return xinteger_t<XInteger>::One;
        else if (aExponent == xinteger_t<XInteger>::One)
            return Modulo;
        return XInteger{ xinteger_t<XInteger>::One << Modulo } << ( Modulo << (aExponent - xinteger_t<XInteger>::One) );
    }

    template <typename XInteger>
    inline XInteger pow_algorithm_1(const XInteger& aBase, const XInteger& aExponent)
    {
        static XInteger const zero = xinteger_t<XInteger>::Zero;
        static XInteger const one = xinteger_t<XInteger>::One;
        static XInteger const maxSupportedPower = xinteger_traits<XInteger>::MaxSupportedPower;

        auto x = aBase;
        auto n = aExponent;
        if (xinteger_detail<XInteger>::is_negative(n))
        {
            x = 1 / x;
            n = -n;
        }
        if (n == zero)
            return one;
        if (n > maxSupportedPower)
            return aBase.template raise_signal<not_yet_implemented>();
        XInteger y = one;
        while (n > one)
        {
            if ((n & architecture_t::LowBit) == xinteger_t<XInteger>::Zero)
            {
                x = x * x;
                n >>= one;
            }
            else
            {
                y = x * y;
                x = x * x;
                n = (n - one) >> one;
            }
        }
        return x * y;
    }

    template <typename XInteger>
    inline XInteger bitwise_or(const XInteger& aLhs, const XInteger& aRhs)
    {
        thread_local XInteger result;
        result = aLhs;
        bitwise_or(result, aRhs);
        return result;
    }

    template <typename XInteger>
    inline XInteger bitwise_and(const XInteger& aLhs, const XInteger& aRhs)
    {
        thread_local XInteger result;
        result = aLhs;
        bitwise_and(result, aRhs);
        return result;
    }

    template <typename XInteger>
    inline XInteger bitwise_xor(const XInteger& aLhs, const XInteger& aRhs)
    {
        thread_local XInteger result;
        result = aLhs;
        bitwise_xor(result, aRhs);
        return result;
    }

    template <typename XInteger>
    inline XInteger bitwise_not(const XInteger& aLhs)
    {
        thread_local XInteger result;
        result = aLhs;
        bitwise_not(result);
        return result;
    }

    template <typename XInteger>
    inline XInteger right_shift(const XInteger& aLhs, typename xinteger_t<XInteger>::signed_word_t aRhs)
    {
        thread_local XInteger result;
        result = aLhs;
        right_shift(result, aRhs);
        return result;
    }

    template <typename XInteger>
    inline XInteger left_shift(const XInteger& aLhs, typename xinteger_t<XInteger>::signed_word_t aRhs)
    {
        thread_local XInteger result;
        result = aLhs;
        left_shift(result, aRhs);
        return result;
    }

    template <typename XInteger>
    inline XInteger& bitwise_or(XInteger& aLhs, const XInteger& aRhs)
    {
        auto args = as_arguments<XInteger>(aLhs, aRhs);
        auto& lhs = args.lhs;
        auto const& rhs = args.rhs;
        auto& result = lhs;
        auto const end = std::max(xinteger_detail<XInteger>::size(lhs), xinteger_detail<XInteger>::size(rhs));
        for (typename xinteger_t<XInteger>::word_index_t index = 0; index < end; ++index)
            xinteger_detail<XInteger>::word(lhs, index) |= xinteger_detail<XInteger>::cword(rhs, index);
        return result;
    }

    template <typename XInteger>
    inline XInteger& bitwise_and(XInteger& aLhs, const XInteger& aRhs)
    {
        auto args = as_arguments<XInteger>(aLhs, aRhs);
        auto& lhs = args.lhs;
        auto const& rhs = args.rhs;
        auto& result = lhs;
        auto const end = std::max(xinteger_detail<XInteger>::size(lhs), xinteger_detail<XInteger>::size(rhs));
        for (typename xinteger_t<XInteger>::word_index_t index = 0; index < end; ++index)
            xinteger_detail<XInteger>::word(lhs, index) &= xinteger_detail<XInteger>::cword(rhs, index);
        return result;
    }

    template <typename XInteger>
    inline XInteger& bitwise_xor(XInteger& aLhs, const XInteger& aRhs)
    {
        auto args = as_arguments<XInteger>(aLhs, aRhs);
        auto& lhs = args.lhs;
        auto const& rhs = args.rhs;
        auto& result = lhs;
        auto const end = std::max(xinteger_detail<XInteger>::size(lhs), xinteger_detail<XInteger>::size(rhs));
        for (typename xinteger_t<XInteger>::word_index_t index = 0; index < end; ++index)
            xinteger_detail<XInteger>::word(lhs, index) ^= xinteger_detail<XInteger>::cword(rhs, index);
        return result;
    }

    template <typename XInteger>
    inline XInteger& bitwise_not(XInteger& aLhs)
    {
        auto arg = as_argument<XInteger>(aLhs);
        auto& result = arg.arg;

        auto const end = xinteger_detail<XInteger>::size(result);
        for (typename xinteger_t<XInteger>::word_index_t index = 0; index < end; ++index)
            xinteger_detail<XInteger>::word(result, index) = xinteger_detail<XInteger>::cword(~result, index);

        return result;
    }

    template <typename XInteger>
    inline XInteger& right_shift(XInteger& aLhs, typename xinteger_t<XInteger>::signed_word_t aRhs)
    {
        static XInteger const zero = xinteger_t<XInteger>::Zero;

        auto arg = as_argument<XInteger>(aLhs);
        auto& lhs = arg.arg;
        auto& result = lhs;

        if (aRhs < typename xinteger_t<XInteger>::signed_word_t{})
            return left_shift(aLhs, -aRhs);
            
        if (lhs == zero)
            return lhs;
            
        architecture_t::word_t shiftAmount = static_cast<architecture_t::word_t>(aRhs);
            
        if (shiftAmount == xinteger_t<XInteger>::Zero)
            return result;
            
        architecture_t::word_t constexpr bitIndexRadix = architecture_t::WordBits - xinteger_t<XInteger>::One;
            
        auto const partialShift = shiftAmount & bitIndexRadix;
        if (partialShift != xinteger_t<XInteger>::Zero)
        {
            auto const end = xinteger_detail<XInteger>::words(lhs).end();
            auto const last = (xinteger_detail<XInteger>::words(lhs).begin() != end ?
                std::prev(end) : xinteger_detail<XInteger>::words(lhs).begin());
            for (auto i = xinteger_detail<XInteger>::words(result).begin(); i != end; ++i)
            {
                auto& w = *i;
                w >>= partialShift;
                if (i != last)
                    w |= (*std::next(i) << (architecture_t::WordBits - partialShift));
                else if (xinteger_detail<XInteger>::is_negative(result))
                    w |= (~xinteger_t<XInteger>::Zero << (architecture_t::WordBits - partialShift));
            }
        }
            
        architecture_t::word_t constexpr bitIndexShift = bits(bitIndexRadix);
        shiftAmount >>= bitIndexShift;
            
        shiftAmount = std::min(shiftAmount, xinteger_detail<XInteger>::words(result).size());
            
        if (shiftAmount == xinteger_t<XInteger>::Zero)
            return result;
            
        if constexpr (!xinteger_traits<XInteger>::FixedSize)
            xinteger_detail<XInteger>::words(result).erase(xinteger_detail<XInteger>::words(result).begin(), xinteger_detail<XInteger>::words(result).begin() + shiftAmount);
        else
        {
            std::copy(xinteger_detail<XInteger>::words(result).begin() + shiftAmount, xinteger_detail<XInteger>::words(result).end(), xinteger_detail<XInteger>::words(result).begin());
            std::fill(xinteger_detail<XInteger>::words(result).end() - shiftAmount, xinteger_detail<XInteger>::words(result).end(), xinteger_detail<XInteger>::is_positive(result) ? xinteger_t<XInteger>::Zero : ~xinteger_t<XInteger>::Zero);
        }
            
        return result;
    }

    template <typename XInteger>
    inline XInteger& left_shift(XInteger& aLhs, typename xinteger_t<XInteger>::signed_word_t aRhs)
    {
        static XInteger const zero = xinteger_t<XInteger>::Zero;
        static XInteger const maxLeftShift = xinteger_traits<XInteger>::MaxLeftShift;

        auto arg = as_argument<XInteger>(aLhs);
        auto& lhs = arg.arg;
        auto& result = lhs;

        if (aRhs < typename xinteger_t<XInteger>::signed_word_t{})
            return right_shift(aLhs, -aRhs);

        if (lhs == zero)
            return lhs;

        if (aRhs > maxLeftShift)
            return result.template raise_signal<not_yet_implemented>();

        architecture_t::word_t shiftAmount = static_cast<architecture_t::word_t>(aRhs);
            
        if (shiftAmount == xinteger_t<XInteger>::Zero)
            return result;

        architecture_t::word_t constexpr bitIndexRadix = architecture_t::WordBits - xinteger_t<XInteger>::One;

        auto const partialShift = shiftAmount & bitIndexRadix;
        if (partialShift != xinteger_t<XInteger>::Zero)
        {
            if constexpr (!xinteger_traits<XInteger>::FixedSize)
            {
                if (xinteger_detail<XInteger>::words(result).back() != xinteger_t<XInteger>::Zero)
                    xinteger_detail<XInteger>::words(result).push_back(xinteger_t<XInteger>::Zero);
            }
            auto const end = xinteger_detail<XInteger>::words(result).rend();
            auto const last = (xinteger_detail<XInteger>::words(result).rbegin() != end ?
                std::prev(end) : xinteger_detail<XInteger>::words(result).rbegin());
            for (auto i = xinteger_detail<XInteger>::words(result).rbegin(); i != end; ++i)
            {
                auto& w = *i;
                w <<= partialShift;
                if (i != last)
                    w |= (*std::next(i) >> (architecture_t::WordBits - partialShift));
            }
        }
            
        architecture_t::word_t constexpr bitIndexShift = bits(bitIndexRadix);
        shiftAmount >>= bitIndexShift;

        if constexpr (!xinteger_traits<XInteger>::FixedSize)
        {
            if (xinteger_detail<XInteger>::words(result).back() == xinteger_t<XInteger>::Zero)
                xinteger_detail<XInteger>::words(result).pop_back();
        }

        if (shiftAmount == xinteger_t<XInteger>::Zero)
            return result;

        if constexpr (!xinteger_traits<XInteger>::FixedSize)
        {
            try
            {
                xinteger_detail<XInteger>::words(result).insert(xinteger_detail<XInteger>::words(result).begin(), shiftAmount, xinteger_t<XInteger>::Zero);
            }
            catch (std::bad_alloc)
            {
                return result.template raise_signal<number_allocation_failure>();
            }
        }
        else
        {
            std::copy(xinteger_detail<XInteger>::words(result).begin(), xinteger_detail<XInteger>::words(result).end() - shiftAmount, xinteger_detail<XInteger>::words(result).begin() + shiftAmount);
            std::fill(xinteger_detail<XInteger>::words(result).begin(), xinteger_detail<XInteger>::words(result).begin() + shiftAmount, xinteger_t<XInteger>::Zero);
        }

        return result;
    }
}
