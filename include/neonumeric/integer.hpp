// integer.hpp
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

#include <vector>
#include <array>
#include <optional>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <neonumeric/neonumeric.hpp>
#include <neonumeric/vecarray.hpp>
#include <neonumeric/exceptions.hpp>
#include <neonumeric/algorithm.hpp>

namespace neonumeric
{
    template <uint32_t, uint32_t, uint32_t, bool, std::size_t>
    class xreal;
        
    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    class xinteger;

    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    inline xinteger<Size, Type, Signalling, SmallBufferSize> pow(
        const xinteger<Size, Type, Signalling, SmallBufferSize>& aBase, const xinteger<Size, Type, Signalling, SmallBufferSize>& aExponent);
    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    inline xinteger<Size, Type, Signalling, SmallBufferSize> pow(
        const xinteger<Size, Type, Signalling, SmallBufferSize>& aBase, architecture_t::word_t aExponent);

    template <typename T, uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    inline T integer_cast(const xinteger<Size, Type, Signalling, SmallBufferSize>& aValue);

    template <typename T, std::size_t SmallBufferSize>
    using fast_vector = vecarray<T, SmallBufferSize, ~std::size_t{}>;

    template <typename WordType, uint32_t Size, std::size_t>
    struct storage_traits
    {
        typedef WordType word_t;
        typedef std::array<word_t, Size> words_t;
        static constexpr bool FixedSize = true;
    };
    template <typename WordType, std::size_t SmallBufferSize>
    struct storage_traits<WordType, 0, SmallBufferSize>
    {
        typedef WordType word_t;
        typedef fast_vector<word_t, SmallBufferSize> words_t;
        static constexpr bool FixedSize = false;
    };

    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize = 8u>
    class xinteger
    {
        template <uint32_t, uint32_t, uint32_t, bool, std::size_t>
        friend class xreal;
        template <uint32_t, integer_type, bool, std::size_t>
        friend class xinteger;
        template <uint32_t Size2, integer_type Type2, bool Signalling2, std::size_t SmallBufferSize2>
        friend xinteger<Size2, Type2, Signalling2, SmallBufferSize2> pow(const xinteger<Size2, Type2, Signalling2, SmallBufferSize2>&, const xinteger<Size2, Type2, Signalling2, SmallBufferSize2>&);
        template <typename T, uint32_t Size2, integer_type Type2, bool Signalling2, std::size_t SmallBufferSize2>
        friend T integer_cast(const xinteger<Size2, Type2, Signalling2, SmallBufferSize2>&);
        template <typename XInteger>
        friend struct xinteger_traits;
        template <typename XInteger>
        friend struct xinteger_detail;
    public:
        typedef xreal<Size, Size, Size, Signalling, SmallBufferSize> real_type;
    public:
        typedef architecture_t::word_t word_t;
        typedef architecture_t::signed_word_t signed_word_t;
        typedef uint32_t word_index_t;
        typedef word_index_t partial_word_index_t;
    private:
        typedef xinteger<Size, Type, Signalling, SmallBufferSize> self_type;
        typedef xinteger<Size, integer_type::Signed, Signalling, SmallBufferSize> signed_type;
        typedef xinteger<Size, integer_type::Unsigned, Signalling, SmallBufferSize> unsigned_type;
        typedef storage_traits<word_t, Size, SmallBufferSize> storage_traits_type;
        typedef typename storage_traits_type::words_t words_t;
    public:
        static constexpr bool IsInteger = true;
        static constexpr bool IsReal = false;
        static constexpr bool IsSigned = (Type == integer_type::Signed);
        static constexpr word_t Zero = architecture_t::Zero;
        static constexpr word_t One = architecture_t::One;
        static constexpr word_t Two = architecture_t::Two;
        static constexpr word_index_t MaxSize = (Size != 0u ? Size : std::numeric_limits<uint32_t>::max());
        static constexpr word_index_t MaxSizeHalfWords = (Size != 0u ? Size * 2u : std::numeric_limits<uint32_t>::max());
    private:
        static constexpr bool FixedSize = storage_traits_type::FixedSize;
        static constexpr word_t MaxSupportedPower = (~Zero >> Two);
        static constexpr word_t MaxLeftShift = architecture_64::LowerQuarterWordMask << (architecture_64::QuarterWordBits >> One);
        static constexpr word_t MultiplyAlgorithm1Threshold = 256u; // todo (profile/benchmark to determine optimum value)
        static constexpr word_t MultiplyAlgorithm2Threshold = 512u; // todo (profile/benchmark to determine optimum value)
        static constexpr word_t DivideAlgorithm1Threshold = 9u; // todo (profile/benchmark to determine optimum value)
    public:
        typedef words_t representation_t;
    public:
        xinteger() : 
            iNegative{ false }
        {
        }
        template <typename NativeT>
        xinteger(NativeT aNative, std::enable_if_t<std::is_integral_v<NativeT>, int> = {}) :
            iNegative{ false }
        {
            word(0u) = static_cast<word_t>(aNative);
            if constexpr (IsSigned)
                if ((static_cast<word_t>(aNative) & architecture_t::SignBit) == architecture_t::SignBit)
                    set_negative();
        }
        template <typename NativeT, std::size_t N>
        xinteger(std::array<NativeT, N> const& aNatives, std::enable_if_t<std::is_integral_v<NativeT>, int> = {}) :
            iNegative{ false }
        {
            assign(aNatives.begin(), aNatives.end());
        }
        xinteger(const self_type& aOther) :
            iWords{ aOther.iWords },
            iSignal{ aOther.iSignal },
            iNegative{ aOther.iNegative },
            iMagnitude{ aOther.iMagnitude },
            iMagnitudeInBits{ aOther.iMagnitudeInBits },
            iRepetend{ aOther.iRepetend }
        {
            words() = aOther.words();
        }
        xinteger(self_type&& aOther) :
            iWords{ std::move(aOther.iWords) },
            iSignal{ std::move(aOther.iSignal) },
            iNegative{ aOther.iNegative },
            iMagnitude{ aOther.iMagnitude },
            iMagnitudeInBits{ aOther.iMagnitudeInBits },
            iRepetend{ aOther.iRepetend }
        {
        }
        xinteger(const representation_t& aRepresentation) :
            iNegative{ false }
        {
            if constexpr (!FixedSize)
                iWords.emplace(aRepresentation);
            else
                iWords = aRepresentation;
            if constexpr (IsSigned)
                if ((cword(0u) & architecture_t::SignBit) == architecture_t::SignBit)
                    set_negative();
        }
        template <typename InputIter>
        xinteger(InputIter aFirst, InputIter aLast, const typename std::enable_if<std::is_same_v<typename std::iterator_traits<InputIter>::value_type, word_t>, sfinae>::type& = sfinae{}) :
            iNegative{ false }
        {
            assign(aFirst, aLast);
        }
    public:
        self_type& operator=(const self_type& aOther)
        {
            if (&aOther == this)
                return *this;
            words() = aOther.words();
            iNegative = aOther.iNegative;
            iSignal = aOther.iSignal;
            iMagnitude = aOther.iMagnitude;
            iMagnitudeInBits = aOther.iMagnitudeInBits;
            iRepetend = aOther.iRepetend;
            return *this;
        }
        self_type& operator=(self_type&& aOther)
        {
            if (&aOther == this)
                return *this;
            if constexpr (!FixedSize)
                iWords = std::move(aOther.iWords);
            else
                iWords = aOther.iWords;
            iNegative = aOther.iNegative;
            iSignal = std::move(aOther.iSignal);
            iMagnitude = aOther.iMagnitude;
            iMagnitudeInBits = aOther.iMagnitudeInBits;
            iRepetend = aOther.iRepetend;
            return *this;
        }
        self_type& operator=(word_t aNative)
        {
            if constexpr (!FixedSize)
            {
                words().resize(1u);
                words()[0] = aNative;
            }
            else
                words() = representation_t{ aNative };
            if constexpr (IsSigned)
                if ((aNative & architecture_t::SignBit) == architecture_t::SignBit)
                    set_negative(true);
            iSignal = std::nullopt;
            iMagnitude = 1u;
            iMagnitudeInBits = std::nullopt;
            iRepetend = std::nullopt;
            return *this;
        }
        template <typename InputIter>
        void assign(InputIter aFirst, InputIter aLast, const typename std::enable_if<std::is_same_v<typename std::iterator_traits<InputIter>::value_type, word_t>, sfinae>::type & = sfinae{})
        {                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
            reset_cache();
            auto const inputLength = static_cast<uint32_t>(std::distance(aFirst, aLast));
            if constexpr (!FixedSize)
            {
                words().clear();
                words().resize(inputLength);
            }
            std::copy(aFirst, aFirst + std::min(inputLength, max_size()), words().begin());
            if constexpr (IsSigned)
                set_negative((words().back() & architecture_t::SignBit) == architecture_t::SignBit);
        }
    public:
        explicit operator bool() const
        {
            return !is_zero();
        }
        friend bool operator==(const self_type& aLhs, const self_type& aRhs)
        {
            auto const end = std::max(aLhs.size(), aRhs.size());
            for (word_index_t index = 0; index < end; ++index)
                if (aLhs.cword(index) != aRhs.cword(index))
                    return false;
            return true;
        }
        friend bool operator!=(const self_type& aLhs, const self_type& aRhs)
        {
            return !(aLhs == aRhs);
        }
        friend bool operator<(const self_type& aLhs, const self_type& aRhs)
        {
            if constexpr (IsSigned)
            {
                if (aLhs.is_negative() && aRhs.is_positive())
                    return true;
                else if (aLhs.is_positive() && aRhs.is_negative())
                    return false;
                else if (aLhs.is_positive() && aRhs.is_positive())
                    return aLhs.to_unsigned(true) < aRhs.to_unsigned(true);
                else
                    return aLhs.to_unsigned(true) > aRhs.to_unsigned(true);
            }
            if (aLhs.magnitude() < aRhs.magnitude())
                return true;
            else if (aLhs.magnitude() > aRhs.magnitude())
                return false;
            for (word_index_t i = static_cast<word_index_t>(aLhs.magnitude()); i --> Zero;)
            {
                auto const& lhs = aLhs.cword(i);
                auto const& rhs = aRhs.cword(i);
                if (lhs < rhs)
                    return true;
                else if (lhs > rhs)
                    return false;
            }
            return false;
        }
        friend bool operator<=(const self_type& aLhs, const self_type& aRhs)
        {
            return !(aRhs < aLhs);
        }
        friend bool operator>(const self_type& aLhs, const self_type& aRhs)
        {
            return aRhs < aLhs;
        }
        friend bool operator>=(const self_type& aLhs, const self_type& aRhs)
        {
            return !(aLhs < aRhs);
        }
    public:
        friend self_type operator+(const self_type& aLhs, const self_type& aRhs)
        {
            bool overflow;
            return add_algorithm_0(aLhs, aRhs, overflow);
        }
        friend self_type operator-(const self_type& aLhs, const self_type& aRhs)
        {
            bool overflow;
            return subtract_algorithm_0(aLhs, aRhs, overflow);
        }
        self_type operator-() const
        {
            self_type result = *this;
            negate(result);
            return result;
        }
        friend self_type operator*(const self_type& aLhs, const self_type& aRhs)
        {
            return multiply_algorithm_0(aLhs, aRhs);
        }
        friend self_type operator/(const self_type& aLhs, const self_type& aRhs)
        {
            return divide_algorithm_0(aLhs, aRhs);
        }
        friend self_type operator%(const self_type& aLhs, const self_type& aRhs)
        {
            return modulo_algorithm_1(aLhs, aRhs);
        }
        self_type& operator+=(const self_type& aRhs)
        {
            bool overflow;
            return add_algorithm_0(*this, aRhs, overflow);
        }
        self_type& operator-=(const self_type& aRhs)
        {
            bool overflow;
            subtract_algorithm_0(*this, aRhs, overflow);
            return *this;
        }
        self_type& operator*=(const self_type& aRhs)
        {
            *this = multiply_algorithm_0(*this, aRhs);
            return *this;
        }
        self_type& operator/=(const self_type& aRhs)
        {
            *this = divide_algorithm_0(*this, aRhs);
            return *this;
        }
        self_type& operator%=(const self_type& aRhs)
        {
            *this = modulo_algorithm_1(*this, aRhs);
            return *this;
        }
        self_type& operator++()
        {
            static self_type const one = One;
            *this += one;
            return *this;
        }
        self_type operator++(int)
        {
            static self_type const one = One;
            auto previous = *this;
            *this += one;
            return previous;
        }
        self_type& operator--()
        {
            static self_type const one = One;
            *this -= one;
            return *this;
        }
        self_type operator--(int)
        {
            static self_type const one = One;
            auto previous = *this;
            *this -= one;
            return previous;
        }
        friend self_type operator|(const self_type& aLhs, const self_type& aRhs)
        {
            return bitwise_or(aLhs, aRhs);
        }
        friend self_type operator&(const self_type& aLhs, const self_type& aRhs)
        {
            return bitwise_and(aLhs, aRhs);
        }
        friend self_type operator^(const self_type& aLhs, const self_type& aRhs)
        {
            return bitwise_xor(aLhs, aRhs);
        }
        friend self_type operator~(const self_type& aLhs)
        {
            return bitwise_not(aLhs);
        }
        friend self_type operator>>(const self_type& aLhs, const self_type& aRhs)
        {
            return aLhs >> integer_cast<signed_word_t>(aRhs);
        }
        friend self_type operator<<(const self_type& aLhs, const self_type& aRhs)
        {
            return aLhs << integer_cast<signed_word_t>(aRhs);
        }
        friend self_type operator>>(const self_type& aLhs, signed_word_t aRhs)
        {
            return right_shift(aLhs, aRhs);
        }
        friend self_type operator<<(const self_type& aLhs, signed_word_t aRhs)
        {
            return left_shift(aLhs, aRhs);
        }
        self_type& operator|=(const self_type& aRhs)
        {
            return bitwise_or(*this, aRhs);
        }
        self_type& operator&=(const self_type& aRhs)
        {
            return bitwise_and(*this, aRhs);
        }
        self_type& operator^=(const self_type& aRhs)
        {
            return bitwise_xor(*this, aRhs);
        }
        self_type& operator>>=(const self_type& aRhs)
        {
            return *this >>= integer_cast<signed_word_t>(aRhs);
        }
        self_type& operator<<=(const self_type& aRhs)
        {
            return *this <<= integer_cast<signed_word_t>(aRhs);
        }
        self_type& operator>>=(signed_word_t aRhs)
        {
            return right_shift(*this, aRhs);
        }
        self_type& operator<<=(signed_word_t aRhs)
        {
            return left_shift(*this, aRhs);
        }
    public:
        template <typename Exception>
        const self_type& raise_signal(bool aForceThrow = false) const
        {
            iSignal = Exception{}.what();
            if (Signalling || aForceThrow)
                throw Exception();
            return *this;
        }
        template <typename Exception>
        self_type& raise_signal()
        {
            return const_cast<self_type&>(const_cast<const self_type*>(this)->raise_signal<Exception>());
        }
        bool is_signalled() const
        {
            return iSignal != std::nullopt;
        }
    public:
        static self_type from_string(const std::string& aString, uint32_t aBase = 10u)
        {
            std::optional<uint32_t> ignore;
            return from_string<>(aString, aBase, ignore);
        }
        std::string to_string(uint32_t aBase = 10u, uint32_t aGrouping = 0u) const
        {
            static unsigned_type const zero = Zero;
            
            real_type const base = aBase;
            real_type digitValue;

            if (is_signalled())
                return "sNaN";

            real_type value{ to_unsigned(true) };
            real_type coefficient;
            std::string digits;
            while(value != zero)
            {
                value.div3(base, digitValue, coefficient);
                uint8_t digit = integer_cast<uint8_t>(static_cast<unsigned_type>(digitValue));
                digit += '0';
                if (digit > '9')
                    digit += ('A' - ('0' + 10u));
                digits += digit;
            }
            if (digits.empty())
                digits = "0";
            if (aGrouping != 0u)
            {
                auto const extraZeros = aGrouping - (digits.size() % aGrouping);
                if (extraZeros != aGrouping)
                    digits.insert(digits.end(), extraZeros, '0');
            }

            std::string result;

            result.reserve(digits.size() * 2u + 1u);
            if (IsSigned && is_negative())
                result += '-';
            bool first = true;
            uint32_t count = 0u;
            for (auto d = digits.rbegin(); d != digits.rend(); ++d)
            {
                if (!first && aGrouping != 0u && (count % aGrouping) == 0u)
                    result += ' ';
                first = false;
                ++count;
                result += *d;
            }

            return result;
        }
    public:
        friend std::string hex_representation(const self_type& aInteger, bool aSigned = false)
        {
            auto const signedInteger = (!aSigned || aInteger.is_positive() ? aInteger : -aInteger);
            std::ostringstream oss;
            oss << std::hex << std::setfill('0');
            if (aInteger.is_negative() && aSigned)
                oss << "-";
            for (auto w = signedInteger.words().rbegin(); w != signedInteger.words().rend(); ++w)
                oss << std::uppercase << std::setw(architecture_t::Nybbles) << *w;
            return oss.str();
        }
    private:
        bool is_initialized() const
        {
            return iWords != std::nullopt;
        }
        bool is_zero() const
        {
            if (!is_initialized())
                return true;
            static self_type const zero = Zero;
            return *this == zero;
        }
        template <bool AsFloatingPoint = false>
        static self_type from_string(const std::string& aString, uint32_t aBase, std::optional<uint32_t>& aFractionalPlaces)
        {
            static self_type const zero = Zero;

            self_type result;

            unsigned_type const base = aBase;
            unsigned_type unsignedResult;
            unsigned_type multiplier = 1u;
            unsigned_type digitValue;
            bool negative = false;
            bool allowWhitespace = true;
            bool gotDigit = false;
            bool gotPoint = false;
            auto start = std::find_if(aString.rbegin(), aString.rend(), [](auto const& c) { return (std::isblank(static_cast<int>(c)) == 0); });
            auto nonZero = std::find_if(start, aString.rend(), [](auto const& c) { return c != '0'; });
            if (nonZero != aString.rend() && *nonZero == '.')
                start = std::next(nonZero);
            for (auto iterString = start; iterString != aString.rend(); ++iterString)
            {
                auto digitCharacter = *iterString;
                bool const isWhitespace = (std::isblank(static_cast<int>(digitCharacter)) != 0);
                if (isWhitespace)
                {
                    if (allowWhitespace)
                        continue;
                    else
                        return (result = zero).template raise_signal<invalid_string>();
                }
                allowWhitespace = false;
                if (digitCharacter == '-')
                {
                    if (!negative)
                    {
                        negative = true;
                        continue;
                    }
                    else
                        return (result = zero).template raise_signal<invalid_string>();
                }
                if (digitCharacter == '.')
                {
                    if constexpr (!AsFloatingPoint)
                        return (result = zero).template raise_signal<invalid_string>();
                    if (gotPoint)
                        return (result = zero).template raise_signal<invalid_string>();
                    aFractionalPlaces = static_cast<uint32_t>(iterString - start);
                    gotPoint = true;
                    continue;
                }
                if (!gotDigit)
                    gotDigit = true;
                uint8_t digit = digitCharacter - '0';
                if (digit > 9u)
                    digit -= ('A' - ('0' + 10u));
                if (digit > 0xFu)
                    digit -= ('a' - 'A');
                if (digit >= aBase)
                    return (result = zero).template raise_signal<invalid_string>();
                digitValue = digit * multiplier;
                unsignedResult += digitValue;
                multiplier *= base;
            }
            if (gotPoint)
            {
                unsignedResult *= base;
                ++*aFractionalPlaces;
            }
            if constexpr (IsSigned)
                result = unsignedResult.to_signed(true);
            else
                result = unsignedResult;
            if (negative)
                result = -result;
            return result;
        }
    private:
        unsigned_type to_unsigned(bool aTakeAbsolute = false) const
        {
            unsigned_type result;
            to_unsigned(result, aTakeAbsolute);
            return result;
        }
        unsigned_type& to_unsigned(unsigned_type& aLhs, bool aTakeAbsolute = false) const
        {
            auto& result = aLhs;

            result.words() = words();

            if (aTakeAbsolute && is_negative())
                negate(result);

            if constexpr (!FixedSize)
            {
                auto nonZero = std::find_if(result.words().rbegin(), result.words().rend(), [](auto const& aValue) { return aValue != Zero; });
                if (nonZero != result.words().rend())
                    result.words().erase(nonZero.base(), result.words().end());
            }

            return result;
        }
        signed_type to_signed(bool aIsAbsolute = false, bool aHasSignBit = true) const
        {
            signed_type result;
            to_signed(result, aIsAbsolute, aHasSignBit);
            return result;
        }
        signed_type& to_signed(signed_type& aLhs, bool aIsAbsolute = false, bool aHasSignBit = true) const
        {
            auto& result = aLhs;

            result.words() = words();
            result.set_positive();

            if (result.is_sign_bit_set())
            {
                if (!aIsAbsolute)
                    result.set_negative();
                else if constexpr (!FixedSize)
                {
                    if (aHasSignBit)
                        result.words().push_back(Zero);
                }
            }

            return result;
        }
        static constexpr word_index_t word_index_of_bit(word_t aBitPosition)
        {
            word_t constexpr bitIndexRadixMask = architecture_t::WordBits - One;
            word_t constexpr bitIndexShift = bits(bitIndexRadixMask);
            auto const wordIndex = static_cast<word_index_t>(aBitPosition >> bitIndexShift);
            return wordIndex;
        }
        word_t bit(word_t aBitPosition) const
        {
            word_t constexpr bitIndexRadixMask = architecture_t::WordBits - One;
            word_t const bitToCheck = (One << (aBitPosition & bitIndexRadixMask));
            auto const wordIndex = word_index_of_bit(aBitPosition);
            auto const& w = word(wordIndex);
            return (w & bitToCheck) >> (aBitPosition & bitIndexRadixMask);
        }
        void set_bit(word_t aBitPosition, word_t aValue)
        {
            word_t constexpr bitIndexRadixMask = architecture_t::WordBits - One;
            word_t const bitToSetOrClear = (One << (aBitPosition & bitIndexRadixMask));
            auto const wordIndex = word_index_of_bit(aBitPosition);
            auto& w = word(wordIndex);
            w = (w & ~bitToSetOrClear) | (aValue << (aBitPosition & bitIndexRadixMask));
        }
    private:
        const word_t& high_word() const
        {
            return cword(static_cast<word_index_t>(magnitude() - 1));
        }
        word_t& high_word()
        {
            return word(static_cast<word_index_t>(magnitude() - 1));
        }
        bool is_high_bit_set() const
        {
            return ((high_word() & architecture_t::HighBit) == architecture_t::HighBit);
        }
        void set_high_bit(word_t aValue)
        {
            auto& msw = high_word();
            if (aValue)
                msw |= architecture_t::HighBit;
            else
                msw &= ~architecture_t::HighBit;
        }
        bool is_sign_bit_set() const
        {
            if constexpr (IsSigned)
                return is_high_bit_set();
            else
                return false;
        }
        bool is_positive() const
        {
            if constexpr (IsSigned)
                return !iNegative;
            else
                return true;
        }
        bool is_negative() const
        {
            if constexpr (IsSigned)
                return iNegative;
            else
                return false;
        }
        void set_positive(bool aPositive = true)
        {
            if (!aPositive)
                return set_negative();
            if constexpr (IsSigned)
                iNegative = false;
        }
        void set_negative(bool aNegative = true)
        {
            if (!aNegative)
                return set_positive();
            if constexpr (IsSigned)
                iNegative = true;
        }
        void correct_sign(bool aPreviousLhsSignBit, bool aPreviousRhsSignBit)
        {
            word_t carry = Zero;
            correct_sign(aPreviousLhsSignBit, aPreviousRhsSignBit, carry);
        }
        void correct_sign(bool aPreviousLhsSignBit, bool aPreviousRhsSignBit, word_t& aCarry)
        {
            if constexpr (IsSigned)
            {
                if (aPreviousLhsSignBit != aPreviousRhsSignBit)
                {
                    if (!is_sign_bit_set() && is_negative())
                    {
                        if (is_sign_bit_set() != aPreviousLhsSignBit)
                            set_positive();
                        aCarry = Zero;
                    }
                    else if (is_sign_bit_set() && is_positive())
                    {
                        if (is_sign_bit_set() != aPreviousLhsSignBit)
                            set_negative();
                        aCarry = Zero;
                    }
                    else if (aCarry == One)
                        aCarry = Zero;
                }
            }
        }
        word_t magnitude(bool aTruncate = true) const
        {
            if (iMagnitude != std::nullopt)
                return *iMagnitude;
            auto i = aTruncate ? std::find_if(words().rbegin(), words().rend(), [this](auto const& w)
            {
                if (is_positive())
                    return w != Zero;
                else
                    return w != ~Zero;
            }) : words().rbegin();
            auto msw = static_cast<std::size_t>(i - words().rbegin());
            msw = (msw < words().size() ? words().size() - 1u - msw : 0u);
            if constexpr (IsSigned)
                if (msw + 1u < words().size() && 
                    ((is_negative() && ((cword(static_cast<word_index_t>(msw)) & architecture_t::SignBit) != architecture_t::SignBit)) || 
                     (is_positive() && ((cword(static_cast<word_index_t>(msw)) & architecture_t::SignBit) == architecture_t::SignBit))))
                    ++msw;
            iMagnitude = msw + 1u;
            if constexpr (!FixedSize)
            {
                if (aTruncate)
                    iWords->resize(*iMagnitude);
            }
            return *iMagnitude;
        }
        word_t magnitude_in_bits() const
        {
            if (iMagnitudeInBits != std::nullopt)
                return *iMagnitudeInBits;
            if (is_zero())
                iMagnitudeInBits = Zero;
            else
            {
                word_t msb = magnitude() * architecture_t::WordBits - One;
                while (msb > Zero && bit(msb) == Zero)
                    --msb;
                iMagnitudeInBits = (msb > Zero ? msb + One : bit(Zero) ? One : Zero);
            }
            return *iMagnitudeInBits;
        }
        template <word_t Bits>
        word_t partial_magnitude() const
        {
            auto result = magnitude() * (architecture_t::WordBits / Bits);
            while (result > One && partial_word<Bits>(result - 1) == Zero)
                --result;
            return result;
        }
        word_index_t size() const
        {
            return static_cast<word_index_t>(words().size());
        }
        word_index_t max_size() const
        {
            return MaxSize;
        }
        word_index_t max_half_word_size() const
        {
            return MaxSizeHalfWords;
        }
        template <word_t Bits>
        partial_word_index_t size_in_partial_words() const
        {
            return static_cast<partial_word_index_t>(size() * (architecture_t::WordBits / Bits));
        }
        void resize(word_index_t aSize)
        {
            if constexpr (!FixedSize)
            {
                if (aSize == 0u)
                    words().clear();
                else
                    words().resize(aSize, cword(aSize - 1));
            }
        }
        self_type with_size(word_index_t aSize) const
        {
            self_type result = *this;
            result.resize(aSize);
            return result;
        }
        const words_t& words() const
        {
            if (iWords == std::nullopt)
                iWords.emplace();
            return *iWords;
        }
        words_t& words()
        {
            reset_cache();
            return const_cast<words_t&>(const_cast<const self_type*>(this)->words());
        }
        const word_t& cword(word_index_t aIndex) const
        {
            static word_t const zero = Zero;
            static word_t const neg = ~Zero;
            if (is_signalled())
                return zero;
            else if (aIndex < size())
                return words()[aIndex];
            else if (is_positive())
                return zero;
            else
                return neg;
        }
        const word_t& word(word_index_t aIndex) const
        {
            return cword(aIndex);
        }
        word_t& word(word_index_t aIndex)
        {
            reset_cache();
            if (aIndex >= max_size())
                throw out_of_range();
            if constexpr (!FixedSize)
            {
                try
                {
                    while (aIndex >= size())
                        words().push_back(cword(size()));
                }
                catch (std::bad_alloc)
                {
                    raise_signal<number_allocation_failure>(true);
                }
            }
            return words()[aIndex];
        }
        word_t& set_word(word_index_t aIndex, word_t aValue)
        {
            auto& w = word(aIndex);
            w = aValue;
            return w;
        }
        template <word_t Bits>
        static constexpr word_index_t partial_word_index(word_t aPartialPosition)
        {
            word_t constexpr partialIndexShift = bits(architecture_t::WordBits / Bits - One);
            auto const wordIndex = static_cast<word_index_t>(aPartialPosition >> partialIndexShift);
            return wordIndex;
        }
        template <word_t Bits>
        word_t partial_word(word_t aPartialPosition) const
        {
            if ((aPartialPosition & architecture_t::SignBit) == architecture_t::SignBit)
                return Zero;
            word_t constexpr partialIndexRadixMask = (One << Bits) - One;
            word_t constexpr partialIndexShift = bits(partialIndexRadixMask);
            word_t const partialMaskShift = (partialIndexShift * (aPartialPosition & partialIndexRadixMask));
            word_t const bitsToCheck = partialIndexRadixMask << partialMaskShift;
            auto const wordIndex = partial_word_index<Bits>(aPartialPosition);
            auto& w = word(wordIndex);
            return (w & bitsToCheck) >> partialMaskShift;
        }
        template <word_t Bits>
        void set_partial_word(word_t aPartialPosition, word_t aValue)
        {
            if ((aPartialPosition & architecture_t::SignBit) == architecture_t::SignBit)
                return;
            word_t constexpr partialIndexRadixMask = (One << Bits) - One;
            word_t constexpr partialIndexShift = bits(partialIndexRadixMask);
            word_t const partialMaskShift = (partialIndexShift * (aPartialPosition & partialIndexRadixMask));
            word_t const bitToSetOrClear = partialIndexRadixMask << partialMaskShift;
            if ((aValue & ~partialIndexRadixMask) != Zero)
                throw invalid_partial_value();
            auto const wordIndex = partial_word_index<Bits>(aPartialPosition);
            auto& w = word(wordIndex);
            w = (w & ~bitToSetOrClear) | (aValue << partialMaskShift);
        }
        template <word_t Bits>
        self_type& pack()
        {
            auto constexpr partialWords = static_cast<word_index_t>(architecture_t::WordBits / Bits);
            auto end = static_cast<word_index_t>(magnitude());
            for (word_index_t index = 0u; index < end; ++index)
                set_partial_word<Bits>(index, partial_word<Bits>(index * partialWords));
            if constexpr (!FixedSize)
                words().resize((end - 1u) / partialWords + 1u);
            else
                std::fill(words().begin() + end / partialWords, words().end(), Zero);
            return *this;
        }
        static word_t add_words(word_t aLhs, word_t aRhs, word_t& aCarry)
        {
            auto result = aLhs;
            result += aCarry;
            aCarry = (result >= aLhs ? Zero : architecture_t::LowBit);
            result += aRhs;
            aCarry += (result >= aRhs ? Zero : architecture_t::LowBit);
            return result;
        }
        static word_t multiply_words(word_t aLhs, word_t aRhs, word_t& aCarry)
        {
            auto u = aLhs;
            auto v = aRhs;

            word_t u1 = architecture_t::low_half(u);
            word_t v1 = architecture_t::low_half(v);
            word_t t = u1 * v1;
            word_t w3 = architecture_t::low_half(t);
            word_t k = architecture_t::high_half(t);

            u = architecture_t::high_half(u);
            t = (u * v1) + k;
            k = architecture_t::low_half(t);
            word_t w1 = architecture_t::high_half(t);

            v = architecture_t::high_half(v);
            t = (u1 * v) + k;
            k = architecture_t::high_half(t);

            auto const product = to_word(w3, t);
            aCarry = (u * v) + w1 + k;
            return product;
        }
        static constexpr word_t low_half(word_t value)
        {
            return architecture_t::low_half(value);
        }
        static constexpr word_t high_half(word_t value) 
        { 
            return architecture_t::high_half(value);
        }
        static constexpr word_t to_word(word_t lower, word_t upper) 
        { 
            return architecture_t::to_word(lower, upper);
        }
        void reset_cache()
        {
            iSignal = std::nullopt;
            iMagnitude = std::nullopt;
            iMagnitudeInBits = std::nullopt;
        }
    private:
        bool has_repetend() const
        {
            return iRepetend != std::nullopt;
        }
        const neonumeric::repetend& repetend() const
        {
            if (!has_repetend())
                iRepetend.emplace();
            return *iRepetend;
        }
        neonumeric::repetend& repetend()
        {
            return const_cast<neonumeric::repetend&>(const_cast<const self_type*>(this)->repetend());
        }
        self_type unpack(word_t aMagntitudeInBits) const
        {
            if (!has_repetend())
                throw not_packed();

            static const self_type zero = Zero;
            if (aMagntitudeInBits == Zero)
                return zero;

            word_index_t const resultWordCount = static_cast<word_index_t>((aMagntitudeInBits - One) / architecture_t::WordBits + One);
            aMagntitudeInBits = resultWordCount * architecture_t::WordBits;
            if (resultWordCount > max_size())
                throw number_too_large();

            word_index_t const sourceWordCount = static_cast<word_index_t>(magnitude());
            word_index_t sourceWordIndex = sourceWordCount - 1u;
            word_index_t const cycleStartWordIndex = static_cast<word_index_t>(repetend().cycleStart / architecture_t::WordBits);
            word_index_t const cycleEndWordIndex = static_cast<word_index_t>(repetend().cycleEnd / architecture_t::WordBits);

            self_type result;
            for (auto resultWordIndex = resultWordCount; resultWordIndex--> cycleEndWordIndex; )
                result.word(resultWordIndex) = word(sourceWordIndex);

            word_t sourceBitIndex = repetend().cycleEnd;
            word_t destinationBitIndex = aMagntitudeInBits - (magnitude_in_bits() - repetend().cycleEnd);

            do
            {
                result.set_bit(destinationBitIndex, bit(sourceBitIndex));
                if (sourceBitIndex-- == repetend().cycleStart)
                    sourceBitIndex = repetend().cycleEnd;
            } while (destinationBitIndex --> Zero);

            return result;
        }
    private:
        mutable std::optional<words_t> iWords;
        bool iNegative;
        mutable std::optional<std::string> iSignal;
        mutable std::optional<word_t> iMagnitude;
        mutable std::optional<word_t> iMagnitudeInBits;
        mutable std::optional<neonumeric::repetend> iRepetend;
        self_type& iUnpacker = *this;
        mutable std::optional<word_t> iUnpacked;
    };

    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    inline xinteger<Size, Type, Signalling, SmallBufferSize> pow(const xinteger<Size, Type, Signalling, SmallBufferSize>& aBase, const xinteger<Size, Type, Signalling, SmallBufferSize>& aExponent)
    {
        return pow_algorithm_0(aBase, aExponent);
    }

    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    inline xinteger<Size, Type, Signalling, SmallBufferSize> pow(const xinteger<Size, Type, Signalling, SmallBufferSize>& aBase, architecture_t::word_t aExponent)
    {
        return pow(aBase, xinteger<Size, Type, Signalling, SmallBufferSize>{ aExponent});
    }

    template <typename T, uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize>
    inline T integer_cast(const xinteger<Size, Type, Signalling, SmallBufferSize>& aValue)
    {
        if (aValue.magnitude() != 1u && Signalling)
            throw number_too_large();
        return static_cast<T>(aValue.cword(0u));
    }

    template <uint32_t Size, integer_type Type, bool Signalling, std::size_t SmallBufferSize, typename Char, typename CharTraits>
    inline std::basic_ostream<Char, CharTraits>& operator<<(std::basic_ostream<Char, CharTraits>& aOutput, const xinteger<Size, Type, Signalling, SmallBufferSize>& aInteger)
    {
        if (aOutput.flags() & std::ios_base::dec)
            aOutput << aInteger.to_string(10);
        else if (aOutput.flags() & std::ios_base::hex)
            aOutput << aInteger.to_string(16);
        else if (aOutput.flags() & std::ios_base::oct)
            aOutput << aInteger.to_string(8);
        else
            aOutput << aInteger.to_string();
        return aOutput;
    }

    template <uint32_t Size, bool Signalling = true>
    using uinteger = xinteger<Size, integer_type::Unsigned, Signalling>;
    template <uint32_t Size, bool Signalling = true>
    using integer = xinteger<Size, integer_type::Signed, Signalling>;
}

using neonumeric::integer_cast;
