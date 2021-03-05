// real.hpp
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
#include <neonumeric/exceptions.hpp>
#include <neonumeric/integer.hpp>

namespace neonumeric
{
    template <typename RealType, typename IntegerType>
    inline RealType frac(const IntegerType& aLhs, const IntegerType& aRhs);

    template <uint32_t Size, uint32_t MantissaSize = Size, uint32_t ExponentSize = Size, bool Signalling = true, std::size_t SmallBufferSize = 8u>
    class xreal
    {
        template <typename RealType, typename IntegerType>
        friend RealType frac(const IntegerType&, const IntegerType&);
    private:
        typedef xreal<Size, MantissaSize, ExponentSize, Signalling, SmallBufferSize> self_type;
    public:
        typedef xinteger<Size, integer_type::Signed, Signalling, SmallBufferSize> signed_integer_type;
        typedef xinteger<Size, integer_type::Unsigned, Signalling, SmallBufferSize> unsigned_integer_type;
    public:
        typedef architecture_t::word_t word_t;
        typedef architecture_t::signed_word_t signed_word_t;
        typedef uint32_t precision_t;
    private:
        typedef signed_word_t exponent_t;
        typedef xinteger<MantissaSize, integer_type::Unsigned, Signalling, SmallBufferSize> mantissa_t;
        typedef signed_integer_type integer_t;
    private:
        enum class special
        {
            Normal,
            Infinity,
            NaN
        };
    public:
        static constexpr bool IsReal = true;
        static constexpr bool IsInteger = false;
    private:
        static constexpr word_t Zero = architecture_t::Zero;
        static constexpr word_t One = architecture_t::One;
        static constexpr word_t Two = architecture_t::Two;
        static constexpr signed_word_t SZero = architecture_t::SZero;
        static constexpr signed_word_t SOne = architecture_t::SOne;
        static constexpr signed_word_t STwo = architecture_t::STwo;
    public:
        xreal()
        {
        }
        xreal(const self_type& aOther) :
            iSignBit{ aOther.iSignBit },
            iSpecial{ aOther.iSpecial },
            iExponent{ aOther.iExponent },
            iMantissa{ aOther.iMantissa },
            iNormalized{ aOther.iNormalized },
            iPrecision{ aOther.iPrecision }
        {
        }
        xreal(self_type&& aOther) :
            iSignBit{ aOther.iSignBit },
            iSpecial{ aOther.iSpecial },
            iExponent{ std::move(aOther.iExponent) },
            iMantissa{ std::move(aOther.iMantissa) },
            iNormalized{ aOther.iNormalized },
            iPrecision{ aOther.iPrecision }
        {
        }
        xreal(double aNativeFloat)
        {
            *this = aNativeFloat;
        }
        explicit xreal(const signed_integer_type& aInteger)
        {
            *this = aInteger;
        }
        explicit xreal(const unsigned_integer_type& aInteger)
        {
            *this = aInteger;
        }
        explicit xreal(word_t aInteger)
        {
            *this = aInteger;
        }
    public:
        self_type& operator=(const self_type& aOther)
        {
            if (&aOther == this)
                return *this;
            iSignBit = aOther.iSignBit;
            iSpecial = aOther.iSpecial;
            iExponent = aOther.iExponent;
            iMantissa = aOther.iMantissa;
            iNormalized = aOther.iNormalized;
            if (aOther.iPrecision != std::nullopt)
                set_precision_at_least(*aOther.iPrecision);
            return *this;
        }
        self_type& operator=(self_type&& aOther)
        {
            if (&aOther == this)
                return *this;
            iSignBit = aOther.iSignBit;
            iSpecial = aOther.iSpecial;
            iExponent = std::move(aOther.iExponent);
            iMantissa = std::move(aOther.iMantissa);
            iNormalized = aOther.iNormalized;
            if (aOther.iPrecision != std::nullopt)
                set_precision_at_least(*aOther.iPrecision);
            return *this;
        }
        self_type& operator=(double aNativeFloat)
        {
            float_assign(aNativeFloat);
            return *this;
        }
        self_type& operator=(const signed_integer_type& aInteger)
        {
            word_t ignore;
            integer_assign(aInteger, ignore);
            update_mantissa_precision();
            return *this;
        }
        self_type& operator=(const unsigned_integer_type& aInteger)
        {
            *this = aInteger.to_signed(true);
            update_mantissa_precision();
            return *this;
        }
        self_type& operator=(word_t aInteger)
        {
            *this = integer_t{ aInteger };
            update_mantissa_precision();
            return *this;
        }
    public:
        explicit operator double() const
        {
            switch (iSpecial)
            {
            case special::Infinity:
                return is_positive() ? std::numeric_limits<double>::infinity() : -std::numeric_limits<double>::infinity();
            case special::NaN:
                return std::numeric_limits<double>::quiet_NaN();
            }
            if (is_zero())
                return 0.0;
            // todo: raise exception and/or nan on too big for double
            auto const& arg = as_argument(*this).arg;
            uint64_t bits = {};
            if (arg.is_negative())
                bits |= 1ull;
            bits <<= 1; // sign
            bits <<= 11; // exponent
            auto const start = arg.mantissa().magnitude() * architecture_t::WordBits - Two;
            auto nextBit = start;
            static unsigned_integer_type const one = One;
            auto test = one << nextBit;
            while (nextBit > start - 52)
            {
                if (nextBit != start)
                    bits <<= 1;
                if (arg.mantissa() & test)
                    bits |= 1;
                --nextBit;
                test >>= one;
            }
            double result = std::pow(2.0, exponent());
            *reinterpret_cast<uint64_t*>(&result) |= bits;
            return result;
        }
        operator signed_integer_type() const
        {
            auto result = static_cast<unsigned_integer_type>(*this).to_signed(true);
            if (is_negative())
                negate(result);
            return result;
        }
        operator unsigned_integer_type() const
        {
            auto const& m = mantissa();
            auto const mantissaMagnitudeInBits = static_cast<signed_word_t>(m.magnitude_in_bits());
            auto const exponentAdjust = exponent() + SOne;
            if (mantissaMagnitudeInBits > exponentAdjust)
                return m >> (mantissaMagnitudeInBits - exponentAdjust);
            else
                return m << (exponentAdjust - mantissaMagnitudeInBits);
        }
    public:
        explicit operator bool() const
        {
            return !is_zero();
        }
        friend bool operator==(const self_type& aLhs, const self_type& aRhs)
        {
            static mantissa_t const zero = mantissa_t::Zero;
            return (aLhs.mantissa() == zero && aRhs.mantissa() == zero) ||
                (aLhs.is_positive() == aRhs.is_positive() &&
                aLhs.normalized_exponent() == aRhs.normalized_exponent() && 
                aLhs.normalized_mantissa() == aRhs.normalized_mantissa());
        }
        friend bool operator!=(const self_type& aLhs, const self_type& aRhs)
        {
            return !(aLhs == aRhs);
        }
        friend bool operator<(const self_type& aLhs, const self_type& aRhs)
        {
            if (aLhs.is_positive() != aRhs.is_positive())
                return aLhs.is_negative();
            if (aLhs.normalized_exponent() < aRhs.normalized_exponent())
                return true;
            else if (aLhs.normalized_exponent() > aRhs.normalized_exponent())
                return false;
            if (aLhs.normalized_mantissa() < aRhs.normalized_mantissa())
                return true;
            else if (aLhs.normalized_mantissa() > aRhs.normalized_mantissa())
                return false;
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
            self_type result = aLhs;
            result += aRhs;
            return result;
        }
        friend self_type operator-(const self_type& aLhs, const self_type& aRhs)
        {
            self_type result = aLhs;
            result -= aRhs;
            return result;
        }
        self_type operator-() const
        {
            self_type result;
            result.mantissa() = negate(mantissa());
            result.exponent() = exponent();
            return result;
        }
        friend self_type operator*(const self_type& aLhs, const self_type& aRhs)
        {
            self_type result = aLhs;
            result *= aRhs;
            return result;
        }
        friend self_type operator/(const self_type& aLhs, const self_type& aRhs)
        {
            self_type result = aLhs;
            result /= aRhs;
            return result;
        }
        friend self_type operator%(const self_type& aLhs, const self_type& aRhs)
        {
            self_type result = aLhs;
            result %= aRhs;
            return result;
        }
        self_type& operator+=(const self_type& aRhs)
        {
            return add_subtract_algorithm_0<true>(aRhs);
        }
        self_type& operator-=(const self_type& aRhs)
        {
            return add_subtract_algorithm_0<false>(aRhs);
        }
        self_type& operator*=(const self_type& aRhs)
        {
            return multiply_algorithm_0(aRhs);
        }
        self_type& operator/=(const self_type& aRhs)
        {
            bool const resultNegative = (is_positive() != aRhs.is_positive());
            division_algorithm_0(aRhs);
            set_negative(resultNegative);
            return *this;
        }
        self_type& operator%=(const self_type& aRhs)
        {
            self_type remainder;
            div2(aRhs, remainder);
            return *this = remainder;
        }
        self_type& div2(const self_type& aRhs, self_type& aRemainder)
        {
            self_type ignore;
            return div3(aRhs, aRemainder, ignore);
        }
        self_type& div3(const self_type& aRhs, self_type& aRemainder, self_type& aCoefficient)
        {
            auto const maxMagnitude = std::min(precision_magnitude(), std::max(mantissa().magnitude(), aRhs.mantissa().magnitude()));
            static const self_type zero = Zero;
            signed_integer_type const integerNumerator = *this;
            signed_integer_type const integerDenominator = aRhs;
            if (aRhs == zero)
            {
                if (*this == zero)
                {
                    *this = zero;
                    iSpecial = special::NaN;
                }
                else
                {
                    *this = zero;
                    iSpecial = special::Infinity;
                }
                return *this;
            }
            else if (integerNumerator == integerDenominator)
            {
                aRemainder = Zero;
                *this = One;
            }
            else if (integerNumerator < integerDenominator)
            {
                aRemainder = integerNumerator;
                *this = Zero;
            }
            else
            {
                division_algorithm_0(aRhs, aCoefficient);
                if (iSpecial != special::Normal)
                    return *this;
                truncate_mantissa(maxMagnitude);
                auto const integerQuotient = signed_integer_type{ *this };
                auto const integerNumeratorMinusRemainder = integerDenominator * integerQuotient;
                auto const integerRemainder = integerNumerator - integerNumeratorMinusRemainder;
                aRemainder = integerRemainder;
                *this = integerQuotient;
            }
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
        word_t precision_magnitude() const
        {
            return ((precision() - One) / architecture_t::WordBits) + One;
        }
        precision_t precision() const
        {
            if (iPrecision != std::nullopt)
                return *iPrecision;
            else if constexpr (mantissa_t::FixedSize)
                return mantissa_t::MaxSize * architecture_t::WordBits;
            else
                return static_cast<precision_t>(mantissa().magnitude() * architecture_t::WordBits);
        }
        void set_precision(std::optional<precision_t> const& aPrecision)
        {
            iPrecision = aPrecision;
            update_mantissa_precision();
        }
        void set_precision_at_least(precision_t const& aPrecision)
        {
            set_precision(iPrecision == std::nullopt ? aPrecision : std::max(*iPrecision, aPrecision));
        }
        static self_type from_string(const std::string& aString, uint32_t aBase = 10u)
        {
            std::optional<uint32_t> fractionalPlaces;
            auto signedResult = signed_integer_type::template from_string<true>(aString, aBase, fractionalPlaces);;
            self_type result;
            result = signedResult.to_unsigned(true);
            result.set_precision(static_cast<precision_t>(result.mantissa().magnitude() * architecture_t::WordBits));
            if (signedResult.is_negative())
                result.set_negative();
            if (fractionalPlaces)
            {
                self_type const d{ pow(integer_t{ aBase }, *fractionalPlaces) };
                result /= d;
            }
            return result;
        }
        std::string to_string(uint32_t aBase = 10u, precision_t aPrecision = std::numeric_limits<double>::digits10 + 1, uint32_t aGrouping = 0u) const
        {
            static self_type const zero = Zero;

            self_type const base = aBase;
            self_type digitValue;

            if (is_signalled())
                return "sNaN";

            switch (iSpecial)
            {
            case special::Infinity:
                return is_positive() ? "inf" : "-inf";
            case special::NaN:
                return "nan";
            }

            std::string digits;

            self_type value = *this;
            value.set_positive();

            auto const integerValue = static_cast<signed_integer_type>(value);
            value -= self_type{ integerValue };
            if (aPrecision > 0u && value != zero)
            {
                self_type round = (aBase == 2 ? One : base / 2);
                round /= self_type{ pow(integer_t{ aBase }, integer_t{ aPrecision + 1u }) };
                value += round;
            }
            while (aPrecision-- > 0u)
            {
                value *= base;
                digitValue = value % base;
                value -= digitValue;
                uint8_t digit = integer_cast<uint8_t>(static_cast<typename mantissa_t::unsigned_type>(digitValue));
                digit += '0';
                if (digit > '9')
                    digit += ('A' - ('0' + 10u));
                digits = static_cast<char>(digit) + digits;
            }

            digits += '.';

            value = *this;
            value.set_positive();

            if (value != zero)
            {
                self_type coefficient;
                while (value != zero)
                {
                    value.div3(base, digitValue, coefficient);
                    uint8_t digit = integer_cast<uint8_t>(static_cast<typename mantissa_t::unsigned_type>(digitValue));
                    digit += '0';
                    if (digit > '9')
                        digit += ('A' - ('0' + 10u));
                    digits += digit;
                }
            }
            else
                digits += '0';

            if (aGrouping != 0u)
            {
                auto const extraZeros = aGrouping - (digits.size() % aGrouping);
                if (extraZeros != aGrouping)
                    digits.insert(digits.end(), extraZeros, '0');
            }

            std::string result;

            result.reserve(digits.size() * 2u + 1u);
            if (is_negative())
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
    private:
        bool is_initialized() const
        {
            return iMantissa.is_initialized();
        }
        bool is_zero() const
        {
            if (!is_initialized())
                return true;
            static mantissa_t const zero = Zero;
            return mantissa() == zero;
        }
        bool is_positive() const
        {
            return iSignBit == false;
        }
        bool is_negative() const
        {
            return iSignBit == true;
        }
        void set_positive(bool aPositive = true)
        {
            iSignBit = !aPositive;
        }
        void set_negative(bool aNegative = true)
        {
            iSignBit = aNegative;
        }
        self_type& float_assign(double aNativeFloat)
        {
            if (std::isnan(aNativeFloat))
            {
                static const self_type zero = Zero;
                *this = zero;
                iSpecial = special::NaN;
                return *this;
            }
            else if (std::isinf(aNativeFloat))
            {
                static const self_type zero = Zero;
                *this = zero;
                iSpecial = special::Infinity;
                set_positive(aNativeFloat > 0.0);
                return *this;
            }
            static mantissa_t const zero = Zero;
            int e;
            double f = std::frexp(aNativeFloat, &e);
            --e;
            mantissa_t& m = mantissa();
            exponent() = (f != 0.0 ? e : 0);
            set_positive(aNativeFloat >= 0.0);
            uint64_t const nativeRepresentation = *reinterpret_cast<uint64_t*>(&aNativeFloat);
            uint64_t const nativeMantissaRepresentation = (nativeRepresentation << 12 >> (architecture_64::WordBits - architecture_t::WordBits));
            m = (nativeMantissaRepresentation >> 1);
            if (f != 0.0)
                m |= architecture_t::HighBit;
            return *this;
        }
        self_type& integer_assign(const signed_integer_type& aInteger, word_t& aMantissaShift)
        {
            integer_assign(aInteger.to_unsigned(true), aMantissaShift);
            if (aInteger.is_negative())
                set_negative();
            return *this;
        }
        self_type& integer_assign(const unsigned_integer_type& aInteger, word_t& aMantissaShift)
        {
            mantissa() = aInteger;
            set_positive();
            auto exponentAdjust = Zero;
            if constexpr (!mantissa_t::FixedSize)
            {
                if (mantissa().is_sign_bit_set() && aInteger.is_positive())
                {
                    mantissa() >>= One;
                    exponentAdjust = One;
                }
            }
            exponent() = shift_mantissa(aMantissaShift) + exponentAdjust;
            return *this;
        }
        word_t shift_mantissa(word_t& aMantissaShift) const
        {
            if (iMantissa.is_zero())
            {
                aMantissaShift = Zero;
                return Zero;
            }
            static mantissa_t const one = One;
            if constexpr (!mantissa_t::FixedSize)
            {
                auto nonZero = std::find_if(iMantissa.words().rbegin(), iMantissa.words().rend(), [](auto const& aValue) { return aValue != Zero; });
                if (nonZero != iMantissa.words().rend())
                    iMantissa.words().erase(nonZero.base(), iMantissa.words().end());
            }
            auto const start = iMantissa.magnitude() * architecture_t::WordBits - One;
            auto msb = start;
            auto test = one << msb;
            while (msb > Zero && (iMantissa & test) == 0u)
            {
                --msb;
                test >>= one;
            }
            aMantissaShift = start - msb;
            iMantissa <<= aMantissaShift;
            return msb;
        }
        void truncate_mantissa(word_t aMagnitude, bool aRound = true) const
        {
            if constexpr (!mantissa_t::FixedSize)
            {
                if (iMantissa.magnitude() > aMagnitude)
                {
                    auto const end = std::next(iMantissa.words().begin(), iMantissa.magnitude() - aMagnitude);
                    bool const carry = (*std::prev(end) & architecture_t::HighBit) != Zero;
                    iMantissa.words().erase(iMantissa.words().begin(), end);
                    if (carry && aRound)
                    {
                        auto preCarryMagntitude = iMantissa.magnitude();
                        ++iMantissa;
                        if (iMantissa.magnitude() > preCarryMagntitude)
                            ++iExponent;
                        word_t ignore;
                        shift_mantissa(ignore);
                    }
                }
            }
        }
        void update_mantissa_precision() const
        {
            if constexpr (!mantissa_t::FixedSize)
                if (!mantissa().has_repetend() && iPrecision != std::nullopt)
                    truncate_mantissa(precision_magnitude());
        }
        bool is_normalized() const
        {
            return iNormalized;
        }
        void normalize() const
        {
            if (!iNormalized)
            {
                iNormalized = true;
                word_t adjust;
                shift_mantissa(adjust);
                iExponent -= adjust;
            }
            update_mantissa_precision();
        }
        const exponent_t& exponent() const
        {
            return iExponent;
        }
        const exponent_t& normalized_exponent() const
        {
            normalize();
            return iExponent;
        }
        exponent_t& exponent()
        {
            iNormalized = false;
            return iExponent;
        }
        const mantissa_t& mantissa() const
        {
            return iMantissa;
        }
        const mantissa_t& normalized_mantissa() const
        {
            normalize();
            return iMantissa;
        }
        mantissa_t& mantissa()
        {
            iNormalized = false;
            return iMantissa;
        }
    private:
        template <bool Add>
        self_type& add_subtract_algorithm_0(const self_type& aRhs)
        {
            auto args = as_arguments(*this, aRhs);
            auto& lhsMantissa = args.lhs.mantissa();
            auto& rhsMantissa = args.rhs.mantissa();
            auto const maxMagnitude = std::min(precision_magnitude(), std::max(lhsMantissa.magnitude(), rhsMantissa.magnitude()));

            if constexpr (!mantissa_t::FixedSize)
            {
                if (lhsMantissa.magnitude() < rhsMantissa.magnitude())
                    lhsMantissa.words().insert(lhsMantissa.words().begin(), rhsMantissa.magnitude() - lhsMantissa.magnitude(), {});
                else if (rhsMantissa.magnitude() < lhsMantissa.magnitude())
                    rhsMantissa.words().insert(rhsMantissa.words().begin(), lhsMantissa.magnitude() - rhsMantissa.magnitude(), {});
            }

            exponent_t exponentAdjust = Zero;

            // todo max precision for add op
            if (!rhsMantissa.is_zero() && (aRhs.exponent() > exponent() || lhsMantissa.is_zero()))
            {
                exponentAdjust = aRhs.exponent() - exponent();
                if (!lhsMantissa.is_zero())
                    rhsMantissa <<= exponentAdjust;
            }
            else if (exponent() > aRhs.exponent())
            {
                auto const adjust = exponent() - aRhs.exponent();
                lhsMantissa <<= adjust;
            }

            auto const lhsMagnitudeBefore = lhsMantissa.magnitude_in_bits();
            auto const rhsMagnitudeBefore = rhsMantissa.magnitude_in_bits();

            auto lhsSignedMantissa = lhsMantissa.to_signed(true);
            auto rhsSignedMantissa = rhsMantissa.to_signed(true);

            if (is_negative())
                negate(lhsSignedMantissa);
            if (aRhs.is_negative())
                negate(rhsSignedMantissa);

            signed_integer_type result;

            if constexpr (Add)
                result = lhsSignedMantissa + rhsSignedMantissa;
            else
                result = lhsSignedMantissa - rhsSignedMantissa;

            set_positive(result.is_positive());

            mantissa() = result.to_unsigned(true);

            auto const maxMagntitudeBefore = std::max(lhsMagnitudeBefore, rhsMagnitudeBefore);
            auto const lhsMagnitudeAfter = mantissa().magnitude_in_bits();
            if (lhsMagnitudeAfter > maxMagntitudeBefore)
                exponentAdjust += (lhsMagnitudeAfter - maxMagntitudeBefore);
            else if (lhsMagnitudeAfter < maxMagntitudeBefore)
                exponentAdjust -= (maxMagntitudeBefore - lhsMagnitudeAfter);

            word_t ignore;
            shift_mantissa(ignore);

            if constexpr (!mantissa_t::FixedSize)
                if (mantissa().magnitude() > maxMagnitude)
                {
                    auto const wordShift = mantissa().magnitude() - maxMagnitude;
                    mantissa().words().erase(mantissa().words().begin(), std::next(mantissa().words().begin(), wordShift));
                }

            exponent() += exponentAdjust;

            return *this;
        }
        self_type& multiply_algorithm_0(const self_type& aRhs)
        {
            static const self_type zero = Zero;

            if (is_zero())
                return *this;
            else if (aRhs.is_zero())
            {
                *this = zero;
                return *this;
            }
            
            auto args = as_arguments(*this, aRhs);
            auto& lhsExponent = args.lhs.exponent();
            auto& lhsMantissa = args.lhs.mantissa();
            auto const& rhsExponent = args.rhs.exponent();
            auto const& rhsMantissa = args.rhs.mantissa();

            lhsExponent += (rhsExponent + One);

            auto const maxMagnitude = std::min(precision_magnitude(), std::max(lhsMantissa.magnitude(), rhsMantissa.magnitude()));

            lhsMantissa *= rhsMantissa;

            if (mantissa().magnitude() > maxMagnitude)
            {
                auto const wordShift = mantissa().magnitude() - maxMagnitude;
                auto const bitShift = wordShift * architecture_t::WordBits;
                mantissa() >>= bitShift;
            }

            normalize();

            set_negative(is_positive() != aRhs.is_positive());

            return *this;
        }
        self_type& division_algorithm_0(const self_type& aRhs)
        {
            self_type ignore;
            return division_algorithm_0(aRhs, ignore);
        }
        self_type& division_algorithm_0(const self_type& aRhs, self_type& aCoefficient)
        {
            static self_type const zero = Zero;
            static self_type const one = One;
            static self_type const negativeOne = -one;

            auto const& n = *this;
            auto& result = *this;
            if (aRhs == zero)
            {
                if (n == zero)
                {
                    result = zero;
                    result.iSpecial = special::NaN;
                }
                else
                {
                    bool wasNegative = is_negative();
                    result = zero;
                    result.iSpecial = special::Infinity;
                    set_negative(wasNegative);
                }
                return *this;
            }
            else if (n == zero)
                return (result = zero);
            else if (aRhs == one || aRhs == negativeOne)
                return *this;
            else if (n == aRhs)
            {
                bool wasNegative = is_negative();
                result = one;
                if (wasNegative)                
                    result.set_negative();
                return result;
            }

            auto const desiredPrecision = static_cast<precision_t>(std::min(std::max(precision_magnitude(), aRhs.precision_magnitude()), std::max(n.mantissa().magnitude(), aRhs.mantissa().magnitude())));
            auto const desiredPrecisionBits = static_cast<precision_t>(desiredPrecision * architecture_t::WordBits);
            auto const coefficientPrecisionBits = static_cast<precision_t>(desiredPrecisionBits + architecture_t::WordBits);
            result.set_precision(desiredPrecisionBits);

            // todo: power of 2 division shortcuts

            // Newton\96Raphson... (TODO)

            auto& x = aCoefficient;
            if (x == zero)
            {
                auto d = aRhs;
                d.set_positive();
                d.exponent() = -1;
                d.set_precision_at_least(coefficientPrecisionBits);
                x.set_precision_at_least(coefficientPrecisionBits);
                static auto const c1 = frac<self_type, signed_integer_type>(48, 17);
                static auto const c2 = frac<self_type, signed_integer_type>(32, 17);
                self_type one = 1.0;
                one.set_precision_at_least(coefficientPrecisionBits);
                x = c1 - c2 * d;
                for (auto s = static_cast<word_t>(std::ceil(std::log2((coefficientPrecisionBits + 1.0) / std::log2(17.0)))); s > Zero; --s)
                    x = x + x * (one - d * x);
                x.truncate_mantissa(desiredPrecision);
            }

            result.exponent() -= (aRhs.exponent() + 1);
            result *= x;

            return result;
        }
        template <uint32_t Size2, integer_type Type2, bool Signalling2, std::size_t SmallBufferSize2>
        static self_type fraction_algorithm_0(const xinteger<Size2, Type2, Signalling2, SmallBufferSize2>& aLhs, const xinteger<Size2, Type2, Signalling2, SmallBufferSize2>& aRhs)
        {
            if constexpr (aLhs.IsSigned)
            {
                auto result = fraction_algorithm_1(aLhs.to_unsigned(true), aRhs.to_unsigned(true));
                return result;
            }
            else
                return fraction_algorithm_1(aLhs, aRhs);
        }
        template <uint32_t Size2, integer_type Type2, bool Signalling2, std::size_t SmallBufferSize2>
        static self_type fraction_algorithm_1(const xinteger<Size2, Type2, Signalling2, SmallBufferSize2>& aLhs, const xinteger<Size2, Type2, Signalling2, SmallBufferSize2>& aRhs)
        {
            static mantissa_t const zero = Zero;
            static mantissa_t const one = One;

            xinteger<Size2, Type2, Signalling2, SmallBufferSize2> r;
            auto qint = divide_algorithm_1(aLhs, aRhs, r);

            auto const d = aRhs;
            auto const n = aLhs - (qint * d);

            word_t cycleStart = Zero;
            word_t cycleEnd = Zero;

            mantissa_t m;
            bool hadFirstCycle = false;
            word_t count = Zero;
            while (r != zero)
            {
                m <<= 1;
                m += divide_algorithm_1(r << 1, d, r);
                if (r == n)
                {
                    if (hadFirstCycle)
                    {
                        cycleEnd = count;
                        break;
                    }
                    hadFirstCycle = true;
                    cycleStart = count + One;
                }
                ++count;
            }

            self_type result;
            word_t mantissaShift;
            result.integer_assign(m | (qint << (count + One)), mantissaShift);
            auto e = qint.magnitude_in_bits();
            result.exponent() = e - One;
            cycleStart += mantissaShift;
            cycleEnd += mantissaShift;
            result.mantissa().repetend() = repetend{ cycleStart, cycleEnd };
            return result;
        }
        template <typename Arg>
        static argument<Arg> as_argument(Arg&& aArg, std::optional<word_t> const& aMagnitiude = {})
        {
            argument<Arg> result{ std::forward<Arg>(aArg) };
            if (result.arg.mantissa().has_repetend())
                result.arg.mantissa() = result.arg.mantissa().unpack(aMagnitiude ? *aMagnitiude : result.arg.precision());
            result.arg.normalize();
            return result;
        }
        template <typename Lhs, typename Rhs>
        static arguments<Lhs, Rhs> as_arguments(Lhs&& aLhs, Rhs&& aRhs)
        {
            arguments<Lhs, Rhs> result{ std::forward<Lhs>(aLhs), std::forward<Rhs>(aRhs) };
            if (result.lhs.mantissa().has_repetend() && !result.rhs.mantissa().has_repetend())
                result.lhs.mantissa() = result.lhs.mantissa().unpack(result.rhs.precision());
            else if (!result.lhs.mantissa().has_repetend() && result.rhs.mantissa().has_repetend())
                result.rhs.mantissa() = result.rhs.mantissa().unpack(result.lhs.precision());
            else if (result.lhs.mantissa().has_repetend() && result.rhs.mantissa().has_repetend())
            {
                auto const magBits = std::max(
                    result.lhs.mantissa().repetend().cycleEnd - result.lhs.mantissa().repetend().cycleStart + One, 
                    result.rhs.mantissa().repetend().cycleEnd - result.rhs.mantissa().repetend().cycleStart + One);
                result.lhs.mantissa() = result.lhs.mantissa().unpack(magBits);
                result.rhs.mantissa() = result.rhs.mantissa().unpack(magBits);
            }
            result.lhs.normalize();
            result.rhs.normalize();
            return result;
        }
    private:
        mutable bool iSignBit = false;
        mutable special iSpecial = special::Normal;
        mutable std::optional<std::string> iSignal;
        mutable exponent_t iExponent = SZero;
        mutable mantissa_t iMantissa;
        mutable bool iNormalized = false;
        mutable std::optional<precision_t> iPrecision;
    };

    template <uint32_t Size, uint32_t MantissaSize, uint32_t ExponentSize, bool Signalling, std::size_t SmallBufferSize>
    inline xreal<Size, MantissaSize, ExponentSize, Signalling, SmallBufferSize> pow(
        const xreal<Size, MantissaSize, ExponentSize, Signalling, SmallBufferSize>& aBase, const xreal<Size, MantissaSize, ExponentSize, Signalling, SmallBufferSize>& aExponent)
    {
        xreal<Size, MantissaSize, ExponentSize, Signalling, SmallBufferSize> result;
        // todo
        return result;
    }

    template <typename RealType, typename IntegerType>
    inline RealType frac(const IntegerType& aLhs, const IntegerType& aRhs)
    {
        auto result = RealType::fraction_algorithm_0(aLhs, aRhs);
        if (xinteger_detail<IntegerType>::is_positive(aLhs) != xinteger_detail<IntegerType>::is_positive(aRhs))
            result.set_negative();
        return result;
    }

    template <uint32_t Size, uint32_t MantissaSize, uint32_t ExponentSize, bool Signalling, std::size_t SmallBufferSize, typename Char, typename CharTraits>
    inline std::basic_ostream<Char, CharTraits>& operator<<(std::basic_ostream<Char, CharTraits>& aOutput, const xreal<Size, MantissaSize, ExponentSize, Signalling, SmallBufferSize>& aReal)
    {
        if (aOutput.flags() & std::ios_base::dec)
            aOutput << aReal.to_string(10u, static_cast<uint32_t>(aOutput.precision()));
        else if (aOutput.flags() & std::ios_base::hex)
            aOutput << aReal.to_string(16u, static_cast<uint32_t>(aOutput.precision()));
        else if (aOutput.flags() & std::ios_base::oct)
            aOutput << aReal.to_string(8u, static_cast<uint32_t>(aOutput.precision()));
        else
            aOutput << aReal.to_string(10u, static_cast<uint32_t>(aOutput.precision()));
        return aOutput;
    }

    template <uint32_t Size, bool Signalling = true>
    using real = xreal<Size, Size, Size, Signalling>;
}
