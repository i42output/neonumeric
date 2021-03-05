// neonumeric.hpp
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

#include <cstdint>
#include <climits>

namespace neonumeric
{
    struct sfinae {};

    class scoped_flag
    {
    public:
        scoped_flag(bool& aFlag, bool aSet = true) : iFlag{ aFlag }, iPreviousValue{ aFlag }
        {
            iFlag = aSet;
        }
        ~scoped_flag()
        {
            iFlag = iPreviousValue;
        }
    private:
        bool& iFlag;
        bool iPreviousValue;
    };

    struct architecture_64 
    { 
        typedef uint64_t word_t;
        typedef int64_t signed_word_t;
        static constexpr word_t WordBits = sizeof(word_t) * CHAR_BIT;
        static constexpr word_t HalfWordBits = WordBits / 2u;
        static constexpr word_t QuarterWordBits = HalfWordBits / 2u;
        static constexpr word_t LowerHalfWordMask = ~word_t{} >> HalfWordBits;
        static constexpr word_t UpperHalfWordMask = LowerHalfWordMask << HalfWordBits;
        static constexpr word_t LowerQuarterWordMask = LowerHalfWordMask >> QuarterWordBits;
        static constexpr word_t UpperQuarterWordMask = LowerQuarterWordMask << QuarterWordBits;
        static constexpr word_t Zero = static_cast<word_t>(0u);
        static constexpr word_t One = static_cast<word_t>(1u);
        static constexpr word_t Two = One << One;
        static constexpr signed_word_t SZero = static_cast<signed_word_t>(0);
        static constexpr signed_word_t SOne = static_cast<signed_word_t>(1);
        static constexpr signed_word_t STwo = SOne << SOne;
        static constexpr word_t LowBit = static_cast<word_t>(1u);
        static constexpr word_t HighBit = static_cast<word_t>(1u) << (WordBits - 1u);
        static constexpr word_t NybbleBits = (Two << One);
        static constexpr word_t Nybbles = WordBits / NybbleBits;
        static constexpr word_t SignBit = HighBit;

        static constexpr word_t low_half(word_t value) { return value & LowerHalfWordMask; }
        static constexpr word_t high_half(word_t value) { return value >> HalfWordBits; }
        static constexpr word_t to_word(word_t lower, word_t upper) { return (upper << HalfWordBits) | (lower & LowerHalfWordMask); }
    };

    struct architecture_32 
    { 
        typedef uint32_t word_t;
        typedef int32_t signed_word_t;
        static constexpr word_t WordBits = sizeof(word_t) * CHAR_BIT;
        static constexpr word_t HalfWordBits = WordBits / 2u;
        static constexpr word_t QuarterWordBits = HalfWordBits / 2u;
        static constexpr word_t LowerHalfWordMask = ~word_t{} >> HalfWordBits;
        static constexpr word_t UpperHalfWordMask = LowerHalfWordMask << HalfWordBits;
        static constexpr word_t LowerQuarterWordMask = LowerHalfWordMask >> QuarterWordBits;
        static constexpr word_t UpperQuarterWordMask = LowerQuarterWordMask << QuarterWordBits;
        static constexpr word_t Zero = static_cast<word_t>(0u);
        static constexpr word_t One = static_cast<word_t>(1u);
        static constexpr word_t Two = One << One;
        static constexpr signed_word_t SZero = static_cast<signed_word_t>(0);
        static constexpr signed_word_t SOne = static_cast<signed_word_t>(1);
        static constexpr signed_word_t STwo = SOne << SOne;
        static constexpr word_t LowBit = static_cast<word_t>(1u);
        static constexpr word_t HighBit = static_cast<word_t>(1u) << (WordBits - 1u);
        static constexpr word_t NybbleBits = (Two << One);
        static constexpr word_t Nybbles = WordBits / NybbleBits;
        static constexpr word_t SignBit = HighBit;

        static constexpr word_t low_half(word_t value) { return value & LowerHalfWordMask; }
        static constexpr word_t high_half(word_t value) { return value >> HalfWordBits; }
        static constexpr word_t to_word(word_t lower, word_t upper) { return (upper << HalfWordBits) | (lower & LowerHalfWordMask); }
    };

#if _WIN32 || _WIN64
#if _WIN64
    typedef architecture_64 architecture_t;
#else
    typedef architecture_32 architecture_t;
#endif
#endif

    // Check GCC
#if __GNUC__
#if __x86_64__ || __ppc64__
    typedef architecture_64 architecture_t;
#else
    typedef architecture_32 architecture_t;
#endif
#endif

    enum class integer_type
    {
        Signed,
        Unsigned
    };

    struct repetend
    {
        typedef architecture_t::word_t word_t;
        word_t cycleStart;
        word_t cycleEnd;
    };
}