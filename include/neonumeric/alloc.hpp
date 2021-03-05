// alloc.hpp - v1.0
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

#include <memory>

namespace neonumeric
{
    template <typename T, std::size_t SmallBufferSize = 8u>
    class basic_small_buffer_allocator;

    template <typename T, typename R>
    struct small_buffer_allocator_types
    {
        typedef T controlled_value_type;
        typedef R rebound_value_type;
    };

    template <typename T, std::size_t SmallBufferSize>
    struct small_buffer
    {
        typedef T value_type;
        typedef std::aligned_storage_t<sizeof(value_type)* SmallBufferSize> buffer_storage_t;
        buffer_storage_t storage;
        bool allocated;
        small_buffer() : allocated{ false } {}
        small_buffer(const small_buffer&) : allocated{ false } {}
        small_buffer& operator=(const small_buffer&) { return *this; }
    };

    template <typename T, typename R, std::size_t SmallBufferSize>
    class basic_small_buffer_allocator<small_buffer_allocator_types<T, R>, SmallBufferSize> : public std::allocator<R>
    {
        template <typename, std::size_t>
        friend class basic_small_buffer_allocator;
    public:
        struct no_small_buffer : std::logic_error { no_small_buffer() : std::logic_error("neolib::basic_small_buffer_allocator::no_small_buffer") {} };
    public:
        typedef small_buffer_allocator_types<T, R> types;
        typedef basic_small_buffer_allocator<types, SmallBufferSize> self_type;
        typedef std::false_type propagate_on_container_move_assignment;
        typedef std::false_type is_always_equal;
        template<class U> struct rebind { typedef basic_small_buffer_allocator<small_buffer_allocator_types<T, U>, SmallBufferSize> other; };
    public:
        typedef T controlled_value_type;
        typedef R value_type;
        typedef std::allocator<value_type> default_allocator_type;
        typedef typename std::allocator_traits<default_allocator_type>::pointer pointer;
        typedef small_buffer<controlled_value_type, SmallBufferSize> small_buffer_type;
    public:
        basic_small_buffer_allocator() :
            default_allocator_type{},
            iBuffer{ nullptr }
        {
        }
        basic_small_buffer_allocator(small_buffer_type& aBuffer) :
            default_allocator_type{},
            iBuffer{ &aBuffer }
        {
        }
        basic_small_buffer_allocator(const basic_small_buffer_allocator& aOther) :
            default_allocator_type{ aOther },
            iBuffer{ aOther.iBuffer }
        {
        }
        basic_small_buffer_allocator(basic_small_buffer_allocator&& aOther) :
            default_allocator_type(std::move(aOther)),
            iBuffer{ nullptr }
        {
        }
        template <typename U>
        basic_small_buffer_allocator(const basic_small_buffer_allocator<U, SmallBufferSize>& aOther) :
            default_allocator_type{ aOther },
            iBuffer{ nullptr }
        {
        }
        template <typename U>
        basic_small_buffer_allocator(const basic_small_buffer_allocator<U, SmallBufferSize>&& aOther) :
            default_allocator_type(std::move(aOther)),
            iBuffer{ nullptr }
        {
        }
    public:
        bool operator==(const basic_small_buffer_allocator& aOther) const
        {
            return false;
        }
        bool operator!=(const basic_small_buffer_allocator& aOther) const
        {
            return true;
        }
    public:
        basic_small_buffer_allocator& operator=(const basic_small_buffer_allocator& aOther)
        {
            iBuffer = aOther.iBuffer;
            return *this;
        }
        basic_small_buffer_allocator& operator=(basic_small_buffer_allocator&& aOther)
        {
            iBuffer = nullptr;
            return *this;
        }
        template <typename U>
        basic_small_buffer_allocator& operator=(const basic_small_buffer_allocator<U, SmallBufferSize>& aOther)
        {
            iBuffer = nullptr;
            return *this;
        }
        template <typename U>
        basic_small_buffer_allocator& operator=(basic_small_buffer_allocator<U, SmallBufferSize>&& aOther)
        {
            iBuffer = nullptr;
            return *this;
        }
    public:
        pointer allocate(std::size_t n)
        {
            return allocate(n, nullptr);
        }
        pointer allocate(std::size_t n, const void*)
        {
            if constexpr (std::is_same_v<value_type, controlled_value_type>)
            {
                if (n <= SmallBufferSize && is_buffer_available())
                {
                    buffer().allocated = true;
                    return reinterpret_cast<pointer>(&buffer().storage);
                }
                else
                    return default_allocator_type::allocate(n);
            }
            else
                return default_allocator_type::allocate(n);
        }
        void deallocate(pointer p, std::size_t n)
        {
            if constexpr (std::is_same_v<value_type, controlled_value_type>)
            {
                if (is_buffer_used() && p == reinterpret_cast<pointer>(&buffer().storage))
                    buffer().allocated = false;
                else
                    default_allocator_type::deallocate(p, n);
            }
            else
                default_allocator_type::deallocate(p, n);
        }
    public:
        bool has_buffer() const
        {
            return iBuffer != nullptr;
        }
        bool is_buffer_available() const
        {
            return has_buffer() && !buffer().allocated;
        }
        bool is_buffer_used() const
        {
            return has_buffer() && buffer().allocated;
        }
        const small_buffer_type& buffer() const
        {
            if (has_buffer())
                return *iBuffer;
            throw no_small_buffer();
        }
        small_buffer_type& buffer()
        {
            return const_cast<small_buffer_type&>(const_cast<const basic_small_buffer_allocator*>(this)->buffer());
        }
    private:
        small_buffer_type* iBuffer;
    };

    template <typename T, typename U, std::size_t SmallBufferSize>
    inline bool operator==(const basic_small_buffer_allocator<T, SmallBufferSize>&, const basic_small_buffer_allocator<U, SmallBufferSize>&)
    {
        return false;
    }

    template <typename T, typename U, std::size_t SmallBufferSize>
    inline bool operator!=(const basic_small_buffer_allocator<T, SmallBufferSize>&, const basic_small_buffer_allocator<U, SmallBufferSize>&)
    {
        return true;
    }

    template <typename T, std::size_t SmallBufferSize = 8u>
    using small_buffer_allocator = basic_small_buffer_allocator<small_buffer_allocator_types<T, T>, SmallBufferSize>;
}
