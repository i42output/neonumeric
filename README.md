# neonumeric
Free C++ library for arbitrary precision arithmetic (permissive software license).

Features:

* Arbitrary precision integers, floats and rationals.
* Numbers can be any fixed precision (specifiable as a template parameter) or can dynamically grow in precision based on the precision of operands.
* Header only with no dependencies on external libraries.

# Building
neonumeric is header only: add its `include` folder to your include path, or use its CMake build (3.15 or later), which installs the headers and a CMake package (`find_package(neonumeric)`, target `neonumeric::neonumeric`) and builds the unit tests:

```
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
ctest --test-dir build
cmake --install build
```

