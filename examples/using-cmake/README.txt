Example usage

Configure, build and install IPhreeqc

1. cd iphreeqc-3.6.2-15100
2. mkdir _build
3. cd _build
4. cmake -DCMAKE_INSTALL_PREFIX:PATH=c:/Users/charlton/iphreeqc ..
5. cmake --build . --config release
6. ctest .
6. cmake --build . --config release --target install

Build example:

1. cd c:\Users\charlton\iphreeqc\examples\using-cmake
2. mkdir _build
3. cmake -DCMAKE_PREFIX_PATH:PATH=c:/Users/charlton/iphreeqc/lib/cmake/IPhreeqc ..
4. cmake --build . --config release

