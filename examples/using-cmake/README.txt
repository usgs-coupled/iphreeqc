Example usage

--------------------------------------------------------------------------------------
Windows
--------------------------------------------------------------------------------------
Configure, build and install IPhreeqc
1. cd iphreeqc-@VERSION@-@REVISION@
2. mkdir _build
3. cd _build
4. cmake -DCMAKE_INSTALL_PREFIX:PATH=c:/Users/charlton/iphreeqc ..
5. cmake --build . --config release
6. ctest .
7. cmake --build . --config release --target install

Build example:
1. cd c:\Users\charlton\iphreeqc\examples\using-cmake
2. mkdir _build
3. cd _build
4. cmake -DCMAKE_PREFIX_PATH:PATH=c:/Users/charlton/iphreeqc ..
5. cmake --build . --config release

--------------------------------------------------------------------------------------
Linux/macOS
--------------------------------------------------------------------------------------
Configure, build and install IPhreeqc
1. cd iphreeqc-@VERSION@-@REVISION@
2. mkdir _build
3. cd _build
4. cmake -DCMAKE_INSTALL_PREFIX:PATH=/home/charlton/iphreeqc ..
5. cmake --build .
6. ctest .
7. cmake --build . --target install

Build example:
1. cd /home/charlton/iphreeqc/share/doc/IPhreeqc/examples/using-cmake
2. mkdir _build
3. cd _build
4. cmake -DCMAKE_PREFIX_PATH:PATH=/home/charlton/iphreeqc ..
5. cmake --build .
