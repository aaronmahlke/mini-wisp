# build for arm
as -o test.o test.s
ld test.o -o test -l System -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _main -arch arm64
