
# Build on Ubuntu:

apt-get install libwine-dev mingw-w64-i686-dev g++-mingw-w64-i686

export WINEPATH=/usr/lib/gcc/i686-w64-mingw32/10-win32/

mkdir werkkzeug3_kkrieger/buildmingw32
cd werkkzeug3_kkrieger/buildmingw32
meson --cross ../../cross-i686-w64-mingw32 ..
ninja
wine player_intro/player_intro.exe ../data/intro.kx
