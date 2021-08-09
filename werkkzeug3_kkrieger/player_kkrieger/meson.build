parentinc = include_directories('..', '../materials')

generators = [
  '../genbitmap.cpp',
  '../genblobspline.cpp',
  '../geneffect.cpp',
  '../genmaterial.cpp',
  '../genmesh.cpp',
  '../genoverlay.cpp',
  '../genscene.cpp']

materials = [
  '../materials/material11.cpp',
  '../materials/material20.cpp',
  '../materials/rtmanager.cpp',
  '../shadercodegen.cpp']

music = [
  '../_viruz2.cpp',
  viruz2a]

system = [
  '../_lekktor.cpp',
  '../_start.cpp',
  '../_types.cpp']

src = [
  '../engine.cpp',
  '../kdoc.cpp',
  'kkrieger_oplist.cpp',
  '../kkriegergame.cpp',
  '../mainplayer.cpp'] + generators + materials + music + system
playerkkrieger = executable('player_kkrieger', src, include_directories : parentinc, cpp_pch : '../player_kkrieger/kkrieger_config.hpp', cpp_args : ['-DKKRIEGER', '-fpermissive'])
