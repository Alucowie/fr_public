parentinc = include_directories('..', '../materials')

generators = [
  '../genbitmap.cpp',
  '../genblobspline.cpp',
  '../geneffect.cpp',
  '../geneffectdebris.cpp',
  '../geneffectex.cpp',
  '../geneffectipp.cpp',
  '../genmaterial.cpp',
  '../genmesh.cpp',
  '../genminmesh.cpp',
  '../genoverlay.cpp',
  '../genscene.cpp']

materials = [
  '../materials/material11.cpp',
  '../materials/material20.cpp',
  '../materials/materialdirect.cpp',
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
  'intro_oplist.cpp',
  '../mainplayer.cpp',
  effect_jpegps_hpp,
  effect_jpegvs_hpp] + generators + materials + music + system

playerintro = executable('player_intro', src, include_directories : parentinc, cpp_pch : '../player_intro/intro_config.hpp', cpp_args : ['-fpermissive'], link_args : cpp_winlibs, win_subsystem : 'windows')
