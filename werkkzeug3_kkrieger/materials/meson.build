parentinc = include_directories('..')

material11ps_hpp = custom_target(
  'material11ps.hpp',
  output : 'material11ps.hpp',
  input : 'material11.psh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material11vs_hpp = custom_target(
  'material11vs.hpp',
  output : 'material11vs.hpp',
  input : 'material11.vsh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material11 = ['material11.cpp', material11ps_hpp, material11vs_hpp]

material20_envips_hpp = custom_target(
  'material20_envips.hpp',
  output : 'material20_envips.hpp',
  input : 'material20_envi.psh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_envivs_hpp = custom_target(
  'material20_envivs.hpp',
  output : 'material20_envivs.hpp',
  input : 'material20_envi.vsh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_fatps_hpp = custom_target(
  'material20_fatps.hpp',
  output : 'material20_fatps.hpp',
  input : 'material20_fat.psh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_fatvs_hpp = custom_target(
  'material20_fatvs.hpp',
  output : 'material20_fatvs.hpp',
  input : 'material20_fat.vsh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_lightps_hpp = custom_target(
  'material20_lightps.hpp',
  output : 'material20_lightps.hpp',
  input : 'material20_light.psh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_lightvs_hpp = custom_target(
  'material20_lightvs.hpp',
  output : 'material20_lightvs.hpp',
  input : 'material20_light.vsh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_texps_hpp = custom_target(
  'material20_texps.hpp',
  output : 'material20_texps.hpp',
  input : 'material20_tex.psh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_texvs_hpp = custom_target(
  'material20_texvs.hpp',
  output : 'material20_texvs.hpp',
  input : 'material20_tex.vsh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_vcolorps_hpp = custom_target(
  'material20_vcolorps.hpp',
  output : 'material20_vcolorps.hpp',
  input : 'material20_vcolor.psh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_vcolorvs_hpp = custom_target(
  'material20_vcolorvs.hpp',
  output : 'material20_vcolorvs.hpp',
  input : 'material20_vcolor.vsh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_zfillps_hpp = custom_target(
  'material20_zfillps.hpp',
  output : 'material20_zfillps.hpp',
  input : 'material20_zfill.psh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20_zfillvs_hpp = custom_target(
  'material20_zfillvs.hpp',
  output : 'material20_zfillvs.hpp',
  input : 'material20_zfill.vsh',
  command : [shadercompile, '@INPUT@', '--out-header', '@OUTPUT@'],
  )

material20 = ['material20.cpp',
              material20_envips_hpp,
              material20_envivs_hpp,
              material20_fatps_hpp,
              material20_fatvs_hpp,
              material20_lightps_hpp,
              material20_lightvs_hpp,
              material20_texps_hpp,
              material20_texvs_hpp,
              material20_vcolorps_hpp,
              material20_vcolorvs_hpp,
              material20_zfillps_hpp,
              material20_zfillvs_hpp]

materiallib_src = material11 + material20 + ['materialdirect.cpp', 'rtmanager.cpp']

materiallib = static_library('materials', materiallib_src, include_directories : parentinc, cpp_args : '-D_LIB')
