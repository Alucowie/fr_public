parentinc = include_directories([ '..', '../materials' ])

depack2 = custom_target('depack2.bin',
                        input : '../depack2.asm',
                        output : 'depack2.bin',
                        command : [nasm, '-f', 'bin',
                                   '-o', '@OUTPUT@',
                                   '@INPUT@'])

depacker = custom_target('depacker.bin',
                        input : '../depacker.asm',
                        output : 'depacker.bin',
                        command : [nasm, '-f', 'bin',
                                   '-o', '@OUTPUT@',
                                   '@INPUT@'])

kkrunchy = [ '../debuginfo.cpp',
             depack2,
             depacker,
             '../depacker.cpp',
             '../dis.cpp',
             '../exepacker.cpp',
             '../mapfile.cpp',
             '../packer.cpp',
             '../pdbfile.cpp']

shaders = [ '../shadercodegen.cpp',
            '../shadercompile.cpp']

music = [ '../_mp3.cpp',
          '../_ogg.cpp',
          '../_viruz2.cpp',
          viruz2a]

system = [ '_bsplines.cpp',
           '../_diskitem.cpp',
           '../_gui.cpp',
           '../_intmath.cpp',
           '../_lekktor.cpp',
           '../_loader.cpp',
           '../_start.cpp',
           '../_types.cpp',
           '../_util.cpp',
           '../_xsi.cpp']

base_data = custom_target('base_data.obj',
                        input : 'base_data.asm',
                        output : 'base_data.obj',
                        depends : [ depacker, depack2 ],
                        command : [nasm, '-f', 'elf',
                                   '-o', '@OUTPUT@',
                                   '@INPUT@',
                                   '-DDEPACKER=@DEPFILE0@',
                                   '-DDEPACK2=@DEPFILE1@'])
                        #additionalDependencies depacker.bin depack2.bin

baselib_src = [ base_data, 'base_oplist.cpp' ] + system + music + kkrunchy + shaders
baselib = static_library('base', baselib_src, include_directories : parentinc, cpp_args : '-D_LIB')
