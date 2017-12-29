
add_languages('cpp')
cpp = meson.get_compiler('cpp')
gmp_lib   = cpp.find_library('gmp', required : false)
mpfr_lib  = cpp.find_library('mpfr', required : false)

iRRAM_dep = disabler()
if gmp_lib.found() and mpfr_lib.found()
  iRRAM = get_option('iRRAM')
  if iRRAM != ''
    iRRAM_libdir = join_paths(iRRAM, 'lib')
    iRRAM_incdir = join_paths(iRRAM, 'include')
    iRRAM_lib = cpp.find_library('iRRAM', dirs : iRRAM_libdir)
  else
    iRRAM_lib = cpp.find_library('iRRAM', required : false)
  endif

  if iRRAM_lib.found()
    iRRAM_inc = include_directories(get_variable('iRRAM_incdir', ''))
    iRRAM_dep = declare_dependency( dependencies : [ iRRAM_lib, mpfr_lib, gmp_lib ]
                                  , include_directories : iRRAM_inc
                                  )
  endif
endif

examples = [ 'abs'
           , 'if'
           , 'if2'
           , 'sqrt'
           , 'logmap'
           ]

if iRRAM_dep.found()
  cclirram_stdlib = library('cclstd-iRRAM', files(join_paths('..', 'std-irram.cc'))
                           , dependencies: iRRAM_dep
                           , build_rpath : get_variable('iRRAM_libdir', '')
                           )
  foreach tgt : examples
    executable(tgt, ccl.process(tgt + '.t17')
              , link_with : cclirram_stdlib
              , dependencies : iRRAM_dep
              , build_rpath : get_variable('iRRAM_libdir', '')
              )
  endforeach
else
  warning('iRRAM-library not found, examples won\'t be built; use "meson configure" to set the "iRRAM" option')
endif
