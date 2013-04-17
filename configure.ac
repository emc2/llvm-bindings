AC_INIT([Haskell LLVM Bindings], [3.2.1.0], [emc2@metricspace.net], [llvm-bindings])

AC_CONFIG_FILES([llvm-bindings.buildinfo])

AC_CONFIG_SRCDIR([LLVM/FFI/Core.hsc])

AC_PROG_CXX

AC_LANG(C++)

AC_ARG_WITH(compiler,
  [AC_HELP_STRING([--with-compiler],
    [use the given Haskell compiler])],
  compiler="$withval",
  compiler=ghc)

AC_ARG_WITH(llvm_prefix,
  [AC_HELP_STRING([--with-llvm-prefix],
    [use the version of LLVM at the given location])],
  llvm_prefix="$withval",
  llvm_prefix="$prefix")

AC_ARG_WITH(llvm_bindir,
  [AC_HELP_STRING([--with-llvm-bindir],
    [use the LLVM binaries at the given location])],
  llvm_prefix="$withval",
  llvm_prefix="$llvm_prefix/bin")

AC_PATH_PROGS(llvm_config, [llvm-config],
  [AC_MSG_ERROR(could not find llvm-config in $llvm_bindir)],
  ["$llvm_bindir:$PATH"])

llvm_systems="core codegen"

llvm_version="`$llvm_config --version`"
llvm_cppflags="`$llvm_config --cppflags`"
llvm_includedir="`$llvm_config --includedir`"
llvm_ldflags="`$llvm_config --ldflags`"
llvm_static_libs="`$llvm_config --libs $llvm_systems`"
llvm_ldoptions="$llvm_ldflags $llvm_static_libs"
llvm_extra_libdirs="`$llvm_config --libdir`"

CPPFLAGS="$llvm_cppflags $CPPFLAGS $TARGET_CPPFLAGS"
LDFLAGS="$llvm_ldflags $LDFLAGS $TARGET_LDFLAGS"

AC_CHECK_HEADERS([llvm-c/Core.h], [],
  [AC_MSG_ERROR(could not find LLVM C bindings)])

AC_CHECK_HEADERS([llvm/ADT/StringRef.h])
AC_CHECK_HEADERS([llvm/Support/DynamicLibrary.h], [], [],
[#ifdef HAVE_LLVM_ADT_STRINGREF_H
# include <llvm/ADT/StringRef.h>
#endif])

save_LIBS="$LIBS"
LIBS="-lLLVMSupport -lLLVMSystem -lpthread -ldl $LIBS"

llvm_num_version="`echo $llvm_version | tr . 0`"
AC_DEFINE_UNQUOTED([HS_LLVM_VERSION], [$llvm_num_version],
  [The version of LLVM, e.g. 209 for 2.9.])

AC_SUBST([llvm_version])
AC_SUBST([llvm_cppflags])
AC_SUBST([llvm_extra_libs])
AC_SUBST([llvm_extra_libdirs])
AC_SUBST([llvm_includedir])
AC_SUBST([llvm_ldoptions])

AC_OUTPUT
