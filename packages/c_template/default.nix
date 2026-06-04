{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation rec {
  buildPhase = ''
    cc -o ${pname} main.c -std=c89 \
    -O3 \
    -Waggregate-return \
    -Waggressive-loop-optimizations \
    -Wall \
    -Walloc-zero \
    -Walloca \
    -Warith-conversion \
    -Warray-bounds=2 \
    -Wattribute-alias \
    -Wattributes \
    -Wbad-function-cast \
    -Wbidi-chars=any \
    -Wbuiltin-declaration-mismatch \
    -Wbuiltin-macro-redefined \
    -Wc90-c99-compat \
    -Wc99-c11-compat \
    -Wcast-align \
    -Wcast-align=strict \
    -Wcast-qual \
    -Wconversion \
    -Wcoverage-mismatch \
    -Wcpp \
    -Wdate-time \
    -Wdeclaration-after-statement \
    -Wdeprecated \
    -Wdeprecated-declarations \
    -Wdesignated-init \
    -Wdisabled-optimization \
    -Wdiscarded-array-qualifiers \
    -Wdiscarded-qualifiers \
    -Wdiv-by-zero \
    -Wdouble-promotion \
    -Wduplicated-branches \
    -Wduplicated-cond \
    -Werror \
    -Wextra \
    -Wfloat-equal \
    -Wformat=2 \
    -Wformat-overflow=2 \
    -Wformat-signedness \
    -Wformat-truncation=2 \
    -Wfree-nonheap-object \
    -Whsa \
    -Wif-not-aligned \
    -Wignored-attributes \
    -Wimport \
    -Wincompatible-pointer-types \
    -Winline \
    -Wint-conversion \
    -Wint-to-pointer-cast \
    -Winvalid-memory-model \
    -Winvalid-pch \
    -Wjump-misses-init \
    -Wlogical-op \
    -Wlto-type-mismatch \
    -Wmissing-declarations \
    -Wmissing-include-dirs \
    -Wmissing-prototypes \
    -Wmultichar \
    -Wnested-externs \
    -Wnull-dereference \
    -Wodr \
    -Wold-style-definition \
    -Woverflow \
    -Woverride-init-side-effects \
    -Wpacked \
    -Wpacked-bitfield-compat \
    -Wpedantic \
    -Wpointer-compare \
    -Wpointer-to-int-cast \
    -Wpragmas \
    -Wreturn-local-addr \
    -Wscalar-storage-order \
    -Wshadow \
    -Wshift-count-negative \
    -Wshift-count-overflow \
    -Wshift-negative-value \
    -Wsizeof-array-argument \
    -Wstack-protector \
    -Wstrict-aliasing \
    -Wstrict-overflow \
    -Wstrict-prototypes \
    -Wstringop-overflow=4 \
    -Wsuggest-attribute=const \
    -Wsuggest-attribute=format \
    -Wsuggest-attribute=malloc \
    -Wsuggest-attribute=noreturn \
    -Wsuggest-attribute=pure \
    -Wsuggest-attribute=returns_nonnull \
    -Wsuggest-final-methods \
    -Wsuggest-final-types \
    -Wswitch-bool \
    -Wswitch-default \
    -Wswitch-enum \
    -Wswitch-unreachable \
    -Wsync-nand \
    -Wtraditional-conversion \
    -Wtrampolines \
    -Wundef \
    -Wunreachable-code \
    -Wunsafe-loop-optimizations \
    -Wunsuffixed-float-constants \
    -Wunused-macros \
    -Wunused-result \
    -Wvarargs \
    -Wvector-operation-performance \
    -Wvla \
    -Wwrite-strings \
    -fanalyzer \
    -fstrict-flex-arrays=3 \
    -fstack-protector-strong \
    -fstack-clash-protection \
    -D_FORTIFY_SOURCE=3 \
    -Wl,-z,relro,-z,now \
    -Wl,-z,noexecstack
  '';
  checkPhase = ''
    clang-tidy main.c -- -std=c89 -I${pkgs.stdenv.cc.libc.dev}/include -I${pkgs.lib.getDev pkgs.stdenv.cc.cc}/include
    cppcheck --enable=all --error-exitcode=1 --inconclusive --force --std=c89 --suppress=missingIncludeSystem .
    ./${pname}
  '';
  doCheck = pkgs.stdenv.isLinux;
  installPhase = ''
    install -Dm755 ${pname} $out/bin/${pname}
  '';
  meta.mainProgram = pname;
  nativeCheckInputs = [
    pkgs.clang-tools
    pkgs.cppcheck
  ];
  pname = baseNameOf ./.;
  src = ./.;
  strictDeps = true;
  version = "0.0.0";
}
