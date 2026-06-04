{
  inputs,
  pkgs,
  ...
}:
pkgs.testers.runNixOSTest rec {
  name = builtins.baseNameOf ./.;
  nodes.machine =
    {
      pkgs,
      ...
    }:
    let
      cTemplateForCheck = inputs.self.packages.${pkgs.stdenv.system}.${name}.overrideAttrs (_: {
        NIX_CFLAGS_COMPILE = "-O1 -g3 -fno-omit-frame-pointer -fno-sanitize-recover=all -fsanitize=address,undefined";
      });
    in
    {
      environment = {
        systemPackages = [
          cTemplateForCheck
        ];
        variables = {
          ASAN_OPTIONS = "abort_on_error=1:strict_string_checks=1:detect_stack_use_after_return=1:check_printf=1:allocator_may_return_null=0";
          UBSAN_OPTIONS = "halt_on_error=1:print_stacktrace=1:report_error_type=1";
        };
      };
    };
  testScript = ''
    machine.succeed("${name}")
  '';
}
