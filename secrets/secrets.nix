{
  "secrets.age".publicKeys = [
    (builtins.readFile ../prm/developer.pub)
    (builtins.readFile ../prm/template.pub)
  ];
}
