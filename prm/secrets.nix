{
  "secrets.age".publicKeys = [
    (builtins.readFile ./developer.pub)
    (builtins.readFile ./template.pub)
  ];
}
