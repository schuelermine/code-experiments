with builtins; rec {
dispType = val: {
  "int" = "an integer";
  "bool" = "a Boolean";
  "string" = "a string";
  "path" = "a path";
  "null" = "null";
  "set" = "a set";
  "list" = "a list";
  "lambda" = "a function";
  "float" = "a float";
}.${typeOf val};
nav = dir: set: if !isList dir then
  throw "path is [95m${dispType dir}(B[m while a list was expected"
else if length dir == 0 then
  set
else
  nav (tail dir) set;
}
