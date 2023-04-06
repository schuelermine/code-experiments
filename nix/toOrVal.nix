{ mkOptionType, optional, mergeDefinitions }:
with builtins; {
  valueOrFunctionTo = elemType:
    if elemType.name == "functionTo" then
      throw "elemType argument of valueOrFunctionTo cannot be functionTo"
    else {
      name = "valueOrFunctionTo";
      description =
        "a(n) ${elemType.name} or a function that returns a(n) ${elemType.name}";
      check = value: elemType.check value || isFunction value;
      merge = loc: defs: fnArgs:
        (mergeDefinitions (loc
          ++ optional (any (def: isFunction def.value) defs)
          "[possibly function body]") elemType (map (def: {
            inherit (def) file;
            value =
              if isFunction def.value then def.value fnArgs else def.value;
          }) defs)).mergedValue;
      substSubModules = m: valueOrFunctionTo (elemType.substSubModules m);
      inherit (elemType) getSubOptions getSubModules;
    };
}
