{ fish, makeWrapper }:
{ source, outputs ? source.outputs, makeWrapperArgs }:

with builtins;
derivation {
  name = source.name + "-wrapped";
  inherit (source) system;
  inherit outputs;

  builder = fish;
  args = [ (builtins.toFile script) ];
  # FUCKING HELL
  inherit makeWrapperArgs;
}
