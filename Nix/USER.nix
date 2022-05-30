with builtins; rec {
  mkReservedUserModule = user@{ uid, name }:
    { config }: {
      config.users.users.${name} = user;
      imports = [ (mkReserveUserModule user) ];
    };
  mkReserveUserModule = { uid, name }:
    { config }: {
      assertions = [{
        assertion = all (user: user.uid != uid) (attrValues config.users.users);
        message = "Duplicate user! UID ${uid} is reserved for user ${name}.";
      }];
    };
}
