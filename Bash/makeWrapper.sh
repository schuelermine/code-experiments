pushd $out/bin
readarray -d '' Binaries < <(find . -type f -print0)
popd

pushd $out
readarray -d '' Others < <(find $in -maxdepth 1 ! -name bin -print0)
popd

for x in $Binaries
do
    mkdir -p $out/bin/$x
    makeWrapper $in/bin/$x $out/bin/$x $makeWrapperArgs
done

for x in $Others
do
    cp $in/$x $out/$x
