#!/bin/bash
/switch-llvm.sh $LLVM_VERSION
mkdir -p $SIMPLL_BUILD_DIR
cd $SIMPLL_BUILD_DIR
cmake $DIFFKEMP_DIR -GNinja -DCMAKE_BUILD_TYPE=Debug
ninja -j4
cd -
ln -sr $(which pip3) /usr/bin/pip
ln -sr $(which pip3) /usr/bin/pip3
python3 -m pip install -e $DIFFKEMP_DIR
