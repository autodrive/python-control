. ./.github/workflows/set_os_env.sh
source activate test-environment

# Install packages needed by python-control
conda install $SCIPY matplotlib

# Build slycot from source
# For python 3, need to provide pointer to python library
# Use "Unix Makefiles" as generator, because Ninja cannot handle Fortran
#! git clone https://github.com/repagh/Slycot.git slycot;
if [[ "$SLYCOT" != "" ]]; then
    git clone https://github.com/python-control/Slycot.git slycot;

    cd slycot; python setup.py install -G "Unix Makefiles"; cd ..;
fi
