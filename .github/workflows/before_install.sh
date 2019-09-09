. ./.github/workflows/set_os_env.sh
# Install gfortran for testing slycot; use apt-get instead of conda in
# order to include the proper CXXABI dependency (updated in GCC 4.9)
if [[ "$SLYCOT" != "" ]]; then
    sudo apt-get update -qq;
    sudo apt-get install gfortran;
fi

# use miniconda to install numpy/scipy, to avoid lengthy build from source
if [[ $RUNNER_OS ==  "macOS" ]]; then
    . ./.github/workflows/wget_install_miniconda.sh
fi

hash -r
conda config --set always_yes yes --set changeps1 no
conda update -q conda
conda config --add channels python-control
conda info -a
conda create -q -n test-environment python="$PYTHON" pip coverage
source activate test-environment
# Install openblas if slycot is being used
# also install scikit-build for the build process
if [[ "$SLYCOT" != "" ]]; then
    conda install openblas;
    conda install -c conda-forge scikit-build;
fi
