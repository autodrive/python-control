# Local unit tests
# TODO: replace with nose?

. ./.github/workflows/service_xvfb.sh
. ./.github/workflows/set_os_env.sh

echo "source activate test-environment ==============="
source activate test-environment

if [ $SLYCOT != "" ]; 
    then python -c "import slycot"; 
fi

coverage run setup.py test

# only run examples if Slycot is install
# set PYTHONPATH for examples
# pmw needed for examples/tfvis.py
# future is needed for Python 2, also for examples/tfvis.py
if [[ "$SLYCOT" != "" ]]; then
    export PYTHONPATH=$PWD;
    conda install -c conda-forge pmw future;
    (cd examples; bash run_examples.sh);
fi
