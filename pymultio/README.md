### Multio Python Interface

If multio is installed you must set MULTIO_DIR and LD_LIBRARY_PATH to where the shared objects are located.

export LD_LIBRARY_PATH=<Location of installed multio objects>

Also need to set multio server file 

export MULTIO_SERVER_CONFIG_FILE=<Location of plan> 

Then run """pip install -e ."""

Now you should be able to import multiopython and use its functionality.