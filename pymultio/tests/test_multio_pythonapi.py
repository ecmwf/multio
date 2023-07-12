import pytest

import multiopython

def test_initialisation():
    with pytest.raises(AttributeError):
        multiopython.Multio()