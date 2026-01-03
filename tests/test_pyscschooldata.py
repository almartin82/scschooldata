"""
Tests for pyscschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pyscschooldata
    assert pyscschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pyscschooldata
    assert hasattr(pyscschooldata, 'fetch_enr')
    assert callable(pyscschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pyscschooldata
    assert hasattr(pyscschooldata, 'get_available_years')
    assert callable(pyscschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pyscschooldata
    assert hasattr(pyscschooldata, '__version__')
    assert isinstance(pyscschooldata.__version__, str)
