"""
sciantix testing suite
author: Elisa Cappellari

Detects whether the Thermochimica (TC) coupling is usable for the current
build/checkout, so Thermochimica-dependent test groups (volatileFPs) can be skipped
gracefully instead of crashing when Thermochimica isn't built or a required
.dat database is missing. Mirrors testing/core/oc_status.py.

Two independent failure modes are distinguished because they need different
messages: (a) Thermochimica isn't linked into sciantix.x at all, vs. (b) it is
linked but a specific .dat database a case needs isn't present.
"""

import os
from dataclasses import dataclass, field
from typing import FrozenSet, Iterable

# testing/core/tc_status.py -> testing/core -> testing -> repo root
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
TC_ROOT = os.path.abspath(os.path.join(REPO_ROOT, "..", "thermochimica-for-sciantix"))
TC_DATA_DIR = os.path.join(TC_ROOT, "data")
# SCIANTIX_BUILD_DIR (set by CTest) points to the active build tree; default is build/
BUILD_DIR = os.path.abspath(os.environ.get("SCIANTIX_BUILD_DIR") or os.path.join(REPO_ROOT, "build"))
CMAKE_CACHE = os.path.join(BUILD_DIR, "CMakeCache.txt")
TC_LIB = os.path.join(TC_ROOT, "build", "libthermochimica.a")


def _detect_build_linked():
    """
    Returns (linked: bool, detail: str).

    Prefers build/CMakeCache.txt's COUPLING_THERMOCHIMICA:BOOL=ON|OFF
    (authoritative: it reflects the actual configured build, including an
    explicit -DCOUPLING_THERMOCHIMICA=OFF/ON override). Falls back to the same
    file-existence heuristic CMakeLists.txt itself uses to auto-detect
    COUPLING_THERMOCHIMICA when the build hasn't been configured yet.
    """
    if os.path.isfile(CMAKE_CACHE):
        with open(CMAKE_CACHE, "r") as f:
            for line in f:
                if line.startswith("COUPLING_THERMOCHIMICA:BOOL="):
                    value = line.strip().split("=", 1)[1]
                    linked = value.upper() == "ON"
                    return linked, f"build/CMakeCache.txt: COUPLING_THERMOCHIMICA={value}"

    if os.path.isfile(TC_LIB):
        return True, f"no configured build/ yet, but {TC_LIB} found"

    return False, (
        f"no configured build/ and {TC_LIB} not found "
        f"(build with 'Allmake.sh --tc' to enable the Thermochimica coupling)"
    )


@dataclass(frozen=True)
class TCStatus:
    build_linked: bool
    build_detail: str
    missing_databases: FrozenSet[str] = field(default_factory=frozenset)

    def available_for(self, databases: Iterable[str]) -> bool:
        """True if Thermochimica is linked and every named database is present."""
        if not self.build_linked:
            return False
        return not (set(databases) & self.missing_databases)

    def reason_for(self, databases: Iterable[str]) -> str:
        """A one-line human-readable explanation of why `databases` aren't usable."""
        if not self.build_linked:
            return f"Thermochimica not linked into sciantix.x ({self.build_detail})"
        missing = sorted(set(databases) & self.missing_databases)
        if missing:
            return (
                f"Thermochimica is linked, but missing database(s) {missing} "
                f"under {TC_DATA_DIR}"
            )
        return "Thermochimica and required database(s) available"


def detect_tc_status(databases: Iterable[str]) -> TCStatus:
    """
    Detect Thermochimica build/database availability once per runner invocation.

    Args:
        databases: every .dat filename any registered group might need
            (e.g. {"CsITe_Clean.dat"}).
    """
    linked, detail = _detect_build_linked()
    missing = frozenset(
        name for name in databases if not os.path.isfile(os.path.join(TC_DATA_DIR, name))
    )
    return TCStatus(build_linked=linked, build_detail=detail, missing_databases=missing)
