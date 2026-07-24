"""
sciantix testing suite
author: Elisa Cappellari

Detects whether the OpenCalphad (OC) coupling is usable for the current
build/checkout, so OC-dependent test groups (jog, the oxygen-potential
validation groups, MOX pO2 verification) can degrade gracefully instead of
crashing when OpenCalphad isn't built or a required .TDB database is missing.

Two independent failure modes are distinguished because they need different
messages: (a) OpenCalphad isn't linked into sciantix.x at all, vs. (b) OC is
linked but a specific .TDB database a case needs isn't present.
"""

import os
from dataclasses import dataclass, field
from typing import FrozenSet, Iterable

# testing/core/oc_status.py -> testing/core -> testing -> repo root
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
OC_ROOT = os.path.abspath(os.path.join(REPO_ROOT, "..", "opencalphad-for-sciantix"))
OC_DATA_DIR = os.path.join(OC_ROOT, "data")
CMAKE_CACHE = os.path.join(REPO_ROOT, "build", "CMakeCache.txt")
OC_LIBS = (
    os.path.join(OC_ROOT, "build", "liboctq-isoc.a"),
    os.path.join(OC_ROOT, "build", "liboctq-isoc-gb.a"),
)


def _detect_build_linked():
    """
    Returns (linked: bool, detail: str).

    Prefers build/CMakeCache.txt's COUPLING_OC:BOOL=ON|OFF (authoritative: it
    reflects the actual configured build, including an explicit
    -DCOUPLING_OC=OFF/ON override). Falls back to the same file-existence
    heuristic CMakeLists.txt itself uses to auto-detect COUPLING_OC when the
    build hasn't been configured yet.
    """
    if os.path.isfile(CMAKE_CACHE):
        with open(CMAKE_CACHE, "r") as f:
            for line in f:
                if line.startswith("COUPLING_OC:BOOL="):
                    value = line.strip().split("=", 1)[1]
                    linked = value.upper() == "ON"
                    return linked, f"build/CMakeCache.txt: COUPLING_OC={value}"

    if all(os.path.isfile(p) for p in OC_LIBS):
        return True, f"no configured build/ yet, but OC libraries found under {OC_ROOT}/build"

    return False, (
        f"no configured build/ and OC libraries not found under {OC_ROOT}/build "
        f"(build with 'Allmake.sh --oc' to enable OpenCalphad coupling)"
    )


@dataclass(frozen=True)
class OCStatus:
    build_linked: bool
    build_detail: str
    missing_databases: FrozenSet[str] = field(default_factory=frozenset)

    def available_for(self, databases: Iterable[str]) -> bool:
        """True if OC is linked and every named database is present."""
        if not self.build_linked:
            return False
        return not (set(databases) & self.missing_databases)

    def reason_for(self, databases: Iterable[str]) -> str:
        """A one-line human-readable explanation of why `databases` aren't usable."""
        if not self.build_linked:
            return f"OpenCalphad not linked into sciantix.x ({self.build_detail})"
        missing = sorted(set(databases) & self.missing_databases)
        if missing:
            return (
                f"OpenCalphad is linked, but missing database(s) {missing} "
                f"under {OC_DATA_DIR}"
            )
        return "OpenCalphad and required database(s) available"


def detect_oc_status(databases: Iterable[str]) -> OCStatus:
    """
    Detect OC build/database availability once per runner invocation.

    Args:
        databases: every .TDB filename any registered group might need
            (e.g. {"upuo-v21.TDB", "BaMoO_CsMoO_MoPdRhRuTc_merged.TDB"}).
    """
    linked, detail = _detect_build_linked()
    missing = frozenset(
        name for name in databases if not os.path.isfile(os.path.join(OC_DATA_DIR, name))
    )
    return OCStatus(build_linked=linked, build_detail=detail, missing_databases=missing)
