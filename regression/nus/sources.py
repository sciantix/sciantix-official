"""
sciantix regression suite
author: Giovanni Zullo

Reading and evaluation of the piecewise-linear radial source profiles used by the
non-uniform-source (NUS) spectral diffusion solver.

The file format, shared by non_uniform_source.txt and initial_distribution.txt, is one
record per line:

    time # r0 r1 ... rn # A1 ... An # B1 ... Bn

with the domain normalised to the grain radius (r/a, so r0 = 0 and rn = 1) and carrying
one value more than the slopes and intercepts. Region i spans [r_i, r_i+1] and holds

    S(r) = A_i * r + B_i

with r in metres, matching Solver::SourceProjection_i on the C++ side.
"""

import numpy as np

# initial_distribution.txt carries one record per spectral mode block rather than per
# instant, in the order Initialization.C projects them. Naming them makes an initial
# condition readable; the time field of those records is unused.
MODE_BLOCKS = [
    "Xe in grain",
    "Xe in solution",
    "Xe in bubbles",
    "Kr in grain",
    "Kr in solution",
    "Kr in bubbles",
    "He in grain",
    "He in solution",
    "He in bubbles",
    "Xe133 in grain",
    "Xe133 in solution",
    "Xe133 in bubbles",
    "Kr85m in grain",
    "Kr85m in solution",
    "Kr85m in bubbles",
    "Xe in HBS",
    "Xe in HBS solution",
    "Xe in HBS bubbles",
]


class Source:
    """One radial source profile, at one instant."""

    def __init__(self, time, domain, slopes, intercepts):
        self.time = time
        self.domain = np.asarray(domain, dtype=float)
        self.slopes = np.asarray(slopes, dtype=float)
        self.intercepts = np.asarray(intercepts, dtype=float)

    @property
    def n_regions(self):
        return self.slopes.size

    @property
    def is_null(self):
        """True when the profile is identically zero, i.e. carries no information."""
        return not (np.any(self.slopes) or np.any(self.intercepts))

    def volume_average(self, a):
        """
        Volume average over the grain, the quantity the solver uses as the effective
        uniform rate. Mirrors Source_Volume_Average() in SourceHandler.C.
        """
        if self.n_regions < 1 or a <= 0.0:
            return 0.0

        r = self.domain * a
        integral = 0.0
        volume = 0.0
        for i in range(self.n_regions):
            r0, r1 = r[i], r[i + 1]
            A, B = self.slopes[i], self.intercepts[i]
            integral += A * (r1**4 - r0**4) / 4.0 + B * (r1**3 - r0**3) / 3.0
            volume += (r1**3 - r0**3) / 3.0

        return integral / volume if volume != 0.0 else 0.0

    def evaluate(self, a, n_points=400):
        """
        Sample the profile over [0, a]. Returns (r, S) with r in metres.

        Region boundaries are sampled from both sides, so a discontinuity between two
        regions -- which is exactly what the grain-boundary resolution term looks like --
        is drawn as a step rather than as a spurious ramp.
        """
        r_edges = self.domain * a
        r_out, s_out = [], []

        for i in range(self.n_regions):
            r0, r1 = r_edges[i], r_edges[i + 1]
            if r1 <= r0:
                continue
            share = max(2, int(round(n_points * (r1 - r0) / a))) if a > 0 else 2
            r_seg = np.linspace(r0, r1, share)
            r_out.append(r_seg)
            s_out.append(self.slopes[i] * r_seg + self.intercepts[i])
            r_out.append(np.array([np.nan]))  # break the line between regions
            s_out.append(np.array([np.nan]))

        if not r_out:
            return np.array([]), np.array([])

        return np.concatenate(r_out[:-1]), np.concatenate(s_out[:-1])


def parse_line(line):
    """Parse one record. Returns None for blank lines and for malformed records."""
    fields = line.split("#")
    if len(fields) < 4:
        return None

    try:
        time = float(fields[0])
        domain = [float(v) for v in fields[1].split()]
        slopes = [float(v) for v in fields[2].split()]
        intercepts = [float(v) for v in fields[3].split()]
    except ValueError:
        return None

    # The solver indexes domain[i] and domain[i+1] per region, so the domain must carry
    # exactly one value more than the coefficients.
    if len(slopes) != len(intercepts) or len(domain) != len(slopes) + 1:
        return None

    return Source(time, domain, slopes, intercepts)


def load_sources(path):
    """
    Read a source file. Returns the list of records, ordered as in the file; an empty
    list if the file does not exist. Malformed lines are reported and skipped.
    """
    try:
        with open(path) as f:
            lines = f.readlines()
    except OSError:
        return []

    sources = []
    for number, line in enumerate(lines, start=1):
        if not line.strip():
            continue
        source = parse_line(line)
        if source is None:
            print(f"Warning: skipping malformed record at {path}:{number}")
            continue
        sources.append(source)

    return sources


def format_line(source):
    """Inverse of parse_line, for generating source files."""
    domain = " ".join(f"{v:g}" for v in source.domain)
    slopes = " ".join(f"{v:.6e}" for v in source.slopes)
    intercepts = " ".join(f"{v:.6e}" for v in source.intercepts)
    return f"{source.time:g} # {domain} # {slopes} # {intercepts}"


def write_sources(path, sources):
    """Write source records in the format the solver reads."""
    with open(path, "w") as f:
        for source in sources:
            f.write(format_line(source) + "\n")
