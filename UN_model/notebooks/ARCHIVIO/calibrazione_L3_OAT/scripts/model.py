"""Load verbatim notebook cells. No physical/numerical changes are applied."""
import os
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
os.environ['MPLCONFIGDIR'] = str(ROOT / 'scripts' / '.mplconfig')
os.environ['MPLBACKEND'] = 'Agg'
for _index in (1, 2, 6, 8, 10):
    _path = ROOT / 'scripts' / f'cell_{_index:02d}.py'
    exec(compile(_path.read_text(), str(_path), 'exec'), globals())
