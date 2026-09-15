"""This program generates samples of all categories, with all data bits set to
0 and 1.
"""

from binascii import hexlify
from typing import *

from asterix.base import *
from asterix.generated import manifest
from tests import sample_records

latest = [manifest['CATS'][cat][-1] for cat in manifest['CATS']]


def dump(Spec: AstCat, r: Record) -> None:
    db = Spec.create([r])
    print(hexlify(db.unparse().to_bytes()).decode('utf-8'))


for Spec in latest:
    for (_m_name, r1, r2) in sample_records(Spec):
        dump(Spec, r1)
        dump(Spec, r2)
