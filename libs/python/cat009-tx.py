# Encode ARSO WX picture to asterix Cat009

# standard imports
from typing import *
from dataclasses import dataclass
from binascii import hexlify

# asterix
from asterix.base import *
import asterix.generated as gen

Cat009: TypeAlias = gen.Cat_009_2_1

@dataclass
class Vector:
    x: int
    y: int
    l: int

@dataclass
class Vectors:
    intensity: int
    lst: List[Vector]

WxPicture: TypeAlias = List[Vectors]

# constants
max_bytes_per_datablock = 1000

def split_at(n: int, lst: Any) -> Any:
    # Split list to list of lists, where each sublist is at most n elements long
    while lst:
        yield(list(lst[0:n]))
        lst = lst[n:]

def make_encoder(sac: int, sic: int, scaling_factor: int) -> Callable[[WxPicture], List[bytes]]:
    def f(wx: WxPicture) -> List[bytes]:
        records = []

        # SOP record
        records.append(Cat009.cv_record.create({
            '010': (('SAC', sac), ('SIC', sic)),
            '000': 254, # SOP message
            '080': ((('F', scaling_factor), 0, 0, None),),
        }))

        # vector records
        cnt = 0
        for vectors in wx:
            i = vectors.intensity

            # max vectors per record is 255.
            lists = split_at(255, vectors.lst)
            for lst in lists:
                cnt += 1
                records.append(Cat009.cv_record.create({
                    '010': (('SAC', sac), ('SIC', sic)),
                    '000': 2, # Cartesian vector
                    '020': ((('ORG', 0), ('I', i), ('S', 4), None),),
                    '030': [(('X', v.x), ('Y', v.y), ('L', v.l)) for v in lst],
                }))

        # EOP record
        records.append(Cat009.cv_record.create({
            '010': (('SAC', sac), ('SIC', sic)),
            '000': 255, # EOP message
            '100': cnt,
        }))

        # group records to datablocks (check max size)
        output: List[bytes] = []
        total_bytes = 0
        record_group = []
        for r in records:
            rec_bytes = len(r.unparse()) // 8
            if total_bytes + rec_bytes <= max_bytes_per_datablock:
                total_bytes += rec_bytes
                record_group.append(r)
            else:
                output.append(Cat009.create(record_group).unparse().to_bytes())
                total_bytes = rec_bytes
                record_group = [r]
        if record_group:
            output.append(Cat009.create(record_group).unparse().to_bytes())

        return output
    return f

# example usage

# create encoder once, at program init
encode = make_encoder(sac=0x00, sic=0x01, scaling_factor=6)

# create test wx picture
def test_vector(intensity: int) -> Vectors:
    return Vectors(intensity=intensity, lst=[Vector(x=1, y=i, l=3) for i in range(30)])
test_picture = [test_vector(i) for i in range(100)]

# encode wx picture, return value is list of datagrams
datagrams = encode(test_picture)
for datagram in datagrams:
    # send to the network
    print(hexlify(datagram))

