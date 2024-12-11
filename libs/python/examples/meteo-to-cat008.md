# Encoding meteorological radar reflections in asterix cat008 format

This example assumes rotating meteorological radar, where:

- A radar rotation cycle is devided into 360 angles, that is: a radar
  generates 360 *rays* per cycle [0..359].
- Each *ray* ranges from 0km to 60km, devided into 100m cells, which is
  a total of 600 cells per *ray* [0..599].
- Each *cell* represents *reflectivity value* (precipitation level) at
  particular range. Reflectivity range is between
  -33 dBZ (minimum) and 93 dBZ (maximum).
- In total, there are `360*600` reflectivity samples per rotation scan.

In python terms, a *ray* could be represented as a list of values and
a complete weather picture is a list of such rays. So we have:

```python
Ray = List[float]
WxPicture = List[Ray]
```

A task is to encode `WxPicture` to asterix `cat008` format (`bytes`).
The result is normally sent over the network or saved to a file.
Due to various size limitations, a multiple datagrams might be required for
a single `WxPicture`. For simplicity, this example assumes a complete
picture availability at the same moment, while in a real-life scenario,
the encoder might send data as soon as new *Ray* is available.

So, in this example we want encoding function and the main processing
loop in the following form:

```python
def encode(t: datetime, wx: WxPicture) -> [bytes]:
    ...

while True:
    wx = get_new_wx_picture_from_the_radar()
    t = datetime.now(timezone.utc)
    datagrams = encode(t, wx)
    for datagram in datagrams:
        send_over_the_network(datagram)
```

## Implementation

Required imports and type definitions...

``` {.python file=test.py}
from typing import *
from dataclasses import dataclass
from datetime import datetime, timezone

from asterix.base import *
import asterix.generated as gen

Cat008 = gen.Cat_008_1_3

Ray = List[float]
WxPicture = List[Ray]
```

There are several conversion steps required before encoding to asterix:

- Radar raw data cell sizes dimensions are expressed in *meters*, while
  asterix specification uses *nautical miles*. Resampling and maybe
  interpolation (depending on required accuracy) is required.
- Intensity level in asterix is unitless 3-bit unsigned integer and so
  the continuous reflection values need to be quantified to 8 discrete
  values.
- Asterix encoding does not use individual cells, but *vectors*, which is
  a compressed version of the same data.
- Cat008 asterix uses so called scaling factor `f`.

A *vector* has *start range*, *end range* and the *intensity*. It defines
a continuous region of samples, where the intensity level is at least
as specified (or more). For example:

If the intensity samples (resampled) are
`ray = [None, None, 0, 1, 3, 2, 1, None,...]`,
we can derive the following vectors (range starts at index '0'):
- `Vector(intensity=0, start_range=2, end_range=7)`
- `Vector(intensity=1, start_range=3, end_range=7)`
- `Vector(intensity=2, start_range=4, end_range=6)`
- `Vector(intensity=3, start_range=4, end_range=5)`

Another simple example `ray = [None, None, 3, None, 1, None, ...],`
is converted to the following list of vectors:
- `Vector(intensity=1, start_range=4, end_range=5)`
- `Vector(intensity=3, start_range=2, end_range=3)`

The vector *start* and *end* range values get encoded in asterix
item `I008/034`.

`STR` and `ENDR` are both 8-bits wide, so for maximum range of
60km, we can get down to 234m (0.1266NM) per cell. A scaling factor
- 4 gives 0.125 NM per cell which is just below required value
- 5 gives 0.25 NM per cell

We need to to choose scaling factor 5 or more (bigger range, worse
resolution).

For example simplification, hardcode constants:

``` {.python file=test.py}
sac, sic = 0x00, 0x00 # radar source address
scaling_factor = 5
reflection_range = (-30.0, 75.0) # min dBZ for (level0, level7)
ray_angle = 1.0
max_records_per_datablock = 8 # This will define datagram size
```

A threshold reflection values can be calculated from `reflection_range`
as a linear range between min and max values.

``` {.python file=test.py}
reflection_step = (reflection_range[1] - reflection_range[0]) / 7
reflection_thresholds = [reflection_range[0] + n * reflection_step for n in range(8)]
```

This conversion is performed *per ray* and it is convenient to perform
all steps in one go.

``` {.python file=test.py}
@dataclass
class Vector:
    intensity: int # 0..7
    start_range: int
    end_range: int

def resample_and_vectorize(ray: Ray) -> List[Vector]:
    ...
```

With `resample_and_vectorize` function implemented, the `encode`
function can be implemented as follows:

``` {.python file=test.py}
def encode(t: datetime, wx: WxPicture) -> [bytes]:
    output = []

    # encode SOP as one datagram
    seconds_since_midnight = 0.0 # TODO: calculate from 't'
    sop_record = Cat008.cv_record.create({
        '000': 254, # SOP message
        '010': (('SAC', sac), ('SIC', sic)),
        '090': (seconds_since_midnight, 's'),
        '100': ((('F', scaling_factor), 0, 0, None),),
    })
    output.append(Cat008.create([sop_record]).unparse().to_bytes())
    
    # encode vector_records to as many datagrams as needed
    vector_records = []
    for ix, ray in enumerate(wx):
        azimuth = float(ix) * ray_angle
        vectors = resample_and_vectorize(ray)

        # now we have azimuth and vectors and we are ready
        # to encode vector records
        r = ... # TODO
        vector_records.append(r)

    # encode EOP as one datagram
    eop_record = Cat008.cv_record.create({
        '000': 255, # EOP message
        '010': (('SAC', sac), ('SIC', sic)),
        '090': (seconds_since_midnight, 's'),
        # '120': TODO...
    })
    output.append(Cat008.create([eop_record]).unparse().to_bytes())

    return output
```

Run example:

``` {.python file=test.py}
wx = [] # TODO
t = datetime.now(timezone.utc)
datagrams = encode(t, wx)
for datagram in datagrams:
    print(hexlify(datagram))
```
