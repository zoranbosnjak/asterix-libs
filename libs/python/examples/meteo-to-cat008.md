# Encoding meteorological radar reflections in asterix cat008 format

This example assumes rotating meteorological radar, where:

- A radar rotation cycle is devided into 360 angles, that is: a radar
  generates 360 *rays* per cycle [0..359].
- Each *ray* ranges from 0km to 60km, devided into 100m cells, which is
  a total of 600 cells per *ray* [0..599].
- Each *cell* represents *reflectivity value* (precipitation level) at
  particular range. Reflectivity ranges between
  ??? dBZ (minimum) and ??? dBZ (maximum).
- In total, there are `360*600` reflectivity samples per rotation scan.

In python terms, a *ray* could be represented as a list of values and
a complete weather picture is a list of such rays. So we have:

```python
type Ray = list[float]
type WxPicture = list[Ray]
```

A task is to encode `WxPicture` to asterix `cat008` format (`bytes`).
The result is normally sent over the network or saved to a file.
Due to various size limitations, a multiple datagrams might be required for 
a single `WxPicture`. Eventually, we want encoding function in the form:

```python
def encode(t: datetime, wx: WxPicture) -> [bytes]:
    ...
```

The usage of `encode` function in a real-time main processing loop might look
like this:

```python
while True:
    wx = get_new_wx_picture_from_the_radar()
    t = now(timezone.utc)
    datagrams = encode(t, wx)
    for datagram in datagrams:
        send_over_the_network(datagram)
```
