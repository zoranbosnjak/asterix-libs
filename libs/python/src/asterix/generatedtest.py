# Asterix specifications

# This file is generated, DO NOT EDIT!
# For more details, see:
#     - https://github.com/zoranbosnjak/asterix-specs

from asterix.base import *

reference = "unknown"

version = "19700101.0"

# Content set
class Content_0(ContentRaw):
    pass
class Content_1(ContentTable):
    values = { 0: "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001" }
class Content_2(ContentTable):
    values = { 0: "Plot", 1: "Track" }
class Content_3(ContentTable):
    values = { 0: "Test 0", 1: "Test 1", 2: "Test 2", 3: "Test 3" }
class Content_4(ContentString):
    t = StringAscii
class Content_5(ContentString):
    t = StringICAO
class Content_6(ContentString):
    t = StringOctal
class Content_7(ContentInteger):
    sig = Signed
class Content_8(ContentInteger):
    sig = Unsigned
class Content_9(ContentQuantity):
    sig = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"
class Content_10(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = ""
class Content_11(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "kt"
class Content_12(ContentQuantity):
    sig = Unsigned
    lsb = -0.5
    unit = ""

# Variation and Item set

# Uap set
class Uap_0(UapSingle):
    var = Variation_42
class Uap_1(UapMultiple):
    lst = [("plot", Variation_43), ("track", Variation_44)]
    selector = (["020", "TYP"], {0: "plot", 1: "track"})
class Uap_2(UapMultiple):
    lst = [("uap1", Variation_39), ("uap2", Variation_40), ("uap3", Variation_41), ("uap4", Variation_38)]
    selector = None

# Spec set
class AstSpec_0(AstCat):
    uap = Uap_0
class AstSpec_1(AstCat):
    uap = Uap_1
class AstSpec_2(AstCat):
    uap = Uap_2
class AstSpec_3(AstRef):
    var = Variation_47

# Asterix set
class Asterix_0(Asterix):
    cat = 0
    edition = (1, 0)
    spec = AstSpec_0
class Asterix_1(Asterix):
    cat = 0
    edition = (1, 0)
    spec = AstSpec_3
class Asterix_2(Asterix):
    cat = 1
    edition = (1, 0)
    spec = AstSpec_1
class Asterix_3(Asterix):
    cat = 2
    edition = (1, 0)
    spec = AstSpec_2

# Aliases
Cat_000_1_0: TypeAlias = AstSpec_0
Ref_000_1_0: TypeAlias = AstSpec_3
Cat_001_1_0: TypeAlias = AstSpec_1
Cat_002_1_0: TypeAlias = AstSpec_2

# Manifest
manifest = [Cat_000_1_0, Ref_000_1_0, Cat_001_1_0, Cat_002_1_0]
