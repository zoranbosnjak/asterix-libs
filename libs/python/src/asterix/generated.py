# Asterix specifications

# This file is generated, DO NOT EDIT!
# For more details, see:
#     - https://github.com/zoranbosnjak/asterix-specs

from asterix.base import *

reference = "unknown"
version = "19700101.0"

# Asterix types

class Content_0(ContentRaw):
    pass

class RuleContent_0(RuleContentContextFree):
    variation = Content_0

class Variation_159(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_0

class RuleVariation_154(RuleVariationContextFree):
    variation = Variation_159

class NonSpare_1563(NonSpare):
    name = "SAC"
    title = "System Area Code"
    rule = RuleVariation_154

class Item_811(Item):
    non_spare = NonSpare_1563

class NonSpare_1618(NonSpare):
    name = "SIC"
    title = "System Identification Code"
    rule = RuleVariation_154

class Item_842(Item):
    non_spare = NonSpare_1618

class Variation_1154(Group):
    bit_size = 16
    items_list = [Item_811, Item_842]
    items_dict = {"SAC": NonSpare_1563, "SIC": NonSpare_1618}

class RuleVariation_1104(RuleVariationContextFree):
    variation = Variation_1154

class NonSpare_35(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_405(ContentTable):
    values = {0: "Plot", 1: "Track"}

class RuleContent_405(RuleContentContextFree):
    variation = Content_405

class Variation_67(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_405

class RuleVariation_67(RuleVariationContextFree):
    variation = Variation_67

class NonSpare_1857(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_67

class Item_1018(Item):
    non_spare = NonSpare_1857

class Content_17(ContentTable):
    values = {0: "Actual plot or track", 1: "Simulated plot or track"}

class RuleContent_17(RuleContentContextFree):
    variation = Content_17

class Variation_393(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_17

class RuleVariation_383(RuleVariationContextFree):
    variation = Variation_393

class NonSpare_1634(NonSpare):
    name = "SIM"
    title = ""
    rule = RuleVariation_383

class Item_856(Item):
    non_spare = NonSpare_1634

class Content_334(ContentTable):
    values = {0: "No detection", 1: "Sole primary detection", 2: "Sole secondary detection", 3: "Combined primary and secondary detection"}

class RuleContent_334(RuleContentContextFree):
    variation = Content_334

class Variation_570(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_334

class RuleVariation_560(RuleVariationContextFree):
    variation = Variation_570

class NonSpare_1675(NonSpare):
    name = "SSRPSR"
    title = "Radar Detection in Last Antenna Scan"
    rule = RuleVariation_560

class Item_887(Item):
    non_spare = NonSpare_1675

class Content_461(ContentTable):
    values = {0: "Target report from antenna 1", 1: "Target report from antenna 2"}

class RuleContent_461(RuleContentContextFree):
    variation = Content_461

class Variation_726(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_461

class RuleVariation_716(RuleVariationContextFree):
    variation = Variation_726

class NonSpare_566(NonSpare):
    name = "ANT"
    title = ""
    rule = RuleVariation_716

class Item_70(Item):
    non_spare = NonSpare_566

class Content_146(ContentTable):
    values = {0: "Default", 1: "Special Position Identification"}

class RuleContent_146(RuleContentContextFree):
    variation = Content_146

class Variation_808(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_146

class RuleVariation_779(RuleVariationContextFree):
    variation = Variation_808

class NonSpare_1649(NonSpare):
    name = "SPI"
    title = ""
    rule = RuleVariation_779

class Item_869(Item):
    non_spare = NonSpare_1649

class Content_134(ContentTable):
    values = {0: "Default", 1: "Plot or track from a fixed transponder"}

class RuleContent_134(RuleContentContextFree):
    variation = Content_134

class Variation_885(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_134

class RuleVariation_856(RuleVariationContextFree):
    variation = Variation_885

class NonSpare_1454(NonSpare):
    name = "RAB"
    title = ""
    rule = RuleVariation_856

class Item_730(Item):
    non_spare = NonSpare_1454

class Content_150(ContentTable):
    values = {0: "Default", 1: "Test target indicator"}

class RuleContent_150(RuleContentContextFree):
    variation = Content_150

class Variation_34(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_150

class RuleVariation_34(RuleVariationContextFree):
    variation = Variation_34

class NonSpare_1833(NonSpare):
    name = "TST"
    title = ""
    rule = RuleVariation_34

class Item_997(Item):
    non_spare = NonSpare_1833

class Content_153(ContentTable):
    values = {0: "Default", 1: "Unlawful interference (code 7500)", 2: "Radio-communication failure (code 7600)", 3: "Emergency (code 7700)"}

class RuleContent_153(RuleContentContextFree):
    variation = Content_153

class Variation_454(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_153

class RuleVariation_444(RuleVariationContextFree):
    variation = Variation_454

class NonSpare_849(NonSpare):
    name = "DS1DS2"
    title = "Radar Detection in Last Antenna Scan"
    rule = RuleVariation_444

class Item_266(Item):
    non_spare = NonSpare_849

class Content_111(ContentTable):
    values = {0: "Default", 1: "Military emergency"}

class RuleContent_111(RuleContentContextFree):
    variation = Content_111

class Variation_602(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_111

class RuleVariation_592(RuleVariationContextFree):
    variation = Variation_602

class NonSpare_1188(NonSpare):
    name = "ME"
    title = ""
    rule = RuleVariation_592

class Item_511(Item):
    non_spare = NonSpare_1188

class Content_112(ContentTable):
    values = {0: "Default", 1: "Military identification"}

class RuleContent_112(RuleContentContextFree):
    variation = Content_112

class Variation_681(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_112

class RuleVariation_671(RuleVariationContextFree):
    variation = Variation_681

class NonSpare_1206(NonSpare):
    name = "MI"
    title = ""
    rule = RuleVariation_671

class Item_518(Item):
    non_spare = NonSpare_1206

class Item_24(Spare):
    bit_offset8 = 5
    bit_size = 2

class Variation_1319(Extended):
    items = [Item_1018, Item_856, Item_887, Item_70, Item_869, Item_730, None, Item_997, Item_266, Item_511, Item_518, Item_24, None]

class RuleVariation_1249(RuleVariationContextFree):
    variation = Variation_1319

class NonSpare_84(NonSpare):
    name = "020"
    title = "Target Report Descriptor"
    rule = RuleVariation_1249

class Content_741(ContentQuantity):
    signedness = Unsigned
    lsb = 7.8125e-3
    unit = "NM"

class RuleContent_740(RuleContentContextFree):
    variation = Content_741

class Variation_324(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_740

class RuleVariation_318(RuleVariationContextFree):
    variation = Variation_324

class NonSpare_1502(NonSpare):
    name = "RHO"
    title = ""
    rule = RuleVariation_318

class Item_773(Item):
    non_spare = NonSpare_1502

class Content_766(ContentQuantity):
    signedness = Unsigned
    lsb = 5.4931640625e-3
    unit = "°"

class RuleContent_765(RuleContentContextFree):
    variation = Content_766

class Variation_335(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_765

class RuleVariation_329(RuleVariationContextFree):
    variation = Variation_335

class NonSpare_1761(NonSpare):
    name = "THETA"
    title = ""
    rule = RuleVariation_329

class Item_947(Item):
    non_spare = NonSpare_1761

class Variation_1140(Group):
    bit_size = 32
    items_list = [Item_773, Item_947]
    items_dict = {"RHO": NonSpare_1502, "THETA": NonSpare_1761}

class RuleVariation_1093(RuleVariationContextFree):
    variation = Variation_1140

class NonSpare_131(NonSpare):
    name = "040"
    title = "Measured Position in Polar Co-ordinates"
    rule = RuleVariation_1093

class Content_53(ContentTable):
    values = {0: "Code validated", 1: "Code not validated"}

class RuleContent_53(RuleContentContextFree):
    variation = Content_53

class Variation_14(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_53

class RuleVariation_14(RuleVariationContextFree):
    variation = Variation_14

class NonSpare_1890(NonSpare):
    name = "V"
    title = ""
    rule = RuleVariation_14

class Item_1049(Item):
    non_spare = NonSpare_1890

class Content_92(ContentTable):
    values = {0: "Default", 1: "Garbled code"}

class RuleContent_92(RuleContentContextFree):
    variation = Content_92

class Variation_397(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_92

class RuleVariation_387(RuleVariationContextFree):
    variation = Variation_397

class NonSpare_943(NonSpare):
    name = "G"
    title = ""
    rule = RuleVariation_387

class Item_333(Item):
    non_spare = NonSpare_943

class Content_278(ContentTable):
    values = {0: "Mode-3/A code derived from the reply of the transponder", 1: "Smoothed Mode-3/A code as provided by a local tracker"}

class RuleContent_278(RuleContentContextFree):
    variation = Content_278

class Variation_532(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_278

class RuleVariation_522(RuleVariationContextFree):
    variation = Variation_532

class NonSpare_1068(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_522

class Item_425(Item):
    non_spare = NonSpare_1068

class Item_16(Spare):
    bit_offset8 = 3
    bit_size = 1

class Content_583(ContentString):
    string_type = StringOctal

class RuleContent_583(RuleContentContextFree):
    variation = Content_583

class Variation_780(Element):
    bit_offset8 = 4
    bit_size = 12
    rule = RuleContent_583

class RuleVariation_751(RuleVariationContextFree):
    variation = Variation_780

class NonSpare_1232(NonSpare):
    name = "MODE3A"
    title = "Mode-3/A Reply in Octal Representation"
    rule = RuleVariation_751

class Item_543(Item):
    non_spare = NonSpare_1232

class Variation_1220(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_425, Item_16, Item_543]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "L": NonSpare_1068, "MODE3A": NonSpare_1232}

class RuleVariation_1154(RuleVariationContextFree):
    variation = Variation_1220

class NonSpare_199(NonSpare):
    name = "070"
    title = "Mode-3/A Code in Octal Representation"
    rule = RuleVariation_1154

class Content_624(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "FL"

class RuleContent_624(RuleContentContextFree):
    variation = Content_624

class Variation_585(Element):
    bit_offset8 = 2
    bit_size = 14
    rule = RuleContent_624

class RuleVariation_575(RuleVariationContextFree):
    variation = Variation_585

class NonSpare_1001(NonSpare):
    name = "HGT"
    title = "Mode-C HEIGHT"
    rule = RuleVariation_575

class Item_372(Item):
    non_spare = NonSpare_1001

class Variation_1214(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_372]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "HGT": NonSpare_1001}

class RuleVariation_1148(RuleVariationContextFree):
    variation = Variation_1214

class NonSpare_235(NonSpare):
    name = "090"
    title = "Mode-C Code in Binary Representation"
    rule = RuleVariation_1148

class Variation_145(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_0

class Variation_1375(Repetitive):
    rep_bytes = None
    variation = Variation_145

class RuleVariation_1305(RuleVariationContextFree):
    variation = Variation_1375

class NonSpare_289(NonSpare):
    name = "130"
    title = "Radar Plot Characteristics"
    rule = RuleVariation_1305

class Content_743(ContentQuantity):
    signedness = Unsigned
    lsb = 7.8125e-3
    unit = "s"

class RuleContent_742(RuleContentContextFree):
    variation = Content_743

class Variation_326(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_742

class RuleVariation_320(RuleVariationContextFree):
    variation = Variation_326

class NonSpare_313(NonSpare):
    name = "141"
    title = "Truncated Time of Day"
    rule = RuleVariation_320

class Content_273(ContentTable):
    values = {0: "Mode-2 code as derived from the reply of the transponder", 1: "Smoothed Mode-2 code as provided by a local tracker"}

class RuleContent_273(RuleContentContextFree):
    variation = Content_273

class Variation_527(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_273

class RuleVariation_517(RuleVariationContextFree):
    variation = Variation_527

class NonSpare_1063(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_517

class Item_420(Item):
    non_spare = NonSpare_1063

class NonSpare_1226(NonSpare):
    name = "MODE2"
    title = "Mode-2 Code in Octal Representation"
    rule = RuleVariation_751

class Item_537(Item):
    non_spare = NonSpare_1226

class Variation_1216(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_420, Item_16, Item_537]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "L": NonSpare_1063, "MODE2": NonSpare_1226}

class RuleVariation_1150(RuleVariationContextFree):
    variation = Variation_1216

class NonSpare_163(NonSpare):
    name = "050"
    title = "Mode-2 Code in Octal Representation"
    rule = RuleVariation_1150

class Content_652(ContentQuantity):
    signedness = Signed
    lsb = 3.90625e-3
    unit = "NM/s"

class RuleContent_652(RuleContentContextFree):
    variation = Content_652

class Variation_209(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_652

class RuleVariation_203(RuleVariationContextFree):
    variation = Variation_209

class NonSpare_278(NonSpare):
    name = "120"
    title = "Measured Radial Doppler Speed"
    rule = RuleVariation_203

class Content_597(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = "dBm"

class RuleContent_597(RuleContentContextFree):
    variation = Content_597

class Variation_198(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_597

class RuleVariation_192(RuleVariationContextFree):
    variation = Variation_198

class NonSpare_293(NonSpare):
    name = "131"
    title = "Received Power"
    rule = RuleVariation_192

class Item_3(Spare):
    bit_offset8 = 0
    bit_size = 4

class Content_221(ContentTable):
    values = {0: "High quality pulse A4", 1: "Low quality pulse A4"}

class RuleContent_221(RuleContentContextFree):
    variation = Content_221

class Variation_699(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_221

class RuleVariation_689(RuleVariationContextFree):
    variation = Variation_699

class NonSpare_1412(NonSpare):
    name = "QA4"
    title = ""
    rule = RuleVariation_689

class Item_691(Item):
    non_spare = NonSpare_1412

class Content_220(ContentTable):
    values = {0: "High quality pulse A2", 1: "Low quality pulse A2"}

class RuleContent_220(RuleContentContextFree):
    variation = Content_220

class Variation_823(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_220

class RuleVariation_794(RuleVariationContextFree):
    variation = Variation_823

class NonSpare_1407(NonSpare):
    name = "QA2"
    title = ""
    rule = RuleVariation_794

class Item_686(Item):
    non_spare = NonSpare_1407

class Content_219(ContentTable):
    values = {0: "High quality pulse A1", 1: "Low quality pulse A1"}

class RuleContent_219(RuleContentContextFree):
    variation = Content_219

class Variation_897(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_219

class RuleVariation_868(RuleVariationContextFree):
    variation = Variation_897

class NonSpare_1404(NonSpare):
    name = "QA1"
    title = ""
    rule = RuleVariation_868

class Item_683(Item):
    non_spare = NonSpare_1404

class Content_224(ContentTable):
    values = {0: "High quality pulse B4", 1: "Low quality pulse B4"}

class RuleContent_224(RuleContentContextFree):
    variation = Content_224

class Variation_944(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_224

class RuleVariation_915(RuleVariationContextFree):
    variation = Variation_944

class NonSpare_1423(NonSpare):
    name = "QB4"
    title = ""
    rule = RuleVariation_915

class Item_702(Item):
    non_spare = NonSpare_1423

class Content_223(ContentTable):
    values = {0: "High quality pulse B2", 1: "Low quality pulse B2"}

class RuleContent_223(RuleContentContextFree):
    variation = Content_223

class Variation_44(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_223

class RuleVariation_44(RuleVariationContextFree):
    variation = Variation_44

class NonSpare_1418(NonSpare):
    name = "QB2"
    title = ""
    rule = RuleVariation_44

class Item_697(Item):
    non_spare = NonSpare_1418

class Content_222(ContentTable):
    values = {0: "High quality pulse B1", 1: "Low quality pulse B1"}

class RuleContent_222(RuleContentContextFree):
    variation = Content_222

class Variation_422(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_222

class RuleVariation_412(RuleVariationContextFree):
    variation = Variation_422

class NonSpare_1414(NonSpare):
    name = "QB1"
    title = ""
    rule = RuleVariation_412

class Item_693(Item):
    non_spare = NonSpare_1414

class Content_227(ContentTable):
    values = {0: "High quality pulse C4", 1: "Low quality pulse C4"}

class RuleContent_227(RuleContentContextFree):
    variation = Content_227

class Variation_519(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_227

class RuleVariation_509(RuleVariationContextFree):
    variation = Variation_519

class NonSpare_1431(NonSpare):
    name = "QC4"
    title = ""
    rule = RuleVariation_509

class Item_710(Item):
    non_spare = NonSpare_1431

class Content_226(ContentTable):
    values = {0: "High quality pulse C2", 1: "Low quality pulse C2"}

class RuleContent_226(RuleContentContextFree):
    variation = Content_226

class Variation_617(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_226

class RuleVariation_607(RuleVariationContextFree):
    variation = Variation_617

class NonSpare_1427(NonSpare):
    name = "QC2"
    title = ""
    rule = RuleVariation_607

class Item_706(Item):
    non_spare = NonSpare_1427

class Content_225(ContentTable):
    values = {0: "High quality pulse C1", 1: "Low quality pulse C1"}

class RuleContent_225(RuleContentContextFree):
    variation = Content_225

class Variation_701(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_225

class RuleVariation_691(RuleVariationContextFree):
    variation = Variation_701

class NonSpare_1425(NonSpare):
    name = "QC1"
    title = ""
    rule = RuleVariation_691

class Item_704(Item):
    non_spare = NonSpare_1425

class Content_230(ContentTable):
    values = {0: "High quality pulse D4", 1: "Low quality pulse D4"}

class RuleContent_230(RuleContentContextFree):
    variation = Content_230

class Variation_826(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_230

class RuleVariation_797(RuleVariationContextFree):
    variation = Variation_826

class NonSpare_1439(NonSpare):
    name = "QD4"
    title = ""
    rule = RuleVariation_797

class Item_718(Item):
    non_spare = NonSpare_1439

class Content_229(ContentTable):
    values = {0: "High quality pulse D2", 1: "Low quality pulse D2"}

class RuleContent_229(RuleContentContextFree):
    variation = Content_229

class Variation_901(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_229

class RuleVariation_872(RuleVariationContextFree):
    variation = Variation_901

class NonSpare_1437(NonSpare):
    name = "QD2"
    title = ""
    rule = RuleVariation_872

class Item_716(Item):
    non_spare = NonSpare_1437

class Content_228(ContentTable):
    values = {0: "High quality pulse D1", 1: "Low quality pulse D1"}

class RuleContent_228(RuleContentContextFree):
    variation = Content_228

class Variation_945(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_228

class RuleVariation_916(RuleVariationContextFree):
    variation = Variation_945

class NonSpare_1434(NonSpare):
    name = "QD1"
    title = ""
    rule = RuleVariation_916

class Item_713(Item):
    non_spare = NonSpare_1434

class Variation_997(Group):
    bit_size = 16
    items_list = [Item_3, Item_691, Item_686, Item_683, Item_702, Item_697, Item_693, Item_710, Item_706, Item_704, Item_718, Item_716, Item_713]
    items_dict = {"QA4": NonSpare_1412, "QA2": NonSpare_1407, "QA1": NonSpare_1404, "QB4": NonSpare_1423, "QB2": NonSpare_1418, "QB1": NonSpare_1414, "QC4": NonSpare_1431, "QC2": NonSpare_1427, "QC1": NonSpare_1425, "QD4": NonSpare_1439, "QD2": NonSpare_1437, "QD1": NonSpare_1434}

class RuleVariation_966(RuleVariationContextFree):
    variation = Variation_997

class NonSpare_217(NonSpare):
    name = "080"
    title = "Mode-3/A Code Confidence Indicator"
    rule = RuleVariation_966

class Item_12(Spare):
    bit_offset8 = 2
    bit_size = 2

class Variation_779(Element):
    bit_offset8 = 4
    bit_size = 12
    rule = RuleContent_0

class RuleVariation_750(RuleVariationContextFree):
    variation = Variation_779

class NonSpare_1233(NonSpare):
    name = "MODEC"
    title = "Mode-C Reply in Gray Notation"
    rule = RuleVariation_750

class Item_544(Item):
    non_spare = NonSpare_1233

class Variation_822(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_219

class RuleVariation_793(RuleVariationContextFree):
    variation = Variation_822

class NonSpare_1403(NonSpare):
    name = "QA1"
    title = ""
    rule = RuleVariation_793

class Item_682(Item):
    non_spare = NonSpare_1403

class Variation_900(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_226

class RuleVariation_871(RuleVariationContextFree):
    variation = Variation_900

class NonSpare_1428(NonSpare):
    name = "QC2"
    title = ""
    rule = RuleVariation_871

class Item_707(Item):
    non_spare = NonSpare_1428

class Variation_942(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_220

class RuleVariation_913(RuleVariationContextFree):
    variation = Variation_942

class NonSpare_1408(NonSpare):
    name = "QA2"
    title = ""
    rule = RuleVariation_913

class Item_687(Item):
    non_spare = NonSpare_1408

class Variation_45(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_227

class RuleVariation_45(RuleVariationContextFree):
    variation = Variation_45

class NonSpare_1430(NonSpare):
    name = "QC4"
    title = ""
    rule = RuleVariation_45

class Item_709(Item):
    non_spare = NonSpare_1430

class Variation_421(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_221

class RuleVariation_411(RuleVariationContextFree):
    variation = Variation_421

class NonSpare_1410(NonSpare):
    name = "QA4"
    title = ""
    rule = RuleVariation_411

class Item_689(Item):
    non_spare = NonSpare_1410

class Variation_518(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_222

class RuleVariation_508(RuleVariationContextFree):
    variation = Variation_518

class NonSpare_1415(NonSpare):
    name = "QB1"
    title = ""
    rule = RuleVariation_508

class Item_694(Item):
    non_spare = NonSpare_1415

class Variation_618(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_228

class RuleVariation_608(RuleVariationContextFree):
    variation = Variation_618

class NonSpare_1433(NonSpare):
    name = "QD1"
    title = ""
    rule = RuleVariation_608

class Item_712(Item):
    non_spare = NonSpare_1433

class Variation_700(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_223

class RuleVariation_690(RuleVariationContextFree):
    variation = Variation_700

class NonSpare_1419(NonSpare):
    name = "QB2"
    title = ""
    rule = RuleVariation_690

class Item_698(Item):
    non_spare = NonSpare_1419

class Variation_825(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_229

class RuleVariation_796(RuleVariationContextFree):
    variation = Variation_825

class NonSpare_1436(NonSpare):
    name = "QD2"
    title = ""
    rule = RuleVariation_796

class Item_715(Item):
    non_spare = NonSpare_1436

class Variation_899(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_224

class RuleVariation_870(RuleVariationContextFree):
    variation = Variation_899

class NonSpare_1422(NonSpare):
    name = "QB4"
    title = ""
    rule = RuleVariation_870

class Item_701(Item):
    non_spare = NonSpare_1422

class Variation_946(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_230

class RuleVariation_917(RuleVariationContextFree):
    variation = Variation_946

class NonSpare_1440(NonSpare):
    name = "QD4"
    title = ""
    rule = RuleVariation_917

class Item_719(Item):
    non_spare = NonSpare_1440

class Variation_1211(Group):
    bit_size = 32
    items_list = [Item_1049, Item_333, Item_12, Item_544, Item_3, Item_704, Item_682, Item_707, Item_687, Item_709, Item_689, Item_694, Item_712, Item_698, Item_715, Item_701, Item_719]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "MODEC": NonSpare_1233, "QC1": NonSpare_1425, "QA1": NonSpare_1403, "QC2": NonSpare_1428, "QA2": NonSpare_1408, "QC4": NonSpare_1430, "QA4": NonSpare_1410, "QB1": NonSpare_1415, "QD1": NonSpare_1433, "QB2": NonSpare_1419, "QD2": NonSpare_1436, "QB4": NonSpare_1422, "QD4": NonSpare_1440}

class RuleVariation_1145(RuleVariationContextFree):
    variation = Variation_1211

class NonSpare_256(NonSpare):
    name = "100"
    title = "Mode-C Code and Code Confidence Indicator"
    rule = RuleVariation_1145

class NonSpare_178(NonSpare):
    name = "060"
    title = "Mode-2 Code Confidence Indicator"
    rule = RuleVariation_966

class Content_362(ContentTable):
    values = {0: "No warning nor error condition", 1: "Garbled reply", 2: "Reflection", 3: "Sidelobe reply", 4: "Split plot", 5: "Second time around reply", 6: "Angels", 7: "Terrestrial vehicles", 64: "Possible wrong code in Mode-3/A", 65: "Possible wrong altitude information, transmitted when the Code C credibility check fails together with the Mode-C code in binary notation", 66: "Possible phantom MSSR plot", 80: "Fixed PSR plot", 81: "Slow PSR plot", 82: "Low quality PSR plot"}

class RuleContent_362(RuleContentContextFree):
    variation = Content_362

class Variation_147(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_362

class Variation_1376(Repetitive):
    rep_bytes = None
    variation = Variation_147

class RuleVariation_1306(RuleVariationContextFree):
    variation = Variation_1376

class NonSpare_111(NonSpare):
    name = "030"
    title = "Warning/Error Conditions"
    rule = RuleVariation_1306

class Content_161(ContentTable):
    values = {0: "Default", 1: "X-pulse received in Mode-3/A reply"}

class RuleContent_161(RuleContentContextFree):
    variation = Content_161

class Variation_35(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_161

class RuleVariation_35(RuleVariationContextFree):
    variation = Variation_35

class NonSpare_1995(NonSpare):
    name = "XA"
    title = ""
    rule = RuleVariation_35

class Item_1144(Item):
    non_spare = NonSpare_1995

class Item_7(Spare):
    bit_offset8 = 1
    bit_size = 1

class Content_162(ContentTable):
    values = {0: "Default", 1: "X-pulse received in Mode-C reply"}

class RuleContent_162(RuleContentContextFree):
    variation = Content_162

class Variation_506(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_162

class RuleVariation_496(RuleVariationContextFree):
    variation = Variation_506

class NonSpare_1996(NonSpare):
    name = "XC"
    title = ""
    rule = RuleVariation_496

class Item_1145(Item):
    non_spare = NonSpare_1996

class Item_17(Spare):
    bit_offset8 = 3
    bit_size = 2

class Content_160(ContentTable):
    values = {0: "Default", 1: "X-pulse received in Mode-2 reply"}

class RuleContent_160(RuleContentContextFree):
    variation = Content_160

class Variation_812(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_160

class RuleVariation_783(RuleVariationContextFree):
    variation = Variation_812

class NonSpare_1989(NonSpare):
    name = "X2"
    title = ""
    rule = RuleVariation_783

class Item_1138(Item):
    non_spare = NonSpare_1989

class Item_27(Spare):
    bit_offset8 = 6
    bit_size = 2

class Variation_1273(Group):
    bit_size = 8
    items_list = [Item_1144, Item_7, Item_1145, Item_17, Item_1138, Item_27]
    items_dict = {"XA": NonSpare_1995, "XC": NonSpare_1996, "X2": NonSpare_1989}

class RuleVariation_1203(RuleVariationContextFree):
    variation = Variation_1273

class NonSpare_322(NonSpare):
    name = "150"
    title = "Presence of X-Pulse"
    rule = RuleVariation_1203

class Variation_1383(Explicit):
    explicit_type = SpecialPurpose

class RuleVariation_1313(RuleVariationContextFree):
    variation = Variation_1383

class NonSpare_1641(NonSpare):
    name = "SP"
    title = "Special Purpose Field"
    rule = RuleVariation_1313

class Record_17(Record):
    items_list = [NonSpare_35, NonSpare_84, NonSpare_131, NonSpare_199, NonSpare_235, NonSpare_289, NonSpare_313, NonSpare_163, NonSpare_278, NonSpare_293, NonSpare_217, NonSpare_256, NonSpare_178, NonSpare_111, NonSpare_322, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1641, UapItemRFS]
    items_dict = {"010": NonSpare_35, "020": NonSpare_84, "040": NonSpare_131, "070": NonSpare_199, "090": NonSpare_235, "130": NonSpare_289, "141": NonSpare_313, "050": NonSpare_163, "120": NonSpare_278, "131": NonSpare_293, "080": NonSpare_217, "100": NonSpare_256, "060": NonSpare_178, "030": NonSpare_111, "150": NonSpare_322, "SP": NonSpare_1641}

class Variation_246(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_0

class RuleVariation_240(RuleVariationContextFree):
    variation = Variation_246

class NonSpare_339(NonSpare):
    name = "161"
    title = "Track Plot Number"
    rule = RuleVariation_240

class Content_645(ContentQuantity):
    signedness = Signed
    lsb = 1.5625e-2
    unit = "NM"

class RuleContent_645(RuleContentContextFree):
    variation = Content_645

class Variation_275(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_645

class RuleVariation_269(RuleVariationContextFree):
    variation = Variation_275

class NonSpare_1981(NonSpare):
    name = "X"
    title = "X-Component"
    rule = RuleVariation_269

class Item_1130(Item):
    non_spare = NonSpare_1981

class NonSpare_2034(NonSpare):
    name = "Y"
    title = "Y-Component"
    rule = RuleVariation_269

class Item_1180(Item):
    non_spare = NonSpare_2034

class Variation_1266(Group):
    bit_size = 32
    items_list = [Item_1130, Item_1180]
    items_dict = {"X": NonSpare_1981, "Y": NonSpare_2034}

class RuleVariation_1199(RuleVariationContextFree):
    variation = Variation_1266

class NonSpare_151(NonSpare):
    name = "042"
    title = "Calculated Position in Cartesian Co-ordinates"
    rule = RuleVariation_1199

class Content_751(ContentQuantity):
    signedness = Unsigned
    lsb = 6.103515625e-5
    unit = "NM/s"

class RuleContent_750(RuleContentContextFree):
    variation = Content_751

class Variation_330(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_750

class RuleVariation_324(RuleVariationContextFree):
    variation = Variation_330

class NonSpare_973(NonSpare):
    name = "GSP"
    title = "Calculated Groundspeed"
    rule = RuleVariation_324

class Item_352(Item):
    non_spare = NonSpare_973

class NonSpare_993(NonSpare):
    name = "HDG"
    title = "Calculated Heading"
    rule = RuleVariation_329

class Item_366(Item):
    non_spare = NonSpare_993

class Variation_1079(Group):
    bit_size = 32
    items_list = [Item_352, Item_366]
    items_dict = {"GSP": NonSpare_973, "HDG": NonSpare_993}

class RuleVariation_1038(RuleVariationContextFree):
    variation = Variation_1079

class NonSpare_358(NonSpare):
    name = "200"
    title = "Calculated Track Velocity in Polar Co-ordinates"
    rule = RuleVariation_1038

class Content_58(ContentTable):
    values = {0: "Confirmed Track", 1: "Track in initialisation phase"}

class RuleContent_58(RuleContentContextFree):
    variation = Content_58

class Variation_16(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_58

class RuleVariation_16(RuleVariationContextFree):
    variation = Variation_16

class NonSpare_741(NonSpare):
    name = "CON"
    title = ""
    rule = RuleVariation_16

class Item_184(Item):
    non_spare = NonSpare_741

class Content_408(ContentTable):
    values = {0: "Primary track", 1: "SSR/Combined track"}

class RuleContent_408(RuleContentContextFree):
    variation = Content_408

class Variation_437(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_408

class RuleVariation_427(RuleVariationContextFree):
    variation = Variation_437

class NonSpare_1459(NonSpare):
    name = "RAD"
    title = ""
    rule = RuleVariation_427

class Item_735(Item):
    non_spare = NonSpare_1459

class Content_79(ContentTable):
    values = {0: "Default", 1: "Aircraft manoeuvring"}

class RuleContent_79(RuleContentContextFree):
    variation = Content_79

class Variation_496(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_79

class RuleVariation_486(RuleVariationContextFree):
    variation = Variation_496

class NonSpare_1152(NonSpare):
    name = "MAN"
    title = ""
    rule = RuleVariation_486

class Item_496(Item):
    non_spare = NonSpare_1152

class Content_89(ContentTable):
    values = {0: "Default", 1: "Doubtful plot to track association"}

class RuleContent_89(RuleContentContextFree):
    variation = Content_89

class Variation_596(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_89

class RuleVariation_586(RuleVariationContextFree):
    variation = Variation_596

class NonSpare_842(NonSpare):
    name = "DOU"
    title = ""
    rule = RuleVariation_586

class Item_261(Item):
    non_spare = NonSpare_842

class Content_410(ContentTable):
    values = {0: "RDP Chain 1", 1: "RDP Chain 2"}

class RuleContent_410(RuleContentContextFree):
    variation = Content_410

class Variation_719(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_410

class RuleVariation_709(RuleVariationContextFree):
    variation = Variation_719

class NonSpare_1474(NonSpare):
    name = "RDPC"
    title = "Radar Data Processing Chain"
    rule = RuleVariation_709

class Item_748(Item):
    non_spare = NonSpare_1474

class Item_23(Spare):
    bit_offset8 = 5
    bit_size = 1

class Content_94(ContentTable):
    values = {0: "Default", 1: "Ghost track"}

class RuleContent_94(RuleContentContextFree):
    variation = Content_94

class Variation_880(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_94

class RuleVariation_851(RuleVariationContextFree):
    variation = Variation_880

class NonSpare_968(NonSpare):
    name = "GHO"
    title = ""
    rule = RuleVariation_851

class Item_348(Item):
    non_spare = NonSpare_968

class Content_107(ContentTable):
    values = {0: "Default", 1: "Last report for a track"}

class RuleContent_107(RuleContentContextFree):
    variation = Content_107

class Variation_30(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_107

class RuleVariation_30(RuleVariationContextFree):
    variation = Variation_30

class NonSpare_1815(NonSpare):
    name = "TRE"
    title = ""
    rule = RuleVariation_30

class Item_983(Item):
    non_spare = NonSpare_1815

class Item_9(Spare):
    bit_offset8 = 1
    bit_size = 6

class Variation_1287(Extended):
    items = [Item_184, Item_735, Item_496, Item_261, Item_748, Item_23, Item_348, None, Item_983, Item_9, None]

class RuleVariation_1217(RuleVariationContextFree):
    variation = Variation_1287

class NonSpare_350(NonSpare):
    name = "170"
    title = "Track Status"
    rule = RuleVariation_1217

class NonSpare_384(NonSpare):
    name = "210"
    title = "Track Quality"
    rule = RuleVariation_1305

class Record_18(Record):
    items_list = [NonSpare_35, NonSpare_84, NonSpare_339, NonSpare_131, NonSpare_151, NonSpare_358, NonSpare_199, NonSpare_235, NonSpare_313, NonSpare_289, NonSpare_293, NonSpare_278, NonSpare_350, NonSpare_384, NonSpare_163, NonSpare_217, NonSpare_256, NonSpare_178, NonSpare_111, NonSpare_1641, UapItemRFS, NonSpare_322]
    items_dict = {"010": NonSpare_35, "020": NonSpare_84, "161": NonSpare_339, "040": NonSpare_131, "042": NonSpare_151, "200": NonSpare_358, "070": NonSpare_199, "090": NonSpare_235, "141": NonSpare_313, "130": NonSpare_289, "131": NonSpare_293, "120": NonSpare_278, "170": NonSpare_350, "210": NonSpare_384, "050": NonSpare_163, "080": NonSpare_217, "100": NonSpare_256, "060": NonSpare_178, "030": NonSpare_111, "SP": NonSpare_1641, "150": NonSpare_322}

class Uap_45(UapMultiple):
    uaps = {"plot": Record_17, "track": Record_18}
    selector = (["020", "TYP"], {0: "plot", 1: "track"})

class Asterix_0(AstCat):
    category = 1
    edition = (1, 2)
    uap = Uap_45

class NonSpare_36(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Record_24(Record):
    items_list = [NonSpare_36, NonSpare_84, NonSpare_131, NonSpare_199, NonSpare_235, NonSpare_289, NonSpare_313, NonSpare_163, NonSpare_278, NonSpare_293, NonSpare_217, NonSpare_256, NonSpare_178, NonSpare_111, NonSpare_322, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1641, UapItemRFS]
    items_dict = {"010": NonSpare_36, "020": NonSpare_84, "040": NonSpare_131, "070": NonSpare_199, "090": NonSpare_235, "130": NonSpare_289, "141": NonSpare_313, "050": NonSpare_163, "120": NonSpare_278, "131": NonSpare_293, "080": NonSpare_217, "100": NonSpare_256, "060": NonSpare_178, "030": NonSpare_111, "150": NonSpare_322, "SP": NonSpare_1641}

class Record_25(Record):
    items_list = [NonSpare_36, NonSpare_84, NonSpare_339, NonSpare_131, NonSpare_151, NonSpare_358, NonSpare_199, NonSpare_235, NonSpare_313, NonSpare_289, NonSpare_293, NonSpare_278, NonSpare_350, NonSpare_384, NonSpare_163, NonSpare_217, NonSpare_256, NonSpare_178, NonSpare_111, NonSpare_1641, UapItemRFS, NonSpare_322]
    items_dict = {"010": NonSpare_36, "020": NonSpare_84, "161": NonSpare_339, "040": NonSpare_131, "042": NonSpare_151, "200": NonSpare_358, "070": NonSpare_199, "090": NonSpare_235, "141": NonSpare_313, "130": NonSpare_289, "131": NonSpare_293, "120": NonSpare_278, "170": NonSpare_350, "210": NonSpare_384, "050": NonSpare_163, "080": NonSpare_217, "100": NonSpare_256, "060": NonSpare_178, "030": NonSpare_111, "SP": NonSpare_1641, "150": NonSpare_322}

class Uap_46(UapMultiple):
    uaps = {"plot": Record_24, "track": Record_25}
    selector = (["020", "TYP"], {0: "plot", 1: "track"})

class Asterix_1(AstCat):
    category = 1
    edition = (1, 3)
    uap = Uap_46

class Asterix_2(AstCat):
    category = 1
    edition = (1, 4)
    uap = Uap_46

class NonSpare_46(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_563(ContentTable):
    values = {1: "North marker message", 2: "Sector crossing message", 3: "South marker message", 8: "Activation of blind zone filtering", 9: "Stop of blind zone filtering"}

class RuleContent_563(RuleContentContextFree):
    variation = Content_563

class Variation_182(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_563

class RuleVariation_176(RuleVariationContextFree):
    variation = Variation_182

class NonSpare_6(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_176

class Content_763(ContentQuantity):
    signedness = Unsigned
    lsb = 1.40625
    unit = "°"

class RuleContent_762(RuleContentContextFree):
    variation = Content_763

class Variation_236(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_762

class RuleVariation_230(RuleVariationContextFree):
    variation = Variation_236

class NonSpare_79(NonSpare):
    name = "020"
    title = "Sector Number"
    rule = RuleVariation_230

class Variation_362(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_742

class RuleVariation_356(RuleVariationContextFree):
    variation = Variation_362

class NonSpare_103(NonSpare):
    name = "030"
    title = "Time of Day"
    rule = RuleVariation_356

class NonSpare_144(NonSpare):
    name = "041"
    title = "Antenna Rotation Speed"
    rule = RuleVariation_320

class NonSpare_169(NonSpare):
    name = "050"
    title = "Station Configuration Status"
    rule = RuleVariation_1305

class NonSpare_185(NonSpare):
    name = "060"
    title = "Station Processing Mode"
    rule = RuleVariation_1305

class Content_66(ContentTable):
    values = {0: "Counter for antenna 1", 1: "Counter for antenna 2"}

class RuleContent_66(RuleContentContextFree):
    variation = Content_66

class Variation_20(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_66

class RuleVariation_20(RuleVariationContextFree):
    variation = Variation_20

class NonSpare_510(NonSpare):
    name = "A"
    title = "Aerial Identification"
    rule = RuleVariation_20

class Item_33(Item):
    non_spare = NonSpare_510

class Content_567(ContentTable):
    values = {1: "Sole primary plots", 2: "Sole SSR plots", 3: "Combined plots"}

class RuleContent_567(RuleContentContextFree):
    variation = Content_567

class Variation_473(Element):
    bit_offset8 = 1
    bit_size = 5
    rule = RuleContent_567

class RuleVariation_463(RuleVariationContextFree):
    variation = Variation_473

class NonSpare_1043(NonSpare):
    name = "IDENT"
    title = ""
    rule = RuleVariation_463

class Item_405(Item):
    non_spare = NonSpare_1043

class Content_585(ContentInteger):
    signedness = Unsigned

class RuleContent_585(RuleContentContextFree):
    variation = Content_585

class Variation_933(Element):
    bit_offset8 = 6
    bit_size = 10
    rule = RuleContent_585

class RuleVariation_904(RuleVariationContextFree):
    variation = Variation_933

class NonSpare_751(NonSpare):
    name = "COUNTER"
    title = ""
    rule = RuleVariation_904

class Item_194(Item):
    non_spare = NonSpare_751

class Variation_1009(Group):
    bit_size = 16
    items_list = [Item_33, Item_405, Item_194]
    items_dict = {"A": NonSpare_510, "IDENT": NonSpare_1043, "COUNTER": NonSpare_751}

class Variation_1337(Repetitive):
    rep_bytes = 1
    variation = Variation_1009

class RuleVariation_1267(RuleVariationContextFree):
    variation = Variation_1337

class NonSpare_201(NonSpare):
    name = "070"
    title = "Plot Count Values"
    rule = RuleVariation_1267

class Content_740(ContentQuantity):
    signedness = Unsigned
    lsb = 7.8125e-3
    unit = "NM"

class RuleContent_739(RuleContentContextFree):
    variation = Content_740

class Variation_323(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_739

class RuleVariation_317(RuleVariationContextFree):
    variation = Variation_323

class NonSpare_1526(NonSpare):
    name = "RS"
    title = "Rho Start"
    rule = RuleVariation_317

class Item_790(Item):
    non_spare = NonSpare_1526

class NonSpare_1482(NonSpare):
    name = "RE"
    title = "Rho End"
    rule = RuleVariation_317

class Item_753(Item):
    non_spare = NonSpare_1482

class NonSpare_1827(NonSpare):
    name = "TS"
    title = "Theta Start"
    rule = RuleVariation_329

class Item_991(Item):
    non_spare = NonSpare_1827

class NonSpare_1758(NonSpare):
    name = "TE"
    title = "Theta End"
    rule = RuleVariation_329

class Item_945(Item):
    non_spare = NonSpare_1758

class Variation_1147(Group):
    bit_size = 64
    items_list = [Item_790, Item_753, Item_991, Item_945]
    items_dict = {"RS": NonSpare_1526, "RE": NonSpare_1482, "TS": NonSpare_1827, "TE": NonSpare_1758}

class RuleVariation_1099(RuleVariationContextFree):
    variation = Variation_1147

class NonSpare_251(NonSpare):
    name = "100"
    title = "Dynamic Window Type 1"
    rule = RuleVariation_1099

class Content_648(ContentQuantity):
    signedness = Signed
    lsb = 7.8125e-3
    unit = "NM"

class RuleContent_648(RuleContentContextFree):
    variation = Content_648

class Variation_206(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_648

class RuleVariation_200(RuleVariationContextFree):
    variation = Variation_206

class NonSpare_1479(NonSpare):
    name = "RE"
    title = "Range Error"
    rule = RuleVariation_200

class Item_751(Item):
    non_spare = NonSpare_1479

class Content_679(ContentQuantity):
    signedness = Signed
    lsb = 2.197265625e-2
    unit = "°"

class RuleContent_679(RuleContentContextFree):
    variation = Content_679

class Variation_211(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_679

class RuleVariation_205(RuleVariationContextFree):
    variation = Variation_211

class NonSpare_540(NonSpare):
    name = "AE"
    title = "Azimuth Error"
    rule = RuleVariation_205

class Item_48(Item):
    non_spare = NonSpare_540

class Variation_1133(Group):
    bit_size = 16
    items_list = [Item_751, Item_48]
    items_dict = {"RE": NonSpare_1479, "AE": NonSpare_540}

class RuleVariation_1086(RuleVariationContextFree):
    variation = Variation_1133

class NonSpare_228(NonSpare):
    name = "090"
    title = "Collimation Error"
    rule = RuleVariation_1086

class NonSpare_226(NonSpare):
    name = "080"
    title = "Warning/Error Conditions"
    rule = RuleVariation_1305

class Record_44(Record):
    items_list = [NonSpare_46, NonSpare_6, NonSpare_79, NonSpare_103, NonSpare_144, NonSpare_169, NonSpare_185, NonSpare_201, NonSpare_251, NonSpare_228, NonSpare_226, UapItemSpare, NonSpare_1641, UapItemRFS]
    items_dict = {"010": NonSpare_46, "000": NonSpare_6, "020": NonSpare_79, "030": NonSpare_103, "041": NonSpare_144, "050": NonSpare_169, "060": NonSpare_185, "070": NonSpare_201, "100": NonSpare_251, "090": NonSpare_228, "080": NonSpare_226, "SP": NonSpare_1641}

class Uap_40(UapSingle):
    record = Record_44

class Asterix_3(AstCat):
    category = 2
    edition = (1, 0)
    uap = Uap_40

class NonSpare_37(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Record_26(Record):
    items_list = [NonSpare_37, NonSpare_6, NonSpare_79, NonSpare_103, NonSpare_144, NonSpare_169, NonSpare_185, NonSpare_201, NonSpare_251, NonSpare_228, NonSpare_226, UapItemSpare, NonSpare_1641, UapItemRFS]
    items_dict = {"010": NonSpare_37, "000": NonSpare_6, "020": NonSpare_79, "030": NonSpare_103, "041": NonSpare_144, "050": NonSpare_169, "060": NonSpare_185, "070": NonSpare_201, "100": NonSpare_251, "090": NonSpare_228, "080": NonSpare_226, "SP": NonSpare_1641}

class Uap_22(UapSingle):
    record = Record_26

class Asterix_4(AstCat):
    category = 2
    edition = (1, 1)
    uap = Uap_22

class NonSpare_33(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_550(ContentTable):
    values = {1: "Alive Message (AM)", 2: "Route Adherence Monitor Longitudinal Deviation (RAMLD)", 3: "Route Adherence Monitor Heading Deviation (RAMHD)", 4: "Minimum Safe Altitude Warning (MSAW)", 5: "Area Proximity Warning (APW)", 6: "Clearance Level Adherence Monitor (CLAM)", 7: "Short Term Conflict Alert (STCA)", 8: "Approach Path Monitor (APM)", 9: "RIMCAS Arrival / Landing Monitor (ALM)", 10: "RIMCAS Arrival / Departure Wrong Runway Alert (WRA)", 11: "RIMCAS Arrival / Departure Opposite Traffic Alert (OTA)", 12: "RIMCAS Departure Monitor (RDM)", 13: "RIMCAS Runway / Taxiway Crossing Monitor (RCM)", 14: "RIMCAS Taxiway Separation Monitor (TSM)", 15: "RIMCAS Unauthorized Taxiway Movement Monitor(UTMM)", 16: "RIMCAS Stop Bar Overrun Alert (SBOA)", 17: "End Of Conflict (EOC)", 18: "ACAS Resolution Advisory (ACASRA)", 19: "Near Term Conflict Alert (NTCA)", 20: "Downlinked Barometric Pressure Setting Monitor (DBPSM)", 21: "Speed Adherence Monitor (SAM)", 22: "Outside Controlled Airspace Tool (OCAT)", 23: "Vertical Conflict Detection (VCD)", 24: "Vertical Rate Adherence Monitor (VRAM)", 25: "Cleared Heading Adherence Monitor (CHAM)", 26: "Downlinked Selected Altitude Monitor (DSAM)", 27: "Holding Adherence Monitor (HAM)", 28: "Vertical Path Monitor (VPM)", 29: "RIMCAS Taxiway Traffic Alert (TTA)", 30: "RIMCAS Arrival/Departure Close Runway Alert (CRA)", 31: "RIMCAS Arrival/Departure Aircraft Separation Monitor (ASM)", 32: "RIMCAS ILS Area Violation Monitor (IAVM)", 33: "Final Target Distance Indicator (FTD)", 34: "Initial Target Distance Indicator (ITD)", 35: "Wake Vortex Indicator Infringement Alert (IIA)", 36: "Sequence Warning (SQW)", 37: "Catch Up Warning (CUW)", 38: "Conflicting ATC Clearances (CATC)", 39: "No ATC Clearance (NOCLR)", 40: "Aircraft Not Moving despite ATC Clearance (NOMOV)", 41: "Aircraft leaving/entering the aerodrome area without proper handover (NOH)", 42: "Wrong Runway or Taxiway Type (WRTY)", 43: "Stand Occupied (STOCC)", 44: "Ongoing Alert (ONGOING)", 97: "Lost Track Warning (LTW)", 98: "Holding Volume Infringement (HVI)", 99: "Airspace Infringement Warning (AIW)"}

class RuleContent_550(RuleContentContextFree):
    variation = Content_550

class Variation_173(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_550

class RuleVariation_167(RuleVariationContextFree):
    variation = Variation_173

class NonSpare_2(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_167

class Variation_1354(Repetitive):
    rep_bytes = 1
    variation = Variation_1154

class RuleVariation_1284(RuleVariationContextFree):
    variation = Variation_1354

class NonSpare_56(NonSpare):
    name = "015"
    title = "SDPS Identifier"
    rule = RuleVariation_1284

class NonSpare_89(NonSpare):
    name = "020"
    title = "Time of Message"
    rule = RuleVariation_356

class NonSpare_129(NonSpare):
    name = "040"
    title = "Alert Identifier"
    rule = RuleVariation_240

class Variation_745(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_0

class RuleVariation_735(RuleVariationContextFree):
    variation = Variation_745

class NonSpare_1691(NonSpare):
    name = "STAT"
    title = "Status of the Alert"
    rule = RuleVariation_735

class Item_899(Item):
    non_spare = NonSpare_1691

class Item_29(Spare):
    bit_offset8 = 7
    bit_size = 1

class Variation_998(Group):
    bit_size = 8
    items_list = [Item_3, Item_899, Item_29]
    items_dict = {"STAT": NonSpare_1691}

class RuleVariation_967(RuleVariationContextFree):
    variation = Variation_998

class NonSpare_155(NonSpare):
    name = "045"
    title = "Alert Status"
    rule = RuleVariation_967

class Content_109(ContentTable):
    values = {0: "Default", 1: "MRVA function"}

class RuleContent_109(RuleContentContextFree):
    variation = Content_109

class Variation_31(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_109

class RuleVariation_31(RuleVariationContextFree):
    variation = Variation_31

class NonSpare_1241(NonSpare):
    name = "MRVA"
    title = ""
    rule = RuleVariation_31

class Item_552(Item):
    non_spare = NonSpare_1241

class Content_136(ContentTable):
    values = {0: "Default", 1: "RAMLD function"}

class RuleContent_136(RuleContentContextFree):
    variation = Content_136

class Variation_406(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_136

class RuleVariation_396(RuleVariationContextFree):
    variation = Variation_406

class NonSpare_1462(NonSpare):
    name = "RAMLD"
    title = ""
    rule = RuleVariation_396

class Item_738(Item):
    non_spare = NonSpare_1462

class Content_135(ContentTable):
    values = {0: "Default", 1: "RAMHD function"}

class RuleContent_135(RuleContentContextFree):
    variation = Content_135

class Variation_505(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_135

class RuleVariation_495(RuleVariationContextFree):
    variation = Variation_505

class NonSpare_1461(NonSpare):
    name = "RAMHD"
    title = ""
    rule = RuleVariation_495

class Item_737(Item):
    non_spare = NonSpare_1461

class Content_110(ContentTable):
    values = {0: "Default", 1: "MSAW function"}

class RuleContent_110(RuleContentContextFree):
    variation = Content_110

class Variation_601(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_110

class RuleVariation_591(RuleVariationContextFree):
    variation = Variation_601

class NonSpare_1246(NonSpare):
    name = "MSAW"
    title = ""
    rule = RuleVariation_591

class Item_555(Item):
    non_spare = NonSpare_1246

class Content_77(ContentTable):
    values = {0: "Default", 1: "APW function"}

class RuleContent_77(RuleContentContextFree):
    variation = Content_77

class Variation_678(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_77

class RuleVariation_668(RuleVariationContextFree):
    variation = Variation_678

class NonSpare_574(NonSpare):
    name = "APW"
    title = ""
    rule = RuleVariation_668

class Item_74(Item):
    non_spare = NonSpare_574

class Content_82(ContentTable):
    values = {0: "Default", 1: "CLAM function"}

class RuleContent_82(RuleContentContextFree):
    variation = Content_82

class Variation_800(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_82

class RuleVariation_771(RuleVariationContextFree):
    variation = Variation_800

class NonSpare_693(NonSpare):
    name = "CLAM"
    title = ""
    rule = RuleVariation_771

class Item_156(Item):
    non_spare = NonSpare_693

class Content_144(ContentTable):
    values = {0: "Default", 1: "STCA function"}

class RuleContent_144(RuleContentContextFree):
    variation = Content_144

class Variation_887(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_144

class RuleVariation_858(RuleVariationContextFree):
    variation = Variation_887

class NonSpare_1695(NonSpare):
    name = "STCA"
    title = ""
    rule = RuleVariation_858

class Item_903(Item):
    non_spare = NonSpare_1695

class Content_76(ContentTable):
    values = {0: "Default", 1: "APM function"}

class RuleContent_76(RuleContentContextFree):
    variation = Content_76

class Variation_23(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_76

class RuleVariation_23(RuleVariationContextFree):
    variation = Variation_23

class NonSpare_573(NonSpare):
    name = "APM"
    title = ""
    rule = RuleVariation_23

class Item_73(Item):
    non_spare = NonSpare_573

class Content_137(ContentTable):
    values = {0: "Default", 1: "RIMCA function"}

class RuleContent_137(RuleContentContextFree):
    variation = Content_137

class Variation_407(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_137

class RuleVariation_397(RuleVariationContextFree):
    variation = Variation_407

class NonSpare_1510(NonSpare):
    name = "RIMCA"
    title = ""
    rule = RuleVariation_397

class Item_781(Item):
    non_spare = NonSpare_1510

class Content_74(ContentTable):
    values = {0: "Default", 1: "ACAS RA function"}

class RuleContent_74(RuleContentContextFree):
    variation = Content_74

class Variation_495(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_74

class RuleVariation_485(RuleVariationContextFree):
    variation = Variation_495

class NonSpare_522(NonSpare):
    name = "ACASRA"
    title = ""
    rule = RuleVariation_485

class Item_40(Item):
    non_spare = NonSpare_522

class Content_127(ContentTable):
    values = {0: "Default", 1: "NTCA function"}

class RuleContent_127(RuleContentContextFree):
    variation = Content_127

class Variation_606(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_127

class RuleVariation_596(RuleVariationContextFree):
    variation = Variation_606

class NonSpare_1303(NonSpare):
    name = "NTCA"
    title = ""
    rule = RuleVariation_596

class Item_607(Item):
    non_spare = NonSpare_1303

class Content_147(ContentTable):
    values = {0: "Default", 1: "System degraded"}

class RuleContent_147(RuleContentContextFree):
    variation = Content_147

class Variation_684(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_147

class RuleVariation_674(RuleVariationContextFree):
    variation = Variation_684

class NonSpare_831(NonSpare):
    name = "DG"
    title = ""
    rule = RuleVariation_674

class Item_252(Item):
    non_spare = NonSpare_831

class Content_130(ContentTable):
    values = {0: "Default", 1: "Overflow error"}

class RuleContent_130(RuleContentContextFree):
    variation = Content_130

class Variation_804(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_130

class RuleVariation_775(RuleVariationContextFree):
    variation = Variation_804

class NonSpare_1316(NonSpare):
    name = "OF"
    title = ""
    rule = RuleVariation_775

class Item_620(Item):
    non_spare = NonSpare_1316

class Content_132(ContentTable):
    values = {0: "Default", 1: "Overload error"}

class RuleContent_132(RuleContentContextFree):
    variation = Content_132

class Variation_884(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_132

class RuleVariation_855(RuleVariationContextFree):
    variation = Variation_884

class NonSpare_1317(NonSpare):
    name = "OL"
    title = ""
    rule = RuleVariation_855

class Item_621(Item):
    non_spare = NonSpare_1317

class Content_75(ContentTable):
    values = {0: "Default", 1: "AIW function"}

class RuleContent_75(RuleContentContextFree):
    variation = Content_75

class Variation_22(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_75

class RuleVariation_22(RuleVariationContextFree):
    variation = Variation_22

class NonSpare_550(NonSpare):
    name = "AIW"
    title = ""
    rule = RuleVariation_22

class Item_55(Item):
    non_spare = NonSpare_550

class Content_133(ContentTable):
    values = {0: "Default", 1: "PAIW function"}

class RuleContent_133(RuleContentContextFree):
    variation = Content_133

class Variation_405(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_133

class RuleVariation_395(RuleVariationContextFree):
    variation = Variation_405

class NonSpare_1341(NonSpare):
    name = "PAIW"
    title = ""
    rule = RuleVariation_395

class Item_643(Item):
    non_spare = NonSpare_1341

class Content_128(ContentTable):
    values = {0: "Default", 1: "OCAT function"}

class RuleContent_128(RuleContentContextFree):
    variation = Content_128

class Variation_503(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_128

class RuleVariation_493(RuleVariationContextFree):
    variation = Variation_503

class NonSpare_1310(NonSpare):
    name = "OCAT"
    title = ""
    rule = RuleVariation_493

class Item_614(Item):
    non_spare = NonSpare_1310

class Content_142(ContentTable):
    values = {0: "Default", 1: "SAM function"}

class RuleContent_142(RuleContentContextFree):
    variation = Content_142

class Variation_607(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_142

class RuleVariation_597(RuleVariationContextFree):
    variation = Variation_607

class NonSpare_1569(NonSpare):
    name = "SAM"
    title = ""
    rule = RuleVariation_597

class Item_814(Item):
    non_spare = NonSpare_1569

class Content_154(ContentTable):
    values = {0: "Default", 1: "VCD function"}

class RuleContent_154(RuleContentContextFree):
    variation = Content_154

class Variation_685(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_154

class RuleVariation_675(RuleVariationContextFree):
    variation = Variation_685

class NonSpare_1912(NonSpare):
    name = "VCD"
    title = ""
    rule = RuleVariation_675

class Item_1070(Item):
    non_spare = NonSpare_1912

class Content_81(ContentTable):
    values = {0: "Default", 1: "CHAM function"}

class RuleContent_81(RuleContentContextFree):
    variation = Content_81

class Variation_799(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_81

class RuleVariation_770(RuleVariationContextFree):
    variation = Variation_799

class NonSpare_683(NonSpare):
    name = "CHAM"
    title = ""
    rule = RuleVariation_770

class Item_149(Item):
    non_spare = NonSpare_683

class Content_88(ContentTable):
    values = {0: "Default", 1: "DSAM function"}

class RuleContent_88(RuleContentContextFree):
    variation = Content_88

class Variation_878(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_88

class RuleVariation_849(RuleVariationContextFree):
    variation = Variation_878

class NonSpare_850(NonSpare):
    name = "DSAM"
    title = ""
    rule = RuleVariation_849

class Item_267(Item):
    non_spare = NonSpare_850

class Content_85(ContentTable):
    values = {0: "Default", 1: "DBPSM ARR sub-function"}

class RuleContent_85(RuleContentContextFree):
    variation = Content_85

class Variation_26(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_85

class RuleVariation_26(RuleVariationContextFree):
    variation = Variation_26

class NonSpare_813(NonSpare):
    name = "DBPSMARR"
    title = ""
    rule = RuleVariation_26

class Item_235(Item):
    non_spare = NonSpare_813

class Content_86(ContentTable):
    values = {0: "Default", 1: "DBPSM DEP sub-function"}

class RuleContent_86(RuleContentContextFree):
    variation = Content_86

class Variation_396(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_86

class RuleVariation_386(RuleVariationContextFree):
    variation = Variation_396

class NonSpare_815(NonSpare):
    name = "DBPSMDEP"
    title = ""
    rule = RuleVariation_386

class Item_237(Item):
    non_spare = NonSpare_815

class Content_87(ContentTable):
    values = {0: "Default", 1: "DBPSM TL sub-function"}

class RuleContent_87(RuleContentContextFree):
    variation = Content_87

class Variation_497(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_87

class RuleVariation_487(RuleVariationContextFree):
    variation = Variation_497

class NonSpare_817(NonSpare):
    name = "DBPSMTL"
    title = ""
    rule = RuleVariation_487

class Item_239(Item):
    non_spare = NonSpare_817

class Content_156(ContentTable):
    values = {0: "Default", 1: "VRAM CRM sub-function"}

class RuleContent_156(RuleContentContextFree):
    variation = Content_156

class Variation_609(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_156

class RuleVariation_599(RuleVariationContextFree):
    variation = Variation_609

class NonSpare_1924(NonSpare):
    name = "VRAMCRM"
    title = ""
    rule = RuleVariation_599

class Item_1081(Item):
    non_spare = NonSpare_1924

class Content_158(ContentTable):
    values = {0: "Default", 1: "VRAM VTM sub-function"}

class RuleContent_158(RuleContentContextFree):
    variation = Content_158

class Variation_687(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_158

class RuleVariation_677(RuleVariationContextFree):
    variation = Variation_687

class NonSpare_1927(NonSpare):
    name = "VRAMVTM"
    title = ""
    rule = RuleVariation_677

class Item_1084(Item):
    non_spare = NonSpare_1927

class Content_157(ContentTable):
    values = {0: "Default", 1: "VRAM VRM sub-function"}

class RuleContent_157(RuleContentContextFree):
    variation = Content_157

class Variation_811(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_157

class RuleVariation_782(RuleVariationContextFree):
    variation = Variation_811

class NonSpare_1926(NonSpare):
    name = "VRAMVRM"
    title = ""
    rule = RuleVariation_782

class Item_1083(Item):
    non_spare = NonSpare_1926

class Content_96(ContentTable):
    values = {0: "Default", 1: "HAM HD sub-function"}

class RuleContent_96(RuleContentContextFree):
    variation = Content_96

class Variation_881(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_96

class RuleVariation_852(RuleVariationContextFree):
    variation = Variation_881

class NonSpare_986(NonSpare):
    name = "HAMHD"
    title = ""
    rule = RuleVariation_852

class Item_359(Item):
    non_spare = NonSpare_986

class Content_97(ContentTable):
    values = {0: "Default", 1: "HAM RD sub-function"}

class RuleContent_97(RuleContentContextFree):
    variation = Content_97

class Variation_28(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_97

class RuleVariation_28(RuleVariationContextFree):
    variation = Variation_28

class NonSpare_988(NonSpare):
    name = "HAMRD"
    title = ""
    rule = RuleVariation_28

class Item_361(Item):
    non_spare = NonSpare_988

class Content_98(ContentTable):
    values = {0: "Default", 1: "HAM VD sub-function"}

class RuleContent_98(RuleContentContextFree):
    variation = Content_98

class Variation_399(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_98

class RuleVariation_389(RuleVariationContextFree):
    variation = Variation_399

class NonSpare_990(NonSpare):
    name = "HAMVD"
    title = ""
    rule = RuleVariation_389

class Item_363(Item):
    non_spare = NonSpare_990

class Content_99(ContentTable):
    values = {0: "Default", 1: "HVI function"}

class RuleContent_99(RuleContentContextFree):
    variation = Content_99

class Variation_499(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_99

class RuleVariation_489(RuleVariationContextFree):
    variation = Variation_499

class NonSpare_1015(NonSpare):
    name = "HVI"
    title = ""
    rule = RuleVariation_489

class Item_382(Item):
    non_spare = NonSpare_1015

class Content_106(ContentTable):
    values = {0: "Default", 1: "LTW function"}

class RuleContent_106(RuleContentContextFree):
    variation = Content_106

class Variation_600(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_106

class RuleVariation_590(RuleVariationContextFree):
    variation = Variation_600

class NonSpare_1125(NonSpare):
    name = "LTW"
    title = ""
    rule = RuleVariation_590

class Item_478(Item):
    non_spare = NonSpare_1125

class Content_155(ContentTable):
    values = {0: "Default", 1: "VPM function"}

class RuleContent_155(RuleContentContextFree):
    variation = Content_155

class Variation_686(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_155

class RuleVariation_676(RuleVariationContextFree):
    variation = Variation_686

class NonSpare_1922(NonSpare):
    name = "VPM"
    title = ""
    rule = RuleVariation_676

class Item_1079(Item):
    non_spare = NonSpare_1922

class Content_148(ContentTable):
    values = {0: "Default", 1: "TTA function"}

class RuleContent_148(RuleContentContextFree):
    variation = Content_148

class Variation_809(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_148

class RuleVariation_780(RuleVariationContextFree):
    variation = Variation_809

class NonSpare_1846(NonSpare):
    name = "TTA"
    title = ""
    rule = RuleVariation_780

class Item_1008(Item):
    non_spare = NonSpare_1846

class Content_83(ContentTable):
    values = {0: "Default", 1: "CRA function"}

class RuleContent_83(RuleContentContextFree):
    variation = Content_83

class Variation_876(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_83

class RuleVariation_847(RuleVariationContextFree):
    variation = Variation_876

class NonSpare_776(NonSpare):
    name = "CRA"
    title = ""
    rule = RuleVariation_847

class Item_206(Item):
    non_spare = NonSpare_776

class Content_78(ContentTable):
    values = {0: "Default", 1: "ASM sub-function"}

class RuleContent_78(RuleContentContextFree):
    variation = Content_78

class Variation_24(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_78

class RuleVariation_24(RuleVariationContextFree):
    variation = Variation_24

class NonSpare_590(NonSpare):
    name = "ASM"
    title = ""
    rule = RuleVariation_24

class Item_82(Item):
    non_spare = NonSpare_590

class Content_101(ContentTable):
    values = {0: "Default", 1: "IAVM sub-function"}

class RuleContent_101(RuleContentContextFree):
    variation = Content_101

class Variation_400(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_101

class RuleVariation_390(RuleVariationContextFree):
    variation = Variation_400

class NonSpare_1035(NonSpare):
    name = "IAVM"
    title = ""
    rule = RuleVariation_390

class Item_398(Item):
    non_spare = NonSpare_1035

class Content_91(ContentTable):
    values = {0: "Default", 1: "FTD Function"}

class RuleContent_91(RuleContentContextFree):
    variation = Content_91

class Variation_498(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_91

class RuleVariation_488(RuleVariationContextFree):
    variation = Variation_498

class NonSpare_939(NonSpare):
    name = "FTD"
    title = ""
    rule = RuleVariation_488

class Item_329(Item):
    non_spare = NonSpare_939

class Content_103(ContentTable):
    values = {0: "Default", 1: "ITD function"}

class RuleContent_103(RuleContentContextFree):
    variation = Content_103

class Variation_598(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_103

class RuleVariation_588(RuleVariationContextFree):
    variation = Variation_598

class NonSpare_1056(NonSpare):
    name = "ITD"
    title = ""
    rule = RuleVariation_588

class Item_413(Item):
    non_spare = NonSpare_1056

class Content_102(ContentTable):
    values = {0: "Default", 1: "IIA function"}

class RuleContent_102(RuleContentContextFree):
    variation = Content_102

class Variation_680(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_102

class RuleVariation_670(RuleVariationContextFree):
    variation = Variation_680

class NonSpare_1049(NonSpare):
    name = "IIA"
    title = ""
    rule = RuleVariation_670

class Item_408(Item):
    non_spare = NonSpare_1049

class Content_143(ContentTable):
    values = {0: "Default", 1: "SQW function"}

class RuleContent_143(RuleContentContextFree):
    variation = Content_143

class Variation_806(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_143

class RuleVariation_777(RuleVariationContextFree):
    variation = Variation_806

class NonSpare_1652(NonSpare):
    name = "SQW"
    title = ""
    rule = RuleVariation_777

class Item_872(Item):
    non_spare = NonSpare_1652

class Content_84(ContentTable):
    values = {0: "Default", 1: "CUW function"}

class RuleContent_84(RuleContentContextFree):
    variation = Content_84

class Variation_877(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_84

class RuleVariation_848(RuleVariationContextFree):
    variation = Variation_877

class NonSpare_799(NonSpare):
    name = "CUW"
    title = ""
    rule = RuleVariation_848

class Item_224(Item):
    non_spare = NonSpare_799

class Content_80(ContentTable):
    values = {0: "Default", 1: "CATC function"}

class RuleContent_80(RuleContentContextFree):
    variation = Content_80

class Variation_25(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_80

class RuleVariation_25(RuleVariationContextFree):
    variation = Variation_25

class NonSpare_659(NonSpare):
    name = "CATC"
    title = ""
    rule = RuleVariation_25

class Item_132(Item):
    non_spare = NonSpare_659

class Content_124(ContentTable):
    values = {0: "Default", 1: "NOCLR sub-function"}

class RuleContent_124(RuleContentContextFree):
    variation = Content_124

class Variation_404(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_124

class RuleVariation_394(RuleVariationContextFree):
    variation = Variation_404

class NonSpare_1285(NonSpare):
    name = "NOCLR"
    title = ""
    rule = RuleVariation_394

class Item_590(Item):
    non_spare = NonSpare_1285

class Content_126(ContentTable):
    values = {0: "Default", 1: "NOMOV Function"}

class RuleContent_126(RuleContentContextFree):
    variation = Content_126

class Variation_502(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_126

class RuleVariation_492(RuleVariationContextFree):
    variation = Variation_502

class NonSpare_1298(NonSpare):
    name = "NOMOV"
    title = ""
    rule = RuleVariation_492

class Item_603(Item):
    non_spare = NonSpare_1298

class Content_125(ContentTable):
    values = {0: "Default", 1: "NOH function"}

class RuleContent_125(RuleContentContextFree):
    variation = Content_125

class Variation_605(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_125

class RuleVariation_595(RuleVariationContextFree):
    variation = Variation_605

class NonSpare_1293(NonSpare):
    name = "NOH"
    title = ""
    rule = RuleVariation_595

class Item_598(Item):
    non_spare = NonSpare_1293

class Content_159(ContentTable):
    values = {0: "Default", 1: "WRTY function"}

class RuleContent_159(RuleContentContextFree):
    variation = Content_159

class Variation_688(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_159

class RuleVariation_678(RuleVariationContextFree):
    variation = Variation_688

class NonSpare_1948(NonSpare):
    name = "WRTY"
    title = ""
    rule = RuleVariation_678

class Item_1100(Item):
    non_spare = NonSpare_1948

class Content_145(ContentTable):
    values = {0: "Default", 1: "STOCC function"}

class RuleContent_145(RuleContentContextFree):
    variation = Content_145

class Variation_807(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_145

class RuleVariation_778(RuleVariationContextFree):
    variation = Variation_807

class NonSpare_1702(NonSpare):
    name = "STOCC"
    title = ""
    rule = RuleVariation_778

class Item_909(Item):
    non_spare = NonSpare_1702

class Content_129(ContentTable):
    values = {0: "Default", 1: "ONGOING function"}

class RuleContent_129(RuleContentContextFree):
    variation = Content_129

class Variation_883(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_129

class RuleVariation_854(RuleVariationContextFree):
    variation = Variation_883

class NonSpare_1319(NonSpare):
    name = "ONGOING"
    title = ""
    rule = RuleVariation_854

class Item_623(Item):
    non_spare = NonSpare_1319

class Variation_1303(Extended):
    items = [Item_552, Item_738, Item_737, Item_555, Item_74, Item_156, Item_903, None, Item_73, Item_781, Item_40, Item_607, Item_252, Item_620, Item_621, None, Item_55, Item_643, Item_614, Item_814, Item_1070, Item_149, Item_267, None, Item_235, Item_237, Item_239, Item_1081, Item_1084, Item_1083, Item_359, None, Item_361, Item_363, Item_382, Item_478, Item_1079, Item_1008, Item_206, None, Item_82, Item_398, Item_329, Item_413, Item_408, Item_872, Item_224, None, Item_132, Item_590, Item_603, Item_598, Item_1100, Item_909, Item_623, None]

class RuleVariation_1233(RuleVariationContextFree):
    variation = Variation_1303

class NonSpare_183(NonSpare):
    name = "060"
    title = "Safety Net Function and System Status"
    rule = RuleVariation_1233

class NonSpare_108(NonSpare):
    name = "030"
    title = "Track Number 1"
    rule = RuleVariation_240

class Content_581(ContentString):
    string_type = StringAscii

class RuleContent_581(RuleContentContextFree):
    variation = Content_581

class Variation_385(Element):
    bit_offset8 = 0
    bit_size = 56
    rule = RuleContent_581

class RuleVariation_378(RuleVariationContextFree):
    variation = Variation_385

class NonSpare_547(NonSpare):
    name = "AI1"
    title = "Aircraft Identifier (in 7 Characters) of Aircraft 1 Involved in the Conflict"
    rule = RuleVariation_378

class NonSpare_1230(NonSpare):
    name = "MODE3A"
    title = "Mode-3/A Code (Converted Into Octal Representation) of Aircraft 1 Involved in the Conflict"
    rule = RuleVariation_751

class Item_541(Item):
    non_spare = NonSpare_1230

class Variation_992(Group):
    bit_size = 16
    items_list = [Item_3, Item_541]
    items_dict = {"MODE3A": NonSpare_1230}

class RuleVariation_962(RuleVariationContextFree):
    variation = Variation_992

class NonSpare_1137(NonSpare):
    name = "M31"
    title = "Mode 3/A Code Aircraft 1"
    rule = RuleVariation_962

class Content_670(ContentQuantity):
    signedness = Signed
    lsb = 5.364418029785156e-6
    unit = "°"

class RuleContent_670(RuleContentContextFree):
    variation = Content_670

class Variation_370(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_670

class RuleVariation_364(RuleVariationContextFree):
    variation = Variation_370

class NonSpare_1074(NonSpare):
    name = "LAT"
    title = "In WGS-84 in Two’s Complement"
    rule = RuleVariation_364

class Item_431(Item):
    non_spare = NonSpare_1074

class Content_669(ContentQuantity):
    signedness = Signed
    lsb = 5.364418029785156e-6
    unit = "°"

class RuleContent_669(RuleContentContextFree):
    variation = Content_669

class Variation_369(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_669

class RuleVariation_363(RuleVariationContextFree):
    variation = Variation_369

class NonSpare_1101(NonSpare):
    name = "LON"
    title = "In WGS-84 in Two’s Complement"
    rule = RuleVariation_363

class Item_456(Item):
    non_spare = NonSpare_1101

class Content_606(ContentQuantity):
    signedness = Signed
    lsb = 25.0
    unit = "ft"

class RuleContent_606(RuleContentContextFree):
    variation = Content_606

class Variation_254(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_606

class RuleVariation_248(RuleVariationContextFree):
    variation = Variation_254

class NonSpare_557(NonSpare):
    name = "ALT"
    title = "Altitude of Predicted Conflict"
    rule = RuleVariation_248

class Item_62(Item):
    non_spare = NonSpare_557

class Variation_1089(Group):
    bit_size = 80
    items_list = [Item_431, Item_456, Item_62]
    items_dict = {"LAT": NonSpare_1074, "LON": NonSpare_1101, "ALT": NonSpare_557}

class RuleVariation_1048(RuleVariationContextFree):
    variation = Variation_1089

class NonSpare_772(NonSpare):
    name = "CPW"
    title = "Predicted Conflict Position Target 1 in WGS-84 Coordinates"
    rule = RuleVariation_1048

class Content_611(ContentQuantity):
    signedness = Signed
    lsb = 0.5
    unit = "m"

class RuleContent_611(RuleContentContextFree):
    variation = Content_611

class Variation_344(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_611

class RuleVariation_338(RuleVariationContextFree):
    variation = Variation_344

class NonSpare_1976(NonSpare):
    name = "X"
    title = "Starting X-position of the Conflict"
    rule = RuleVariation_338

class Item_1125(Item):
    non_spare = NonSpare_1976

class NonSpare_2028(NonSpare):
    name = "Y"
    title = "Starting Y-position of the Conflict"
    rule = RuleVariation_338

class Item_1174(Item):
    non_spare = NonSpare_2028

class NonSpare_2045(NonSpare):
    name = "Z"
    title = "Starting Z-position of the Conflict"
    rule = RuleVariation_248

class Item_1191(Item):
    non_spare = NonSpare_2045

class Variation_1261(Group):
    bit_size = 64
    items_list = [Item_1125, Item_1174, Item_1191]
    items_dict = {"X": NonSpare_1976, "Y": NonSpare_2028, "Z": NonSpare_2045}

class RuleVariation_1195(RuleVariationContextFree):
    variation = Variation_1261

class NonSpare_768(NonSpare):
    name = "CPC"
    title = "Predicted Conflict Position for the Aircraft 1 Involved in the Conflict"
    rule = RuleVariation_1195

class NonSpare_1844(NonSpare):
    name = "TT1"
    title = "Time to Runway Threshold for First Approaching Aircraft in a RIMCA"
    rule = RuleVariation_356

class Content_703(ContentQuantity):
    signedness = Unsigned
    lsb = 0.5
    unit = "m"

class RuleContent_703(RuleContentContextFree):
    variation = Content_703

class Variation_302(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_703

class RuleVariation_296(RuleVariationContextFree):
    variation = Variation_302

class NonSpare_852(NonSpare):
    name = "DT1"
    title = "Distance to Runway Threshold for Aircraft 1 Involved in a RIMCA"
    rule = RuleVariation_296

class Content_524(ContentTable):
    values = {0: "Unknown", 1: "General Air Traffic", 2: "Operational Air Traffic", 3: "Not applicable"}

class RuleContent_524(RuleContentContextFree):
    variation = Content_524

class Variation_111(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_524

class RuleVariation_111(RuleVariationContextFree):
    variation = Variation_111

class NonSpare_959(NonSpare):
    name = "GATOAT"
    title = "Identification of Conflict Categories Definition Table"
    rule = RuleVariation_111

class Item_341(Item):
    non_spare = NonSpare_959

class Content_236(ContentTable):
    values = {0: "Instrument Flight Rules", 1: "Visual Flight rules", 2: "Not applicable", 3: "Controlled Visual Flight Rules"}

class RuleContent_236(RuleContentContextFree):
    variation = Content_236

class Variation_569(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_236

class RuleVariation_559(RuleVariationContextFree):
    variation = Variation_569

class NonSpare_929(NonSpare):
    name = "FR1FR2"
    title = "Flight Rules"
    rule = RuleVariation_559

class Item_322(Item):
    non_spare = NonSpare_929

class Content_517(ContentTable):
    values = {0: "Unknown", 1: "Approved", 2: "Exempt", 3: "Not Approved"}

class RuleContent_517(RuleContentContextFree):
    variation = Content_517

class Variation_740(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_517

class RuleVariation_730(RuleVariationContextFree):
    variation = Variation_740

class NonSpare_1547(NonSpare):
    name = "RVSM"
    title = ""
    rule = RuleVariation_730

class Item_799(Item):
    non_spare = NonSpare_1547

class Content_367(ContentTable):
    values = {0: "Normal Priority Flight", 1: "High Priority Flight"}

class RuleContent_367(RuleContentContextFree):
    variation = Content_367

class Variation_910(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_367

class RuleVariation_881(RuleVariationContextFree):
    variation = Variation_910

class NonSpare_1007(NonSpare):
    name = "HPR"
    title = ""
    rule = RuleVariation_881

class Item_376(Item):
    non_spare = NonSpare_1007

class Content_251(ContentTable):
    values = {0: "Maintaining", 1: "Climbing", 2: "Descending", 3: "Invalid"}

class RuleContent_251(RuleContentContextFree):
    variation = Content_251

class Variation_99(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_251

class RuleVariation_99(RuleVariationContextFree):
    variation = Variation_99

class NonSpare_667(NonSpare):
    name = "CDM"
    title = "Climbing/Descending Mode"
    rule = RuleVariation_99

class Item_137(Item):
    non_spare = NonSpare_667

class Content_363(ContentTable):
    values = {0: "Non primary target", 1: "Primary target"}

class RuleContent_363(RuleContentContextFree):
    variation = Content_363

class Variation_542(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_363

class RuleVariation_532(RuleVariationContextFree):
    variation = Variation_542

class NonSpare_1381(NonSpare):
    name = "PRI"
    title = ""
    rule = RuleVariation_532

class Item_669(Item):
    non_spare = NonSpare_1381

class Content_95(ContentTable):
    values = {0: "Default", 1: "Ground Vehicle"}

class RuleContent_95(RuleContentContextFree):
    variation = Content_95

class Variation_597(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_95

class RuleVariation_587(RuleVariationContextFree):
    variation = Variation_597

class NonSpare_978(NonSpare):
    name = "GV"
    title = ""
    rule = RuleVariation_587

class Item_356(Item):
    non_spare = NonSpare_978

class Item_21(Spare):
    bit_offset8 = 4
    bit_size = 3

class Variation_1293(Extended):
    items = [Item_341, Item_322, Item_799, Item_376, None, Item_137, Item_669, Item_356, Item_21, None]

class RuleVariation_1223(RuleVariationContextFree):
    variation = Variation_1293

class NonSpare_520(NonSpare):
    name = "AC1"
    title = "Characteristics of Aircraft 1 Involved in the Conflict"
    rule = RuleVariation_1223

class Variation_382(Element):
    bit_offset8 = 0
    bit_size = 48
    rule = RuleContent_581

class RuleVariation_375(RuleVariationContextFree):
    variation = Variation_382

class NonSpare_1244(NonSpare):
    name = "MS1"
    title = "Aircraft Identification Downloaded from Aircraft 1 Involved in the Conflict If Equipped with a Mode-S Transponder"
    rule = RuleVariation_375

class Item_4(Spare):
    bit_offset8 = 0
    bit_size = 5

class Content_681(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = ""

class RuleContent_681(RuleContentContextFree):
    variation = Content_681

class Variation_866(Element):
    bit_offset8 = 5
    bit_size = 27
    rule = RuleContent_681

class RuleVariation_837(RuleVariationContextFree):
    variation = Variation_866

class NonSpare_1275(NonSpare):
    name = "NBR"
    title = ""
    rule = RuleVariation_837

class Item_580(Item):
    non_spare = NonSpare_1275

class Variation_1003(Group):
    bit_size = 32
    items_list = [Item_4, Item_580]
    items_dict = {"NBR": NonSpare_1275}

class RuleVariation_972(RuleVariationContextFree):
    variation = Variation_1003

class NonSpare_920(NonSpare):
    name = "FP1"
    title = "Number of the Flight Plan Correlated to Aircraft 1 Involved in the Conflict"
    rule = RuleVariation_972

class Content_723(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "FL"

class RuleContent_722(RuleContentContextFree):
    variation = Content_723

class Variation_312(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_722

class RuleVariation_306(RuleVariationContextFree):
    variation = Variation_312

class NonSpare_675(NonSpare):
    name = "CF1"
    title = "Cleared Flight Level for Aircraft 1 Involved in the Conflict"
    rule = RuleVariation_306

class Variation_1388(Compound):
    items_list = [NonSpare_547, NonSpare_1137, NonSpare_772, NonSpare_768, NonSpare_1844, NonSpare_852, NonSpare_520, NonSpare_1244, NonSpare_920, NonSpare_675]
    items_dict = {"AI1": NonSpare_547, "M31": NonSpare_1137, "CPW": NonSpare_772, "CPC": NonSpare_768, "TT1": NonSpare_1844, "DT1": NonSpare_852, "AC1": NonSpare_520, "MS1": NonSpare_1244, "FP1": NonSpare_920, "CF1": NonSpare_675}

class RuleVariation_1318(RuleVariationContextFree):
    variation = Variation_1388

class NonSpare_343(NonSpare):
    name = "170"
    title = "Aircraft Identification and Characteristics 1"
    rule = RuleVariation_1318

class Content_63(ContentTable):
    values = {0: "Conflict not predicted to occur in military airspace", 1: "Conflict predicted to occur in military airspace"}

class RuleContent_63(RuleContentContextFree):
    variation = Content_63

class Variation_19(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_63

class RuleVariation_19(RuleVariationContextFree):
    variation = Variation_19

class NonSpare_1153(NonSpare):
    name = "MAS"
    title = "Conflict Location in Military Airspace"
    rule = RuleVariation_19

class Item_497(Item):
    non_spare = NonSpare_1153

class Content_62(ContentTable):
    values = {0: "Conflict not predicted to occur in civil airspace", 1: "Conflict predicted to occur in civil airspace"}

class RuleContent_62(RuleContentContextFree):
    variation = Content_62

class Variation_394(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_62

class RuleVariation_384(RuleVariationContextFree):
    variation = Variation_394

class NonSpare_657(NonSpare):
    name = "CAS"
    title = "Conflict Location in Civil Airspace"
    rule = RuleVariation_384

class Item_130(Item):
    non_spare = NonSpare_657

class Content_24(ContentTable):
    values = {0: "Aircraft are not fast diverging laterally at current time", 1: "Aircraft are fast diverging laterally at current time"}

class RuleContent_24(RuleContentContextFree):
    variation = Content_24

class Variation_491(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_24

class RuleVariation_481(RuleVariationContextFree):
    variation = Variation_491

class NonSpare_910(NonSpare):
    name = "FLD"
    title = "Fast Lateral Divergence"
    rule = RuleVariation_481

class Item_310(Item):
    non_spare = NonSpare_910

class Content_25(ContentTable):
    values = {0: "Aircraft are not fast diverging vertically at current time", 1: "Aircraft are fast diverging vertically at current time"}

class RuleContent_25(RuleContentContextFree):
    variation = Content_25

class Variation_592(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_25

class RuleVariation_582(RuleVariationContextFree):
    variation = Variation_592

class NonSpare_942(NonSpare):
    name = "FVD"
    title = "Fast Vertical Divergence"
    rule = RuleVariation_582

class Item_332(Item):
    non_spare = NonSpare_942

class Content_258(ContentTable):
    values = {0: "Minor separation infringement", 1: "Major separation infringement"}

class RuleContent_258(RuleContentContextFree):
    variation = Content_258

class Variation_705(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_258

class RuleVariation_695(RuleVariationContextFree):
    variation = Variation_705

class NonSpare_1875(NonSpare):
    name = "TYPE"
    title = "Type of Separation Infringement"
    rule = RuleVariation_695

class Item_1035(Item):
    non_spare = NonSpare_1875

class Content_27(ContentTable):
    values = {0: "Aircraft have not crossed at starting time of conflict", 1: "Aircraft have crossed at starting time of conflict"}

class RuleContent_27(RuleContentContextFree):
    variation = Content_27

class Variation_792(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_27

class RuleVariation_763(RuleVariationContextFree):
    variation = Variation_792

class NonSpare_777(NonSpare):
    name = "CROSS"
    title = "Crossing Test"
    rule = RuleVariation_763

class Item_207(Item):
    non_spare = NonSpare_777

class Content_23(ContentTable):
    values = {0: "Aircraft are not diverging at starting time of conflict", 1: "Aircraft are diverging at starting time of conflict"}

class RuleContent_23(RuleContentContextFree):
    variation = Content_23

class Variation_870(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_23

class RuleVariation_841(RuleVariationContextFree):
    variation = Variation_870

class NonSpare_834(NonSpare):
    name = "DIV"
    title = "Divergence Test"
    rule = RuleVariation_841

class Item_255(Item):
    non_spare = NonSpare_834

class Content_140(ContentTable):
    values = {0: "Default", 1: "Runway/Runway Crossing"}

class RuleContent_140(RuleContentContextFree):
    variation = Content_140

class Variation_33(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_140

class RuleVariation_33(RuleVariationContextFree):
    variation = Variation_33

class NonSpare_1524(NonSpare):
    name = "RRC"
    title = "Runway/Runway Crossing in RIMCAS"
    rule = RuleVariation_33

class Item_788(Item):
    non_spare = NonSpare_1524

class Content_141(ContentTable):
    values = {0: "Default", 1: "Runway/Taxiway Crossing"}

class RuleContent_141(RuleContentContextFree):
    variation = Content_141

class Variation_408(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_141

class RuleVariation_398(RuleVariationContextFree):
    variation = Variation_408

class NonSpare_1544(NonSpare):
    name = "RTC"
    title = "Runway/Taxiway Crossing in RIMCAS"
    rule = RuleVariation_398

class Item_797(Item):
    non_spare = NonSpare_1544

class Content_122(ContentTable):
    values = {0: "Default", 1: "Msg Type 4 (MSAW) indicates MRVA"}

class RuleContent_122(RuleContentContextFree):
    variation = Content_122

class Variation_501(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_122

class RuleVariation_491(RuleVariationContextFree):
    variation = Variation_501

class NonSpare_1242(NonSpare):
    name = "MRVA"
    title = ""
    rule = RuleVariation_491

class Item_553(Item):
    non_spare = NonSpare_1242

class Content_116(ContentTable):
    values = {0: "Default", 1: "Msg Type 25 (VRAM) indicates CRM"}

class RuleContent_116(RuleContentContextFree):
    variation = Content_116

class Variation_604(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_116

class RuleVariation_594(RuleVariationContextFree):
    variation = Variation_604

class NonSpare_1923(NonSpare):
    name = "VRAMCRM"
    title = ""
    rule = RuleVariation_594

class Item_1080(Item):
    non_spare = NonSpare_1923

class Content_117(ContentTable):
    values = {0: "Default", 1: "Msg Type 25 (VRAM) indicates VRM"}

class RuleContent_117(RuleContentContextFree):
    variation = Content_117

class Variation_683(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_117

class RuleVariation_673(RuleVariationContextFree):
    variation = Variation_683

class NonSpare_1925(NonSpare):
    name = "VRAMVRM"
    title = ""
    rule = RuleVariation_673

class Item_1082(Item):
    non_spare = NonSpare_1925

class Content_118(ContentTable):
    values = {0: "Default", 1: "Msg Type 25 (VRAM) indicates VTM"}

class RuleContent_118(RuleContentContextFree):
    variation = Content_118

class Variation_802(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_118

class RuleVariation_773(RuleVariationContextFree):
    variation = Variation_802

class NonSpare_1928(NonSpare):
    name = "VRAMVTM"
    title = ""
    rule = RuleVariation_773

class Item_1085(Item):
    non_spare = NonSpare_1928

class Content_119(ContentTable):
    values = {0: "Default", 1: "Msg Type 29 (HAM) indicates HD"}

class RuleContent_119(RuleContentContextFree):
    variation = Content_119

class Variation_882(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_119

class RuleVariation_853(RuleVariationContextFree):
    variation = Variation_882

class NonSpare_987(NonSpare):
    name = "HAMHD"
    title = ""
    rule = RuleVariation_853

class Item_360(Item):
    non_spare = NonSpare_987

class Content_120(ContentTable):
    values = {0: "Default", 1: "Msg Type 29 (HAM) indicates RD"}

class RuleContent_120(RuleContentContextFree):
    variation = Content_120

class Variation_32(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_120

class RuleVariation_32(RuleVariationContextFree):
    variation = Variation_32

class NonSpare_989(NonSpare):
    name = "HAMRD"
    title = ""
    rule = RuleVariation_32

class Item_362(Item):
    non_spare = NonSpare_989

class Content_121(ContentTable):
    values = {0: "Default", 1: "Msg Type 29 (HAM) indicates VD"}

class RuleContent_121(RuleContentContextFree):
    variation = Content_121

class Variation_403(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_121

class RuleVariation_393(RuleVariationContextFree):
    variation = Variation_403

class NonSpare_991(NonSpare):
    name = "HAMVD"
    title = ""
    rule = RuleVariation_393

class Item_364(Item):
    non_spare = NonSpare_991

class Content_113(ContentTable):
    values = {0: "Default", 1: "Msg Type 20 (DBPSM) indicates ARR"}

class RuleContent_113(RuleContentContextFree):
    variation = Content_113

class Variation_500(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_113

class RuleVariation_490(RuleVariationContextFree):
    variation = Variation_500

class NonSpare_814(NonSpare):
    name = "DBPSMARR"
    title = ""
    rule = RuleVariation_490

class Item_236(Item):
    non_spare = NonSpare_814

class Content_114(ContentTable):
    values = {0: "Default", 1: "Msg Type 20 (DBPSM) indicates DEP"}

class RuleContent_114(RuleContentContextFree):
    variation = Content_114

class Variation_603(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_114

class RuleVariation_593(RuleVariationContextFree):
    variation = Variation_603

class NonSpare_816(NonSpare):
    name = "DBPSMDEP"
    title = ""
    rule = RuleVariation_593

class Item_238(Item):
    non_spare = NonSpare_816

class Content_115(ContentTable):
    values = {0: "Default", 1: "Msg Type 20 (DBPSM) indicates above TL"}

class RuleContent_115(RuleContentContextFree):
    variation = Content_115

class Variation_682(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_115

class RuleVariation_672(RuleVariationContextFree):
    variation = Variation_682

class NonSpare_818(NonSpare):
    name = "DBPSMTL"
    title = ""
    rule = RuleVariation_672

class Item_240(Item):
    non_spare = NonSpare_818

class Content_123(ContentTable):
    values = {0: "Default", 1: "Msg Type 99 (AIW) indicates pAIW Alert"}

class RuleContent_123(RuleContentContextFree):
    variation = Content_123

class Variation_803(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_123

class RuleVariation_774(RuleVariationContextFree):
    variation = Variation_803

class NonSpare_551(NonSpare):
    name = "AIW"
    title = ""
    rule = RuleVariation_774

class Item_56(Item):
    non_spare = NonSpare_551

class Item_26(Spare):
    bit_offset8 = 6
    bit_size = 1

class Variation_1295(Extended):
    items = [Item_497, Item_130, Item_310, Item_332, Item_1035, Item_207, Item_255, None, Item_788, Item_797, Item_553, Item_1080, Item_1082, Item_1085, Item_360, None, Item_362, Item_364, Item_236, Item_238, Item_240, Item_56, Item_26, None]

class RuleVariation_1225(RuleVariationContextFree):
    variation = Variation_1295

class NonSpare_696(NonSpare):
    name = "CN"
    title = "Conflict Nature"
    rule = RuleVariation_1225

class Variation_127(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_0

class RuleVariation_127(RuleVariationContextFree):
    variation = Variation_127

class NonSpare_1768(NonSpare):
    name = "TID"
    title = "Identification of Conflict Categories Definition Table"
    rule = RuleVariation_127

class Item_952(Item):
    non_spare = NonSpare_1768

class Content_10(ContentTable):
    values = {0: "APW Low Severity", 1: "APW Medium Severity", 2: "APW High Severity"}

class RuleContent_10(RuleContentContextFree):
    variation = Content_10

class Variation_748(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_10

class Content_557(ContentTable):
    values = {1: "Major seperation infringement and not (crossed and diverging)", 2: "Minor seperation infringement and not (crossed and diverging)", 3: "Major seperation infringement and (crossed and diverging)", 4: "Minor seperation infringement and (crossed and diverging)"}

class RuleContent_557(RuleContentContextFree):
    variation = Content_557

class Variation_769(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_557

class Content_206(ContentTable):
    values = {0: "Filter not set", 1: "Filter set"}

class RuleContent_206(RuleContentContextFree):
    variation = Content_206

class Variation_697(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_206

class RuleVariation_687(RuleVariationContextFree):
    variation = Variation_697

class NonSpare_1120(NonSpare):
    name = "LPF"
    title = "Linear Prediction Filter"
    rule = RuleVariation_687

class Item_473(Item):
    non_spare = NonSpare_1120

class Variation_821(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_206

class RuleVariation_792(RuleVariationContextFree):
    variation = Variation_821

class NonSpare_769(NonSpare):
    name = "CPF"
    title = "Current Proximity Filter"
    rule = RuleVariation_792

class Item_202(Item):
    non_spare = NonSpare_769

class Variation_895(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_206

class RuleVariation_866(RuleVariationContextFree):
    variation = Variation_895

class NonSpare_1202(NonSpare):
    name = "MHF"
    title = "Manoeuvre Hazard Filter"
    rule = RuleVariation_866

class Item_517(Item):
    non_spare = NonSpare_1202

class Variation_1101(Group):
    bit_size = 3
    items_list = [Item_473, Item_202, Item_517]
    items_dict = {"LPF": NonSpare_1120, "CPF": NonSpare_769, "MHF": NonSpare_1202}

class Content_435(ContentTable):
    values = {0: "Stage One Alert", 1: "Stage Two Alert"}

class RuleContent_435(RuleContentContextFree):
    variation = Content_435

class Variation_723(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_435

class RuleVariation_713(RuleVariationContextFree):
    variation = Variation_723

class NonSpare_1465(NonSpare):
    name = "RAS"
    title = "RIMCAS Alert Stage"
    rule = RuleVariation_713

class Item_739(Item):
    non_spare = NonSpare_1465

class Variation_1132(Group):
    bit_size = 3
    items_list = [Item_739, Item_24]
    items_dict = {"RAS": NonSpare_1465}

class Content_4(ContentTable):
    values = {0: "2 aircraft, same taxiway, opposite direction", 1: "Aircraft entering wrong direction", 2: "Aircraft entering wrong taxiway", 3: "Speed violation"}

class RuleContent_4(RuleContentContextFree):
    variation = Content_4

class Variation_747(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_4

class Content_532(ContentTable):
    values = {0: "VRM Slow Climb", 1: "VRM Slow Descent"}

class RuleContent_532(RuleContentContextFree):
    variation = Content_532

class Variation_766(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_532

class Content_533(ContentTable):
    values = {0: "VTM Fast Climb", 1: "VTM Fast Descent"}

class RuleContent_533(RuleContentContextFree):
    variation = Content_533

class Variation_767(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_533

class Content_538(ContentTable):
    values = {0: "Vertical manoeuvre deviation prior to reaching its expected level", 1: "Vertical manoeuvre deviation past its expected level"}

class RuleContent_538(RuleContentContextFree):
    variation = Content_538

class Variation_768(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_538

class Content_434(ContentTable):
    values = {0: "Slow Descent", 1: "Fast Descent", 2: "Slow Climb", 3: "Fast Climb"}

class RuleContent_434(RuleContentContextFree):
    variation = Content_434

class Variation_761(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_434

class Content_11(ContentTable):
    values = {0: "Above", 1: "Below"}

class RuleContent_11(RuleContentContextFree):
    variation = Content_11

class Variation_749(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_11

class Content_454(ContentTable):
    values = {0: "Table - Single RWY Operation", 1: "MRS - Single RWY Operation", 2: "ROT - Single RWY Operation", 3: "GAP - Single RWY Operation", 4: "Table - Parallel RWY Operation", 5: "MRS - Parallel RWY Operation", 6: "ROT - Parallel RWY Operation", 7: "GAP - Parallel RWY Operation"}

class RuleContent_454(RuleContentContextFree):
    variation = Content_454

class Variation_762(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_454

class Content_202(ContentTable):
    values = {0: "End of Alert", 1: "Planned Alert", 2: "Alert on TABLE Indicator", 3: "Alert on MRS Indicator", 4: "Alert on ROT Indicator", 5: "Alert on GAP Indicator"}

class RuleContent_202(RuleContentContextFree):
    variation = Content_202

class Variation_753(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_202

class Content_245(ContentTable):
    values = {0: "Line-Up vs. Line-Up", 1: "Line-Up vs. Cross or Enter", 2: "Line-Up vs. Take-Off", 3: "Line-Up vs. Landing"}

class RuleContent_245(RuleContentContextFree):
    variation = Content_245

class Variation_755(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_245

class Content_67(ContentTable):
    values = {0: "Cross or Enter  vs. Line-Up", 1: "Cross or Enter  vs. Cross or Enter", 2: "Cross or Enter  vs. Take-Off", 3: "Cross or Enter  vs. Landing"}

class RuleContent_67(RuleContentContextFree):
    variation = Content_67

class Variation_752(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_67

class Content_455(ContentTable):
    values = {0: "Take-Off vs. Line-Up", 1: "Take-Off vs. Cross or Enter", 2: "Take-Off vs. Take-Off", 3: "Take-Off vs. Landing"}

class RuleContent_455(RuleContentContextFree):
    variation = Content_455

class Variation_763(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_455

class Content_242(ContentTable):
    values = {0: "Landing vs. Line-Up", 1: "Landing vs. Cross or Enter", 2: "Landing vs. Take-Off", 3: "Landing vs. Landing"}

class RuleContent_242(RuleContentContextFree):
    variation = Content_242

class Variation_754(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_242

class Content_409(ContentTable):
    values = {0: "Push-Back vs. Push-Back", 1: "Push-Back vs. Taxi"}

class RuleContent_409(RuleContentContextFree):
    variation = Content_409

class Variation_760(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_409

class Content_462(ContentTable):
    values = {0: "Taxi vs. Push-Back", 1: "TAxi vs. Taxi"}

class RuleContent_462(RuleContentContextFree):
    variation = Content_462

class Variation_764(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_462

class Content_300(ContentTable):
    values = {0: "No Push-Back Clearance", 1: "No Taxi Clearance", 2: "No Line-Up Clearance", 3: "No Crossing Clearance", 4: "No Enter Clearance", 5: "No Take-Off Clearance", 6: "Landing Clearance"}

class RuleContent_300(RuleContentContextFree):
    variation = Content_300

class Variation_756(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_300

class Content_20(ContentTable):
    values = {0: "After Push-Back Clearance", 1: "After Taxi Clearance", 2: "After Line-Up Clearance", 3: "After Crossing Clearance", 4: "After Enter Clearance", 5: "After Take-Off Clearance", 6: "Stationary on Runway", 7: "Stationary on Taxiway"}

class RuleContent_20(RuleContentContextFree):
    variation = Content_20

class Variation_751(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_20

class Content_329(ContentTable):
    values = {0: "No contact (receiving ATSU)", 1: "No transfer (leaving ATSU)"}

class RuleContent_329(RuleContentContextFree):
    variation = Content_329

class Variation_757(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_329

class RuleVariation_1366(RuleVariationDependent):
    depends_on = [["000"], ["120", "CC", "TID"]]
    default_variation = Variation_745
    cases = [
        ([5, 1], Variation_748),
        ([7, 0], Variation_769),
        ([7, 1], Variation_1101),
        ([9, 2], Variation_1132),
        ([10, 2], Variation_1132),
        ([11, 2], Variation_1132),
        ([12, 2], Variation_1132),
        ([13, 2], Variation_1132),
        ([14, 2], Variation_1132),
        ([15, 2], Variation_1132),
        ([16, 2], Variation_1132),
        ([15, 1], Variation_747),
        ([24, 1], Variation_766),
        ([24, 2], Variation_767),
        ([26, 1], Variation_768),
        ([27, 1], Variation_761),
        ([27, 2], Variation_749),
        ([33, 1], Variation_762),
        ([34, 1], Variation_762),
        ([35, 1], Variation_753),
        ([38, 0], Variation_755),
        ([38, 1], Variation_752),
        ([38, 2], Variation_763),
        ([38, 3], Variation_754),
        ([38, 4], Variation_760),
        ([38, 5], Variation_764),
        ([39, 1], Variation_756),
        ([40, 1], Variation_751),
        ([41, 1], Variation_757),
    ]

class NonSpare_767(NonSpare):
    name = "CPC"
    title = "Conflict Properties Class"
    rule = RuleVariation_1366

class Item_201(Item):
    non_spare = NonSpare_767

class Content_241(ContentTable):
    values = {0: "LOW", 1: "HIGH"}

class RuleContent_241(RuleContentContextFree):
    variation = Content_241

class Variation_948(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_241

class RuleVariation_919(RuleVariationContextFree):
    variation = Variation_948

class NonSpare_782(NonSpare):
    name = "CS"
    title = "Conflict Severity"
    rule = RuleVariation_919

class Item_211(Item):
    non_spare = NonSpare_782

class Variation_1184(Group):
    bit_size = 8
    items_list = [Item_952, Item_201, Item_211]
    items_dict = {"TID": NonSpare_1768, "CPC": NonSpare_767, "CS": NonSpare_782}

class RuleVariation_1128(RuleVariationContextFree):
    variation = Variation_1184

class NonSpare_662(NonSpare):
    name = "CC"
    title = "Conflict Classification"
    rule = RuleVariation_1128

class Content_702(ContentQuantity):
    signedness = Unsigned
    lsb = 0.5
    unit = "%"

class RuleContent_702(RuleContentContextFree):
    variation = Content_702

class Variation_217(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_702

class RuleVariation_211(RuleVariationContextFree):
    variation = Variation_217

class NonSpare_765(NonSpare):
    name = "CP"
    title = "Conflict Probability"
    rule = RuleVariation_211

class NonSpare_664(NonSpare):
    name = "CD"
    title = "Conflict Duration"
    rule = RuleVariation_356

class Variation_1397(Compound):
    items_list = [NonSpare_696, NonSpare_662, NonSpare_765, NonSpare_664]
    items_dict = {"CN": NonSpare_696, "CC": NonSpare_662, "CP": NonSpare_765, "CD": NonSpare_664}

class RuleVariation_1327(RuleVariationContextFree):
    variation = Variation_1397

class NonSpare_276(NonSpare):
    name = "120"
    title = "Conflict Characteristics"
    rule = RuleVariation_1327

class NonSpare_1741(NonSpare):
    name = "TC"
    title = "Time to Conflict"
    rule = RuleVariation_356

class NonSpare_1745(NonSpare):
    name = "TCA"
    title = "Time to Closest Approach"
    rule = RuleVariation_356

class Variation_358(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_703

class RuleVariation_352(RuleVariationContextFree):
    variation = Variation_358

class NonSpare_688(NonSpare):
    name = "CHS"
    title = "Current Horizontal Separation"
    rule = RuleVariation_352

class NonSpare_1205(NonSpare):
    name = "MHS"
    title = "Estimated Minimum Horizontal Separation"
    rule = RuleVariation_296

class Content_700(ContentQuantity):
    signedness = Unsigned
    lsb = 25.0
    unit = "ft"

class RuleContent_700(RuleContentContextFree):
    variation = Content_700

class Variation_301(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_700

class RuleVariation_295(RuleVariationContextFree):
    variation = Variation_301

class NonSpare_801(NonSpare):
    name = "CVS"
    title = "Current Vertical Separation"
    rule = RuleVariation_295

class NonSpare_1264(NonSpare):
    name = "MVS"
    title = "Estimated Minimum Vertical Separation"
    rule = RuleVariation_295

class Variation_1430(Compound):
    items_list = [NonSpare_1741, NonSpare_1745, NonSpare_688, NonSpare_1205, NonSpare_801, NonSpare_1264]
    items_dict = {"TC": NonSpare_1741, "TCA": NonSpare_1745, "CHS": NonSpare_688, "MHS": NonSpare_1205, "CVS": NonSpare_801, "MVS": NonSpare_1264}

class RuleVariation_1360(RuleVariationContextFree):
    variation = Variation_1430

class NonSpare_191(NonSpare):
    name = "070"
    title = "Conflict Timing and Separation"
    rule = RuleVariation_1360

class Content_605(ContentQuantity):
    signedness = Signed
    lsb = 25.0
    unit = "ft"

class RuleContent_605(RuleContentContextFree):
    variation = Content_605

class Variation_253(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_605

class RuleVariation_247(RuleVariationContextFree):
    variation = Variation_253

class NonSpare_215(NonSpare):
    name = "076"
    title = "Vertical Deviation"
    rule = RuleVariation_247

class Content_610(ContentQuantity):
    signedness = Signed
    lsb = 32.0
    unit = "m"

class RuleContent_610(RuleContentContextFree):
    variation = Content_610

class Variation_255(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_610

class RuleVariation_249(RuleVariationContextFree):
    variation = Variation_255

class NonSpare_210(NonSpare):
    name = "074"
    title = "Longitudinal Deviation"
    rule = RuleVariation_249

class NonSpare_213(NonSpare):
    name = "075"
    title = "Transversal Distance Deviation"
    rule = RuleVariation_338

class Content_582(ContentString):
    string_type = StringICAO

class RuleContent_582(RuleContentContextFree):
    variation = Content_582

class Variation_383(Element):
    bit_offset8 = 0
    bit_size = 48
    rule = RuleContent_582

class RuleVariation_376(RuleVariationContextFree):
    variation = Variation_383

class NonSpare_565(NonSpare):
    name = "AN"
    title = "Area Name"
    rule = RuleVariation_376

class NonSpare_656(NonSpare):
    name = "CAN"
    title = "Crossing Area Name"
    rule = RuleVariation_378

class NonSpare_1541(NonSpare):
    name = "RT1"
    title = "Runway/Taxiway Designator 1"
    rule = RuleVariation_378

class NonSpare_1542(NonSpare):
    name = "RT2"
    title = "Runway/Taxiway Designator 2"
    rule = RuleVariation_378

class NonSpare_1573(NonSpare):
    name = "SB"
    title = "Stop Bar Designator"
    rule = RuleVariation_378

class NonSpare_946(NonSpare):
    name = "G"
    title = "Gate Designator"
    rule = RuleVariation_378

class Variation_1390(Compound):
    items_list = [NonSpare_565, NonSpare_656, NonSpare_1541, NonSpare_1542, NonSpare_1573, NonSpare_946]
    items_dict = {"AN": NonSpare_565, "CAN": NonSpare_656, "RT1": NonSpare_1541, "RT2": NonSpare_1542, "SB": NonSpare_1573, "G": NonSpare_946}

class RuleVariation_1320(RuleVariationContextFree):
    variation = Variation_1390

class NonSpare_249(NonSpare):
    name = "100"
    title = "Area Definition"
    rule = RuleVariation_1320

class NonSpare_123(NonSpare):
    name = "035"
    title = "Track Number 2"
    rule = RuleVariation_240

class NonSpare_548(NonSpare):
    name = "AI2"
    title = "Aircraft Identifier (in 7 Characters) of Aircraft 2 Involved in the Conflict"
    rule = RuleVariation_378

class NonSpare_1231(NonSpare):
    name = "MODE3A"
    title = "Mode-3/A Code (Converted Into Octal Representation) of Aircraft 2 Involved in the Conflict"
    rule = RuleVariation_751

class Item_542(Item):
    non_spare = NonSpare_1231

class Variation_993(Group):
    bit_size = 16
    items_list = [Item_3, Item_542]
    items_dict = {"MODE3A": NonSpare_1231}

class RuleVariation_963(RuleVariationContextFree):
    variation = Variation_993

class NonSpare_1138(NonSpare):
    name = "M32"
    title = "Mode 3/A Code Aircraft 2"
    rule = RuleVariation_963

class NonSpare_773(NonSpare):
    name = "CPW"
    title = "Predicted Conflict Position Target 2 in WGS-84 Coordinates"
    rule = RuleVariation_1048

class NonSpare_770(NonSpare):
    name = "CPL"
    title = "Predicted Conflict Position for the Aircraft 2 Involved in the Conflict"
    rule = RuleVariation_1195

class NonSpare_1845(NonSpare):
    name = "TT2"
    title = "Time to Runway Threshold for Second Approaching Aircraft in a RIMCA"
    rule = RuleVariation_356

class NonSpare_853(NonSpare):
    name = "DT2"
    title = "Distance to Runway Threshold for Aircraft 2 Involved in a RIMCA"
    rule = RuleVariation_296

class NonSpare_521(NonSpare):
    name = "AC2"
    title = "Characteristics of Aircraft 2 Involved in the Conflict"
    rule = RuleVariation_1223

class NonSpare_1245(NonSpare):
    name = "MS2"
    title = "Aircraft Identification Downloaded From Aircraft 2 Involved in the Conflict If Eequipped With a Mode-S Transponder"
    rule = RuleVariation_375

class NonSpare_921(NonSpare):
    name = "FP2"
    title = "Number of the Flight Plan Correlated to Aircraft 2 Involved in the Conflict"
    rule = RuleVariation_972

class NonSpare_676(NonSpare):
    name = "CF2"
    title = "Cleared Flight Level for Aircraft 2 Involved in the Conflict"
    rule = RuleVariation_306

class Variation_1389(Compound):
    items_list = [NonSpare_548, NonSpare_1138, NonSpare_773, NonSpare_770, NonSpare_1845, NonSpare_853, NonSpare_521, NonSpare_1245, NonSpare_921, NonSpare_676]
    items_dict = {"AI2": NonSpare_548, "M32": NonSpare_1138, "CPW": NonSpare_773, "CPL": NonSpare_770, "TT2": NonSpare_1845, "DT2": NonSpare_853, "AC2": NonSpare_521, "MS2": NonSpare_1245, "FP2": NonSpare_921, "CF2": NonSpare_676}

class RuleVariation_1319(RuleVariationContextFree):
    variation = Variation_1389

class NonSpare_354(NonSpare):
    name = "171"
    title = "Aircraft Identification and Characteristics 2"
    rule = RuleVariation_1319

class NonSpare_672(NonSpare):
    name = "CEN"
    title = ""
    rule = RuleVariation_154

class Item_142(Item):
    non_spare = NonSpare_672

class NonSpare_1361(NonSpare):
    name = "POS"
    title = ""
    rule = RuleVariation_154

class Item_655(Item):
    non_spare = NonSpare_1361

class Variation_1030(Group):
    bit_size = 16
    items_list = [Item_142, Item_655]
    items_dict = {"CEN": NonSpare_672, "POS": NonSpare_1361}

class Variation_1342(Repetitive):
    rep_bytes = 1
    variation = Variation_1030

class RuleVariation_1272(RuleVariationContextFree):
    variation = Variation_1342

class NonSpare_267(NonSpare):
    name = "110"
    title = "FDPS Sector Control Identification"
    rule = RuleVariation_1272

class Variation_1382(Explicit):
    explicit_type = ReservedExpansion

class RuleVariation_1312(RuleVariationContextFree):
    variation = Variation_1382

class NonSpare_1481(NonSpare):
    name = "RE"
    title = "Reserved Expansion Field"
    rule = RuleVariation_1312

class Record_13(Record):
    items_list = [NonSpare_33, NonSpare_2, NonSpare_56, NonSpare_89, NonSpare_129, NonSpare_155, NonSpare_183, NonSpare_108, NonSpare_343, NonSpare_276, NonSpare_191, NonSpare_215, NonSpare_210, NonSpare_213, NonSpare_249, NonSpare_123, NonSpare_354, NonSpare_267, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_33, "000": NonSpare_2, "015": NonSpare_56, "020": NonSpare_89, "040": NonSpare_129, "045": NonSpare_155, "060": NonSpare_183, "030": NonSpare_108, "170": NonSpare_343, "120": NonSpare_276, "070": NonSpare_191, "076": NonSpare_215, "074": NonSpare_210, "075": NonSpare_213, "100": NonSpare_249, "035": NonSpare_123, "171": NonSpare_354, "110": NonSpare_267, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_13(UapSingle):
    record = Record_13

class Asterix_5(AstCat):
    category = 4
    edition = (1, 12)
    uap = Uap_13

class Content_564(ContentTable):
    values = {1: "Polar vector", 2: "Cartesian vector of start point/length", 3: "Contour record", 4: "Cartesian start point and end point vector", 254: "SOP message", 255: "EOP message"}

class RuleContent_564(RuleContentContextFree):
    variation = Content_564

class Variation_183(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_564

class RuleVariation_177(RuleVariationContextFree):
    variation = Variation_183

class NonSpare_7(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_177

class Content_247(ContentTable):
    values = {0: "Local Coordinates", 1: "System Coordinates"}

class RuleContent_247(RuleContentContextFree):
    variation = Content_247

class Variation_49(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_247

class RuleVariation_49(RuleVariationContextFree):
    variation = Variation_49

class NonSpare_1322(NonSpare):
    name = "ORG"
    title = ""
    rule = RuleVariation_49

class Item_626(Item):
    non_spare = NonSpare_1322

class Variation_471(Element):
    bit_offset8 = 1
    bit_size = 3
    rule = RuleContent_585

class RuleVariation_461(RuleVariationContextFree):
    variation = Variation_471

class NonSpare_1017(NonSpare):
    name = "I"
    title = "Intensity Level"
    rule = RuleVariation_461

class Item_384(Item):
    non_spare = NonSpare_1017

class Content_2(ContentTable):
    values = {0: "0°", 1: "22.5°", 2: "45°", 3: "67.5°", 4: "90°", 5: "112.5°", 6: "135°", 7: "157.5°"}

class RuleContent_2(RuleContentContextFree):
    variation = Content_2

class Variation_746(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_2

class RuleVariation_736(RuleVariationContextFree):
    variation = Variation_746

class NonSpare_1555(NonSpare):
    name = "S"
    title = "Shading Orientation with Respect to North"
    rule = RuleVariation_736

class Item_805(Item):
    non_spare = NonSpare_1555

class Content_151(ContentTable):
    values = {0: "Default", 1: "Test vector"}

class RuleContent_151(RuleContentContextFree):
    variation = Content_151

class Variation_810(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_151

class RuleVariation_781(RuleVariationContextFree):
    variation = Variation_810

class NonSpare_1836(NonSpare):
    name = "TST"
    title = ""
    rule = RuleVariation_781

class Item_1000(Item):
    non_spare = NonSpare_1836

class Content_90(ContentTable):
    values = {0: "Default", 1: "Error condition encountered"}

class RuleContent_90(RuleContentContextFree):
    variation = Content_90

class Variation_879(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_90

class RuleVariation_850(RuleVariationContextFree):
    variation = Variation_879

class NonSpare_891(NonSpare):
    name = "ER"
    title = ""
    rule = RuleVariation_850

class Item_296(Item):
    non_spare = NonSpare_891

class Variation_1310(Extended):
    items = [Item_626, Item_384, Item_805, None, Item_4, Item_1000, Item_296, None]

class RuleVariation_1240(RuleVariationContextFree):
    variation = Variation_1310

class NonSpare_91(NonSpare):
    name = "020"
    title = "Vector Qualifier"
    rule = RuleVariation_1240

class Content_644(ContentQuantity):
    signedness = Signed
    lsb = 1.5625e-2
    unit = "(NM)"

class RuleContent_644(RuleContentContextFree):
    variation = Content_644

class Variation_204(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_644

class RuleVariation_198(RuleVariationContextFree):
    variation = Variation_204

class NonSpare_1979(NonSpare):
    name = "X"
    title = "X-Component"
    rule = RuleVariation_198

class Item_1128(Item):
    non_spare = NonSpare_1979

class NonSpare_2032(NonSpare):
    name = "Y"
    title = "Y-Component"
    rule = RuleVariation_198

class Item_1178(Item):
    non_spare = NonSpare_2032

class Content_735(ContentQuantity):
    signedness = Unsigned
    lsb = 1.5625e-2
    unit = "(NM)"

class RuleContent_734(RuleContentContextFree):
    variation = Content_735

class Variation_228(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_734

class RuleVariation_222(RuleVariationContextFree):
    variation = Variation_228

class NonSpare_1094(NonSpare):
    name = "LENGTH"
    title = "Length"
    rule = RuleVariation_222

class Item_449(Item):
    non_spare = NonSpare_1094

class Variation_1264(Group):
    bit_size = 24
    items_list = [Item_1128, Item_1178, Item_449]
    items_dict = {"X": NonSpare_1979, "Y": NonSpare_2032, "LENGTH": NonSpare_1094}

class Variation_1371(Repetitive):
    rep_bytes = 1
    variation = Variation_1264

class RuleVariation_1301(RuleVariationContextFree):
    variation = Variation_1371

class NonSpare_126(NonSpare):
    name = "036"
    title = "Sequence of Cartesian Vectors in SPF Notation"
    rule = RuleVariation_1301

class Content_738(ContentQuantity):
    signedness = Unsigned
    lsb = 7.8125e-3
    unit = "(NM)"

class RuleContent_737(RuleContentContextFree):
    variation = Content_738

class Variation_229(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_737

class RuleVariation_223(RuleVariationContextFree):
    variation = Variation_229

class NonSpare_1705(NonSpare):
    name = "STR"
    title = "Start Range"
    rule = RuleVariation_223

class Item_912(Item):
    non_spare = NonSpare_1705

class NonSpare_879(NonSpare):
    name = "ENDR"
    title = "End Range"
    rule = RuleVariation_223

class Item_284(Item):
    non_spare = NonSpare_879

class NonSpare_615(NonSpare):
    name = "AZ"
    title = "Azimuth"
    rule = RuleVariation_329

class Item_101(Item):
    non_spare = NonSpare_615

class Variation_1180(Group):
    bit_size = 32
    items_list = [Item_912, Item_284, Item_101]
    items_dict = {"STR": NonSpare_1705, "ENDR": NonSpare_879, "AZ": NonSpare_615}

class Variation_1358(Repetitive):
    rep_bytes = 1
    variation = Variation_1180

class RuleVariation_1288(RuleVariationContextFree):
    variation = Variation_1358

class NonSpare_121(NonSpare):
    name = "034"
    title = "Sequence of Polar Vectors in SPF Notation"
    rule = RuleVariation_1288

class Variation_467(Element):
    bit_offset8 = 1
    bit_size = 3
    rule = RuleContent_0

class RuleVariation_457(RuleVariationContextFree):
    variation = Variation_467

class NonSpare_1016(NonSpare):
    name = "I"
    title = "Intensity Level"
    rule = RuleVariation_457

class Item_383(Item):
    non_spare = NonSpare_1016

class Item_20(Spare):
    bit_offset8 = 4
    bit_size = 2

class Content_237(ContentTable):
    values = {0: "Intermediate record of a contour", 1: "Last record of a contour of at least two records", 2: "First record of a contour of at least two records", 3: "First and only record, fully defining a contour"}

class RuleContent_237(RuleContentContextFree):
    variation = Content_237

class Variation_928(Element):
    bit_offset8 = 6
    bit_size = 2
    rule = RuleContent_237

class RuleVariation_899(RuleVariationContextFree):
    variation = Variation_928

class NonSpare_938(NonSpare):
    name = "FSTLST"
    title = ""
    rule = RuleVariation_899

class Item_328(Item):
    non_spare = NonSpare_938

class NonSpare_784(NonSpare):
    name = "CSN"
    title = "Contour Serial Number"
    rule = RuleVariation_154

class Item_212(Item):
    non_spare = NonSpare_784

class Variation_1119(Group):
    bit_size = 16
    items_list = [Item_626, Item_383, Item_20, Item_328, Item_212]
    items_dict = {"ORG": NonSpare_1322, "I": NonSpare_1016, "FSTLST": NonSpare_938, "CSN": NonSpare_784}

class RuleVariation_1074(RuleVariationContextFree):
    variation = Variation_1119

class NonSpare_130(NonSpare):
    name = "040"
    title = "Contour Identifier"
    rule = RuleVariation_1074

class NonSpare_1986(NonSpare):
    name = "X1"
    title = ""
    rule = RuleVariation_198

class Item_1135(Item):
    non_spare = NonSpare_1986

class NonSpare_2039(NonSpare):
    name = "Y1"
    title = ""
    rule = RuleVariation_198

class Item_1185(Item):
    non_spare = NonSpare_2039

class Variation_1271(Group):
    bit_size = 16
    items_list = [Item_1135, Item_1185]
    items_dict = {"X1": NonSpare_1986, "Y1": NonSpare_2039}

class Variation_1373(Repetitive):
    rep_bytes = 1
    variation = Variation_1271

class RuleVariation_1303(RuleVariationContextFree):
    variation = Variation_1373

class NonSpare_167(NonSpare):
    name = "050"
    title = "Sequence of Contour Points in SPF Notation"
    rule = RuleVariation_1303

class NonSpare_241(NonSpare):
    name = "090"
    title = "Time of Day"
    rule = RuleVariation_356

class Content_584(ContentInteger):
    signedness = Signed

class RuleContent_584(RuleContentContextFree):
    variation = Content_584

class Variation_141(Element):
    bit_offset8 = 0
    bit_size = 5
    rule = RuleContent_584

class RuleVariation_141(RuleVariationContextFree):
    variation = Variation_141

class NonSpare_903(NonSpare):
    name = "F"
    title = "Scaling Factor"
    rule = RuleVariation_141

class Item_305(Item):
    non_spare = NonSpare_903

class Variation_858(Element):
    bit_offset8 = 5
    bit_size = 3
    rule = RuleContent_0

class RuleVariation_829(RuleVariationContextFree):
    variation = Variation_858

class NonSpare_1446(NonSpare):
    name = "R"
    title = "Current Reduction Stage in Use"
    rule = RuleVariation_829

class Item_724(Item):
    non_spare = NonSpare_1446

class Variation_244(Element):
    bit_offset8 = 0
    bit_size = 15
    rule = RuleContent_0

class RuleVariation_238(RuleVariationContextFree):
    variation = Variation_244

class NonSpare_1402(NonSpare):
    name = "Q"
    title = "Processing Parameters"
    rule = RuleVariation_238

class Item_681(Item):
    non_spare = NonSpare_1402

class Variation_1291(Extended):
    items = [Item_305, Item_724, Item_681, None]

class RuleVariation_1221(RuleVariationContextFree):
    variation = Variation_1291

class NonSpare_257(NonSpare):
    name = "100"
    title = "Processing Status"
    rule = RuleVariation_1221

class NonSpare_272(NonSpare):
    name = "110"
    title = "Station Configuration Status"
    rule = RuleVariation_1305

class Variation_247(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_585

class RuleVariation_241(RuleVariationContextFree):
    variation = Variation_247

class NonSpare_282(NonSpare):
    name = "120"
    title = "Total Number of Items Constituting One Weather Picture"
    rule = RuleVariation_241

class NonSpare_1988(NonSpare):
    name = "X1"
    title = "X1-Component"
    rule = RuleVariation_198

class Item_1137(Item):
    non_spare = NonSpare_1988

class NonSpare_2040(NonSpare):
    name = "Y1"
    title = "Y1-Component"
    rule = RuleVariation_198

class Item_1186(Item):
    non_spare = NonSpare_2040

class NonSpare_1992(NonSpare):
    name = "X2"
    title = "X2-Component"
    rule = RuleVariation_198

class Item_1141(Item):
    non_spare = NonSpare_1992

class NonSpare_2042(NonSpare):
    name = "Y2"
    title = "Y2-Component"
    rule = RuleVariation_198

class Item_1188(Item):
    non_spare = NonSpare_2042

class Variation_1272(Group):
    bit_size = 32
    items_list = [Item_1137, Item_1186, Item_1141, Item_1188]
    items_dict = {"X1": NonSpare_1988, "Y1": NonSpare_2040, "X2": NonSpare_1992, "Y2": NonSpare_2042}

class Variation_1374(Repetitive):
    rep_bytes = 1
    variation = Variation_1272

class RuleVariation_1304(RuleVariationContextFree):
    variation = Variation_1374

class NonSpare_128(NonSpare):
    name = "038"
    title = "Sequence of Weather Vectors in SPF Notation"
    rule = RuleVariation_1304

class Record_19(Record):
    items_list = [NonSpare_36, NonSpare_7, NonSpare_91, NonSpare_126, NonSpare_121, NonSpare_130, NonSpare_167, NonSpare_241, NonSpare_257, NonSpare_272, NonSpare_282, NonSpare_128, NonSpare_1641, UapItemRFS]
    items_dict = {"010": NonSpare_36, "000": NonSpare_7, "020": NonSpare_91, "036": NonSpare_126, "034": NonSpare_121, "040": NonSpare_130, "050": NonSpare_167, "090": NonSpare_241, "100": NonSpare_257, "110": NonSpare_272, "120": NonSpare_282, "038": NonSpare_128, "SP": NonSpare_1641}

class Uap_17(UapSingle):
    record = Record_19

class Asterix_6(AstCat):
    category = 8
    edition = (1, 2)
    uap = Uap_17

class Asterix_7(AstCat):
    category = 8
    edition = (1, 3)
    uap = Uap_17

class Content_577(ContentTable):
    values = {2: "Cartesian vector", 253: "Intermediate-update-step message", 254: "Start-of-picture message", 255: "End-of-picture message"}

class RuleContent_577(RuleContentContextFree):
    variation = Content_577

class Variation_192(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_577

class RuleVariation_186(RuleVariationContextFree):
    variation = Variation_192

class NonSpare_15(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_186

class Variation_1309(Extended):
    items = [Item_626, Item_384, Item_805, None]

class RuleVariation_1239(RuleVariationContextFree):
    variation = Variation_1309

class NonSpare_90(NonSpare):
    name = "020"
    title = "Vector Qualifier"
    rule = RuleVariation_1239

class Variation_274(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_644

class RuleVariation_268(RuleVariationContextFree):
    variation = Variation_274

class NonSpare_1983(NonSpare):
    name = "X"
    title = "X-coordinate"
    rule = RuleVariation_268

class Item_1132(Item):
    non_spare = NonSpare_1983

class NonSpare_2036(NonSpare):
    name = "Y"
    title = "Y-coordinate"
    rule = RuleVariation_268

class Item_1182(Item):
    non_spare = NonSpare_2036

class Variation_319(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_734

class RuleVariation_313(RuleVariationContextFree):
    variation = Variation_319

class NonSpare_1070(NonSpare):
    name = "L"
    title = "Vector Length"
    rule = RuleVariation_313

class Item_427(Item):
    non_spare = NonSpare_1070

class Variation_1269(Group):
    bit_size = 48
    items_list = [Item_1132, Item_1182, Item_427]
    items_dict = {"X": NonSpare_1983, "Y": NonSpare_2036, "L": NonSpare_1070}

class Variation_1372(Repetitive):
    rep_bytes = 1
    variation = Variation_1269

class RuleVariation_1302(RuleVariationContextFree):
    variation = Variation_1372

class NonSpare_101(NonSpare):
    name = "030"
    title = "Sequence of Cartesian Vectors"
    rule = RuleVariation_1302

class Variation_144(Element):
    bit_offset8 = 0
    bit_size = 6
    rule = RuleContent_585

class RuleVariation_144(RuleVariationContextFree):
    variation = Variation_144

class NonSpare_1638(NonSpare):
    name = "SN"
    title = "Step Number"
    rule = RuleVariation_144

class Item_860(Item):
    non_spare = NonSpare_1638

class Variation_1313(Extended):
    items = [Item_860, Item_26, None]

class RuleVariation_1243(RuleVariationContextFree):
    variation = Variation_1313

class NonSpare_186(NonSpare):
    name = "060"
    title = "Synchronisation/Control Signal"
    rule = RuleVariation_1243

class NonSpare_206(NonSpare):
    name = "070"
    title = "Time of Day"
    rule = RuleVariation_356

class NonSpare_218(NonSpare):
    name = "080"
    title = "Processing Status"
    rule = RuleVariation_1221

class NonSpare_1562(NonSpare):
    name = "SAC"
    title = "SAC of Radar Concerned"
    rule = RuleVariation_154

class Item_810(Item):
    non_spare = NonSpare_1562

class NonSpare_1617(NonSpare):
    name = "SIC"
    title = "SIC of Radar Concerned"
    rule = RuleVariation_154

class Item_841(Item):
    non_spare = NonSpare_1617

class Item_2(Spare):
    bit_offset8 = 0
    bit_size = 3

class Variation_590(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_0

class RuleVariation_580(RuleVariationContextFree):
    variation = Variation_590

class NonSpare_764(NonSpare):
    name = "CP"
    title = "Circular Polarisation"
    rule = RuleVariation_580

class Item_200(Item):
    non_spare = NonSpare_764

class Variation_673(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_0

class RuleVariation_663(RuleVariationContextFree):
    variation = Variation_673

class NonSpare_1947(NonSpare):
    name = "WO"
    title = "Weather Channel Overload"
    rule = RuleVariation_663

class Item_1099(Item):
    non_spare = NonSpare_1947

class NonSpare_1448(NonSpare):
    name = "R"
    title = "Reduction Step in Use By Radar  Concerned"
    rule = RuleVariation_829

class Item_725(Item):
    non_spare = NonSpare_1448

class Variation_1153(Group):
    bit_size = 24
    items_list = [Item_810, Item_841, Item_2, Item_200, Item_1099, Item_725]
    items_dict = {"SAC": NonSpare_1562, "SIC": NonSpare_1617, "CP": NonSpare_764, "WO": NonSpare_1947, "R": NonSpare_1448}

class Variation_1353(Repetitive):
    rep_bytes = 1
    variation = Variation_1153

class RuleVariation_1283(RuleVariationContextFree):
    variation = Variation_1353

class NonSpare_239(NonSpare):
    name = "090"
    title = "Radar Configuration and Status"
    rule = RuleVariation_1283

class NonSpare_260(NonSpare):
    name = "100"
    title = "Vector Count"
    rule = RuleVariation_241

class Record_20(Record):
    items_list = [NonSpare_36, NonSpare_15, NonSpare_90, NonSpare_101, NonSpare_186, NonSpare_206, NonSpare_218, NonSpare_239, NonSpare_260]
    items_dict = {"010": NonSpare_36, "000": NonSpare_15, "020": NonSpare_90, "030": NonSpare_101, "060": NonSpare_186, "070": NonSpare_206, "080": NonSpare_218, "090": NonSpare_239, "100": NonSpare_260}

class Uap_18(UapSingle):
    record = Record_20

class Asterix_8(AstCat):
    category = 9
    edition = (2, 1)
    uap = Uap_18

class NonSpare_42(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_571(ContentTable):
    values = {1: "Target Report", 2: "Start of Update Cycle", 3: "Periodic Status Message", 4: "Event-triggered Status Message"}

class RuleContent_571(RuleContentContextFree):
    variation = Content_571

class Variation_189(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_571

class RuleVariation_183(RuleVariationContextFree):
    variation = Variation_189

class NonSpare_12(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_183

class Content_428(ContentTable):
    values = {0: "SSR multilateration", 1: "Mode S multilateration", 2: "ADS-B", 3: "PSR", 4: "Magnetic Loop System", 5: "HF multilateration", 6: "Not defined", 7: "Other types"}

class RuleContent_428(RuleContentContextFree):
    variation = Content_428

class Variation_126(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_428

class RuleVariation_126(RuleVariationContextFree):
    variation = Variation_126

class NonSpare_1860(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_126

class Item_1021(Item):
    non_spare = NonSpare_1860

class Content_338(ContentTable):
    values = {0: "No differential correction (ADS-B)", 1: "Differential correction (ADS-B)"}

class RuleContent_338(RuleContentContextFree):
    variation = Content_338

class Variation_626(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_338

class RuleVariation_616(RuleVariationContextFree):
    variation = Variation_626

class NonSpare_823(NonSpare):
    name = "DCR"
    title = ""
    rule = RuleVariation_616

class Item_245(Item):
    non_spare = NonSpare_823

class Content_50(ContentTable):
    values = {0: "Chain 1", 1: "Chain 2"}

class RuleContent_50(RuleContentContextFree):
    variation = Content_50

class Variation_677(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_50

class RuleVariation_667(RuleVariationContextFree):
    variation = Variation_677

class NonSpare_685(NonSpare):
    name = "CHN"
    title = ""
    rule = RuleVariation_667

class Item_151(Item):
    non_spare = NonSpare_685

class Content_498(ContentTable):
    values = {0: "Transponder Ground bit not set", 1: "Transponder Ground bit set"}

class RuleContent_498(RuleContentContextFree):
    variation = Content_498

class Variation_844(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_498

class RuleVariation_815(RuleVariationContextFree):
    variation = Variation_844

class NonSpare_962(NonSpare):
    name = "GBS"
    title = ""
    rule = RuleVariation_815

class Item_344(Item):
    non_spare = NonSpare_962

class Content_292(ContentTable):
    values = {0: "No Corrupted reply in multilateration", 1: "Corrupted replies in multilateration"}

class RuleContent_292(RuleContentContextFree):
    variation = Content_292

class Variation_908(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_292

class RuleVariation_879(RuleVariationContextFree):
    variation = Variation_908

class NonSpare_779(NonSpare):
    name = "CRT"
    title = ""
    rule = RuleVariation_879

class Item_209(Item):
    non_spare = NonSpare_779

class Content_18(ContentTable):
    values = {0: "Actual target report", 1: "Simulated target report"}

class RuleContent_18(RuleContentContextFree):
    variation = Content_18

class Variation_4(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_18

class RuleVariation_4(RuleVariationContextFree):
    variation = Variation_4

class NonSpare_1632(NonSpare):
    name = "SIM"
    title = ""
    rule = RuleVariation_4

class Item_854(Item):
    non_spare = NonSpare_1632

class Content_149(ContentTable):
    values = {0: "Default", 1: "Test Target"}

class RuleContent_149(RuleContentContextFree):
    variation = Content_149

class Variation_409(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_149

class RuleVariation_399(RuleVariationContextFree):
    variation = Variation_409

class NonSpare_1835(NonSpare):
    name = "TST"
    title = ""
    rule = RuleVariation_399

class Item_999(Item):
    non_spare = NonSpare_1835

class Content_419(ContentTable):
    values = {0: "Report from target transponder", 1: "Report from field monitor (fixed transponder)"}

class RuleContent_419(RuleContentContextFree):
    variation = Content_419

class Variation_551(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_419

class RuleVariation_541(RuleVariationContextFree):
    variation = Variation_551

class NonSpare_1452(NonSpare):
    name = "RAB"
    title = ""
    rule = RuleVariation_541

class Item_728(Item):
    non_spare = NonSpare_1452

class Content_509(ContentTable):
    values = {0: "Undetermined", 1: "Loop start", 2: "Loop finish"}

class RuleContent_509(RuleContentContextFree):
    variation = Content_509

class Variation_656(Element):
    bit_offset8 = 3
    bit_size = 2
    rule = RuleContent_509

class RuleVariation_646(RuleVariationContextFree):
    variation = Variation_656

class NonSpare_1117(NonSpare):
    name = "LOP"
    title = ""
    rule = RuleVariation_646

class Item_472(Item):
    non_spare = NonSpare_1117

class Content_508(ContentTable):
    values = {0: "Undetermined", 1: "Aircraft", 2: "Ground vehicle", 3: "Helicopter"}

class RuleContent_508(RuleContentContextFree):
    variation = Content_508

class Variation_856(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_508

class RuleVariation_827(RuleVariationContextFree):
    variation = Variation_856

class NonSpare_1794(NonSpare):
    name = "TOT"
    title = ""
    rule = RuleVariation_827

class Item_964(Item):
    non_spare = NonSpare_1794

class Content_12(ContentTable):
    values = {0: "Absence of SPI", 1: "Special Position Identification"}

class RuleContent_12(RuleContentContextFree):
    variation = Content_12

class Variation_2(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_12

class RuleVariation_2(RuleVariationContextFree):
    variation = Variation_2

class NonSpare_1645(NonSpare):
    name = "SPI"
    title = ""
    rule = RuleVariation_2

class Item_865(Item):
    non_spare = NonSpare_1645

class Variation_1322(Extended):
    items = [Item_1021, Item_245, Item_151, Item_344, Item_209, None, Item_854, Item_999, Item_728, Item_472, Item_964, None, Item_865, Item_9, None]

class RuleVariation_1252(RuleVariationContextFree):
    variation = Variation_1322

class NonSpare_87(NonSpare):
    name = "020"
    title = "Target Report Descriptor"
    rule = RuleVariation_1252

class NonSpare_307(NonSpare):
    name = "140"
    title = "Time of Day"
    rule = RuleVariation_356

class Content_676(ContentQuantity):
    signedness = Signed
    lsb = 8.381903171539307e-8
    unit = "°"

class RuleContent_676(RuleContentContextFree):
    variation = Content_676

class Variation_375(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_676

class RuleVariation_369(RuleVariationContextFree):
    variation = Variation_375

class NonSpare_1080(NonSpare):
    name = "LAT"
    title = "Latitude"
    rule = RuleVariation_369

class Item_437(Item):
    non_spare = NonSpare_1080

class Content_674(ContentQuantity):
    signedness = Signed
    lsb = 8.381903171539307e-8
    unit = "°"

class RuleContent_674(RuleContentContextFree):
    variation = Content_674

class Variation_373(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_674

class RuleVariation_367(RuleVariationContextFree):
    variation = Variation_373

class NonSpare_1108(NonSpare):
    name = "LON"
    title = "Longitude"
    rule = RuleVariation_367

class Item_463(Item):
    non_spare = NonSpare_1108

class Variation_1094(Group):
    bit_size = 64
    items_list = [Item_437, Item_463]
    items_dict = {"LAT": NonSpare_1080, "LON": NonSpare_1108}

class RuleVariation_1053(RuleVariationContextFree):
    variation = Variation_1094

class NonSpare_147(NonSpare):
    name = "041"
    title = "Position in WGS-84 Co-ordinates"
    rule = RuleVariation_1053

class Content_691(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "m"

class RuleContent_691(RuleContentContextFree):
    variation = Content_691

class Variation_296(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_691

class RuleVariation_290(RuleVariationContextFree):
    variation = Variation_296

class NonSpare_1505(NonSpare):
    name = "RHO"
    title = "RHO"
    rule = RuleVariation_290

class Item_776(Item):
    non_spare = NonSpare_1505

class NonSpare_1760(NonSpare):
    name = "TH"
    title = "Theta"
    rule = RuleVariation_329

class Item_946(Item):
    non_spare = NonSpare_1760

class Variation_1143(Group):
    bit_size = 32
    items_list = [Item_776, Item_946]
    items_dict = {"RHO": NonSpare_1505, "TH": NonSpare_1760}

class RuleVariation_1096(RuleVariationContextFree):
    variation = Variation_1143

class NonSpare_133(NonSpare):
    name = "040"
    title = "Measured Position in Polar Co-ordinates"
    rule = RuleVariation_1096

class Content_599(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = "m"

class RuleContent_599(RuleContentContextFree):
    variation = Content_599

class Variation_250(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_599

class RuleVariation_244(RuleVariationContextFree):
    variation = Variation_250

class NonSpare_1977(NonSpare):
    name = "X"
    title = "X Coordinate"
    rule = RuleVariation_244

class Item_1126(Item):
    non_spare = NonSpare_1977

class NonSpare_2030(NonSpare):
    name = "Y"
    title = "Y Coordinate"
    rule = RuleVariation_244

class Item_1176(Item):
    non_spare = NonSpare_2030

class Variation_1262(Group):
    bit_size = 32
    items_list = [Item_1126, Item_1176]
    items_dict = {"X": NonSpare_1977, "Y": NonSpare_2030}

class RuleVariation_1196(RuleVariationContextFree):
    variation = Variation_1262

class NonSpare_153(NonSpare):
    name = "042"
    title = "Position in Cartesian Co-ordinates"
    rule = RuleVariation_1196

class Content_753(ContentQuantity):
    signedness = Unsigned
    lsb = 6.103515625e-5
    unit = "NM/s"

class RuleContent_752(RuleContentContextFree):
    variation = Content_753

class Variation_331(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_752

class RuleVariation_325(RuleVariationContextFree):
    variation = Variation_331

class NonSpare_974(NonSpare):
    name = "GSP"
    title = "Ground Speed"
    rule = RuleVariation_325

class Item_353(Item):
    non_spare = NonSpare_974

class NonSpare_1806(NonSpare):
    name = "TRA"
    title = "Track Angle"
    rule = RuleVariation_329

class Item_976(Item):
    non_spare = NonSpare_1806

class Variation_1080(Group):
    bit_size = 32
    items_list = [Item_353, Item_976]
    items_dict = {"GSP": NonSpare_974, "TRA": NonSpare_1806}

class RuleVariation_1039(RuleVariationContextFree):
    variation = Variation_1080

class NonSpare_360(NonSpare):
    name = "200"
    title = "Calculated Track Velocity in Polar Co-ordinates"
    rule = RuleVariation_1039

class Content_640(ContentQuantity):
    signedness = Signed
    lsb = 6.25e-2
    unit = "m/s"

class RuleContent_640(RuleContentContextFree):
    variation = Content_640

class Variation_273(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_640

class RuleVariation_267(RuleVariationContextFree):
    variation = Variation_273

class NonSpare_1936(NonSpare):
    name = "VX"
    title = "X Velocity"
    rule = RuleVariation_267

class Item_1090(Item):
    non_spare = NonSpare_1936

class NonSpare_1941(NonSpare):
    name = "VY"
    title = "Y Velocity"
    rule = RuleVariation_267

class Item_1095(Item):
    non_spare = NonSpare_1941

class Variation_1237(Group):
    bit_size = 32
    items_list = [Item_1090, Item_1095]
    items_dict = {"VX": NonSpare_1936, "VY": NonSpare_1941}

class RuleVariation_1171(RuleVariationContextFree):
    variation = Variation_1237

class NonSpare_372(NonSpare):
    name = "202"
    title = "Calculated Track Velocity in Cartesian Co-ordinates"
    rule = RuleVariation_1171

class NonSpare_1820(NonSpare):
    name = "TRK"
    title = "Track Number"
    rule = RuleVariation_750

class Item_986(Item):
    non_spare = NonSpare_1820

class Variation_999(Group):
    bit_size = 16
    items_list = [Item_3, Item_986]
    items_dict = {"TRK": NonSpare_1820}

class RuleVariation_968(RuleVariationContextFree):
    variation = Variation_999

class NonSpare_335(NonSpare):
    name = "161"
    title = "Track Number"
    rule = RuleVariation_968

class Content_60(ContentTable):
    values = {0: "Confirmed track", 1: "Track in initialisation phase"}

class RuleContent_60(RuleContentContextFree):
    variation = Content_60

class Variation_17(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_60

class RuleVariation_17(RuleVariationContextFree):
    variation = Variation_17

class NonSpare_697(NonSpare):
    name = "CNF"
    title = ""
    rule = RuleVariation_17

class Item_159(Item):
    non_spare = NonSpare_697

class Variation_401(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_107

class RuleVariation_391(RuleVariationContextFree):
    variation = Variation_401

class NonSpare_1816(NonSpare):
    name = "TRE"
    title = ""
    rule = RuleVariation_391

class Item_984(Item):
    non_spare = NonSpare_1816

class Content_346(ContentTable):
    values = {0: "No extrapolation", 1: "Predictable extrapolation due to sensor refresh period (see NOTE)", 2: "Predictable extrapolation in masked area", 3: "Extrapolation due to unpredictable absence of detection"}

class RuleContent_346(RuleContentContextFree):
    variation = Content_346

class Variation_571(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_346

class RuleVariation_561(RuleVariationContextFree):
    variation = Variation_571

class NonSpare_790(NonSpare):
    name = "CST"
    title = ""
    rule = RuleVariation_561

class Item_217(Item):
    non_spare = NonSpare_790

class Content_100(ContentTable):
    values = {0: "Default", 1: "Horizontal manoeuvre"}

class RuleContent_100(RuleContentContextFree):
    variation = Content_100

class Variation_679(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_100

class RuleVariation_669(RuleVariationContextFree):
    variation = Variation_679

class NonSpare_1147(NonSpare):
    name = "MAH"
    title = ""
    rule = RuleVariation_669

class Item_492(Item):
    non_spare = NonSpare_1147

class Content_492(ContentTable):
    values = {0: "Tracking performed in 'Sensor Plane', i.e. neither slant range correction nor projection was applied", 1: "Slant range correction and a suitable projection technique are used to track in a 2D.reference plane, tangential to the earth model at the Sensor Site co-ordinates"}

class RuleContent_492(RuleContentContextFree):
    variation = Content_492

class Variation_843(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_492

class RuleVariation_814(RuleVariationContextFree):
    variation = Variation_843

class NonSpare_1746(NonSpare):
    name = "TCC"
    title = ""
    rule = RuleVariation_814

class Item_933(Item):
    non_spare = NonSpare_1746

class Content_256(ContentTable):
    values = {0: "Measured position", 1: "Smoothed position"}

class RuleContent_256(RuleContentContextFree):
    variation = Content_256

class Variation_905(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_256

class RuleVariation_876(RuleVariationContextFree):
    variation = Variation_905

class NonSpare_1697(NonSpare):
    name = "STH"
    title = ""
    rule = RuleVariation_876

class Item_904(Item):
    non_spare = NonSpare_1697

class Content_529(ContentTable):
    values = {0: "Unknown type of movement", 1: "Taking-off", 2: "Landing", 3: "Other types of movement"}

class RuleContent_529(RuleContentContextFree):
    variation = Content_529

class Variation_112(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_529

class RuleVariation_112(RuleVariationContextFree):
    variation = Variation_112

class NonSpare_1790(NonSpare):
    name = "TOM"
    title = ""
    rule = RuleVariation_112

class Item_962(Item):
    non_spare = NonSpare_1790

class Content_339(ContentTable):
    values = {0: "No doubt", 1: "Doubtful correlation (undetermined reason)", 2: "Doubtful correlation in clutter", 3: "Loss of accuracy", 4: "Loss of accuracy in clutter", 5: "Unstable track", 6: "Previously coasted"}

class RuleContent_339(RuleContentContextFree):
    variation = Content_339

class Variation_577(Element):
    bit_offset8 = 2
    bit_size = 3
    rule = RuleContent_339

class RuleVariation_567(RuleVariationContextFree):
    variation = Variation_577

class NonSpare_841(NonSpare):
    name = "DOU"
    title = ""
    rule = RuleVariation_567

class Item_260(Item):
    non_spare = NonSpare_841

class Content_257(ContentTable):
    values = {0: "Merge or split indication undetermined", 1: "Track merged by association to plot", 2: "Track merged by non-association to plot", 3: "Split track"}

class RuleContent_257(RuleContentContextFree):
    variation = Content_257

class Variation_848(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_257

class RuleVariation_819(RuleVariationContextFree):
    variation = Variation_848

class NonSpare_1240(NonSpare):
    name = "MRS"
    title = ""
    rule = RuleVariation_819

class Item_551(Item):
    non_spare = NonSpare_1240

class Variation_27(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_94

class RuleVariation_27(RuleVariationContextFree):
    variation = Variation_27

class NonSpare_967(NonSpare):
    name = "GHO"
    title = ""
    rule = RuleVariation_27

class Item_347(Item):
    non_spare = NonSpare_967

class Variation_1283(Extended):
    items = [Item_159, Item_984, Item_217, Item_492, Item_933, Item_904, None, Item_962, Item_260, Item_551, None, Item_347, Item_9, None]

class RuleVariation_1213(RuleVariationContextFree):
    variation = Variation_1283

class NonSpare_346(NonSpare):
    name = "170"
    title = "Track Status"
    rule = RuleVariation_1213

class NonSpare_1891(NonSpare):
    name = "V"
    title = "Validated"
    rule = RuleVariation_14

class Item_1050(Item):
    non_spare = NonSpare_1891

class NonSpare_945(NonSpare):
    name = "G"
    title = "Garbled"
    rule = RuleVariation_387

class Item_335(Item):
    non_spare = NonSpare_945

class Content_276(ContentTable):
    values = {0: "Mode-3/A code derived from the reply of the transponder", 1: "Mode-3/A code not extracted during the last scan"}

class RuleContent_276(RuleContentContextFree):
    variation = Content_276

class Variation_530(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_276

class RuleVariation_520(RuleVariationContextFree):
    variation = Variation_530

class NonSpare_1066(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_520

class Item_423(Item):
    non_spare = NonSpare_1066

class Variation_1229(Group):
    bit_size = 16
    items_list = [Item_1050, Item_335, Item_423, Item_16, Item_543]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "L": NonSpare_1066, "MODE3A": NonSpare_1232}

class RuleVariation_1163(RuleVariationContextFree):
    variation = Variation_1229

class NonSpare_181(NonSpare):
    name = "060"
    title = "Mode-3/A Code in Octal Representation"
    rule = RuleVariation_1163

class Variation_341(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_0

class RuleVariation_335(RuleVariationContextFree):
    variation = Variation_341

class NonSpare_392(NonSpare):
    name = "220"
    title = "Target Address"
    rule = RuleVariation_335

class Content_48(ContentTable):
    values = {0: "Callsign or registration downlinked from transponder", 1: "Callsign not downlinked from transponder", 2: "Registration not downlinked from transponder"}

class RuleContent_48(RuleContentContextFree):
    variation = Content_48

class Variation_93(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_48

class RuleVariation_93(RuleVariationContextFree):
    variation = Variation_93

class NonSpare_1699(NonSpare):
    name = "STI"
    title = ""
    rule = RuleVariation_93

class Item_906(Item):
    non_spare = NonSpare_1699

class Item_15(Spare):
    bit_offset8 = 2
    bit_size = 6

class NonSpare_686(NonSpare):
    name = "CHR"
    title = "Characters 1-8 (Coded on 6 Bits Each) Defining Target Identification"
    rule = RuleVariation_376

class Item_152(Item):
    non_spare = NonSpare_686

class Variation_1177(Group):
    bit_size = 56
    items_list = [Item_906, Item_15, Item_152]
    items_dict = {"STI": NonSpare_1699, "CHR": NonSpare_686}

class RuleVariation_1124(RuleVariationContextFree):
    variation = Variation_1177

class NonSpare_405(NonSpare):
    name = "245"
    title = "Target Identification"
    rule = RuleVariation_1124

class Variation_384(Element):
    bit_offset8 = 0
    bit_size = 56
    rule = RuleContent_0

class RuleVariation_377(RuleVariationContextFree):
    variation = Variation_384

class NonSpare_1158(NonSpare):
    name = "MBDATA"
    title = ""
    rule = RuleVariation_377

class Item_499(Item):
    non_spare = NonSpare_1158

class NonSpare_627(NonSpare):
    name = "BDS1"
    title = ""
    rule = RuleVariation_127

class Item_110(Item):
    non_spare = NonSpare_627

class Variation_770(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_0

class RuleVariation_741(RuleVariationContextFree):
    variation = Variation_770

class NonSpare_629(NonSpare):
    name = "BDS2"
    title = ""
    rule = RuleVariation_741

class Item_112(Item):
    non_spare = NonSpare_629

class Variation_1106(Group):
    bit_size = 64
    items_list = [Item_499, Item_110, Item_112]
    items_dict = {"MBDATA": NonSpare_1158, "BDS1": NonSpare_627, "BDS2": NonSpare_629}

class Variation_1347(Repetitive):
    rep_bytes = 1
    variation = Variation_1106

class RuleVariation_1277(RuleVariationContextFree):
    variation = Variation_1347

class NonSpare_412(NonSpare):
    name = "250"
    title = "Mode S MB Data"
    rule = RuleVariation_1277

class Content_513(ContentTable):
    values = {0: "Unknown", 1: "ATC equipment maintenance", 2: "Airport maintenance", 3: "Fire", 4: "Bird scarer", 5: "Snow plough", 6: "Runway sweeper", 7: "Emergency", 8: "Police", 9: "Bus", 10: "Tug (push/tow)", 11: "Grass cutter", 12: "Fuel", 13: "Baggage", 14: "Catering", 15: "Aircraft maintenance", 16: "Flyco (follow me)"}

class RuleContent_513(RuleContentContextFree):
    variation = Content_513

class Variation_171(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_513

class RuleVariation_165(RuleVariationContextFree):
    variation = Variation_171

class NonSpare_439(NonSpare):
    name = "300"
    title = "Vehicle Fleet Identification"
    rule = RuleVariation_165

class NonSpare_908(NonSpare):
    name = "FL"
    title = "Flight Level"
    rule = RuleVariation_575

class Item_309(Item):
    non_spare = NonSpare_908

class Variation_1225(Group):
    bit_size = 16
    items_list = [Item_1050, Item_335, Item_309]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "FL": NonSpare_908}

class RuleVariation_1159(RuleVariationContextFree):
    variation = Variation_1225

class NonSpare_232(NonSpare):
    name = "090"
    title = "Flight Level in Binary Representation"
    rule = RuleVariation_1159

class Content_656(ContentQuantity):
    signedness = Signed
    lsb = 6.25
    unit = "ft"

class RuleContent_656(RuleContentContextFree):
    variation = Content_656

class Variation_281(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_656

class RuleVariation_275(RuleVariationContextFree):
    variation = Variation_281

class NonSpare_242(NonSpare):
    name = "091"
    title = "Measured Height"
    rule = RuleVariation_275

class Content_690(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "m"

class RuleContent_690(RuleContentContextFree):
    variation = Content_690

class Variation_155(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_690

class RuleVariation_150(RuleVariationContextFree):
    variation = Variation_155

class NonSpare_1093(NonSpare):
    name = "LENGTH"
    title = "Length"
    rule = RuleVariation_150

class Item_448(Item):
    non_spare = NonSpare_1093

class Content_762(ContentQuantity):
    signedness = Unsigned
    lsb = 2.8125
    unit = "°"

class RuleContent_761(RuleContentContextFree):
    variation = Content_762

class Variation_158(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_761

class RuleVariation_153(RuleVariationContextFree):
    variation = Variation_158

class NonSpare_1323(NonSpare):
    name = "ORIENTATION"
    title = "Orientation"
    rule = RuleVariation_153

class Item_627(Item):
    non_spare = NonSpare_1323

class NonSpare_1946(NonSpare):
    name = "WIDTH"
    title = "Width"
    rule = RuleVariation_150

class Item_1098(Item):
    non_spare = NonSpare_1946

class Variation_1294(Extended):
    items = [Item_448, None, Item_627, None, Item_1098, None]

class RuleVariation_1224(RuleVariationContextFree):
    variation = Variation_1294

class NonSpare_423(NonSpare):
    name = "270"
    title = "Target Size and Orientation"
    rule = RuleVariation_1224

class Content_393(ContentTable):
    values = {0: "Operational", 1: "Degraded", 2: "NOGO"}

class RuleContent_393(RuleContentContextFree):
    variation = Content_393

class Variation_103(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_393

class RuleVariation_103(RuleVariationContextFree):
    variation = Variation_103

class NonSpare_1291(NonSpare):
    name = "NOGO"
    title = "Operational Release Status of the System"
    rule = RuleVariation_103

class Item_596(Item):
    non_spare = NonSpare_1291

class Content_354(ContentTable):
    values = {0: "No overload", 1: "Overload"}

class RuleContent_354(RuleContentContextFree):
    variation = Content_354

class Variation_541(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_354

class RuleVariation_531(RuleVariationContextFree):
    variation = Variation_541

class NonSpare_1330(NonSpare):
    name = "OVL"
    title = "Overload Indicator"
    rule = RuleVariation_531

class Item_633(Item):
    non_spare = NonSpare_1330

class Content_534(ContentTable):
    values = {0: "Valid", 1: "Invalid"}

class RuleContent_534(RuleContentContextFree):
    variation = Content_534

class Variation_647(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_534

class RuleVariation_637(RuleVariationContextFree):
    variation = Variation_647

class NonSpare_1841(NonSpare):
    name = "TSV"
    title = "Time Source Validity"
    rule = RuleVariation_637

class Item_1005(Item):
    non_spare = NonSpare_1841

class Content_366(ContentTable):
    values = {0: "Normal Operation", 1: "Diversity degraded"}

class RuleContent_366(RuleContentContextFree):
    variation = Content_366

class Variation_715(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_366

class RuleVariation_705(RuleVariationContextFree):
    variation = Variation_715

class NonSpare_833(NonSpare):
    name = "DIV"
    title = ""
    rule = RuleVariation_705

class Item_254(Item):
    non_spare = NonSpare_833

class Content_464(ContentTable):
    values = {0: "Test Target Operative", 1: "Test Target Failure"}

class RuleContent_464(RuleContentContextFree):
    variation = Content_464

class Variation_841(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_464

class RuleVariation_812(RuleVariationContextFree):
    variation = Variation_841

class NonSpare_1848(NonSpare):
    name = "TTF"
    title = ""
    rule = RuleVariation_812

class Item_1010(Item):
    non_spare = NonSpare_1848

class Variation_1116(Group):
    bit_size = 8
    items_list = [Item_596, Item_633, Item_1005, Item_254, Item_1010, Item_27]
    items_dict = {"NOGO": NonSpare_1291, "OVL": NonSpare_1330, "TSV": NonSpare_1841, "DIV": NonSpare_833, "TTF": NonSpare_1848}

class RuleVariation_1071(RuleVariationContextFree):
    variation = Variation_1116

class NonSpare_485(NonSpare):
    name = "550"
    title = "System Status"
    rule = RuleVariation_1071

class Content_104(ContentTable):
    values = {0: "Default", 1: "In Trouble"}

class RuleContent_104(RuleContentContextFree):
    variation = Content_104

class Variation_29(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_104

class RuleVariation_29(RuleVariationContextFree):
    variation = Variation_29

class NonSpare_1809(NonSpare):
    name = "TRB"
    title = ""
    rule = RuleVariation_29

class Item_979(Item):
    non_spare = NonSpare_1809

class Content_574(ContentTable):
    values = {1: "Towing aircraft", 2: "“Follow me” operation", 3: "Runway check", 4: "Emergency operation (fire, medical...)", 5: "Work in progress (maintenance, birds scarer, sweepers...)"}

class RuleContent_574(RuleContentContextFree):
    variation = Content_574

class Variation_478(Element):
    bit_offset8 = 1
    bit_size = 7
    rule = RuleContent_574

class RuleVariation_468(RuleVariationContextFree):
    variation = Variation_478

class NonSpare_1253(NonSpare):
    name = "MSG"
    title = ""
    rule = RuleVariation_468

class Item_562(Item):
    non_spare = NonSpare_1253

class Variation_1189(Group):
    bit_size = 8
    items_list = [Item_979, Item_562]
    items_dict = {"TRB": NonSpare_1809, "MSG": NonSpare_1253}

class RuleVariation_1132(RuleVariationContextFree):
    variation = Variation_1189

class NonSpare_441(NonSpare):
    name = "310"
    title = "Pre-programmed Message"
    rule = RuleVariation_1132

class Content_726(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "m"

class RuleContent_725(RuleContentContextFree):
    variation = Content_726

class Variation_223(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_725

class RuleVariation_217(RuleVariationContextFree):
    variation = Variation_223

class NonSpare_828(NonSpare):
    name = "DEVX"
    title = "Standard Deviation of X Component"
    rule = RuleVariation_217

class Item_249(Item):
    non_spare = NonSpare_828

class NonSpare_829(NonSpare):
    name = "DEVY"
    title = "Standard Deviation of Y Component"
    rule = RuleVariation_217

class Item_250(Item):
    non_spare = NonSpare_829

class Content_629(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "m"

class RuleContent_629(RuleContentContextFree):
    variation = Content_629

class Variation_266(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_629

class RuleVariation_260(RuleVariationContextFree):
    variation = Variation_266

class NonSpare_763(NonSpare):
    name = "COVXY"
    title = "Covariance in Two’s Complement Form"
    rule = RuleVariation_260

class Item_199(Item):
    non_spare = NonSpare_763

class Variation_1047(Group):
    bit_size = 32
    items_list = [Item_249, Item_250, Item_199]
    items_dict = {"DEVX": NonSpare_828, "DEVY": NonSpare_829, "COVXY": NonSpare_763}

class RuleVariation_1008(RuleVariationContextFree):
    variation = Variation_1047

class NonSpare_480(NonSpare):
    name = "500"
    title = "Standard Deviation of Position"
    rule = RuleVariation_1008

class Content_600(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = "m"

class RuleContent_600(RuleContentContextFree):
    variation = Content_600

class Variation_200(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_600

class RuleVariation_194(RuleVariationContextFree):
    variation = Variation_200

class NonSpare_846(NonSpare):
    name = "DRHO"
    title = ""
    rule = RuleVariation_194

class Item_263(Item):
    non_spare = NonSpare_846

class Content_655(ContentQuantity):
    signedness = Signed
    lsb = 0.15
    unit = "°"

class RuleContent_655(RuleContentContextFree):
    variation = Content_655

class Variation_210(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_655

class RuleVariation_204(RuleVariationContextFree):
    variation = Variation_210

class NonSpare_854(NonSpare):
    name = "DTHETA"
    title = ""
    rule = RuleVariation_204

class Item_268(Item):
    non_spare = NonSpare_854

class Variation_1049(Group):
    bit_size = 16
    items_list = [Item_263, Item_268]
    items_dict = {"DRHO": NonSpare_846, "DTHETA": NonSpare_854}

class Variation_1346(Repetitive):
    rep_bytes = 1
    variation = Variation_1049

class RuleVariation_1276(RuleVariationContextFree):
    variation = Variation_1346

class NonSpare_429(NonSpare):
    name = "280"
    title = "Presence"
    rule = RuleVariation_1276

class NonSpare_291(NonSpare):
    name = "131"
    title = "Amplitude of Primary Plot"
    rule = RuleVariation_154

class Content_642(ContentQuantity):
    signedness = Signed
    lsb = 6.25e-2
    unit = "m/s²"

class RuleContent_642(RuleContentContextFree):
    variation = Content_642

class Variation_203(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_642

class RuleVariation_197(RuleVariationContextFree):
    variation = Variation_203

class NonSpare_610(NonSpare):
    name = "AX"
    title = "X Acceleration"
    rule = RuleVariation_197

class Item_96(Item):
    non_spare = NonSpare_610

class NonSpare_614(NonSpare):
    name = "AY"
    title = "Y Acceleration"
    rule = RuleVariation_197

class Item_100(Item):
    non_spare = NonSpare_614

class Variation_1022(Group):
    bit_size = 16
    items_list = [Item_96, Item_100]
    items_dict = {"AX": NonSpare_610, "AY": NonSpare_614}

class RuleVariation_990(RuleVariationContextFree):
    variation = Variation_1022

class NonSpare_377(NonSpare):
    name = "210"
    title = "Calculated Acceleration"
    rule = RuleVariation_990

class Record_36(Record):
    items_list = [NonSpare_42, NonSpare_12, NonSpare_87, NonSpare_307, NonSpare_147, NonSpare_133, NonSpare_153, NonSpare_360, NonSpare_372, NonSpare_335, NonSpare_346, NonSpare_181, NonSpare_392, NonSpare_405, NonSpare_412, NonSpare_439, NonSpare_232, NonSpare_242, NonSpare_423, NonSpare_485, NonSpare_441, NonSpare_480, NonSpare_429, NonSpare_291, NonSpare_377, UapItemSpare, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_42, "000": NonSpare_12, "020": NonSpare_87, "140": NonSpare_307, "041": NonSpare_147, "040": NonSpare_133, "042": NonSpare_153, "200": NonSpare_360, "202": NonSpare_372, "161": NonSpare_335, "170": NonSpare_346, "060": NonSpare_181, "220": NonSpare_392, "245": NonSpare_405, "250": NonSpare_412, "300": NonSpare_439, "090": NonSpare_232, "091": NonSpare_242, "270": NonSpare_423, "550": NonSpare_485, "310": NonSpare_441, "500": NonSpare_480, "280": NonSpare_429, "131": NonSpare_291, "210": NonSpare_377, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_32(UapSingle):
    record = Record_36

class Asterix_9(AstCat):
    category = 10
    edition = (1, 1)
    uap = Uap_32

class NonSpare_1564(NonSpare):
    name = "SAC"
    title = "System Area Code Fixed to Zero"
    rule = RuleVariation_154

class Item_812(Item):
    non_spare = NonSpare_1564

class Variation_1158(Group):
    bit_size = 16
    items_list = [Item_812, Item_842]
    items_dict = {"SAC": NonSpare_1564, "SIC": NonSpare_1618}

class RuleVariation_1106(RuleVariationContextFree):
    variation = Variation_1158

class NonSpare_47(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1106

class Content_572(ContentTable):
    values = {1: "Target reports, flight plan data and basic alerts", 2: "Manual attachment of flight plan to track", 3: "Manual detachment of flight plan to track", 4: "Insertion of flight plan data", 5: "Suppression of flight plan data", 6: "Modification of flight plan data", 7: "Holdbar status"}

class RuleContent_572(RuleContentContextFree):
    variation = Content_572

class Variation_190(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_572

class RuleVariation_184(RuleVariationContextFree):
    variation = Variation_190

class NonSpare_13(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_184

class NonSpare_60(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class NonSpare_312(NonSpare):
    name = "140"
    title = "Time of Track Information"
    rule = RuleVariation_356

class NonSpare_1084(NonSpare):
    name = "LAT"
    title = "Latitude in WGS-84 in Two's Complement"
    rule = RuleVariation_369

class Item_441(Item):
    non_spare = NonSpare_1084

class NonSpare_1112(NonSpare):
    name = "LON"
    title = "Longitude in WGS-84 in Two's Complement"
    rule = RuleVariation_367

class Item_467(Item):
    non_spare = NonSpare_1112

class Variation_1098(Group):
    bit_size = 64
    items_list = [Item_441, Item_467]
    items_dict = {"LAT": NonSpare_1084, "LON": NonSpare_1112}

class RuleVariation_1057(RuleVariationContextFree):
    variation = Variation_1098

class NonSpare_148(NonSpare):
    name = "041"
    title = "Position in WGS-84 Coordinates"
    rule = RuleVariation_1057

class NonSpare_1980(NonSpare):
    name = "X"
    title = "X-Component"
    rule = RuleVariation_244

class Item_1129(Item):
    non_spare = NonSpare_1980

class NonSpare_2033(NonSpare):
    name = "Y"
    title = "Y-Component"
    rule = RuleVariation_244

class Item_1179(Item):
    non_spare = NonSpare_2033

class Variation_1265(Group):
    bit_size = 32
    items_list = [Item_1129, Item_1179]
    items_dict = {"X": NonSpare_1980, "Y": NonSpare_2033}

class RuleVariation_1198(RuleVariationContextFree):
    variation = Variation_1265

class NonSpare_150(NonSpare):
    name = "042"
    title = "Calculated Position in Cartesian Co-ordinates"
    rule = RuleVariation_1198

class Content_633(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "m/s"

class RuleContent_633(RuleContentContextFree):
    variation = Content_633

class Variation_270(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_633

class RuleVariation_264(RuleVariationContextFree):
    variation = Variation_270

class NonSpare_1935(NonSpare):
    name = "VX"
    title = "Vx"
    rule = RuleVariation_264

class Item_1089(Item):
    non_spare = NonSpare_1935

class NonSpare_1940(NonSpare):
    name = "VY"
    title = "Vy"
    rule = RuleVariation_264

class Item_1094(Item):
    non_spare = NonSpare_1940

class Variation_1236(Group):
    bit_size = 32
    items_list = [Item_1089, Item_1094]
    items_dict = {"VX": NonSpare_1935, "VY": NonSpare_1940}

class RuleVariation_1170(RuleVariationContextFree):
    variation = Variation_1236

class NonSpare_374(NonSpare):
    name = "202"
    title = "Calculated Track Velocity in Cartesian Coordinates"
    rule = RuleVariation_1170

class Content_636(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "m/s²"

class RuleContent_636(RuleContentContextFree):
    variation = Content_636

class Variation_202(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_636

class RuleVariation_196(RuleVariationContextFree):
    variation = Variation_202

class NonSpare_609(NonSpare):
    name = "AX"
    title = "Ax"
    rule = RuleVariation_196

class Item_95(Item):
    non_spare = NonSpare_609

class NonSpare_613(NonSpare):
    name = "AY"
    title = "Ay"
    rule = RuleVariation_196

class Item_99(Item):
    non_spare = NonSpare_613

class Variation_1021(Group):
    bit_size = 16
    items_list = [Item_95, Item_99]
    items_dict = {"AX": NonSpare_609, "AY": NonSpare_613}

class RuleVariation_989(RuleVariationContextFree):
    variation = Variation_1021

class NonSpare_376(NonSpare):
    name = "210"
    title = "Calculated Acceleration"
    rule = RuleVariation_989

class NonSpare_1222(NonSpare):
    name = "MOD3A"
    title = "Mode-3/A Reply in Octal Representation"
    rule = RuleVariation_751

class Item_533(Item):
    non_spare = NonSpare_1222

class Variation_989(Group):
    bit_size = 16
    items_list = [Item_3, Item_533]
    items_dict = {"MOD3A": NonSpare_1222}

class RuleVariation_959(RuleVariationContextFree):
    variation = Variation_989

class NonSpare_180(NonSpare):
    name = "060"
    title = "Mode-3/A Code in Octal Representation"
    rule = RuleVariation_959

class NonSpare_1769(NonSpare):
    name = "TID"
    title = "Target Identification"
    rule = RuleVariation_376

class Item_953(Item):
    non_spare = NonSpare_1769

class Variation_1178(Group):
    bit_size = 56
    items_list = [Item_906, Item_15, Item_953]
    items_dict = {"STI": NonSpare_1699, "TID": NonSpare_1769}

class RuleVariation_1125(RuleVariationContextFree):
    variation = Variation_1178

class NonSpare_406(NonSpare):
    name = "245"
    title = "Target Identification"
    rule = RuleVariation_1125

class Content_771(ContentBds):
    bds_type = BdsWithAddress

class RuleContent_770(RuleContentContextFree):
    variation = Content_771

class Variation_388(Element):
    bit_offset8 = 0
    bit_size = 64
    rule = RuleContent_770

class Variation_1332(Repetitive):
    rep_bytes = 1
    variation = Variation_388

class RuleVariation_1262(RuleVariationContextFree):
    variation = Variation_1332

class NonSpare_1154(NonSpare):
    name = "MB"
    title = "BDS"
    rule = RuleVariation_1262

class NonSpare_532(NonSpare):
    name = "ADR"
    title = "24 Bits Aircraft Address"
    rule = RuleVariation_335

class Content_325(ContentTable):
    values = {0: "No communications capability (surveillance only)", 1: "Comm. A and Comm. B capability", 2: "Comm. A, Comm. B and Uplink ELM", 3: "Comm. A, Comm. B, Uplink ELM and Downlink ELM", 4: "Level 5 Transponder capability", 5: "Not assigned", 6: "Not assigned", 7: "Not assigned"}

class RuleContent_325(RuleContentContextFree):
    variation = Content_325

class Variation_119(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_325

class RuleVariation_119(RuleVariationContextFree):
    variation = Variation_119

class NonSpare_735(NonSpare):
    name = "COM"
    title = "Communications Capability of the Transponder"
    rule = RuleVariation_119

class Item_182(Item):
    non_spare = NonSpare_735

class Content_306(ContentTable):
    values = {0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 6: "General Emergency", 7: "Lifeguard / medical", 8: "Minimum fuel", 9: "No communications", 10: "Unlawful"}

class RuleContent_306(RuleContentContextFree):
    variation = Content_306

class Variation_666(Element):
    bit_offset8 = 3
    bit_size = 4
    rule = RuleContent_306

class RuleVariation_656(RuleVariationContextFree):
    variation = Variation_666

class NonSpare_1687(NonSpare):
    name = "STAT"
    title = "Flight Status"
    rule = RuleVariation_656

class Item_895(Item):
    non_spare = NonSpare_1687

class Content_290(ContentTable):
    values = {0: "No", 1: "Yes"}

class RuleContent_290(RuleContentContextFree):
    variation = Content_290

class Variation_53(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_290

class RuleVariation_53(RuleVariationContextFree):
    variation = Variation_53

class NonSpare_1664(NonSpare):
    name = "SSC"
    title = "Specific Service Capability"
    rule = RuleVariation_53

class Item_881(Item):
    non_spare = NonSpare_1664

class Content_3(ContentTable):
    values = {0: "100 ft resolution", 1: "25 ft resolution"}

class RuleContent_3(RuleContentContextFree):
    variation = Content_3

class Variation_391(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_3

class RuleVariation_381(RuleVariationContextFree):
    variation = Variation_391

class NonSpare_581(NonSpare):
    name = "ARC"
    title = "Altitude Reporting Capability"
    rule = RuleVariation_381

class Item_77(Item):
    non_spare = NonSpare_581

class Variation_535(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_290

class RuleVariation_525(RuleVariationContextFree):
    variation = Variation_535

class NonSpare_549(NonSpare):
    name = "AIC"
    title = "Aircraft Identification Capability"
    rule = RuleVariation_525

class Item_54(Item):
    non_spare = NonSpare_549

class NonSpare_621(NonSpare):
    name = "B1A"
    title = "BDS 1,0 Bit 16"
    rule = RuleVariation_580

class Item_104(Item):
    non_spare = NonSpare_621

class NonSpare_622(NonSpare):
    name = "B1B"
    title = "BDS 1,0 Bit 37/40"
    rule = RuleVariation_741

class Item_105(Item):
    non_spare = NonSpare_622

class NonSpare_517(NonSpare):
    name = "AC"
    title = "ACAS Operational"
    rule = RuleVariation_53

class Item_37(Item):
    non_spare = NonSpare_517

class Variation_428(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_290

class RuleVariation_418(RuleVariationContextFree):
    variation = Variation_428

class NonSpare_1219(NonSpare):
    name = "MN"
    title = "Multiple Navigational Aids Operating"
    rule = RuleVariation_418

class Item_530(Item):
    non_spare = NonSpare_1219

class Content_546(ContentTable):
    values = {0: "Yes", 1: "No"}

class RuleContent_546(RuleContentContextFree):
    variation = Content_546

class Variation_562(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_546

class RuleVariation_552(RuleVariationContextFree):
    variation = Variation_562

class NonSpare_820(NonSpare):
    name = "DC"
    title = "Differential Correction"
    rule = RuleVariation_552

class Item_242(Item):
    non_spare = NonSpare_820

class Item_19(Spare):
    bit_offset8 = 3
    bit_size = 5

class Variation_1042(Group):
    bit_size = 24
    items_list = [Item_182, Item_895, Item_29, Item_881, Item_77, Item_54, Item_104, Item_105, Item_37, Item_530, Item_242, Item_19]
    items_dict = {"COM": NonSpare_735, "STAT": NonSpare_1687, "SSC": NonSpare_1664, "ARC": NonSpare_581, "AIC": NonSpare_549, "B1A": NonSpare_621, "B1B": NonSpare_622, "AC": NonSpare_517, "MN": NonSpare_1219, "DC": NonSpare_820}

class RuleVariation_1003(RuleVariationContextFree):
    variation = Variation_1042

class NonSpare_739(NonSpare):
    name = "COMACAS"
    title = "Communications/ACAS Capability and Flight Status"
    rule = RuleVariation_1003

class Variation_366(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_581

class RuleVariation_360(RuleVariationContextFree):
    variation = Variation_366

class NonSpare_527(NonSpare):
    name = "ACT"
    title = "Aircraft Derived Aircraft Type"
    rule = RuleVariation_360

class Content_554(ContentTable):
    values = {1: "Light aircraft <= 7000 kg", 2: "Reserved", 3: "7000 kg &lt; medium aircraft &lt; 136000 kg", 4: "Reserved", 5: "136000 kg <= heavy aircraft", 6: "Highly manoeuvrable (5g acceleration capability) and high speed (&gt;400 knots cruise)", 7: "Reserved", 8: "Reserved", 9: "Reserved", 10: "Rotocraft", 11: "Glider / sailplane", 12: "Lighter-than-air", 13: "Unmanned aerial vehicle", 14: "Space / transatmospheric vehicle", 15: "Ultralight / handglider / paraglider", 16: "Parachutist / skydiver", 17: "Reserved", 18: "Reserved", 19: "Reserved", 20: "Surface emergency vehicle", 21: "Surface service vehicle", 22: "Fixed ground or tethered obstruction", 23: "Reserved", 24: "Reserved"}

class RuleContent_554(RuleContentContextFree):
    variation = Content_554

class Variation_175(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_554

class RuleVariation_169(RuleVariationContextFree):
    variation = Variation_175

class NonSpare_863(NonSpare):
    name = "ECAT"
    title = "Emitter Category"
    rule = RuleVariation_169

class Content_530(ContentTable):
    values = {0: "VDL Mode 4 available", 1: "VDL Mode 4 not available"}

class RuleContent_530(RuleContentContextFree):
    variation = Content_530

class Variation_90(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_530

class RuleVariation_90(RuleVariationContextFree):
    variation = Variation_90

class NonSpare_1914(NonSpare):
    name = "VDL"
    title = "VDL Mode 4"
    rule = RuleVariation_90

class Item_1071(Item):
    non_spare = NonSpare_1914

class Content_270(ContentTable):
    values = {0: "Mode S available", 1: "Mode S not available"}

class RuleContent_270(RuleContentContextFree):
    variation = Content_270

class Variation_426(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_270

class RuleVariation_416(RuleVariationContextFree):
    variation = Variation_426

class NonSpare_1183(NonSpare):
    name = "MDS"
    title = "Mode S"
    rule = RuleVariation_416

class Item_509(Item):
    non_spare = NonSpare_1183

class Content_504(ContentTable):
    values = {0: "UAT available", 1: "UAT not available"}

class RuleContent_504(RuleContentContextFree):
    variation = Content_504

class Variation_560(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_504

class RuleVariation_550(RuleVariationContextFree):
    variation = Variation_560

class NonSpare_1879(NonSpare):
    name = "UAT"
    title = "UAT"
    rule = RuleVariation_550

class Item_1038(Item):
    non_spare = NonSpare_1879

class Variation_1232(Group):
    bit_size = 8
    items_list = [Item_1071, Item_509, Item_1038, Item_19]
    items_dict = {"VDL": NonSpare_1914, "MDS": NonSpare_1183, "UAT": NonSpare_1879}

class RuleVariation_1166(RuleVariationContextFree):
    variation = Variation_1232

class NonSpare_606(NonSpare):
    name = "AVTECH"
    title = "Available Technologies"
    rule = RuleVariation_1166

class Variation_1411(Compound):
    items_list = [NonSpare_1154, NonSpare_532, None, NonSpare_739, None, None, None, NonSpare_527, NonSpare_863, None, NonSpare_606]
    items_dict = {"MB": NonSpare_1154, "ADR": NonSpare_532, "COMACAS": NonSpare_739, "ACT": NonSpare_527, "ECAT": NonSpare_863, "AVTECH": NonSpare_606}

class RuleVariation_1341(RuleVariationContextFree):
    variation = Variation_1411

class NonSpare_451(NonSpare):
    name = "380"
    title = "Mode-S / ADS-B Related Data"
    rule = RuleVariation_1341

class Item_0(Spare):
    bit_offset8 = 0
    bit_size = 1

class Variation_481(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_0

class RuleVariation_471(RuleVariationContextFree):
    variation = Variation_481

class NonSpare_940(NonSpare):
    name = "FTN"
    title = "Fusion Track Number"
    rule = RuleVariation_471

class Item_330(Item):
    non_spare = NonSpare_940

class Variation_961(Group):
    bit_size = 16
    items_list = [Item_0, Item_330]
    items_dict = {"FTN": NonSpare_940}

class RuleVariation_932(RuleVariationContextFree):
    variation = Variation_961

class NonSpare_334(NonSpare):
    name = "161"
    title = "Track Number"
    rule = RuleVariation_932

class Content_284(ContentTable):
    values = {0: "Multisensor Track", 1: "Monosensor Track"}

class RuleContent_284(RuleContentContextFree):
    variation = Content_284

class Variation_51(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_284

class RuleVariation_51(RuleVariationContextFree):
    variation = Variation_51

class NonSpare_1235(NonSpare):
    name = "MON"
    title = ""
    rule = RuleVariation_51

class Item_546(Item):
    non_spare = NonSpare_1235

class Content_499(ContentTable):
    values = {0: "Transponder Ground bit not set or unknown", 1: "Transponder Ground bit set"}

class RuleContent_499(RuleContentContextFree):
    variation = Content_499

class Variation_449(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_499

class RuleVariation_439(RuleVariationContextFree):
    variation = Variation_449

class NonSpare_960(NonSpare):
    name = "GBS"
    title = ""
    rule = RuleVariation_439

class Item_342(Item):
    non_spare = NonSpare_960

class Content_44(ContentTable):
    values = {0: "Barometric altitude (Mode C) more reliable", 1: "Geometric altitude more reliable"}

class RuleContent_44(RuleContentContextFree):
    variation = Content_44

class Variation_493(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_44

class RuleVariation_483(RuleVariationContextFree):
    variation = Variation_493

class NonSpare_1238(NonSpare):
    name = "MRH"
    title = ""
    rule = RuleVariation_483

class Item_549(Item):
    non_spare = NonSpare_1238

class Content_357(ContentTable):
    values = {0: "No source", 1: "GPS", 2: "3d radar", 3: "Triangulation", 4: "Height from coverage", 5: "Speed look-up table", 6: "Default height", 7: "Multilateration"}

class RuleContent_357(RuleContentContextFree):
    variation = Content_357

class Variation_664(Element):
    bit_offset8 = 3
    bit_size = 3
    rule = RuleContent_357

class RuleVariation_654(RuleVariationContextFree):
    variation = Variation_664

class NonSpare_1656(NonSpare):
    name = "SRC"
    title = ""
    rule = RuleVariation_654

class Item_876(Item):
    non_spare = NonSpare_1656

class Content_59(ContentTable):
    values = {0: "Confirmed track", 1: "Tentative track"}

class RuleContent_59(RuleContentContextFree):
    variation = Content_59

class Variation_875(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_59

class RuleVariation_846(RuleVariationContextFree):
    variation = Variation_875

class NonSpare_699(NonSpare):
    name = "CNF"
    title = ""
    rule = RuleVariation_846

class Item_161(Item):
    non_spare = NonSpare_699

class Content_16(ContentTable):
    values = {0: "Actual Track", 1: "Simulated track"}

class RuleContent_16(RuleContentContextFree):
    variation = Content_16

class Variation_3(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_16

class RuleVariation_3(RuleVariationContextFree):
    variation = Variation_3

class NonSpare_1631(NonSpare):
    name = "SIM"
    title = ""
    rule = RuleVariation_3

class Item_853(Item):
    non_spare = NonSpare_1631

class Content_190(ContentTable):
    values = {0: "Default value", 1: "Track service end (i.e. last message transmitted to the user for the track)"}

class RuleContent_190(RuleContentContextFree):
    variation = Content_190

class Variation_414(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_190

class RuleVariation_404(RuleVariationContextFree):
    variation = Variation_414

class NonSpare_1831(NonSpare):
    name = "TSE"
    title = ""
    rule = RuleVariation_404

class Item_995(Item):
    non_spare = NonSpare_1831

class Content_189(ContentTable):
    values = {0: "Default value", 1: "Track service begin (i.e. first message transmitted to the user for the track)"}

class RuleContent_189(RuleContentContextFree):
    variation = Content_189

class Variation_513(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_189

class RuleVariation_503(RuleVariationContextFree):
    variation = Variation_513

class NonSpare_1829(NonSpare):
    name = "TSB"
    title = ""
    rule = RuleVariation_503

class Item_993(Item):
    non_spare = NonSpare_1829

class Content_297(ContentTable):
    values = {0: "No Mode 4 interrogationt", 1: "Friendly target", 2: "Unknown target", 3: "No reply"}

class RuleContent_297(RuleContentContextFree):
    variation = Content_297

class Variation_653(Element):
    bit_offset8 = 3
    bit_size = 2
    rule = RuleContent_297

class RuleVariation_643(RuleVariationContextFree):
    variation = Variation_653

class NonSpare_930(NonSpare):
    name = "FRIFOE"
    title = ""
    rule = RuleVariation_643

class Item_323(Item):
    non_spare = NonSpare_930

class Content_182(ContentTable):
    values = {0: "Default value", 1: "Military Emergency present in the last report received from a sensor capable of decoding this data"}

class RuleContent_182(RuleContentContextFree):
    variation = Content_182

class Variation_815(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_182

class RuleVariation_786(RuleVariationContextFree):
    variation = Variation_815

class NonSpare_1191(NonSpare):
    name = "ME"
    title = ""
    rule = RuleVariation_786

class Item_514(Item):
    non_spare = NonSpare_1191

class Content_203(ContentTable):
    values = {0: "End of Data Item", 1: "Military Identification present in the last report received from a sensor capable of decoding this data"}

class RuleContent_203(RuleContentContextFree):
    variation = Content_203

class Variation_894(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_203

class RuleVariation_865(RuleVariationContextFree):
    variation = Variation_894

class NonSpare_1209(NonSpare):
    name = "MI"
    title = ""
    rule = RuleVariation_865

class Item_521(Item):
    non_spare = NonSpare_1209

class Content_490(ContentTable):
    values = {0: "Track not resulting from amalgamation process", 1: "Track resulting from amalgamation process"}

class RuleContent_490(RuleContentContextFree):
    variation = Content_490

class Variation_85(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_490

class RuleVariation_85(RuleVariationContextFree):
    variation = Variation_85

class NonSpare_562(NonSpare):
    name = "AMA"
    title = ""
    rule = RuleVariation_85

class Item_67(Item):
    non_spare = NonSpare_562

class Content_184(ContentTable):
    values = {0: "Default value", 1: "SPI present in the last report received from a sensor capable of decoding this data"}

class RuleContent_184(RuleContentContextFree):
    variation = Content_184

class Variation_413(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_184

class RuleVariation_403(RuleVariationContextFree):
    variation = Variation_413

class NonSpare_1647(NonSpare):
    name = "SPI"
    title = ""
    rule = RuleVariation_403

class Item_867(Item):
    non_spare = NonSpare_1647

class Content_173(ContentTable):
    values = {0: "Default value", 1: "Age of the last received track update is higher than system dependent threshold (coasting)"}

class RuleContent_173(RuleContentContextFree):
    variation = Content_173

class Variation_510(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_173

class RuleVariation_500(RuleVariationContextFree):
    variation = Variation_510

class NonSpare_787(NonSpare):
    name = "CST"
    title = ""
    rule = RuleVariation_500

class Item_214(Item):
    non_spare = NonSpare_787

class Content_381(ContentTable):
    values = {0: "Not flight-plan correlated", 1: "Flight plan correlated"}

class RuleContent_381(RuleContentContextFree):
    variation = Content_381

class Variation_632(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_381

class RuleVariation_622(RuleVariationContextFree):
    variation = Variation_632

class NonSpare_922(NonSpare):
    name = "FPC"
    title = ""
    rule = RuleVariation_622

class Item_316(Item):
    non_spare = NonSpare_922

class Content_166(ContentTable):
    values = {0: "Default value", 1: "ADS-B data inconsistent with other surveillance information"}

class RuleContent_166(RuleContentContextFree):
    variation = Content_166

class Variation_689(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_166

class RuleVariation_679(RuleVariationContextFree):
    variation = Variation_689

class NonSpare_541(NonSpare):
    name = "AFF"
    title = ""
    rule = RuleVariation_679

class Item_49(Item):
    non_spare = NonSpare_541

class Variation_1298(Extended):
    items = [Item_546, Item_342, Item_549, Item_876, Item_161, None, Item_853, Item_995, Item_993, Item_323, Item_514, Item_521, None, Item_67, Item_867, Item_214, Item_316, Item_49, Item_24, None]

class RuleVariation_1228(RuleVariationContextFree):
    variation = Variation_1298

class NonSpare_351(NonSpare):
    name = "170"
    title = "Track Status"
    rule = RuleVariation_1228

class Content_730(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "s"

class RuleContent_729(RuleContentContextFree):
    variation = Content_730

class Variation_226(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_729

class RuleVariation_220(RuleVariationContextFree):
    variation = Variation_226

class NonSpare_1389(NonSpare):
    name = "PSR"
    title = "Age of The Last Primary Detection Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1668(NonSpare):
    name = "SSR"
    title = "Age of the Last Secondary Detection Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1175(NonSpare):
    name = "MDA"
    title = "Age of the Last Mode A Detection Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1197(NonSpare):
    name = "MFL"
    title = "Age of the Last Mode C Detection Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1181(NonSpare):
    name = "MDS"
    title = "Age of the Last Mode S Detection Used to Update the Track"
    rule = RuleVariation_220

class Variation_317(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_729

class RuleVariation_311(RuleVariationContextFree):
    variation = Variation_317

class NonSpare_538(NonSpare):
    name = "ADS"
    title = "Age of the Last ADS Report Used to Update the Track"
    rule = RuleVariation_311

class NonSpare_528(NonSpare):
    name = "ADB"
    title = "Age of the Last ADS-B Report Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1164(NonSpare):
    name = "MD1"
    title = "Age of the Last Valid Mode 1 Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1166(NonSpare):
    name = "MD2"
    title = "Age of the Last Mode 2 Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1118(NonSpare):
    name = "LOP"
    title = "Age of the Last Magentic Loop Detection"
    rule = RuleVariation_220

class NonSpare_1818(NonSpare):
    name = "TRK"
    title = "Actual Track Age Since First Occurrence"
    rule = RuleVariation_220

class NonSpare_1259(NonSpare):
    name = "MUL"
    title = "Age of the Last Multilateration Detection"
    rule = RuleVariation_220

class Variation_1416(Compound):
    items_list = [NonSpare_1389, NonSpare_1668, NonSpare_1175, NonSpare_1197, NonSpare_1181, NonSpare_538, NonSpare_528, NonSpare_1164, NonSpare_1166, NonSpare_1118, NonSpare_1818, NonSpare_1259]
    items_dict = {"PSR": NonSpare_1389, "SSR": NonSpare_1668, "MDA": NonSpare_1175, "MFL": NonSpare_1197, "MDS": NonSpare_1181, "ADS": NonSpare_538, "ADB": NonSpare_528, "MD1": NonSpare_1164, "MD2": NonSpare_1166, "LOP": NonSpare_1118, "TRK": NonSpare_1818, "MUL": NonSpare_1259}

class RuleVariation_1346(RuleVariationContextFree):
    variation = Variation_1416

class NonSpare_430(NonSpare):
    name = "290"
    title = "System Track Update Ages"
    rule = RuleVariation_1346

class Content_527(ContentTable):
    values = {0: "Unknown", 1: "On stand", 2: "Taxiing for departure", 3: "Taxiing for arrival", 4: "Runway for departure", 5: "Runway for arrival", 6: "Hold for departure", 7: "Hold for arrival", 8: "Push back", 9: "On finals"}

class RuleContent_527(RuleContentContextFree):
    variation = Content_527

class Variation_172(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_527

class RuleVariation_166(RuleVariationContextFree):
    variation = Variation_172

class NonSpare_468(NonSpare):
    name = "430"
    title = "Phase of Flight"
    rule = RuleVariation_166

class Content_628(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "FL"

class RuleContent_628(RuleContentContextFree):
    variation = Content_628

class Variation_265(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_628

class RuleVariation_259(RuleVariationContextFree):
    variation = Variation_265

class NonSpare_234(NonSpare):
    name = "090"
    title = "Measured Flight Level"
    rule = RuleVariation_259

class Content_302(ContentTable):
    values = {0: "No QNH correction applied", 1: "QNH correction applied"}

class RuleContent_302(RuleContentContextFree):
    variation = Content_302

class Variation_56(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_302

class RuleVariation_56(RuleVariationContextFree):
    variation = Variation_56

class NonSpare_1445(NonSpare):
    name = "QNH"
    title = "QNH Correction Applied"
    rule = RuleVariation_56

class Item_723(Item):
    non_spare = NonSpare_1445

class Content_626(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "FL"

class RuleContent_626(RuleContentContextFree):
    variation = Content_626

class Variation_482(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_626

class RuleVariation_472(RuleVariationContextFree):
    variation = Variation_482

class NonSpare_796(NonSpare):
    name = "CTBA"
    title = "Calculated Track Barometric Altitude"
    rule = RuleVariation_472

class Item_222(Item):
    non_spare = NonSpare_796

class Variation_1130(Group):
    bit_size = 16
    items_list = [Item_723, Item_222]
    items_dict = {"QNH": NonSpare_1445, "CTBA": NonSpare_796}

class RuleVariation_1084(RuleVariationContextFree):
    variation = Variation_1130

class NonSpare_247(NonSpare):
    name = "093"
    title = "Calculated Track Barometric Altitude"
    rule = RuleVariation_1084

class Content_658(ContentQuantity):
    signedness = Signed
    lsb = 6.25
    unit = "ft"

class RuleContent_658(RuleContentContextFree):
    variation = Content_658

class Variation_283(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_658

class RuleVariation_277(RuleVariationContextFree):
    variation = Variation_283

class NonSpare_244(NonSpare):
    name = "092"
    title = "Calculated Track Geometric Altitude"
    rule = RuleVariation_277

class Content_660(ContentQuantity):
    signedness = Signed
    lsb = 6.25
    unit = "ft/min"

class RuleContent_660(RuleContentContextFree):
    variation = Content_660

class Variation_285(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_660

class RuleVariation_279(RuleVariationContextFree):
    variation = Variation_285

class NonSpare_385(NonSpare):
    name = "215"
    title = "Calculated Rate Of Climb/Descent"
    rule = RuleVariation_279

class NonSpare_426(NonSpare):
    name = "270"
    title = "Target Size and Orientation"
    rule = RuleVariation_1224

class NonSpare_1619(NonSpare):
    name = "SIC"
    title = "System Identity Code"
    rule = RuleVariation_154

class Item_843(Item):
    non_spare = NonSpare_1619

class Variation_1157(Group):
    bit_size = 16
    items_list = [Item_811, Item_843]
    items_dict = {"SAC": NonSpare_1563, "SIC": NonSpare_1619}

class RuleVariation_1105(RuleVariationContextFree):
    variation = Variation_1157

class NonSpare_924(NonSpare):
    name = "FPPSID"
    title = "FPPS Identification Tag"
    rule = RuleVariation_1105

class NonSpare_783(NonSpare):
    name = "CSN"
    title = "Callsign"
    rule = RuleVariation_378

class Content_404(ContentTable):
    values = {0: "Plan number", 1: "Unit 1 internal flight number", 2: "Unit 2 internal flight number", 3: "Unit 3 internal flight number"}

class RuleContent_404(RuleContentContextFree):
    variation = Content_404

class Variation_107(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_404

class RuleVariation_107(RuleVariationContextFree):
    variation = Variation_107

class NonSpare_1865(NonSpare):
    name = "TYP"
    title = "IFPS Flight ID Type"
    rule = RuleVariation_107

class Item_1025(Item):
    non_spare = NonSpare_1865

class Item_13(Spare):
    bit_offset8 = 2
    bit_size = 3

class Variation_864(Element):
    bit_offset8 = 5
    bit_size = 27
    rule = RuleContent_0

class RuleVariation_835(RuleVariationContextFree):
    variation = Variation_864

class NonSpare_1276(NonSpare):
    name = "NBR"
    title = "IFPS Flight ID Number"
    rule = RuleVariation_835

class Item_581(Item):
    non_spare = NonSpare_1276

class Variation_1195(Group):
    bit_size = 32
    items_list = [Item_1025, Item_13, Item_581]
    items_dict = {"TYP": NonSpare_1865, "NBR": NonSpare_1276}

class RuleVariation_1136(RuleVariationContextFree):
    variation = Variation_1195

class NonSpare_1048(NonSpare):
    name = "IFPSFLIGHTID"
    title = "IFPS_FLIGHT_ID"
    rule = RuleVariation_1136

class NonSpare_958(NonSpare):
    name = "GATOAT"
    title = "Flight Type"
    rule = RuleVariation_111

class Item_340(Item):
    non_spare = NonSpare_958

class NonSpare_1549(NonSpare):
    name = "RVSM"
    title = "RVSM"
    rule = RuleVariation_730

class Item_801(Item):
    non_spare = NonSpare_1549

class NonSpare_1009(NonSpare):
    name = "HPR"
    title = "Flight Priority"
    rule = RuleVariation_881

class Item_378(Item):
    non_spare = NonSpare_1009

class Variation_1077(Group):
    bit_size = 8
    items_list = [Item_340, Item_322, Item_801, Item_378, Item_29]
    items_dict = {"GATOAT": NonSpare_958, "FR1FR2": NonSpare_929, "RVSM": NonSpare_1549, "HPR": NonSpare_1009}

class RuleVariation_1036(RuleVariationContextFree):
    variation = Variation_1077

class NonSpare_912(NonSpare):
    name = "FLIGHTCAT"
    title = "Flight Category"
    rule = RuleVariation_1036

class NonSpare_1786(NonSpare):
    name = "TOA"
    title = "Type of Aircraft"
    rule = RuleVariation_360

class Content_580(ContentTable):
    values = {76: "Light", 77: "Medium", 72: "Heavy", 74: "Super"}

class RuleContent_580(RuleContentContextFree):
    variation = Content_580

class Variation_193(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_580

class RuleVariation_187(RuleVariationContextFree):
    variation = Variation_193

class NonSpare_1952(NonSpare):
    name = "WTC"
    title = "Wake Turbulence Category"
    rule = RuleVariation_187

class NonSpare_529(NonSpare):
    name = "ADEP"
    title = "Departure Airport"
    rule = RuleVariation_360

class NonSpare_530(NonSpare):
    name = "ADES"
    title = "Destination Airport"
    rule = RuleVariation_360

class Variation_342(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_581

class RuleVariation_336(RuleVariationContextFree):
    variation = Variation_342

class NonSpare_1552(NonSpare):
    name = "RWY"
    title = "Runway Designation"
    rule = RuleVariation_336

class NonSpare_677(NonSpare):
    name = "CFL"
    title = "Current Cleared Flight Level"
    rule = RuleVariation_306

class NonSpare_674(NonSpare):
    name = "CENTRE"
    title = "8-bit Group Identification Code"
    rule = RuleVariation_154

class Item_144(Item):
    non_spare = NonSpare_674

class NonSpare_1369(NonSpare):
    name = "POSITION"
    title = "8-bit Control Position Identification Code"
    rule = RuleVariation_154

class Item_657(Item):
    non_spare = NonSpare_1369

class Variation_1032(Group):
    bit_size = 16
    items_list = [Item_144, Item_657]
    items_dict = {"CENTRE": NonSpare_674, "POSITION": NonSpare_1369}

class RuleVariation_995(RuleVariationContextFree):
    variation = Variation_1032

class NonSpare_663(NonSpare):
    name = "CCP"
    title = "Current Control Position"
    rule = RuleVariation_995

class Content_432(ContentTable):
    values = {0: "Scheduled off-block time", 1: "Estimated off-block time", 2: "Estimated take-off time", 3: "Actual off-block time", 4: "Predicted time at runway hold", 5: "Actual time at runway hold", 6: "Actual line-up time", 7: "Actual take-off time", 8: "Estimated time of arrival", 9: "Predicted landing time", 10: "Actual landing time", 11: "Actual time off runway", 12: "Predicted time to gate", 13: "Actual on-block time"}

class RuleContent_432(RuleContentContextFree):
    variation = Content_432

class Variation_140(Element):
    bit_offset8 = 0
    bit_size = 5
    rule = RuleContent_432

class RuleVariation_140(RuleVariationContextFree):
    variation = Variation_140

class NonSpare_1868(NonSpare):
    name = "TYP"
    title = "Time Type"
    rule = RuleVariation_140

class Item_1028(Item):
    non_spare = NonSpare_1868

class Content_486(ContentTable):
    values = {0: "Today", 1: "Yesterday", 2: "Tomorrow"}

class RuleContent_486(RuleContentContextFree):
    variation = Content_486

class Variation_853(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_486

class RuleVariation_824(RuleVariationContextFree):
    variation = Variation_853

class NonSpare_812(NonSpare):
    name = "DAY"
    title = "Day"
    rule = RuleVariation_824

class Item_234(Item):
    non_spare = NonSpare_812

class Item_30(Spare):
    bit_offset8 = 7
    bit_size = 4

class Content_590(ContentInteger):
    signedness = Unsigned

class RuleContent_590(RuleContentContextFree):
    variation = Content_590

class Variation_670(Element):
    bit_offset8 = 3
    bit_size = 5
    rule = RuleContent_590

class RuleVariation_660(RuleVariationContextFree):
    variation = Variation_670

class NonSpare_1005(NonSpare):
    name = "HOR"
    title = "Hours, from 0 to 23"
    rule = RuleVariation_660

class Item_375(Item):
    non_spare = NonSpare_1005

class Item_1(Spare):
    bit_offset8 = 0
    bit_size = 2

class Content_591(ContentInteger):
    signedness = Unsigned

class RuleContent_591(RuleContentContextFree):
    variation = Content_591

class Variation_580(Element):
    bit_offset8 = 2
    bit_size = 6
    rule = RuleContent_591

class RuleVariation_570(RuleVariationContextFree):
    variation = Variation_580

class NonSpare_1213(NonSpare):
    name = "MIN"
    title = "Minutes, from 0 to 59"
    rule = RuleVariation_570

class Item_525(Item):
    non_spare = NonSpare_1213

class Content_433(ContentTable):
    values = {0: "Seconds available", 1: "Seconds not available"}

class RuleContent_433(RuleContentContextFree):
    variation = Content_433

class Variation_70(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_433

class RuleVariation_70(RuleVariationContextFree):
    variation = Variation_70

class NonSpare_604(NonSpare):
    name = "AVS"
    title = "Seconds Available"
    rule = RuleVariation_70

class Item_91(Item):
    non_spare = NonSpare_604

class NonSpare_1609(NonSpare):
    name = "SEC"
    title = "Seconds, from 0 to 59"
    rule = RuleVariation_570

class Item_835(Item):
    non_spare = NonSpare_1609

class Variation_1198(Group):
    bit_size = 32
    items_list = [Item_1028, Item_234, Item_30, Item_375, Item_1, Item_525, Item_91, Item_7, Item_835]
    items_dict = {"TYP": NonSpare_1868, "DAY": NonSpare_812, "HOR": NonSpare_1005, "MIN": NonSpare_1213, "AVS": NonSpare_604, "SEC": NonSpare_1609}

class Variation_1364(Repetitive):
    rep_bytes = 1
    variation = Variation_1198

class RuleVariation_1294(RuleVariationContextFree):
    variation = Variation_1364

class NonSpare_1787(NonSpare):
    name = "TOD"
    title = "Time of Departure"
    rule = RuleVariation_1294

class NonSpare_591(NonSpare):
    name = "AST"
    title = "Aircraft Stand"
    rule = RuleVariation_375

class Content_199(ContentTable):
    values = {0: "Empty", 1: "Occupied", 2: "Unknown"}

class RuleContent_199(RuleContentContextFree):
    variation = Content_199

class Variation_97(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_199

class RuleVariation_97(RuleVariationContextFree):
    variation = Variation_97

class NonSpare_876(NonSpare):
    name = "EMP"
    title = "Stand Empty"
    rule = RuleVariation_97

class Item_281(Item):
    non_spare = NonSpare_876

class Content_42(ContentTable):
    values = {0: "Available", 1: "Not available", 2: "Unknown"}

class RuleContent_42(RuleContentContextFree):
    variation = Content_42

class Variation_565(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_42

class RuleVariation_555(RuleVariationContextFree):
    variation = Variation_565

class NonSpare_602(NonSpare):
    name = "AVL"
    title = "Stand Available"
    rule = RuleVariation_555

class Item_89(Item):
    non_spare = NonSpare_602

class Item_22(Spare):
    bit_offset8 = 4
    bit_size = 4

class Variation_1051(Group):
    bit_size = 8
    items_list = [Item_281, Item_89, Item_22]
    items_dict = {"EMP": NonSpare_876, "AVL": NonSpare_602}

class RuleVariation_1010(RuleVariationContextFree):
    variation = Variation_1051

class NonSpare_1708(NonSpare):
    name = "STS"
    title = "Stand Status"
    rule = RuleVariation_1010

class Variation_1405(Compound):
    items_list = [NonSpare_924, NonSpare_783, NonSpare_1048, NonSpare_912, NonSpare_1786, NonSpare_1952, NonSpare_529, NonSpare_530, NonSpare_1552, NonSpare_677, NonSpare_663, NonSpare_1787, NonSpare_591, NonSpare_1708]
    items_dict = {"FPPSID": NonSpare_924, "CSN": NonSpare_783, "IFPSFLIGHTID": NonSpare_1048, "FLIGHTCAT": NonSpare_912, "TOA": NonSpare_1786, "WTC": NonSpare_1952, "ADEP": NonSpare_529, "ADES": NonSpare_530, "RWY": NonSpare_1552, "CFL": NonSpare_677, "CCP": NonSpare_663, "TOD": NonSpare_1787, "AST": NonSpare_591, "STS": NonSpare_1708}

class RuleVariation_1335(RuleVariationContextFree):
    variation = Variation_1405

class NonSpare_454(NonSpare):
    name = "390"
    title = "Flight Plan Related Data"
    rule = RuleVariation_1335

class Content_209(ContentTable):
    values = {0: "Flyco (follow me)", 1: "ATC equipment maintenance", 2: "Airport maintenance", 3: "Fire", 4: "Bird scarer", 5: "Snow plough", 6: "Runway sweeper", 7: "Emergency", 8: "Police", 9: "Bus", 10: "Tug (push/tow)", 11: "Grass cutter", 12: "Fuel", 13: "Baggage", 14: "Catering", 15: "Aircraft maintenance", 16: "Unknown"}

class RuleContent_209(RuleContentContextFree):
    variation = Content_209

class Variation_161(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_209

class RuleVariation_156(RuleVariationContextFree):
    variation = Variation_161

class NonSpare_438(NonSpare):
    name = "300"
    title = "Vehicle Fleet Identification"
    rule = RuleVariation_156

class NonSpare_1810(NonSpare):
    name = "TRB"
    title = "In Trouble"
    rule = RuleVariation_29

class Item_980(Item):
    non_spare = NonSpare_1810

class Content_573(ContentTable):
    values = {1: "Towing aircraft", 2: "FOLLOW-ME operation", 3: "Runway check", 4: "Emergency operation (fire, medical...)", 5: "Work in progress (maintenance, birds scarer, sweepers...)"}

class RuleContent_573(RuleContentContextFree):
    variation = Content_573

class Variation_477(Element):
    bit_offset8 = 1
    bit_size = 7
    rule = RuleContent_573

class RuleVariation_467(RuleVariationContextFree):
    variation = Variation_477

class NonSpare_1254(NonSpare):
    name = "MSG"
    title = "Message"
    rule = RuleVariation_467

class Item_563(Item):
    non_spare = NonSpare_1254

class Variation_1190(Group):
    bit_size = 8
    items_list = [Item_980, Item_563]
    items_dict = {"TRB": NonSpare_1810, "MSG": NonSpare_1254}

class RuleVariation_1133(RuleVariationContextFree):
    variation = Variation_1190

class NonSpare_442(NonSpare):
    name = "310"
    title = "Pre-programmed Message"
    rule = RuleVariation_1133

class NonSpare_1973(NonSpare):
    name = "X"
    title = "Estimated Accuracy of the Calculated Position of X Component"
    rule = RuleVariation_217

class Item_1122(Item):
    non_spare = NonSpare_1973

class NonSpare_2025(NonSpare):
    name = "Y"
    title = "Estimated Accuracy of the Calculated Position of Y Component"
    rule = RuleVariation_217

class Item_1171(Item):
    non_spare = NonSpare_2025

class Variation_1258(Group):
    bit_size = 16
    items_list = [Item_1122, Item_1171]
    items_dict = {"X": NonSpare_1973, "Y": NonSpare_2025}

class RuleVariation_1192(RuleVariationContextFree):
    variation = Variation_1258

class NonSpare_571(NonSpare):
    name = "APC"
    title = "Estimated Accuracy Of Track Position (Cartesian)"
    rule = RuleVariation_1192

class Content_673(ContentQuantity):
    signedness = Signed
    lsb = 8.381903171539307e-8
    unit = "°"

class RuleContent_673(RuleContentContextFree):
    variation = Content_673

class Variation_289(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_673

class RuleVariation_283(RuleVariationContextFree):
    variation = Variation_289

class NonSpare_1073(NonSpare):
    name = "LAT"
    title = "APW Latitude Component Accuracy"
    rule = RuleVariation_283

class Item_430(Item):
    non_spare = NonSpare_1073

class NonSpare_1100(NonSpare):
    name = "LON"
    title = "APW Longitude Component Accuracy"
    rule = RuleVariation_283

class Item_455(Item):
    non_spare = NonSpare_1100

class Variation_1088(Group):
    bit_size = 32
    items_list = [Item_430, Item_455]
    items_dict = {"LAT": NonSpare_1073, "LON": NonSpare_1100}

class RuleVariation_1047(RuleVariationContextFree):
    variation = Variation_1088

class NonSpare_576(NonSpare):
    name = "APW"
    title = "Estimated Accuracy Of Track Position (WGS84)"
    rule = RuleVariation_1047

class Variation_256(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_611

class RuleVariation_250(RuleVariationContextFree):
    variation = Variation_256

class NonSpare_593(NonSpare):
    name = "ATH"
    title = "Estimated Accuracy Of Track Height"
    rule = RuleVariation_250

class Content_709(ContentQuantity):
    signedness = Unsigned
    lsb = 0.1
    unit = "m/s"

class RuleContent_709(RuleContentContextFree):
    variation = Content_709

class Variation_219(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_709

class RuleVariation_213(RuleVariationContextFree):
    variation = Variation_219

class NonSpare_1974(NonSpare):
    name = "X"
    title = "Estimated Accuracy of the Calculated Velocity of X Component"
    rule = RuleVariation_213

class Item_1123(Item):
    non_spare = NonSpare_1974

class NonSpare_2026(NonSpare):
    name = "Y"
    title = "Estimated Accuracy of the Calculated Velocity of Y Component"
    rule = RuleVariation_213

class Item_1172(Item):
    non_spare = NonSpare_2026

class Variation_1259(Group):
    bit_size = 16
    items_list = [Item_1123, Item_1172]
    items_dict = {"X": NonSpare_1974, "Y": NonSpare_2026}

class RuleVariation_1193(RuleVariationContextFree):
    variation = Variation_1259

class NonSpare_600(NonSpare):
    name = "AVC"
    title = "Estimated Accuracy Of Track Velocity (Cartesian)"
    rule = RuleVariation_1193

class Content_614(ContentQuantity):
    signedness = Signed
    lsb = 0.1
    unit = "m/s"

class RuleContent_614(RuleContentContextFree):
    variation = Content_614

class Variation_257(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_614

class RuleVariation_251(RuleVariationContextFree):
    variation = Variation_257

class NonSpare_584(NonSpare):
    name = "ARC"
    title = "Estimated Accuracy Of Rate Of Climb / Descent"
    rule = RuleVariation_251

class Content_717(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-2
    unit = "m/s²"

class RuleContent_717(RuleContentContextFree):
    variation = Content_717

class Variation_221(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_717

class RuleVariation_215(RuleVariationContextFree):
    variation = Variation_221

class NonSpare_1972(NonSpare):
    name = "X"
    title = "Estimated Accuracy Of Acceleration of X Component"
    rule = RuleVariation_215

class Item_1121(Item):
    non_spare = NonSpare_1972

class NonSpare_2024(NonSpare):
    name = "Y"
    title = "Estimated Accuracy Of Acceleration of Y Component"
    rule = RuleVariation_215

class Item_1170(Item):
    non_spare = NonSpare_2024

class Variation_1257(Group):
    bit_size = 16
    items_list = [Item_1121, Item_1170]
    items_dict = {"X": NonSpare_1972, "Y": NonSpare_2024}

class RuleVariation_1191(RuleVariationContextFree):
    variation = Variation_1257

class NonSpare_514(NonSpare):
    name = "AAC"
    title = "Estimated Accuracy Of Acceleration (Cartesian)"
    rule = RuleVariation_1191

class Variation_1394(Compound):
    items_list = [NonSpare_571, NonSpare_576, NonSpare_593, NonSpare_600, NonSpare_584, NonSpare_514]
    items_dict = {"APC": NonSpare_571, "APW": NonSpare_576, "ATH": NonSpare_593, "AVC": NonSpare_600, "ARC": NonSpare_584, "AAC": NonSpare_514}

class RuleVariation_1324(RuleVariationContextFree):
    variation = Variation_1394

class NonSpare_478(NonSpare):
    name = "500"
    title = "Estimated Accuracies"
    rule = RuleVariation_1324

class Content_30(ContentTable):
    values = {0: "Alert acknowledged", 1: "Alert not acknowledged"}

class RuleContent_30(RuleContentContextFree):
    variation = Content_30

class Variation_10(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_30

class RuleVariation_10(RuleVariationContextFree):
    variation = Variation_10

class NonSpare_523(NonSpare):
    name = "ACK"
    title = "Alert Acknowleged"
    rule = RuleVariation_10

class Item_41(Item):
    non_spare = NonSpare_523

class Content_201(ContentTable):
    values = {0: "End fo alert", 1: "Pre-alarm", 2: "Severe alert"}

class RuleContent_201(RuleContentContextFree):
    variation = Content_201

class Variation_455(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_201

class RuleVariation_445(RuleVariationContextFree):
    variation = Variation_455

class NonSpare_1722(NonSpare):
    name = "SVR"
    title = "Alert Severity"
    rule = RuleVariation_445

class Item_922(Item):
    non_spare = NonSpare_1722

class NonSpare_592(NonSpare):
    name = "AT"
    title = "Alert Type"
    rule = RuleVariation_154

class Item_83(Item):
    non_spare = NonSpare_592

class NonSpare_564(NonSpare):
    name = "AN"
    title = "Alert Number"
    rule = RuleVariation_154

class Item_69(Item):
    non_spare = NonSpare_564

class Variation_1012(Group):
    bit_size = 24
    items_list = [Item_41, Item_922, Item_19, Item_83, Item_69]
    items_dict = {"ACK": NonSpare_523, "SVR": NonSpare_1722, "AT": NonSpare_592, "AN": NonSpare_564}

class RuleVariation_980(RuleVariationContextFree):
    variation = Variation_1012

class NonSpare_490(NonSpare):
    name = "600"
    title = "Alert Messages"
    rule = RuleVariation_980

class NonSpare_941(NonSpare):
    name = "FTN"
    title = "Fusion Track Number"
    rule = RuleVariation_750

class Item_331(Item):
    non_spare = NonSpare_941

class Variation_987(Group):
    bit_size = 16
    items_list = [Item_3, Item_331]
    items_dict = {"FTN": NonSpare_941}

class Variation_1335(Repetitive):
    rep_bytes = 1
    variation = Variation_987

class RuleVariation_1265(RuleVariationContextFree):
    variation = Variation_1335

class NonSpare_498(NonSpare):
    name = "605"
    title = "Tracks in Alert"
    rule = RuleVariation_1265

class NonSpare_641(NonSpare):
    name = "BKN"
    title = "Bank Number"
    rule = RuleVariation_127

class Item_123(Item):
    non_spare = NonSpare_641

class Content_234(ContentTable):
    values = {0: "Indicator on", 1: "Indicator off"}

class RuleContent_234(RuleContentContextFree):
    variation = Content_234

class Variation_702(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_234

class RuleVariation_692(RuleVariationContextFree):
    variation = Variation_702

class NonSpare_1018(NonSpare):
    name = "I1"
    title = "Indicator 1"
    rule = RuleVariation_692

class Item_385(Item):
    non_spare = NonSpare_1018

class Variation_827(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_234

class RuleVariation_798(RuleVariationContextFree):
    variation = Variation_827

class NonSpare_1022(NonSpare):
    name = "I2"
    title = "Indicator 2"
    rule = RuleVariation_798

class Item_389(Item):
    non_spare = NonSpare_1022

class Variation_903(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_234

class RuleVariation_874(RuleVariationContextFree):
    variation = Variation_903

class NonSpare_1023(NonSpare):
    name = "I3"
    title = "Indicator 3"
    rule = RuleVariation_874

class Item_390(Item):
    non_spare = NonSpare_1023

class Variation_947(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_234

class RuleVariation_918(RuleVariationContextFree):
    variation = Variation_947

class NonSpare_1024(NonSpare):
    name = "I4"
    title = "Indicator 4"
    rule = RuleVariation_918

class Item_391(Item):
    non_spare = NonSpare_1024

class Variation_47(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_234

class RuleVariation_47(RuleVariationContextFree):
    variation = Variation_47

class NonSpare_1025(NonSpare):
    name = "I5"
    title = "Indicator 5"
    rule = RuleVariation_47

class Item_392(Item):
    non_spare = NonSpare_1025

class Variation_423(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_234

class RuleVariation_413(RuleVariationContextFree):
    variation = Variation_423

class NonSpare_1026(NonSpare):
    name = "I6"
    title = "Indicator 6"
    rule = RuleVariation_413

class Item_393(Item):
    non_spare = NonSpare_1026

class Variation_520(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_234

class RuleVariation_510(RuleVariationContextFree):
    variation = Variation_520

class NonSpare_1027(NonSpare):
    name = "I7"
    title = "Indicator 7"
    rule = RuleVariation_510

class Item_394(Item):
    non_spare = NonSpare_1027

class Variation_619(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_234

class RuleVariation_609(RuleVariationContextFree):
    variation = Variation_619

class NonSpare_1028(NonSpare):
    name = "I8"
    title = "Indicator 8"
    rule = RuleVariation_609

class Item_395(Item):
    non_spare = NonSpare_1028

class NonSpare_1029(NonSpare):
    name = "I9"
    title = "Indicator 9"
    rule = RuleVariation_692

class Item_396(Item):
    non_spare = NonSpare_1029

class NonSpare_1019(NonSpare):
    name = "I10"
    title = "Indicator 10"
    rule = RuleVariation_798

class Item_386(Item):
    non_spare = NonSpare_1019

class NonSpare_1020(NonSpare):
    name = "I11"
    title = "Indicator 11"
    rule = RuleVariation_874

class Item_387(Item):
    non_spare = NonSpare_1020

class NonSpare_1021(NonSpare):
    name = "I12"
    title = "Indicator 12"
    rule = RuleVariation_918

class Item_388(Item):
    non_spare = NonSpare_1021

class Variation_1025(Group):
    bit_size = 16
    items_list = [Item_123, Item_385, Item_389, Item_390, Item_391, Item_392, Item_393, Item_394, Item_395, Item_396, Item_386, Item_387, Item_388]
    items_dict = {"BKN": NonSpare_641, "I1": NonSpare_1018, "I2": NonSpare_1022, "I3": NonSpare_1023, "I4": NonSpare_1024, "I5": NonSpare_1025, "I6": NonSpare_1026, "I7": NonSpare_1027, "I8": NonSpare_1028, "I9": NonSpare_1029, "I10": NonSpare_1019, "I11": NonSpare_1020, "I12": NonSpare_1021}

class Variation_1340(Repetitive):
    rep_bytes = 1
    variation = Variation_1025

class RuleVariation_1270(RuleVariationContextFree):
    variation = Variation_1340

class NonSpare_502(NonSpare):
    name = "610"
    title = "Holdbar Status"
    rule = RuleVariation_1270

class Record_46(Record):
    items_list = [NonSpare_47, NonSpare_13, NonSpare_60, NonSpare_312, NonSpare_148, NonSpare_150, NonSpare_374, NonSpare_376, NonSpare_180, NonSpare_406, NonSpare_451, NonSpare_334, NonSpare_351, NonSpare_430, NonSpare_468, NonSpare_234, NonSpare_247, NonSpare_244, NonSpare_385, NonSpare_426, NonSpare_454, NonSpare_438, NonSpare_442, NonSpare_478, NonSpare_490, NonSpare_498, NonSpare_502, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_47, "000": NonSpare_13, "015": NonSpare_60, "140": NonSpare_312, "041": NonSpare_148, "042": NonSpare_150, "202": NonSpare_374, "210": NonSpare_376, "060": NonSpare_180, "245": NonSpare_406, "380": NonSpare_451, "161": NonSpare_334, "170": NonSpare_351, "290": NonSpare_430, "430": NonSpare_468, "090": NonSpare_234, "093": NonSpare_247, "092": NonSpare_244, "215": NonSpare_385, "270": NonSpare_426, "390": NonSpare_454, "300": NonSpare_438, "310": NonSpare_442, "500": NonSpare_478, "600": NonSpare_490, "605": NonSpare_498, "610": NonSpare_502, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_42(UapSingle):
    record = Record_46

class Asterix_10(AstCat):
    category = 11
    edition = (1, 2)
    uap = Uap_42

class NonSpare_311(NonSpare):
    name = "140"
    title = "Time of Track Information"
    rule = RuleVariation_356

class NonSpare_179(NonSpare):
    name = "060"
    title = "Mode-3/A Code in Octal Representation"
    rule = RuleVariation_959

class Content_307(ContentTable):
    values = {0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 6: "General Emergency", 7: "Lifeguard / medical", 8: "Minimum fuel", 9: "No communications", 10: "Unlawful interference"}

class RuleContent_307(RuleContentContextFree):
    variation = Content_307

class Variation_667(Element):
    bit_offset8 = 3
    bit_size = 4
    rule = RuleContent_307

class RuleVariation_657(RuleVariationContextFree):
    variation = Variation_667

class NonSpare_1688(NonSpare):
    name = "STAT"
    title = "Flight Status"
    rule = RuleVariation_657

class Item_896(Item):
    non_spare = NonSpare_1688

class Variation_1043(Group):
    bit_size = 24
    items_list = [Item_182, Item_896, Item_29, Item_881, Item_77, Item_54, Item_104, Item_105, Item_37, Item_530, Item_242, Item_19]
    items_dict = {"COM": NonSpare_735, "STAT": NonSpare_1688, "SSC": NonSpare_1664, "ARC": NonSpare_581, "AIC": NonSpare_549, "B1A": NonSpare_621, "B1B": NonSpare_622, "AC": NonSpare_517, "MN": NonSpare_1219, "DC": NonSpare_820}

class RuleVariation_1004(RuleVariationContextFree):
    variation = Variation_1043

class NonSpare_740(NonSpare):
    name = "COMACAS"
    title = "Communications/ACAS Capability and Flight Status"
    rule = RuleVariation_1004

class Variation_1412(Compound):
    items_list = [NonSpare_1154, NonSpare_532, None, NonSpare_740, None, None, None, NonSpare_527, NonSpare_863, None, NonSpare_606]
    items_dict = {"MB": NonSpare_1154, "ADR": NonSpare_532, "COMACAS": NonSpare_740, "ACT": NonSpare_527, "ECAT": NonSpare_863, "AVTECH": NonSpare_606}

class RuleVariation_1342(RuleVariationContextFree):
    variation = Variation_1412

class NonSpare_452(NonSpare):
    name = "380"
    title = "Mode-S / ADS-B Related Data"
    rule = RuleVariation_1342

class Content_171(ContentTable):
    values = {0: "Default value", 1: "Age of the last received PSR track update is higher than system dependent threshold"}

class RuleContent_171(RuleContentContextFree):
    variation = Content_171

class Variation_410(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_171

class RuleVariation_400(RuleVariationContextFree):
    variation = Variation_410

class NonSpare_1387(NonSpare):
    name = "PSR"
    title = ""
    rule = RuleVariation_400

class Item_674(Item):
    non_spare = NonSpare_1387

class Content_172(ContentTable):
    values = {0: "Default value", 1: "Age of the last received SSR track update is higher than system dependent threshold"}

class RuleContent_172(RuleContentContextFree):
    variation = Content_172

class Variation_509(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_172

class RuleVariation_499(RuleVariationContextFree):
    variation = Variation_509

class NonSpare_1666(NonSpare):
    name = "SSR"
    title = ""
    rule = RuleVariation_499

class Item_883(Item):
    non_spare = NonSpare_1666

class Content_170(ContentTable):
    values = {0: "Default value", 1: "Age of the last received Mode S track update is higher than system dependent threshold"}

class RuleContent_170(RuleContentContextFree):
    variation = Content_170

class Variation_610(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_170

class RuleVariation_600(RuleVariationContextFree):
    variation = Variation_610

class NonSpare_1179(NonSpare):
    name = "MDS"
    title = ""
    rule = RuleVariation_600

class Item_507(Item):
    non_spare = NonSpare_1179

class Content_167(ContentTable):
    values = {0: "Default value", 1: "Age of the last received ADS track update is higher than system dependent threshold"}

class RuleContent_167(RuleContentContextFree):
    variation = Content_167

class Variation_690(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_167

class RuleVariation_680(RuleVariationContextFree):
    variation = Variation_690

class NonSpare_534(NonSpare):
    name = "ADS"
    title = ""
    rule = RuleVariation_680

class Item_44(Item):
    non_spare = NonSpare_534

class Content_186(ContentTable):
    values = {0: "Default value", 1: "Special Used Code (Mode A codes to be defined in the system to mark a track with special interest)"}

class RuleContent_186(RuleContentContextFree):
    variation = Content_186

class Variation_817(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_186

class RuleVariation_788(RuleVariationContextFree):
    variation = Variation_817

class NonSpare_1714(NonSpare):
    name = "SUC"
    title = ""
    rule = RuleVariation_788

class Item_918(Item):
    non_spare = NonSpare_1714

class Content_175(ContentTable):
    values = {0: "Default value", 1: "Assigned Mode A Code Conflict (same individual Mode A Code assigned to another track)"}

class RuleContent_175(RuleContentContextFree):
    variation = Content_175

class Variation_892(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_175

class RuleVariation_863(RuleVariationContextFree):
    variation = Variation_892

class NonSpare_513(NonSpare):
    name = "AAC"
    title = ""
    rule = RuleVariation_863

class Item_35(Item):
    non_spare = NonSpare_513

class Variation_1299(Extended):
    items = [Item_546, Item_342, Item_549, Item_876, Item_161, None, Item_853, Item_995, Item_993, Item_323, Item_514, Item_521, None, Item_67, Item_867, Item_214, Item_316, Item_49, Item_24, None, Item_0, Item_674, Item_883, Item_507, Item_44, Item_918, Item_35, None]

class RuleVariation_1229(RuleVariationContextFree):
    variation = Variation_1299

class NonSpare_352(NonSpare):
    name = "170"
    title = "Track Status"
    rule = RuleVariation_1229

class NonSpare_1390(NonSpare):
    name = "PSR"
    title = "Age of the Last Primary Report Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1669(NonSpare):
    name = "SSR"
    title = "Age of the Last Secondary Report Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1176(NonSpare):
    name = "MDA"
    title = "Age of the Last Valid Mode A Report Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1198(NonSpare):
    name = "MFL"
    title = "Age of the Last Valid and Credible Mode C Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1182(NonSpare):
    name = "MDS"
    title = "Age of the Last Mode S Report Used to Update the Track"
    rule = RuleVariation_220

class NonSpare_1167(NonSpare):
    name = "MD2"
    title = "Age of the Last Valid Mode 2 Used to Update the Track"
    rule = RuleVariation_220

class Variation_1417(Compound):
    items_list = [NonSpare_1390, NonSpare_1669, NonSpare_1176, NonSpare_1198, NonSpare_1182, NonSpare_538, NonSpare_528, NonSpare_1164, NonSpare_1167, NonSpare_1118, NonSpare_1818, NonSpare_1259]
    items_dict = {"PSR": NonSpare_1390, "SSR": NonSpare_1669, "MDA": NonSpare_1176, "MFL": NonSpare_1198, "MDS": NonSpare_1182, "ADS": NonSpare_538, "ADB": NonSpare_528, "MD1": NonSpare_1164, "MD2": NonSpare_1167, "LOP": NonSpare_1118, "TRK": NonSpare_1818, "MUL": NonSpare_1259}

class RuleVariation_1347(RuleVariationContextFree):
    variation = Variation_1417

class NonSpare_431(NonSpare):
    name = "290"
    title = "System Track Update Ages"
    rule = RuleVariation_1347

class Content_301(ContentTable):
    values = {0: "No QNH Correction Applied", 1: "QNH Correction Applied"}

class RuleContent_301(RuleContentContextFree):
    variation = Content_301

class Variation_55(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_301

class RuleVariation_55(RuleVariationContextFree):
    variation = Variation_55

class NonSpare_1444(NonSpare):
    name = "QNH"
    title = "QNH Correction Applied"
    rule = RuleVariation_55

class Item_722(Item):
    non_spare = NonSpare_1444

class Variation_1129(Group):
    bit_size = 16
    items_list = [Item_722, Item_222]
    items_dict = {"QNH": NonSpare_1444, "CTBA": NonSpare_796}

class RuleVariation_1083(RuleVariationContextFree):
    variation = Variation_1129

class NonSpare_246(NonSpare):
    name = "093"
    title = "Calculated Track Barometric Altitude"
    rule = RuleVariation_1083

class NonSpare_425(NonSpare):
    name = "270"
    title = "Target Size and Orientation"
    rule = RuleVariation_1224

class Content_235(ContentTable):
    values = {0: "Instrument Flight Rules", 1: "Visual Flight Rules", 2: "Not applicable", 3: "Controlled Visual Flight Rules"}

class RuleContent_235(RuleContentContextFree):
    variation = Content_235

class Variation_568(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_235

class RuleVariation_558(RuleVariationContextFree):
    variation = Variation_568

class NonSpare_928(NonSpare):
    name = "FR1FR2"
    title = "Flight Rules"
    rule = RuleVariation_558

class Item_321(Item):
    non_spare = NonSpare_928

class Variation_1076(Group):
    bit_size = 8
    items_list = [Item_340, Item_321, Item_801, Item_378, Item_29]
    items_dict = {"GATOAT": NonSpare_958, "FR1FR2": NonSpare_928, "RVSM": NonSpare_1549, "HPR": NonSpare_1009}

class RuleVariation_1035(RuleVariationContextFree):
    variation = Variation_1076

class NonSpare_911(NonSpare):
    name = "FLIGHTCAT"
    title = "Flight Category"
    rule = RuleVariation_1035

class Variation_1404(Compound):
    items_list = [NonSpare_924, NonSpare_783, NonSpare_1048, NonSpare_911, NonSpare_1786, NonSpare_1952, NonSpare_529, NonSpare_530, NonSpare_1552, NonSpare_677, NonSpare_663, NonSpare_1787, NonSpare_591, NonSpare_1708]
    items_dict = {"FPPSID": NonSpare_924, "CSN": NonSpare_783, "IFPSFLIGHTID": NonSpare_1048, "FLIGHTCAT": NonSpare_911, "TOA": NonSpare_1786, "WTC": NonSpare_1952, "ADEP": NonSpare_529, "ADES": NonSpare_530, "RWY": NonSpare_1552, "CFL": NonSpare_677, "CCP": NonSpare_663, "TOD": NonSpare_1787, "AST": NonSpare_591, "STS": NonSpare_1708}

class RuleVariation_1334(RuleVariationContextFree):
    variation = Variation_1404

class NonSpare_453(NonSpare):
    name = "390"
    title = "Flight Plan Related Data"
    rule = RuleVariation_1334

class Record_45(Record):
    items_list = [NonSpare_47, NonSpare_13, NonSpare_60, NonSpare_311, NonSpare_148, NonSpare_150, NonSpare_374, NonSpare_376, NonSpare_179, NonSpare_406, NonSpare_452, NonSpare_334, NonSpare_352, NonSpare_431, NonSpare_468, NonSpare_234, NonSpare_246, NonSpare_244, NonSpare_385, NonSpare_425, NonSpare_453, NonSpare_438, NonSpare_442, NonSpare_478, NonSpare_490, NonSpare_498, NonSpare_502, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_47, "000": NonSpare_13, "015": NonSpare_60, "140": NonSpare_311, "041": NonSpare_148, "042": NonSpare_150, "202": NonSpare_374, "210": NonSpare_376, "060": NonSpare_179, "245": NonSpare_406, "380": NonSpare_452, "161": NonSpare_334, "170": NonSpare_352, "290": NonSpare_431, "430": NonSpare_468, "090": NonSpare_234, "093": NonSpare_246, "092": NonSpare_244, "215": NonSpare_385, "270": NonSpare_425, "390": NonSpare_453, "300": NonSpare_438, "310": NonSpare_442, "500": NonSpare_478, "600": NonSpare_490, "605": NonSpare_498, "610": NonSpare_502, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_41(UapSingle):
    record = Record_45

class Asterix_11(AstCat):
    category = 11
    edition = (1, 3)
    uap = Uap_41

class NonSpare_39(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_558(ContentTable):
    values = {1: "Measurement Plot", 2: "Measurement Track", 3: "Sensor Centric Plot", 4: "Sensor Centric Track", 5: "Track End Message"}

class RuleContent_558(RuleContentContextFree):
    variation = Content_558

class Variation_152(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_558

class RuleVariation_147(RuleVariationContextFree):
    variation = Variation_152

class NonSpare_1256(NonSpare):
    name = "MT"
    title = "Message Type"
    rule = RuleVariation_147

class Item_565(Item):
    non_spare = NonSpare_1256

class Content_402(ContentTable):
    values = {0: "Periodic Report", 1: "Event Driven Report"}

class RuleContent_402(RuleContentContextFree):
    variation = Content_402

class Variation_952(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_402

class RuleVariation_923(RuleVariationContextFree):
    variation = Variation_952

class NonSpare_1499(NonSpare):
    name = "RG"
    title = "Report Generation"
    rule = RuleVariation_923

class Item_770(Item):
    non_spare = NonSpare_1499

class Variation_1109(Group):
    bit_size = 8
    items_list = [Item_565, Item_770]
    items_dict = {"MT": NonSpare_1256, "RG": NonSpare_1499}

class RuleVariation_1064(RuleVariationContextFree):
    variation = Variation_1109

class NonSpare_16(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_1064

class NonSpare_58(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class Content_283(ContentTable):
    values = {0: "Mono-Static Sensor", 1: "Multi-Static Sensor", 2: "Other", 3: "Unknown"}

class RuleContent_283(RuleContentContextFree):
    variation = Content_283

class Variation_100(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_283

class RuleVariation_100(RuleVariationContextFree):
    variation = Variation_100

class NonSpare_1234(NonSpare):
    name = "MOMU"
    title = "Mono-Static Target Report or Multi-Static Target Report"
    rule = RuleVariation_100

class Item_545(Item):
    non_spare = NonSpare_1234

class Content_15(ContentTable):
    values = {0: "Actual Target Report", 1: "Reference Target", 2: "Synthetic Target", 3: "Simulated / Replayed Target"}

class RuleContent_15(RuleContentContextFree):
    variation = Content_15

class Variation_564(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_15

class RuleVariation_554(RuleVariationContextFree):
    variation = Variation_564

class NonSpare_1847(NonSpare):
    name = "TTAX"
    title = "Target Taxonomy"
    rule = RuleVariation_554

class Item_1009(Item):
    non_spare = NonSpare_1847

class Content_523(ContentTable):
    values = {0: "Unknown", 1: "Forward", 2: "Backward", 3: "Static"}

class RuleContent_523(RuleContentContextFree):
    variation = Content_523

class Variation_743(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_523

class RuleVariation_733(RuleVariationContextFree):
    variation = Variation_743

class NonSpare_1576(NonSpare):
    name = "SCD"
    title = "Scanning Direction"
    rule = RuleVariation_733

class Item_818(Item):
    non_spare = NonSpare_1576

class Variation_1297(Extended):
    items = [Item_545, Item_1009, Item_818, Item_26, None]

class RuleVariation_1227(RuleVariationContextFree):
    variation = Variation_1297

class NonSpare_82(NonSpare):
    name = "020"
    title = "Target Report Descriptor"
    rule = RuleVariation_1227

class NonSpare_110(NonSpare):
    name = "030"
    title = "Warning/Error Conditions"
    rule = RuleVariation_1305

class Content_744(ContentQuantity):
    signedness = Unsigned
    lsb = 7.8125e-3
    unit = "s"

class RuleContent_743(RuleContentContextFree):
    variation = Content_744

class Variation_363(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_743

class RuleVariation_357(RuleVariationContextFree):
    variation = Variation_363

class NonSpare_315(NonSpare):
    name = "145"
    title = "Time of Applicability"
    rule = RuleVariation_357

class Content_593(ContentInteger):
    signedness = Unsigned

class RuleContent_593(RuleContentContextFree):
    variation = Content_593

class Variation_248(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_593

class RuleVariation_242(RuleVariationContextFree):
    variation = Variation_248

class NonSpare_340(NonSpare):
    name = "161"
    title = "Track/Plot Number"
    rule = RuleVariation_242

class Content_459(ContentTable):
    values = {0: "Target not in Blind Zone", 1: "Target in Blind Zone"}

class RuleContent_459(RuleContentContextFree):
    variation = Content_459

class Variation_78(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_459

class RuleVariation_78(RuleVariationContextFree):
    variation = Variation_78

class NonSpare_640(NonSpare):
    name = "BIZ"
    title = ""
    rule = RuleVariation_78

class Item_122(Item):
    non_spare = NonSpare_640

class Content_458(ContentTable):
    values = {0: "Target not in Blanked Zone", 1: "Target in Blanked Zone"}

class RuleContent_458(RuleContentContextFree):
    variation = Content_458

class Variation_442(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_458

class RuleVariation_432(RuleVariationContextFree):
    variation = Variation_442

class NonSpare_626(NonSpare):
    name = "BAZ"
    title = ""
    rule = RuleVariation_432

class Item_109(Item):
    non_spare = NonSpare_626

class Content_488(ContentTable):
    values = {0: "Track Alive", 1: "Track Terminated by User Request"}

class RuleContent_488(RuleContentContextFree):
    variation = Content_488

class Variation_559(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_488

class RuleVariation_549(RuleVariationContextFree):
    variation = Variation_559

class NonSpare_1853(NonSpare):
    name = "TUR"
    title = ""
    rule = RuleVariation_549

class Item_1015(Item):
    non_spare = NonSpare_1853

class Content_380(ContentTable):
    values = {0: "Not extrapolated", 1: "Extrapolated"}

class RuleContent_380(RuleContentContextFree):
    variation = Content_380

class Variation_716(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_380

class RuleVariation_706(RuleVariationContextFree):
    variation = Variation_716

class NonSpare_794(NonSpare):
    name = "CSTP"
    title = "Coasted - Position"
    rule = RuleVariation_706

class Item_220(Item):
    non_spare = NonSpare_794

class Variation_836(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_380

class RuleVariation_807(RuleVariationContextFree):
    variation = Variation_836

class NonSpare_793(NonSpare):
    name = "CSTH"
    title = "Coasted – Height"
    rule = RuleVariation_807

class Item_219(Item):
    non_spare = NonSpare_793

class Content_57(ContentTable):
    values = {0: "Confirmed Track", 1: "Tentative Track"}

class RuleContent_57(RuleContentContextFree):
    variation = Content_57

class Variation_874(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_57

class RuleVariation_845(RuleVariationContextFree):
    variation = Variation_874

class NonSpare_701(NonSpare):
    name = "CNF"
    title = "Confirmed vs. Tentative Track"
    rule = RuleVariation_845

class Item_163(Item):
    non_spare = NonSpare_701

class Variation_1282(Extended):
    items = [Item_122, Item_109, Item_1015, Item_16, Item_220, Item_219, Item_163, None]

class RuleVariation_1212(RuleVariationContextFree):
    variation = Variation_1282

class NonSpare_353(NonSpare):
    name = "170"
    title = "Track/Plot Status"
    rule = RuleVariation_1212

class Content_745(ContentQuantity):
    signedness = Unsigned
    lsb = 7.8125e-3
    unit = "s"

class RuleContent_744(RuleContentContextFree):
    variation = Content_745

class Variation_588(Element):
    bit_offset8 = 2
    bit_size = 14
    rule = RuleContent_744

class RuleVariation_578(RuleVariationContextFree):
    variation = Variation_588

class NonSpare_1888(NonSpare):
    name = "UPD"
    title = "Update Period"
    rule = RuleVariation_578

class Item_1047(Item):
    non_spare = NonSpare_1888

class Variation_974(Group):
    bit_size = 16
    items_list = [Item_1, Item_1047]
    items_dict = {"UPD": NonSpare_1888}

class RuleVariation_945(RuleVariationContextFree):
    variation = Variation_974

class NonSpare_171(NonSpare):
    name = "050"
    title = "Update Period"
    rule = RuleVariation_945

class Content_714(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-2
    unit = "m"

class RuleContent_714(RuleContentContextFree):
    variation = Content_714

class Variation_306(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_714

class RuleVariation_300(RuleVariationContextFree):
    variation = Variation_306

class NonSpare_1092(NonSpare):
    name = "LEN"
    title = "Target Length"
    rule = RuleVariation_300

class NonSpare_1945(NonSpare):
    name = "WDT"
    title = "Target Width"
    rule = RuleVariation_300

class NonSpare_1002(NonSpare):
    name = "HGT"
    title = "Target Height"
    rule = RuleVariation_300

class Content_769(ContentQuantity):
    signedness = Unsigned
    lsb = 5.4931640625e-3
    unit = "°"

class RuleContent_768(RuleContentContextFree):
    variation = Content_769

class Variation_338(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_768

class RuleVariation_332(RuleVariationContextFree):
    variation = Variation_338

class NonSpare_1325(NonSpare):
    name = "ORT"
    title = "Target Orientation"
    rule = RuleVariation_332

class Variation_1410(Compound):
    items_list = [NonSpare_1092, NonSpare_1945, NonSpare_1002, NonSpare_1325]
    items_dict = {"LEN": NonSpare_1092, "WDT": NonSpare_1945, "HGT": NonSpare_1002, "ORT": NonSpare_1325}

class RuleVariation_1340(RuleVariationContextFree):
    variation = Variation_1410

class NonSpare_422(NonSpare):
    name = "270"
    title = "Target Size & Orientation"
    rule = RuleVariation_1340

class Variation_239(Element):
    bit_offset8 = 0
    bit_size = 9
    rule = RuleContent_585

class RuleVariation_233(RuleVariationContextFree):
    variation = Variation_239

class NonSpare_694(NonSpare):
    name = "CLS"
    title = "Classification"
    rule = RuleVariation_233

class Item_157(Item):
    non_spare = NonSpare_694

class Variation_479(Element):
    bit_offset8 = 1
    bit_size = 7
    rule = RuleContent_585

class RuleVariation_469(RuleVariationContextFree):
    variation = Variation_479

class NonSpare_1374(NonSpare):
    name = "PRB"
    title = "Probability"
    rule = RuleVariation_469

class Item_662(Item):
    non_spare = NonSpare_1374

class Variation_1034(Group):
    bit_size = 16
    items_list = [Item_157, Item_662]
    items_dict = {"CLS": NonSpare_694, "PRB": NonSpare_1374}

class Variation_1344(Repetitive):
    rep_bytes = 1
    variation = Variation_1034

class RuleVariation_1274(RuleVariationContextFree):
    variation = Variation_1344

class NonSpare_436(NonSpare):
    name = "300"
    title = "Object Classification"
    rule = RuleVariation_1274

class NonSpare_1350(NonSpare):
    name = "PID"
    title = "Pair Identifier"
    rule = RuleVariation_241

class Item_649(Item):
    non_spare = NonSpare_1350

class Variation_343(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_585

class RuleVariation_337(RuleVariationContextFree):
    variation = Variation_343

class NonSpare_1318(NonSpare):
    name = "ON"
    title = "Observation Number"
    rule = RuleVariation_337

class Item_622(Item):
    non_spare = NonSpare_1318

class Variation_1121(Group):
    bit_size = 40
    items_list = [Item_649, Item_622]
    items_dict = {"PID": NonSpare_1350, "ON": NonSpare_1318}

class RuleVariation_1075(RuleVariationContextFree):
    variation = Variation_1121

class NonSpare_460(NonSpare):
    name = "400"
    title = "Measurement Identifier"
    rule = RuleVariation_1075

class Content_675(ContentQuantity):
    signedness = Signed
    lsb = 8.381903171539307e-8
    unit = "°"

class RuleContent_675(RuleContentContextFree):
    variation = Content_675

class Variation_374(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_675

class RuleVariation_368(RuleVariationContextFree):
    variation = Variation_374

class NonSpare_1087(NonSpare):
    name = "LATITUDE"
    title = ""
    rule = RuleVariation_368

class Item_444(Item):
    non_spare = NonSpare_1087

class NonSpare_1116(NonSpare):
    name = "LONGITUDE"
    title = ""
    rule = RuleVariation_367

class Item_471(Item):
    non_spare = NonSpare_1116

class Variation_1100(Group):
    bit_size = 64
    items_list = [Item_444, Item_471]
    items_dict = {"LATITUDE": NonSpare_1087, "LONGITUDE": NonSpare_1116}

class RuleVariation_1059(RuleVariationContextFree):
    variation = Variation_1100

class NonSpare_1338(NonSpare):
    name = "P84"
    title = "Horizontal Position in WGS-84 Coordinates"
    rule = RuleVariation_1059

class Content_704(ContentQuantity):
    signedness = Unsigned
    lsb = 0.5
    unit = "m"

class RuleContent_704(RuleContentContextFree):
    variation = Content_704

class Variation_303(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_704

class RuleVariation_297(RuleVariationContextFree):
    variation = Variation_303

class NonSpare_1531(NonSpare):
    name = "RSHPX"
    title = ""
    rule = RuleVariation_297

class Item_792(Item):
    non_spare = NonSpare_1531

class NonSpare_1532(NonSpare):
    name = "RSHPY"
    title = ""
    rule = RuleVariation_297

class Item_793(Item):
    non_spare = NonSpare_1532

class Content_647(ContentQuantity):
    signedness = Signed
    lsb = 7.8125e-3
    unit = ""

class RuleContent_647(RuleContentContextFree):
    variation = Content_647

class Variation_205(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_647

class RuleVariation_199(RuleVariationContextFree):
    variation = Variation_205

class NonSpare_746(NonSpare):
    name = "CORSHPXY"
    title = ""
    rule = RuleVariation_199

class Item_189(Item):
    non_spare = NonSpare_746

class Variation_1148(Group):
    bit_size = 40
    items_list = [Item_792, Item_793, Item_189]
    items_dict = {"RSHPX": NonSpare_1531, "RSHPY": NonSpare_1532, "CORSHPXY": NonSpare_746}

class RuleVariation_1100(RuleVariationContextFree):
    variation = Variation_1148

class NonSpare_1010(NonSpare):
    name = "HPR"
    title = "Horizontal Position Resolution"
    rule = RuleVariation_1100

class Content_727(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "m"

class RuleContent_726(RuleContentContextFree):
    variation = Content_727

class Variation_316(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_726

class RuleVariation_310(RuleVariationContextFree):
    variation = Variation_316

class NonSpare_1594(NonSpare):
    name = "SDHPX"
    title = ""
    rule = RuleVariation_310

class Item_826(Item):
    non_spare = NonSpare_1594

class NonSpare_1595(NonSpare):
    name = "SDHPY"
    title = ""
    rule = RuleVariation_310

class Item_827(Item):
    non_spare = NonSpare_1595

class NonSpare_748(NonSpare):
    name = "COSDHPXY"
    title = ""
    rule = RuleVariation_199

class Item_191(Item):
    non_spare = NonSpare_748

class Variation_1166(Group):
    bit_size = 40
    items_list = [Item_826, Item_827, Item_191]
    items_dict = {"SDHPX": NonSpare_1594, "SDHPY": NonSpare_1595, "COSDHPXY": NonSpare_748}

class RuleVariation_1113(RuleVariationContextFree):
    variation = Variation_1166

class NonSpare_1006(NonSpare):
    name = "HPP"
    title = "Horizontal Position Precision"
    rule = RuleVariation_1113

class Variation_1414(Compound):
    items_list = [NonSpare_1338, NonSpare_1010, NonSpare_1006]
    items_dict = {"P84": NonSpare_1338, "HPR": NonSpare_1010, "HPP": NonSpare_1006}

class RuleVariation_1344(RuleVariationContextFree):
    variation = Variation_1414

class NonSpare_491(NonSpare):
    name = "600"
    title = "Horizontal Position Information"
    rule = RuleVariation_1344

class Content_617(ContentQuantity):
    signedness = Signed
    lsb = 1.0e-2
    unit = "m"

class RuleContent_617(RuleContentContextFree):
    variation = Content_617

class Variation_348(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_617

class RuleVariation_342(RuleVariationContextFree):
    variation = Variation_348

class NonSpare_965(NonSpare):
    name = "GH"
    title = "Geometric Height (WGS-84)"
    rule = RuleVariation_342

class Content_715(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-2
    unit = "m"

class RuleContent_715(RuleContentContextFree):
    variation = Content_715

class Variation_361(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_715

class RuleVariation_355(RuleVariationContextFree):
    variation = Variation_361

class NonSpare_1530(NonSpare):
    name = "RSGH"
    title = "Geometric Height Resolution"
    rule = RuleVariation_355

class NonSpare_1591(NonSpare):
    name = "SDGH"
    title = "Geometric Height Precision"
    rule = RuleVariation_355

class Content_699(ContentQuantity):
    signedness = Unsigned
    lsb = 16.0
    unit = "m"

class RuleContent_699(RuleContentContextFree):
    variation = Content_699

class Variation_241(Element):
    bit_offset8 = 0
    bit_size = 12
    rule = RuleContent_699

class RuleVariation_235(RuleVariationContextFree):
    variation = Variation_241

class NonSpare_1883(NonSpare):
    name = "UCI6"
    title = ""
    rule = RuleVariation_235

class Item_1042(Item):
    non_spare = NonSpare_1883

class Variation_782(Element):
    bit_offset8 = 4
    bit_size = 12
    rule = RuleContent_699

class RuleVariation_753(RuleVariationContextFree):
    variation = Variation_782

class NonSpare_1088(NonSpare):
    name = "LCI6"
    title = ""
    rule = RuleVariation_753

class Item_445(Item):
    non_spare = NonSpare_1088

class Variation_1205(Group):
    bit_size = 24
    items_list = [Item_1042, Item_445]
    items_dict = {"UCI6": NonSpare_1883, "LCI6": NonSpare_1088}

class RuleVariation_1139(RuleVariationContextFree):
    variation = Variation_1205

class NonSpare_689(NonSpare):
    name = "CI6"
    title = "Confidence Interval for Geometric Height (67%)"
    rule = RuleVariation_1139

class NonSpare_1884(NonSpare):
    name = "UCI9"
    title = ""
    rule = RuleVariation_235

class Item_1043(Item):
    non_spare = NonSpare_1884

class NonSpare_1089(NonSpare):
    name = "LCI9"
    title = ""
    rule = RuleVariation_753

class Item_446(Item):
    non_spare = NonSpare_1089

class Variation_1206(Group):
    bit_size = 24
    items_list = [Item_1043, Item_446]
    items_dict = {"UCI9": NonSpare_1884, "LCI9": NonSpare_1089}

class RuleVariation_1140(RuleVariationContextFree):
    variation = Variation_1206

class NonSpare_690(NonSpare):
    name = "CI9"
    title = "Confidence Interval for Geometric Height (95%)"
    rule = RuleVariation_1140

class NonSpare_1955(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1104(Item):
    non_spare = NonSpare_1955

class NonSpare_2007(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1153(Item):
    non_spare = NonSpare_2007

class Variation_1240(Group):
    bit_size = 16
    items_list = [Item_1104, Item_1153]
    items_dict = {"X": NonSpare_1955, "Y": NonSpare_2007}

class RuleVariation_1174(RuleVariationContextFree):
    variation = Variation_1240

class NonSpare_713(NonSpare):
    name = "COGHHP"
    title = "Correlation of Geometric Height and Horizontal Position"
    rule = RuleVariation_1174

class NonSpare_1956(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1105(Item):
    non_spare = NonSpare_1956

class NonSpare_2008(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1154(Item):
    non_spare = NonSpare_2008

class Variation_1241(Group):
    bit_size = 16
    items_list = [Item_1105, Item_1154]
    items_dict = {"X": NonSpare_1956, "Y": NonSpare_2008}

class RuleVariation_1175(RuleVariationContextFree):
    variation = Variation_1241

class NonSpare_714(NonSpare):
    name = "COGHHV"
    title = "Correlation of Geometric Height and Horizontal Velocity"
    rule = RuleVariation_1175

class NonSpare_1954(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1103(Item):
    non_spare = NonSpare_1954

class NonSpare_2006(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1152(Item):
    non_spare = NonSpare_2006

class Variation_1239(Group):
    bit_size = 16
    items_list = [Item_1103, Item_1152]
    items_dict = {"X": NonSpare_1954, "Y": NonSpare_2006}

class RuleVariation_1173(RuleVariationContextFree):
    variation = Variation_1239

class NonSpare_712(NonSpare):
    name = "COGHHA"
    title = "Correlation of Geometric Height and Horizontal Acceleration"
    rule = RuleVariation_1173

class Variation_1406(Compound):
    items_list = [NonSpare_965, NonSpare_1530, NonSpare_1591, NonSpare_689, NonSpare_690, NonSpare_713, NonSpare_714, NonSpare_712]
    items_dict = {"GH": NonSpare_965, "RSGH": NonSpare_1530, "SDGH": NonSpare_1591, "CI6": NonSpare_689, "CI9": NonSpare_690, "COGHHP": NonSpare_713, "COGHHV": NonSpare_714, "COGHHA": NonSpare_712}

class RuleVariation_1336(RuleVariationContextFree):
    variation = Variation_1406

class NonSpare_494(NonSpare):
    name = "601"
    title = "Geometric Height Information"
    rule = RuleVariation_1336

class Content_619(ContentQuantity):
    signedness = Signed
    lsb = 1.0e-2
    unit = "m/s"

class RuleContent_619(RuleContentContextFree):
    variation = Content_619

class Variation_340(Element):
    bit_offset8 = 0
    bit_size = 20
    rule = RuleContent_619

class RuleVariation_334(RuleVariationContextFree):
    variation = Variation_340

class NonSpare_1967(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_334

class Item_1116(Item):
    non_spare = NonSpare_1967

class Variation_786(Element):
    bit_offset8 = 4
    bit_size = 20
    rule = RuleContent_619

class RuleVariation_757(RuleVariationContextFree):
    variation = Variation_786

class NonSpare_2019(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_757

class Item_1165(Item):
    non_spare = NonSpare_2019

class Variation_1252(Group):
    bit_size = 40
    items_list = [Item_1116, Item_1165]
    items_dict = {"X": NonSpare_1967, "Y": NonSpare_2019}

class RuleVariation_1186(RuleVariationContextFree):
    variation = Variation_1252

class NonSpare_1014(NonSpare):
    name = "HV"
    title = "Horizontal Velocity Vector"
    rule = RuleVariation_1186

class Content_716(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-2
    unit = "m/s"

class RuleContent_716(RuleContentContextFree):
    variation = Content_716

class Variation_307(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_716

class RuleVariation_301(RuleVariationContextFree):
    variation = Variation_307

class NonSpare_1965(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_301

class Item_1114(Item):
    non_spare = NonSpare_1965

class NonSpare_2015(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_301

class Item_1161(Item):
    non_spare = NonSpare_2015

class NonSpare_747(NonSpare):
    name = "CORSHVXY"
    title = ""
    rule = RuleVariation_199

class Item_190(Item):
    non_spare = NonSpare_747

class Variation_1250(Group):
    bit_size = 40
    items_list = [Item_1114, Item_1161, Item_190]
    items_dict = {"X": NonSpare_1965, "Y": NonSpare_2015, "CORSHVXY": NonSpare_747}

class RuleVariation_1184(RuleVariationContextFree):
    variation = Variation_1250

class NonSpare_1533(NonSpare):
    name = "RSHV"
    title = "Horizontal Velocity Resolution"
    rule = RuleVariation_1184

class NonSpare_1966(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_301

class Item_1115(Item):
    non_spare = NonSpare_1966

class NonSpare_2016(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_301

class Item_1162(Item):
    non_spare = NonSpare_2016

class NonSpare_728(NonSpare):
    name = "COHVXY"
    title = ""
    rule = RuleVariation_199

class Item_178(Item):
    non_spare = NonSpare_728

class Variation_1251(Group):
    bit_size = 40
    items_list = [Item_1115, Item_1162, Item_178]
    items_dict = {"X": NonSpare_1966, "Y": NonSpare_2016, "COHVXY": NonSpare_728}

class RuleVariation_1185(RuleVariationContextFree):
    variation = Variation_1251

class NonSpare_1596(NonSpare):
    name = "SDHV"
    title = "Horizontal Velocity Precision"
    rule = RuleVariation_1185

class NonSpare_726(NonSpare):
    name = "COHVXHPX"
    title = ""
    rule = RuleVariation_199

class Item_176(Item):
    non_spare = NonSpare_726

class NonSpare_727(NonSpare):
    name = "COHVXHPY"
    title = ""
    rule = RuleVariation_199

class Item_177(Item):
    non_spare = NonSpare_727

class NonSpare_729(NonSpare):
    name = "COHVYHPX"
    title = ""
    rule = RuleVariation_199

class Item_179(Item):
    non_spare = NonSpare_729

class NonSpare_730(NonSpare):
    name = "COHVYHPY"
    title = ""
    rule = RuleVariation_199

class Item_180(Item):
    non_spare = NonSpare_730

class Variation_1037(Group):
    bit_size = 32
    items_list = [Item_176, Item_177, Item_179, Item_180]
    items_dict = {"COHVXHPX": NonSpare_726, "COHVXHPY": NonSpare_727, "COHVYHPX": NonSpare_729, "COHVYHPY": NonSpare_730}

class RuleVariation_998(RuleVariationContextFree):
    variation = Variation_1037

class NonSpare_725(NonSpare):
    name = "COHVHP"
    title = "Correlation of Horizontal Velocity and Horizontal Position"
    rule = RuleVariation_998

class Variation_1408(Compound):
    items_list = [NonSpare_1014, NonSpare_1533, NonSpare_1596, NonSpare_725]
    items_dict = {"HV": NonSpare_1014, "RSHV": NonSpare_1533, "SDHV": NonSpare_1596, "COHVHP": NonSpare_725}

class RuleVariation_1338(RuleVariationContextFree):
    variation = Variation_1408

class NonSpare_495(NonSpare):
    name = "602"
    title = "Horizontal Velocity Information"
    rule = RuleVariation_1338

class Content_641(ContentQuantity):
    signedness = Signed
    lsb = 6.25e-2
    unit = "m/s²"

class RuleContent_641(RuleContentContextFree):
    variation = Content_641

class Variation_240(Element):
    bit_offset8 = 0
    bit_size = 12
    rule = RuleContent_641

class RuleVariation_234(RuleVariationContextFree):
    variation = Variation_240

class NonSpare_1963(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_234

class Item_1112(Item):
    non_spare = NonSpare_1963

class Variation_781(Element):
    bit_offset8 = 4
    bit_size = 12
    rule = RuleContent_641

class RuleVariation_752(RuleVariationContextFree):
    variation = Variation_781

class NonSpare_2017(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_752

class Item_1163(Item):
    non_spare = NonSpare_2017

class Variation_1248(Group):
    bit_size = 24
    items_list = [Item_1112, Item_1163]
    items_dict = {"X": NonSpare_1963, "Y": NonSpare_2017}

class RuleVariation_1182(RuleVariationContextFree):
    variation = Variation_1248

class NonSpare_985(NonSpare):
    name = "HA"
    title = "Horizontal Acceleration Vector"
    rule = RuleVariation_1182

class Content_734(ContentQuantity):
    signedness = Unsigned
    lsb = 6.25e-2
    unit = "m/s²"

class RuleContent_733(RuleContentContextFree):
    variation = Content_734

class Variation_242(Element):
    bit_offset8 = 0
    bit_size = 12
    rule = RuleContent_733

class RuleVariation_236(RuleVariationContextFree):
    variation = Variation_242

class NonSpare_1964(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_236

class Item_1113(Item):
    non_spare = NonSpare_1964

class Variation_785(Element):
    bit_offset8 = 4
    bit_size = 12
    rule = RuleContent_733

class RuleVariation_756(RuleVariationContextFree):
    variation = Variation_785

class NonSpare_2018(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_756

class Item_1164(Item):
    non_spare = NonSpare_2018

class NonSpare_721(NonSpare):
    name = "COHAXY"
    title = ""
    rule = RuleVariation_199

class Item_172(Item):
    non_spare = NonSpare_721

class Variation_1249(Group):
    bit_size = 32
    items_list = [Item_1113, Item_1164, Item_172]
    items_dict = {"X": NonSpare_1964, "Y": NonSpare_2018, "COHAXY": NonSpare_721}

class RuleVariation_1183(RuleVariationContextFree):
    variation = Variation_1249

class NonSpare_1593(NonSpare):
    name = "SDHA"
    title = "Horizontal Acceleration Precision"
    rule = RuleVariation_1183

class NonSpare_717(NonSpare):
    name = "COHAXHPX"
    title = ""
    rule = RuleVariation_199

class Item_168(Item):
    non_spare = NonSpare_717

class NonSpare_718(NonSpare):
    name = "COHAXHPY"
    title = ""
    rule = RuleVariation_199

class Item_169(Item):
    non_spare = NonSpare_718

class NonSpare_722(NonSpare):
    name = "COHAYHPX"
    title = ""
    rule = RuleVariation_199

class Item_173(Item):
    non_spare = NonSpare_722

class NonSpare_702(NonSpare):
    name = "COAYHPY"
    title = ""
    rule = RuleVariation_199

class Item_164(Item):
    non_spare = NonSpare_702

class Variation_1035(Group):
    bit_size = 32
    items_list = [Item_168, Item_169, Item_173, Item_164]
    items_dict = {"COHAXHPX": NonSpare_717, "COHAXHPY": NonSpare_718, "COHAYHPX": NonSpare_722, "COAYHPY": NonSpare_702}

class RuleVariation_996(RuleVariationContextFree):
    variation = Variation_1035

class NonSpare_715(NonSpare):
    name = "COHAHP"
    title = "Correlation of Horizontal Acceleration and Horizontal Position"
    rule = RuleVariation_996

class NonSpare_719(NonSpare):
    name = "COHAXHVX"
    title = ""
    rule = RuleVariation_199

class Item_170(Item):
    non_spare = NonSpare_719

class NonSpare_720(NonSpare):
    name = "COHAXHVY"
    title = ""
    rule = RuleVariation_199

class Item_171(Item):
    non_spare = NonSpare_720

class NonSpare_723(NonSpare):
    name = "COHAYHVX"
    title = ""
    rule = RuleVariation_199

class Item_174(Item):
    non_spare = NonSpare_723

class NonSpare_724(NonSpare):
    name = "COHAYHVY"
    title = ""
    rule = RuleVariation_199

class Item_175(Item):
    non_spare = NonSpare_724

class Variation_1036(Group):
    bit_size = 32
    items_list = [Item_170, Item_171, Item_174, Item_175]
    items_dict = {"COHAXHVX": NonSpare_719, "COHAXHVY": NonSpare_720, "COHAYHVX": NonSpare_723, "COHAYHVY": NonSpare_724}

class RuleVariation_997(RuleVariationContextFree):
    variation = Variation_1036

class NonSpare_716(NonSpare):
    name = "COHAHV"
    title = "Correlation of Horizontal Acceleration and Horizontal Velocity"
    rule = RuleVariation_997

class Variation_1407(Compound):
    items_list = [NonSpare_985, NonSpare_1593, NonSpare_715, NonSpare_716]
    items_dict = {"HA": NonSpare_985, "SDHA": NonSpare_1593, "COHAHP": NonSpare_715, "COHAHV": NonSpare_716}

class RuleVariation_1337(RuleVariationContextFree):
    variation = Variation_1407

class NonSpare_496(NonSpare):
    name = "603"
    title = "Horizontal Acceleration Information"
    rule = RuleVariation_1337

class Content_618(ContentQuantity):
    signedness = Signed
    lsb = 1.0e-2
    unit = "m/s"

class RuleContent_618(RuleContentContextFree):
    variation = Content_618

class Variation_349(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_618

class RuleVariation_343(RuleVariationContextFree):
    variation = Variation_349

class NonSpare_1931(NonSpare):
    name = "VV"
    title = "Vertical Velocity"
    rule = RuleVariation_343

class NonSpare_1540(NonSpare):
    name = "RSVV"
    title = "Vertical Velocity Resolution"
    rule = RuleVariation_301

class NonSpare_1605(NonSpare):
    name = "SDVV"
    title = ""
    rule = RuleVariation_301

class Item_832(Item):
    non_spare = NonSpare_1605

class NonSpare_759(NonSpare):
    name = "COVVGH"
    title = ""
    rule = RuleVariation_199

class Item_198(Item):
    non_spare = NonSpare_759

class Variation_1170(Group):
    bit_size = 24
    items_list = [Item_832, Item_198]
    items_dict = {"SDVV": NonSpare_1605, "COVVGH": NonSpare_759}

class RuleVariation_1117(RuleVariationContextFree):
    variation = Variation_1170

class NonSpare_1606(NonSpare):
    name = "SDVV"
    title = "Vertical Velocity Precision"
    rule = RuleVariation_1117

class NonSpare_1960(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1109(Item):
    non_spare = NonSpare_1960

class NonSpare_2013(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1159(Item):
    non_spare = NonSpare_2013

class Variation_1245(Group):
    bit_size = 16
    items_list = [Item_1109, Item_1159]
    items_dict = {"X": NonSpare_1960, "Y": NonSpare_2013}

class RuleVariation_1179(RuleVariationContextFree):
    variation = Variation_1245

class NonSpare_761(NonSpare):
    name = "COVVHP"
    title = "Correlation of Vertical Velocity and Horizontal Position"
    rule = RuleVariation_1179

class NonSpare_1961(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1110(Item):
    non_spare = NonSpare_1961

class NonSpare_2014(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1160(Item):
    non_spare = NonSpare_2014

class Variation_1246(Group):
    bit_size = 16
    items_list = [Item_1110, Item_1160]
    items_dict = {"X": NonSpare_1961, "Y": NonSpare_2014}

class RuleVariation_1180(RuleVariationContextFree):
    variation = Variation_1246

class NonSpare_762(NonSpare):
    name = "COVVHV"
    title = "Correlation of Vertical Velocity and Horizontal Velocity"
    rule = RuleVariation_1180

class NonSpare_1962(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1111(Item):
    non_spare = NonSpare_1962

class NonSpare_2012(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1158(Item):
    non_spare = NonSpare_2012

class Variation_1247(Group):
    bit_size = 16
    items_list = [Item_1111, Item_1158]
    items_dict = {"X": NonSpare_1962, "Y": NonSpare_2012}

class RuleVariation_1181(RuleVariationContextFree):
    variation = Variation_1247

class NonSpare_760(NonSpare):
    name = "COVVHA"
    title = "Correlation of Vertical Velocity and Horizontal Acceleration"
    rule = RuleVariation_1181

class Variation_1434(Compound):
    items_list = [NonSpare_1931, NonSpare_1540, NonSpare_1606, NonSpare_761, NonSpare_762, NonSpare_760]
    items_dict = {"VV": NonSpare_1931, "RSVV": NonSpare_1540, "SDVV": NonSpare_1606, "COVVHP": NonSpare_761, "COVVHV": NonSpare_762, "COVVHA": NonSpare_760}

class RuleVariation_1364(RuleVariationContextFree):
    variation = Variation_1434

class NonSpare_497(NonSpare):
    name = "604"
    title = "Vertical Velocity Information"
    rule = RuleVariation_1364

class Content_620(ContentQuantity):
    signedness = Signed
    lsb = 1.0e-2
    unit = "m/s²"

class RuleContent_620(RuleContentContextFree):
    variation = Content_620

class Variation_259(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_620

class RuleVariation_253(RuleVariationContextFree):
    variation = Variation_259

class NonSpare_1893(NonSpare):
    name = "VA"
    title = "Vertical Acceleration"
    rule = RuleVariation_253

class Content_718(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-2
    unit = "m/s²"

class RuleContent_718(RuleContentContextFree):
    variation = Content_718

class Variation_308(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_718

class RuleVariation_302(RuleVariationContextFree):
    variation = Variation_308

class NonSpare_1604(NonSpare):
    name = "SDVA"
    title = ""
    rule = RuleVariation_302

class Item_831(Item):
    non_spare = NonSpare_1604

class NonSpare_753(NonSpare):
    name = "COVAGH"
    title = ""
    rule = RuleVariation_199

class Item_195(Item):
    non_spare = NonSpare_753

class NonSpare_757(NonSpare):
    name = "COVAVV"
    title = ""
    rule = RuleVariation_199

class Item_196(Item):
    non_spare = NonSpare_757

class Variation_1169(Group):
    bit_size = 32
    items_list = [Item_831, Item_195, Item_196]
    items_dict = {"SDVA": NonSpare_1604, "COVAGH": NonSpare_753, "COVAVV": NonSpare_757}

class RuleVariation_1116(RuleVariationContextFree):
    variation = Variation_1169

class NonSpare_1539(NonSpare):
    name = "RSVA"
    title = "Vertical Acceleration Precision"
    rule = RuleVariation_1116

class NonSpare_1958(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1107(Item):
    non_spare = NonSpare_1958

class NonSpare_2010(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1156(Item):
    non_spare = NonSpare_2010

class Variation_1243(Group):
    bit_size = 16
    items_list = [Item_1107, Item_1156]
    items_dict = {"X": NonSpare_1958, "Y": NonSpare_2010}

class RuleVariation_1177(RuleVariationContextFree):
    variation = Variation_1243

class NonSpare_755(NonSpare):
    name = "COVAHP"
    title = "Correlation of Vertical Acceleration and Horizontal Position"
    rule = RuleVariation_1177

class NonSpare_1959(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1108(Item):
    non_spare = NonSpare_1959

class NonSpare_2011(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1157(Item):
    non_spare = NonSpare_2011

class Variation_1244(Group):
    bit_size = 16
    items_list = [Item_1108, Item_1157]
    items_dict = {"X": NonSpare_1959, "Y": NonSpare_2011}

class RuleVariation_1178(RuleVariationContextFree):
    variation = Variation_1244

class NonSpare_756(NonSpare):
    name = "COVAHV"
    title = "Correlation of Vertical Acceleration and Horizontal Velocity"
    rule = RuleVariation_1178

class NonSpare_1957(NonSpare):
    name = "X"
    title = ""
    rule = RuleVariation_199

class Item_1106(Item):
    non_spare = NonSpare_1957

class NonSpare_2009(NonSpare):
    name = "Y"
    title = ""
    rule = RuleVariation_199

class Item_1155(Item):
    non_spare = NonSpare_2009

class Variation_1242(Group):
    bit_size = 16
    items_list = [Item_1106, Item_1155]
    items_dict = {"X": NonSpare_1957, "Y": NonSpare_2009}

class RuleVariation_1176(RuleVariationContextFree):
    variation = Variation_1242

class NonSpare_754(NonSpare):
    name = "COVAHA"
    title = "Correlation of Vertical Acceleration and Horizontal Acceleration"
    rule = RuleVariation_1176

class Variation_1433(Compound):
    items_list = [NonSpare_1893, NonSpare_1539, NonSpare_755, NonSpare_756, NonSpare_754]
    items_dict = {"VA": NonSpare_1893, "RSVA": NonSpare_1539, "COVAHP": NonSpare_755, "COVAHV": NonSpare_756, "COVAHA": NonSpare_754}

class RuleVariation_1363(RuleVariationContextFree):
    variation = Variation_1433

class NonSpare_499(NonSpare):
    name = "605"
    title = "Vertical Velocity Information"
    rule = RuleVariation_1363

class Variation_380(Element):
    bit_offset8 = 0
    bit_size = 40
    rule = RuleContent_0

class Variation_1331(Repetitive):
    rep_bytes = 1
    variation = Variation_380

class RuleVariation_1261(RuleVariationContextFree):
    variation = Variation_1331

class NonSpare_474(NonSpare):
    name = "480"
    title = "Associations"
    rule = RuleVariation_1261

class Content_613(ContentQuantity):
    signedness = Signed
    lsb = 0.1
    unit = "m"

class RuleContent_613(RuleContentContextFree):
    variation = Content_613

class Variation_346(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_613

class RuleVariation_340(RuleVariationContextFree):
    variation = Variation_346

class NonSpare_1447(NonSpare):
    name = "R"
    title = "Range"
    rule = RuleVariation_340

class Content_708(ContentQuantity):
    signedness = Unsigned
    lsb = 0.1
    unit = "m"

class RuleContent_708(RuleContentContextFree):
    variation = Content_708

class Variation_359(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_708

class RuleVariation_353(RuleVariationContextFree):
    variation = Variation_359

class NonSpare_1536(NonSpare):
    name = "RSR"
    title = "Range Resolution"
    rule = RuleVariation_353

class NonSpare_1598(NonSpare):
    name = "SDR"
    title = "Range Precision"
    rule = RuleVariation_353

class Content_615(ContentQuantity):
    signedness = Signed
    lsb = 0.1
    unit = "m/s"

class RuleContent_615(RuleContentContextFree):
    variation = Content_615

class Variation_347(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_615

class RuleVariation_341(RuleVariationContextFree):
    variation = Variation_347

class NonSpare_1523(NonSpare):
    name = "RR"
    title = "Range Rate"
    rule = RuleVariation_341

class Content_710(ContentQuantity):
    signedness = Unsigned
    lsb = 0.1
    unit = "m/s"

class RuleContent_710(RuleContentContextFree):
    variation = Content_710

class Variation_360(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_710

class RuleVariation_354(RuleVariationContextFree):
    variation = Variation_360

class NonSpare_1537(NonSpare):
    name = "RSRR"
    title = "Range Rate Resolution"
    rule = RuleVariation_354

class NonSpare_1601(NonSpare):
    name = "SDRR"
    title = ""
    rule = RuleVariation_354

class Item_829(Item):
    non_spare = NonSpare_1601

class NonSpare_745(NonSpare):
    name = "CORRR"
    title = ""
    rule = RuleVariation_199

class Item_188(Item):
    non_spare = NonSpare_745

class Variation_1168(Group):
    bit_size = 32
    items_list = [Item_829, Item_188]
    items_dict = {"SDRR": NonSpare_1601, "CORRR": NonSpare_745}

class RuleVariation_1115(RuleVariationContextFree):
    variation = Variation_1168

class NonSpare_1602(NonSpare):
    name = "SDRR"
    title = "Range Rate Precision"
    rule = RuleVariation_1115

class Content_646(ContentQuantity):
    signedness = Signed
    lsb = 1.5625e-2
    unit = "m/s²"

class RuleContent_646(RuleContentContextFree):
    variation = Content_646

class Variation_276(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_646

class RuleVariation_270(RuleVariationContextFree):
    variation = Variation_276

class NonSpare_1449(NonSpare):
    name = "RA"
    title = "Range Acceleration"
    rule = RuleVariation_270

class Content_742(ContentQuantity):
    signedness = Unsigned
    lsb = 7.8125e-3
    unit = "m/s²"

class RuleContent_741(RuleContentContextFree):
    variation = Content_742

class Variation_325(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_741

class RuleVariation_319(RuleVariationContextFree):
    variation = Variation_325

class NonSpare_1599(NonSpare):
    name = "SDRA"
    title = ""
    rule = RuleVariation_319

class Item_828(Item):
    non_spare = NonSpare_1599

class NonSpare_743(NonSpare):
    name = "CORAR"
    title = ""
    rule = RuleVariation_199

class Item_186(Item):
    non_spare = NonSpare_743

class NonSpare_744(NonSpare):
    name = "CORARR"
    title = ""
    rule = RuleVariation_199

class Item_187(Item):
    non_spare = NonSpare_744

class Variation_1167(Group):
    bit_size = 32
    items_list = [Item_828, Item_186, Item_187]
    items_dict = {"SDRA": NonSpare_1599, "CORAR": NonSpare_743, "CORARR": NonSpare_744}

class RuleVariation_1114(RuleVariationContextFree):
    variation = Variation_1167

class NonSpare_1600(NonSpare):
    name = "SDRA"
    title = "Range Acceleration Precision"
    rule = RuleVariation_1114

class Variation_1419(Compound):
    items_list = [NonSpare_1447, NonSpare_1536, NonSpare_1598, NonSpare_1523, NonSpare_1537, NonSpare_1602, NonSpare_1449, NonSpare_1600]
    items_dict = {"R": NonSpare_1447, "RSR": NonSpare_1536, "SDR": NonSpare_1598, "RR": NonSpare_1523, "RSRR": NonSpare_1537, "SDRR": NonSpare_1602, "RA": NonSpare_1449, "SDRA": NonSpare_1600}

class RuleVariation_1349(RuleVariationContextFree):
    variation = Variation_1419

class NonSpare_504(NonSpare):
    name = "625"
    title = "Range Information"
    rule = RuleVariation_1349

class NonSpare_860(NonSpare):
    name = "DV"
    title = "Doppler Velocity"
    rule = RuleVariation_343

class Content_736(ContentQuantity):
    signedness = Unsigned
    lsb = 1.5625e-2
    unit = "m/s"

class RuleContent_735(RuleContentContextFree):
    variation = Content_736

class Variation_320(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_735

class RuleVariation_314(RuleVariationContextFree):
    variation = Variation_320

class NonSpare_1587(NonSpare):
    name = "SDDV"
    title = "Precision of Doppler Velocity"
    rule = RuleVariation_314

class NonSpare_807(NonSpare):
    name = "DA"
    title = "Doppler Acceleration"
    rule = RuleVariation_270

class Content_737(ContentQuantity):
    signedness = Unsigned
    lsb = 1.5625e-2
    unit = "m/s²"

class RuleContent_736(RuleContentContextFree):
    variation = Content_737

class Variation_321(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_736

class RuleVariation_315(RuleVariationContextFree):
    variation = Variation_321

class NonSpare_1585(NonSpare):
    name = "SDDA"
    title = ""
    rule = RuleVariation_315

class Item_824(Item):
    non_spare = NonSpare_1585

class NonSpare_704(NonSpare):
    name = "CODADV"
    title = ""
    rule = RuleVariation_199

class Item_166(Item):
    non_spare = NonSpare_704

class Variation_1164(Group):
    bit_size = 24
    items_list = [Item_824, Item_166]
    items_dict = {"SDDA": NonSpare_1585, "CODADV": NonSpare_704}

class RuleVariation_1111(RuleVariationContextFree):
    variation = Variation_1164

class NonSpare_1586(NonSpare):
    name = "SDDA"
    title = "Precision of Doppler Acceleration"
    rule = RuleVariation_1111

class NonSpare_708(NonSpare):
    name = "CODVR"
    title = "Correlation of Doppler Velocity and Range"
    rule = RuleVariation_199

class NonSpare_710(NonSpare):
    name = "CODVRR"
    title = "Correlation of Doppler Velocity and Range Rate"
    rule = RuleVariation_199

class NonSpare_709(NonSpare):
    name = "CODVRA"
    title = "Correlation of Doppler Velocity and Range Acceleration"
    rule = RuleVariation_199

class NonSpare_705(NonSpare):
    name = "CODAR"
    title = "Correlation of Doppler Acceleration and Range"
    rule = RuleVariation_199

class NonSpare_707(NonSpare):
    name = "CODARR"
    title = "Correlation of Doppler Acceleration and Range Rate"
    rule = RuleVariation_199

class NonSpare_706(NonSpare):
    name = "CODARA"
    title = "Correlation of Doppler Acceleration and Range Acceleration"
    rule = RuleVariation_199

class Variation_1402(Compound):
    items_list = [NonSpare_860, NonSpare_1587, NonSpare_807, NonSpare_1586, NonSpare_708, NonSpare_710, NonSpare_709, NonSpare_705, NonSpare_707, NonSpare_706]
    items_dict = {"DV": NonSpare_860, "SDDV": NonSpare_1587, "DA": NonSpare_807, "SDDA": NonSpare_1586, "CODVR": NonSpare_708, "CODVRR": NonSpare_710, "CODVRA": NonSpare_709, "CODAR": NonSpare_705, "CODARR": NonSpare_707, "CODARA": NonSpare_706}

class RuleVariation_1332(RuleVariationContextFree):
    variation = Variation_1402

class NonSpare_505(NonSpare):
    name = "626"
    title = "Doppler Information"
    rule = RuleVariation_1332

class Content_768(ContentQuantity):
    signedness = Unsigned
    lsb = 5.4931640625e-3
    unit = "°"

class RuleContent_767(RuleContentContextFree):
    variation = Content_768

class Variation_337(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_767

class RuleVariation_331(RuleVariationContextFree):
    variation = Variation_337

class NonSpare_616(NonSpare):
    name = "AZ"
    title = "Azimuth"
    rule = RuleVariation_331

class Content_759(ContentQuantity):
    signedness = Unsigned
    lsb = 6.866455078125e-4
    unit = "°"

class RuleContent_758(RuleContentContextFree):
    variation = Content_759

class Variation_332(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_758

class RuleVariation_326(RuleVariationContextFree):
    variation = Variation_332

class NonSpare_1528(NonSpare):
    name = "RSAZ"
    title = "Azimuth Resolution"
    rule = RuleVariation_326

class NonSpare_1582(NonSpare):
    name = "SDASZ"
    title = "Standard Deviation of Azimuth"
    rule = RuleVariation_326

class Content_661(ContentQuantity):
    signedness = Signed
    lsb = 2.74658203125e-3
    unit = "°"

class RuleContent_661(RuleContentContextFree):
    variation = Content_661

class Variation_286(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_661

class RuleVariation_280(RuleVariationContextFree):
    variation = Variation_286

class NonSpare_620(NonSpare):
    name = "AZR"
    title = "Azimuth Rate"
    rule = RuleVariation_280

class NonSpare_1583(NonSpare):
    name = "SDAZR"
    title = ""
    rule = RuleVariation_326

class Item_823(Item):
    non_spare = NonSpare_1583

class NonSpare_703(NonSpare):
    name = "COAZRAZ"
    title = ""
    rule = RuleVariation_199

class Item_165(Item):
    non_spare = NonSpare_703

class Variation_1163(Group):
    bit_size = 24
    items_list = [Item_823, Item_165]
    items_dict = {"SDAZR": NonSpare_1583, "COAZRAZ": NonSpare_703}

class RuleVariation_1110(RuleVariationContextFree):
    variation = Variation_1163

class NonSpare_1584(NonSpare):
    name = "SDAZR"
    title = "Standard Deviation of Azimuth Rate"
    rule = RuleVariation_1110

class NonSpare_1554(NonSpare):
    name = "S"
    title = ""
    rule = RuleVariation_332

class Item_804(Item):
    non_spare = NonSpare_1554

class NonSpare_862(NonSpare):
    name = "E"
    title = ""
    rule = RuleVariation_332

class Item_275(Item):
    non_spare = NonSpare_862

class Variation_1152(Group):
    bit_size = 32
    items_list = [Item_804, Item_275]
    items_dict = {"S": NonSpare_1554, "E": NonSpare_862}

class RuleVariation_1103(RuleVariationContextFree):
    variation = Variation_1152

class NonSpare_618(NonSpare):
    name = "AZEX"
    title = "Azimuth Extent"
    rule = RuleVariation_1103

class Variation_1395(Compound):
    items_list = [NonSpare_616, NonSpare_1528, NonSpare_1582, NonSpare_620, NonSpare_1584, NonSpare_618]
    items_dict = {"AZ": NonSpare_616, "RSAZ": NonSpare_1528, "SDASZ": NonSpare_1582, "AZR": NonSpare_620, "SDAZR": NonSpare_1584, "AZEX": NonSpare_618}

class RuleVariation_1325(RuleVariationContextFree):
    variation = Variation_1395

class NonSpare_506(NonSpare):
    name = "627"
    title = "Azimuth Information"
    rule = RuleVariation_1325

class NonSpare_865(NonSpare):
    name = "EL"
    title = "Elevation"
    rule = RuleVariation_280

class NonSpare_1529(NonSpare):
    name = "RSEL"
    title = "Elevation Resolution"
    rule = RuleVariation_326

class NonSpare_1588(NonSpare):
    name = "SDEL"
    title = "Standard Deviation of Elevation"
    rule = RuleVariation_326

class Content_663(ContentQuantity):
    signedness = Signed
    lsb = 2.74658203125e-3
    unit = "°/s"

class RuleContent_663(RuleContentContextFree):
    variation = Content_663

class Variation_288(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_663

class RuleVariation_282(RuleVariationContextFree):
    variation = Variation_288

class NonSpare_892(NonSpare):
    name = "ER"
    title = "Elevation Rate"
    rule = RuleVariation_282

class Content_760(ContentQuantity):
    signedness = Unsigned
    lsb = 6.866455078125e-4
    unit = "°/s"

class RuleContent_759(RuleContentContextFree):
    variation = Content_760

class Variation_333(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_759

class RuleVariation_327(RuleVariationContextFree):
    variation = Variation_333

class NonSpare_1589(NonSpare):
    name = "SDELR"
    title = ""
    rule = RuleVariation_327

class Item_825(Item):
    non_spare = NonSpare_1589

class NonSpare_711(NonSpare):
    name = "COELREL"
    title = ""
    rule = RuleVariation_199

class Item_167(Item):
    non_spare = NonSpare_711

class Variation_1165(Group):
    bit_size = 24
    items_list = [Item_825, Item_167]
    items_dict = {"SDELR": NonSpare_1589, "COELREL": NonSpare_711}

class RuleVariation_1112(RuleVariationContextFree):
    variation = Variation_1165

class NonSpare_1590(NonSpare):
    name = "SDER"
    title = "Standard Deviation of Elevation Rate"
    rule = RuleVariation_1112

class NonSpare_1553(NonSpare):
    name = "S"
    title = ""
    rule = RuleVariation_280

class Item_803(Item):
    non_spare = NonSpare_1553

class NonSpare_861(NonSpare):
    name = "E"
    title = ""
    rule = RuleVariation_280

class Item_274(Item):
    non_spare = NonSpare_861

class Variation_1151(Group):
    bit_size = 32
    items_list = [Item_803, Item_274]
    items_dict = {"S": NonSpare_1553, "E": NonSpare_861}

class RuleVariation_1102(RuleVariationContextFree):
    variation = Variation_1151

class NonSpare_867(NonSpare):
    name = "ELEX"
    title = "Elevation Extent"
    rule = RuleVariation_1102

class Variation_1403(Compound):
    items_list = [NonSpare_865, NonSpare_1529, NonSpare_1588, NonSpare_892, NonSpare_1590, NonSpare_867]
    items_dict = {"EL": NonSpare_865, "RSEL": NonSpare_1529, "SDEL": NonSpare_1588, "ER": NonSpare_892, "SDER": NonSpare_1590, "ELEX": NonSpare_867}

class RuleVariation_1333(RuleVariationContextFree):
    variation = Variation_1403

class NonSpare_507(NonSpare):
    name = "628"
    title = "Elevation Information"
    rule = RuleVariation_1333

class Content_596(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = "dB"

class RuleContent_596(RuleContentContextFree):
    variation = Content_596

class Variation_197(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_596

class RuleVariation_191(RuleVariationContextFree):
    variation = Variation_197

class NonSpare_844(NonSpare):
    name = "DPP"
    title = "Direct Path - Power"
    rule = RuleVariation_191

class NonSpare_845(NonSpare):
    name = "DPS"
    title = "Direct Path - Signal to Noise Ratio (SNR)"
    rule = RuleVariation_191

class Item_6(Spare):
    bit_offset8 = 0
    bit_size = 7

class Content_595(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = "dB"

class RuleContent_595(RuleContentContextFree):
    variation = Content_595

class Variation_960(Element):
    bit_offset8 = 7
    bit_size = 9
    rule = RuleContent_595

class RuleVariation_931(RuleVariationContextFree):
    variation = Variation_960

class NonSpare_1520(NonSpare):
    name = "RPP"
    title = ""
    rule = RuleVariation_931

class Item_787(Item):
    non_spare = NonSpare_1520

class Variation_1007(Group):
    bit_size = 16
    items_list = [Item_6, Item_787]
    items_dict = {"RPP": NonSpare_1520}

class RuleVariation_976(RuleVariationContextFree):
    variation = Variation_1007

class NonSpare_1521(NonSpare):
    name = "RPP"
    title = "Reflected Path - Power"
    rule = RuleVariation_976

class NonSpare_1522(NonSpare):
    name = "RPS"
    title = "Reflected Path - Signal to Noise Ratio (SNR)"
    rule = RuleVariation_191

class Variation_1401(Compound):
    items_list = [NonSpare_844, NonSpare_845, NonSpare_1521, NonSpare_1522]
    items_dict = {"DPP": NonSpare_844, "DPS": NonSpare_845, "RPP": NonSpare_1521, "RPS": NonSpare_1522}

class RuleVariation_1331(RuleVariationContextFree):
    variation = Variation_1401

class NonSpare_508(NonSpare):
    name = "630"
    title = "Path Quality"
    rule = RuleVariation_1331

class NonSpare_617(NonSpare):
    name = "AZCON"
    title = ""
    rule = RuleVariation_331

class Item_102(Item):
    non_spare = NonSpare_617

class Content_662(ContentQuantity):
    signedness = Signed
    lsb = 2.74658203125e-3
    unit = "°"

class RuleContent_662(RuleContentContextFree):
    variation = Content_662

class Variation_287(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_662

class RuleVariation_281(RuleVariationContextFree):
    variation = Variation_287

class NonSpare_866(NonSpare):
    name = "ELCON"
    title = ""
    rule = RuleVariation_281

class Item_277(Item):
    non_spare = NonSpare_866

class Content_770(ContentQuantity):
    signedness = Unsigned
    lsb = 0.152587890625
    unit = "m"

class RuleContent_769(RuleContentContextFree):
    variation = Content_770

class Variation_339(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_769

class RuleVariation_333(RuleVariationContextFree):
    variation = Variation_339

class NonSpare_1501(NonSpare):
    name = "RGCONSTOP"
    title = ""
    rule = RuleVariation_333

class Item_772(Item):
    non_spare = NonSpare_1501

class NonSpare_1500(NonSpare):
    name = "RGCONSTART"
    title = ""
    rule = RuleVariation_333

class Item_771(Item):
    non_spare = NonSpare_1500

class Variation_1023(Group):
    bit_size = 64
    items_list = [Item_102, Item_277, Item_772, Item_771]
    items_dict = {"AZCON": NonSpare_617, "ELCON": NonSpare_866, "RGCONSTOP": NonSpare_1501, "RGCONSTART": NonSpare_1500}

class Variation_1338(Repetitive):
    rep_bytes = 1
    variation = Variation_1023

class RuleVariation_1268(RuleVariationContextFree):
    variation = Variation_1338

class NonSpare_509(NonSpare):
    name = "631"
    title = "Contour (Azimuth, Elevation Angle, Range Extent)"
    rule = RuleVariation_1268

class Record_32(Record):
    items_list = [NonSpare_39, NonSpare_16, NonSpare_58, NonSpare_82, NonSpare_110, NonSpare_315, NonSpare_340, NonSpare_353, NonSpare_171, NonSpare_422, NonSpare_436, NonSpare_460, NonSpare_491, NonSpare_494, NonSpare_495, NonSpare_496, NonSpare_497, NonSpare_499, NonSpare_474, NonSpare_504, NonSpare_505, NonSpare_506, NonSpare_507, NonSpare_508, NonSpare_509, NonSpare_1641]
    items_dict = {"010": NonSpare_39, "000": NonSpare_16, "015": NonSpare_58, "020": NonSpare_82, "030": NonSpare_110, "145": NonSpare_315, "161": NonSpare_340, "170": NonSpare_353, "050": NonSpare_171, "270": NonSpare_422, "300": NonSpare_436, "400": NonSpare_460, "600": NonSpare_491, "601": NonSpare_494, "602": NonSpare_495, "603": NonSpare_496, "604": NonSpare_497, "605": NonSpare_499, "480": NonSpare_474, "625": NonSpare_504, "626": NonSpare_505, "627": NonSpare_506, "628": NonSpare_507, "630": NonSpare_508, "631": NonSpare_509, "SP": NonSpare_1641}

class Uap_28(UapSingle):
    record = Record_32

class Asterix_12(AstCat):
    category = 15
    edition = (1, 0)
    uap = Uap_28

class Asterix_13(AstCat):
    category = 15
    edition = (1, 1)
    uap = Uap_28

class NonSpare_30(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class NonSpare_65(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class Content_569(ContentTable):
    values = {1: "System Configuration", 2: "Transmitter / Receiver Configuration"}

class RuleContent_569(RuleContentContextFree):
    variation = Content_569

class Variation_187(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_569

class RuleVariation_181(RuleVariationContextFree):
    variation = Variation_187

class NonSpare_10(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_181

class NonSpare_303(NonSpare):
    name = "140"
    title = "Time of Day"
    rule = RuleVariation_356

class Content_696(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "s"

class RuleContent_696(RuleContentContextFree):
    variation = Content_696

class Variation_215(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_696

class RuleVariation_209(RuleVariationContextFree):
    variation = Variation_215

class NonSpare_365(NonSpare):
    name = "200"
    title = "System Configuration Reporting Period"
    rule = RuleVariation_209

class NonSpare_1349(NonSpare):
    name = "PID"
    title = "Pair Identification"
    rule = RuleVariation_240

class Item_648(Item):
    non_spare = NonSpare_1349

class NonSpare_1775(NonSpare):
    name = "TID"
    title = "Transmitter Identification"
    rule = RuleVariation_240

class Item_956(Item):
    non_spare = NonSpare_1775

class NonSpare_1509(NonSpare):
    name = "RID"
    title = "Receiver Identification"
    rule = RuleVariation_240

class Item_780(Item):
    non_spare = NonSpare_1509

class Variation_1120(Group):
    bit_size = 48
    items_list = [Item_648, Item_956, Item_780]
    items_dict = {"PID": NonSpare_1349, "TID": NonSpare_1775, "RID": NonSpare_1509}

class Variation_1350(Repetitive):
    rep_bytes = 1
    variation = Variation_1120

class RuleVariation_1280(RuleVariationContextFree):
    variation = Variation_1350

class NonSpare_437(NonSpare):
    name = "300"
    title = "Pair Identification"
    rule = RuleVariation_1280

class NonSpare_461(NonSpare):
    name = "400"
    title = "Position of the System Reference Point"
    rule = RuleVariation_1053

class Content_630(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "m"

class RuleContent_630(RuleContentContextFree):
    variation = Content_630

class Variation_267(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_630

class RuleVariation_261(RuleVariationContextFree):
    variation = Variation_267

class NonSpare_463(NonSpare):
    name = "405"
    title = "Height of System Reference Point"
    rule = RuleVariation_261

class NonSpare_1774(NonSpare):
    name = "TID"
    title = "Transmitter ID"
    rule = RuleVariation_240

class Item_955(Item):
    non_spare = NonSpare_1774

class NonSpare_552(NonSpare):
    name = "ALT"
    title = "Altitude"
    rule = RuleVariation_261

class Item_57(Item):
    non_spare = NonSpare_552

class Content_603(ContentQuantity):
    signedness = Signed
    lsb = 2.0
    unit = "ns"

class RuleContent_603(RuleContentContextFree):
    variation = Content_603

class Variation_368(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_603

class RuleVariation_362(RuleVariationContextFree):
    variation = Variation_368

class NonSpare_1851(NonSpare):
    name = "TTO"
    title = "Transmission Time Offset"
    rule = RuleVariation_362

class Item_1013(Item):
    non_spare = NonSpare_1851

class Content_694(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "ns"

class RuleContent_694(RuleContentContextFree):
    variation = Content_694

class Variation_787(Element):
    bit_offset8 = 4
    bit_size = 20
    rule = RuleContent_694

class RuleVariation_758(RuleVariationContextFree):
    variation = Variation_787

class NonSpare_595(NonSpare):
    name = "ATO"
    title = "Accuracy of Transmission Time Offset"
    rule = RuleVariation_758

class Item_84(Item):
    non_spare = NonSpare_595

class NonSpare_1344(NonSpare):
    name = "PCI"
    title = "Parallel Transmitter Index"
    rule = RuleVariation_241

class Item_645(Item):
    non_spare = NonSpare_1344

class Variation_1185(Group):
    bit_size = 168
    items_list = [Item_955, Item_437, Item_463, Item_57, Item_1013, Item_3, Item_84, Item_645]
    items_dict = {"TID": NonSpare_1774, "LAT": NonSpare_1080, "LON": NonSpare_1108, "ALT": NonSpare_552, "TTO": NonSpare_1851, "ATO": NonSpare_595, "PCI": NonSpare_1344}

class Variation_1361(Repetitive):
    rep_bytes = 1
    variation = Variation_1185

class RuleVariation_1291(RuleVariationContextFree):
    variation = Variation_1361

class NonSpare_465(NonSpare):
    name = "410"
    title = "Transmitter Properties"
    rule = RuleVariation_1291

class NonSpare_1508(NonSpare):
    name = "RID"
    title = "Receiver Component ID"
    rule = RuleVariation_240

class Item_779(Item):
    non_spare = NonSpare_1508

class Variation_1145(Group):
    bit_size = 96
    items_list = [Item_779, Item_437, Item_463, Item_57]
    items_dict = {"RID": NonSpare_1508, "LAT": NonSpare_1080, "LON": NonSpare_1108, "ALT": NonSpare_552}

class Variation_1351(Repetitive):
    rep_bytes = 1
    variation = Variation_1145

class RuleVariation_1281(RuleVariationContextFree):
    variation = Variation_1351

class NonSpare_467(NonSpare):
    name = "420"
    title = "Receiver Properties"
    rule = RuleVariation_1281

class Record_10(Record):
    items_list = [NonSpare_30, NonSpare_65, NonSpare_10, NonSpare_303, NonSpare_365, NonSpare_437, NonSpare_461, NonSpare_463, NonSpare_465, NonSpare_467, NonSpare_1641]
    items_dict = {"010": NonSpare_30, "015": NonSpare_65, "000": NonSpare_10, "140": NonSpare_303, "200": NonSpare_365, "300": NonSpare_437, "400": NonSpare_461, "405": NonSpare_463, "410": NonSpare_465, "420": NonSpare_467, "SP": NonSpare_1641}

class Uap_10(UapSingle):
    record = Record_10

class Asterix_14(AstCat):
    category = 16
    edition = (1, 0)
    uap = Uap_10

class NonSpare_40(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class NonSpare_52(NonSpare):
    name = "012"
    title = "Data Destination Identifier"
    rule = RuleVariation_1104

class Content_289(ContentTable):
    values = {0: "Network information", 10: "Track data", 20: "Track data request", 21: "Track data stop", 22: "Cancel track data request", 23: "Track data stop acknowledgement", 30: "New Node / Change-over Initial or intermediate message segment", 31: "New Node / Change-over Final or only message segment", 32: "New Node / Change-over Initial or intermediate message segment reply", 33: "New Node / Change-over Final or only message segment reply", 110: "Move node to new cluster state;", 111: "Move node to new cluster state acknowledgement"}

class RuleContent_289(RuleContentContextFree):
    variation = Content_289

class Variation_163(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_289

class RuleVariation_158(RuleVariationContextFree):
    variation = Variation_163

class NonSpare_1(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_158

class NonSpare_445(NonSpare):
    name = "350"
    title = "Cluster Station/Node List"
    rule = RuleVariation_1284

class NonSpare_386(NonSpare):
    name = "220"
    title = "Aircraft Address"
    rule = RuleVariation_335

class NonSpare_394(NonSpare):
    name = "221"
    title = "Duplicate Address Reference Number (DRN)"
    rule = RuleVariation_240

class NonSpare_310(NonSpare):
    name = "140"
    title = "Time of Day"
    rule = RuleVariation_357

class Variation_356(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_670

class RuleVariation_350(RuleVariationContextFree):
    variation = Variation_356

class NonSpare_1077(NonSpare):
    name = "LAT"
    title = "Latitude"
    rule = RuleVariation_350

class Item_434(Item):
    non_spare = NonSpare_1077

class Variation_355(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_669

class RuleVariation_349(RuleVariationContextFree):
    variation = Variation_355

class NonSpare_1105(NonSpare):
    name = "LON"
    title = "Longitude"
    rule = RuleVariation_349

class Item_460(Item):
    non_spare = NonSpare_1105

class Variation_1091(Group):
    bit_size = 48
    items_list = [Item_434, Item_460]
    items_dict = {"LAT": NonSpare_1077, "LON": NonSpare_1105}

class RuleVariation_1050(RuleVariationContextFree):
    variation = Variation_1091

class NonSpare_156(NonSpare):
    name = "045"
    title = "Calculated Position in WGS-84 Coordinates"
    rule = RuleVariation_1050

class Content_279(ContentTable):
    values = {0: "Mode-3/A code derived from the reply of the transponder", 1: "Smoothed Mode-3/A code not extracted during the last scan"}

class RuleContent_279(RuleContentContextFree):
    variation = Content_279

class Variation_533(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_279

class RuleVariation_523(RuleVariationContextFree):
    variation = Variation_533

class NonSpare_1069(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_523

class Item_426(Item):
    non_spare = NonSpare_1069

class NonSpare_1229(NonSpare):
    name = "MODE3A"
    title = "Mode 3/A Reply in Octal Representation"
    rule = RuleVariation_751

class Item_540(Item):
    non_spare = NonSpare_1229

class Variation_1221(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_426, Item_16, Item_540]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "L": NonSpare_1069, "MODE3A": NonSpare_1229}

class RuleVariation_1155(RuleVariationContextFree):
    variation = Variation_1221

class NonSpare_197(NonSpare):
    name = "070"
    title = "Mode 3/A Code in Octal Representation"
    rule = RuleVariation_1155

class Content_93(ContentTable):
    values = {0: "Default", 1: "Garbled code / Error correction applied"}

class RuleContent_93(RuleContentContextFree):
    variation = Content_93

class Variation_398(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_93

class RuleVariation_388(RuleVariationContextFree):
    variation = Variation_398

class NonSpare_944(NonSpare):
    name = "G"
    title = ""
    rule = RuleVariation_388

class Item_334(Item):
    non_spare = NonSpare_944

class Variation_587(Element):
    bit_offset8 = 2
    bit_size = 14
    rule = RuleContent_722

class RuleVariation_577(RuleVariationContextFree):
    variation = Variation_587

class NonSpare_553(NonSpare):
    name = "ALT"
    title = "Altitude"
    rule = RuleVariation_577

class Item_58(Item):
    non_spare = NonSpare_553

class Variation_1222(Group):
    bit_size = 16
    items_list = [Item_1049, Item_334, Item_58]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_944, "ALT": NonSpare_553}

class RuleVariation_1156(RuleVariationContextFree):
    variation = Variation_1222

class NonSpare_160(NonSpare):
    name = "050"
    title = "Flight Level in Binary Representation"
    rule = RuleVariation_1156

class NonSpare_371(NonSpare):
    name = "200"
    title = "Track Velocity in Polar Co-ordinates"
    rule = RuleVariation_1038

class Content_326(ContentTable):
    values = {0: "No communications capability (surveillance only), no ability to set CA code 7 either airborne or on the ground", 1: "Reserved", 2: "Reserved", 3: "Reserved", 4: "At Least Comm. A and Comm. B capability and the ability to set CA code 7 and on the ground", 5: "At Least Comm. A and Comm. B capability and the ability to set CA code 7 and airborne", 6: "At Least Comm. A and Comm. B capability and the ability to set CA code 7 and either airborne or on the ground", 7: "Signifies the DR field is not equal to 0 or the FS field equals 2, 3, 4 or 5 and either airborne or on the ground SI/II-capabilities of the Transponder"}

class RuleContent_326(RuleContentContextFree):
    variation = Content_326

class Variation_120(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_326

class RuleVariation_120(RuleVariationContextFree):
    variation = Variation_120

class NonSpare_653(NonSpare):
    name = "CA"
    title = "Communications Capability of the Transponder"
    rule = RuleVariation_120

class Item_128(Item):
    non_spare = NonSpare_653

class Content_500(ContentTable):
    values = {0: "Transponder SI capable", 1: "Transponder not SI capable"}

class RuleContent_500(RuleContentContextFree):
    variation = Content_500

class Variation_643(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_500

class RuleVariation_633(RuleVariationContextFree):
    variation = Variation_643

class NonSpare_1616(NonSpare):
    name = "SI"
    title = "SI/II-capabilities of the Transponder"
    rule = RuleVariation_633

class Item_840(Item):
    non_spare = NonSpare_1616

class Variation_1027(Group):
    bit_size = 8
    items_list = [Item_128, Item_840, Item_22]
    items_dict = {"CA": NonSpare_653, "SI": NonSpare_1616}

class RuleVariation_992(RuleVariationContextFree):
    variation = Variation_1027

class NonSpare_400(NonSpare):
    name = "230"
    title = "Transponder Capability"
    rule = RuleVariation_992

class Content_255(ContentTable):
    values = {0: "Measured position", 1: "No measured position (coasted)"}

class RuleContent_255(RuleContentContextFree):
    variation = Content_255

class Variation_50(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_255

class RuleVariation_50(RuleVariationContextFree):
    variation = Variation_50

class NonSpare_792(NonSpare):
    name = "CST"
    title = "Track Coasted"
    rule = RuleVariation_50

class Item_218(Item):
    non_spare = NonSpare_792

class Content_243(ContentTable):
    values = {0: "Last Measured Flight Level", 1: "Predicted Flight Level"}

class RuleContent_243(RuleContentContextFree):
    variation = Content_243

class Variation_425(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_243

class RuleVariation_415(RuleVariationContextFree):
    variation = Variation_425

class NonSpare_913(NonSpare):
    name = "FLT"
    title = "Flight Level Tracking"
    rule = RuleVariation_415

class Item_311(Item):
    non_spare = NonSpare_913

class Variation_1044(Group):
    bit_size = 8
    items_list = [Item_218, Item_311, Item_15]
    items_dict = {"CST": NonSpare_792, "FLT": NonSpare_913}

class RuleVariation_1005(RuleVariationContextFree):
    variation = Variation_1044

class NonSpare_403(NonSpare):
    name = "240"
    title = "Track Status"
    rule = RuleVariation_1005

class Variation_1328(Repetitive):
    rep_bytes = 1
    variation = Variation_341

class RuleVariation_1258(RuleVariationContextFree):
    variation = Variation_1328

class NonSpare_382(NonSpare):
    name = "210"
    title = "Mode S Address List"
    rule = RuleVariation_1258

class NonSpare_446(NonSpare):
    name = "360"
    title = "Cluster Controller Command State"
    rule = RuleVariation_154

class Record_33(Record):
    items_list = [NonSpare_40, NonSpare_52, NonSpare_1, NonSpare_445, NonSpare_386, NonSpare_394, NonSpare_310, NonSpare_156, NonSpare_197, NonSpare_160, NonSpare_371, NonSpare_400, NonSpare_403, NonSpare_382, NonSpare_446, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1641]
    items_dict = {"010": NonSpare_40, "012": NonSpare_52, "000": NonSpare_1, "350": NonSpare_445, "220": NonSpare_386, "221": NonSpare_394, "140": NonSpare_310, "045": NonSpare_156, "070": NonSpare_197, "050": NonSpare_160, "200": NonSpare_371, "230": NonSpare_400, "240": NonSpare_403, "210": NonSpare_382, "360": NonSpare_446, "SP": NonSpare_1641}

class Uap_29(UapSingle):
    record = Record_33

class Asterix_15(AstCat):
    category = 17
    edition = (1, 3)
    uap = Uap_29

class NonSpare_125(NonSpare):
    name = "036"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class NonSpare_127(NonSpare):
    name = "037"
    title = "Data Destination Identifier"
    rule = RuleVariation_1104

class Content_35(ContentTable):
    values = {0: "Associate_req", 1: "Associate_resp", 2: "Release_req", 3: "Release_resp", 4: "Abort_req", 5: "Keep_alive", 16: "Aircraft_report", 17: "Aircraft_command", 18: "II_code_change", 32: "Uplink_packet", 33: "Cancel_uplink_packet", 34: "Uplink_packet_ack", 35: "Downlink_packet", 38: "Data_XON", 39: "Data_XOFF", 48: "Uplink_broadcast", 49: "Cancel_uplink_broadcast", 50: "Uplink_broadcast_ack", 52: "Downlink_broadcast", 64: "GICB_extraction", 65: "Cancel_GICB_extraction", 66: "GICB_extraction_ack", 67: "GICB_response"}

class RuleContent_35(RuleContentContextFree):
    variation = Content_35

class Variation_160(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_35

class RuleVariation_155(RuleVariationContextFree):
    variation = Variation_160

class NonSpare_0(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_155

class Content_13(ContentTable):
    values = {0: "Accepted, the request is accepted and is under processing", 1: "Rejected, the request has not been accepted", 2: "Cancelled, the request has been cancelled", 3: "Finished, the request has been accepted and successfully processed", 4: "Delayed, the request processing is temporarily delayed but the request is still valid", 5: "In Progress, the request is being successfully processed", 6: "In Progress, the request is being successfully processed"}

class RuleContent_13(RuleContentContextFree):
    variation = Content_13

class Variation_128(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_13

class RuleVariation_128(RuleVariationContextFree):
    variation = Variation_128

class NonSpare_661(NonSpare):
    name = "CAUSE"
    title = "Cause"
    rule = RuleVariation_128

class Item_134(Item):
    non_spare = NonSpare_661

class Content_337(ContentTable):
    values = {0: "No diagnostic available", 1: "Aircraft Exit", 2: "Incorrect aircraft address", 3: "Impossibility to process the message", 4: "Insufficient or change in data link capability", 5: "Invalid LV field", 6: "Duplicate request number", 7: "Unknown request number", 8: "Timer T3 expiry", 9: "Expiry of I/R delivery timer", 10: "Uplink flow disabled by UC"}

class RuleContent_337(RuleContentContextFree):
    variation = Content_337

class Variation_772(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_337

class RuleVariation_743(RuleVariationContextFree):
    variation = Variation_772

class NonSpare_832(NonSpare):
    name = "DIAG"
    title = "Diagnostic"
    rule = RuleVariation_743

class Item_253(Item):
    non_spare = NonSpare_832

class Variation_1029(Group):
    bit_size = 8
    items_list = [Item_134, Item_253]
    items_dict = {"CAUSE": NonSpare_661, "DIAG": NonSpare_832}

class RuleVariation_993(RuleVariationContextFree):
    variation = Variation_1029

class NonSpare_19(NonSpare):
    name = "001"
    title = "Result"
    rule = RuleVariation_993

class NonSpare_22(NonSpare):
    name = "005"
    title = "Mode S Address"
    rule = RuleVariation_335

class Variation_367(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_585

class RuleVariation_361(RuleVariationContextFree):
    variation = Variation_367

class NonSpare_69(NonSpare):
    name = "016"
    title = "Packet Number"
    rule = RuleVariation_361

class Variation_1330(Repetitive):
    rep_bytes = 1
    variation = Variation_367

class RuleVariation_1260(RuleVariationContextFree):
    variation = Variation_1330

class NonSpare_71(NonSpare):
    name = "017"
    title = "Packet Number List"
    rule = RuleVariation_1260

class Variation_474(Element):
    bit_offset8 = 1
    bit_size = 5
    rule = RuleContent_585

class RuleVariation_464(RuleVariationContextFree):
    variation = Variation_474

class NonSpare_1372(NonSpare):
    name = "PR"
    title = "Mode S Packet Internal Priority"
    rule = RuleVariation_464

class Item_660(Item):
    non_spare = NonSpare_1372

class Content_430(ContentTable):
    values = {0: "SVC packets", 1: "MSP packets", 2: "Route packets"}

class RuleContent_430(RuleContentContextFree):
    variation = Content_430

class Variation_931(Element):
    bit_offset8 = 6
    bit_size = 2
    rule = RuleContent_430

class RuleVariation_902(RuleVariationContextFree):
    variation = Variation_931

class NonSpare_1396(NonSpare):
    name = "PT"
    title = "Packet Type"
    rule = RuleVariation_902

class Item_678(Item):
    non_spare = NonSpare_1396

class Variation_962(Group):
    bit_size = 8
    items_list = [Item_0, Item_660, Item_678]
    items_dict = {"PR": NonSpare_1372, "PT": NonSpare_1396}

class RuleVariation_933(RuleVariationContextFree):
    variation = Variation_962

class NonSpare_73(NonSpare):
    name = "018"
    title = "Mode S Packet Properties"
    rule = RuleVariation_933

class Variation_1381(Explicit):
    explicit_type = None

class RuleVariation_1311(RuleVariationContextFree):
    variation = Variation_1381

class NonSpare_74(NonSpare):
    name = "019"
    title = "Mode S Packet"
    rule = RuleVariation_1311

class Content_695(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "s"

class RuleContent_695(RuleContentContextFree):
    variation = Content_695

class Variation_299(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_695

class RuleVariation_293(RuleVariationContextFree):
    variation = Variation_299

class NonSpare_98(NonSpare):
    name = "028"
    title = "GICB Extraction Periodicity"
    rule = RuleVariation_293

class Variation_142(Element):
    bit_offset8 = 0
    bit_size = 5
    rule = RuleContent_585

class RuleVariation_142(RuleVariationContextFree):
    variation = Variation_142

class NonSpare_1382(NonSpare):
    name = "PRIORITY"
    title = "GICB Priority"
    rule = RuleVariation_142

class Item_670(Item):
    non_spare = NonSpare_1382

class Item_25(Spare):
    bit_offset8 = 5
    bit_size = 3

class Content_485(ContentTable):
    values = {0: "The periodicity may not be strictly respected", 1: "The periodicity shall be strictly respected"}

class RuleContent_485(RuleContentContextFree):
    variation = Content_485

class Variation_84(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_485

class RuleVariation_84(RuleVariationContextFree):
    variation = Variation_84

class NonSpare_1343(NonSpare):
    name = "PC"
    title = "Periodicity Constraint"
    rule = RuleVariation_84

class Item_644(Item):
    non_spare = NonSpare_1343

class Content_214(ContentTable):
    values = {0: "GICB extractions should be sent only when required by the periodicity", 1: "If a GICB extraction is done due to external conditions, an update will also be sent, even if it does not match the expected periodicity"}

class RuleContent_214(RuleContentContextFree):
    variation = Content_214

class Variation_418(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_214

class RuleVariation_408(RuleVariationContextFree):
    variation = Variation_418

class NonSpare_599(NonSpare):
    name = "AU"
    title = "Asynchronous Update"
    rule = RuleVariation_408

class Item_87(Item):
    non_spare = NonSpare_599

class Content_466(ContentTable):
    values = {0: "The GICB extraction is attempted according to the periodicity", 1: "There will no GICB attempts"}

class RuleContent_466(RuleContentContextFree):
    variation = Content_466

class Variation_555(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_466

class RuleVariation_545(RuleVariationContextFree):
    variation = Variation_555

class NonSpare_1281(NonSpare):
    name = "NE"
    title = "Non Extraction"
    rule = RuleVariation_545

class Item_586(Item):
    non_spare = NonSpare_1281

class Content_469(ContentTable):
    values = {0: "The extracted GICB must be sent only on the Data Link line", 1: "The extracted GICB must be sent only on the Surveillance line", 2: "The extracted GICB must be sent both on the Data Link and on the Surveillance lines"}

class RuleContent_469(RuleContentContextFree):
    variation = Content_469

class Variation_655(Element):
    bit_offset8 = 3
    bit_size = 2
    rule = RuleContent_469

class RuleVariation_645(RuleVariationContextFree):
    variation = Variation_655

class NonSpare_1472(NonSpare):
    name = "RD"
    title = "Reply Destination"
    rule = RuleVariation_645

class Item_746(Item):
    non_spare = NonSpare_1472

class Variation_1126(Group):
    bit_size = 16
    items_list = [Item_670, Item_25, Item_644, Item_87, Item_586, Item_746, Item_25]
    items_dict = {"PRIORITY": NonSpare_1382, "PC": NonSpare_1343, "AU": NonSpare_599, "NE": NonSpare_1281, "RD": NonSpare_1472}

class RuleVariation_1080(RuleVariationContextFree):
    variation = Variation_1126

class NonSpare_100(NonSpare):
    name = "030"
    title = "GICB Properties"
    rule = RuleVariation_1080

class NonSpare_96(NonSpare):
    name = "025"
    title = "GICB Number"
    rule = RuleVariation_361

class NonSpare_97(NonSpare):
    name = "027"
    title = "BDS Code"
    rule = RuleVariation_154

class Content_772(ContentBds):
    bds_type = (BdsAt, None)

class RuleContent_771(RuleContentContextFree):
    variation = Content_772

class Variation_386(Element):
    bit_offset8 = 0
    bit_size = 56
    rule = RuleContent_771

class RuleVariation_379(RuleVariationContextFree):
    variation = Variation_386

class NonSpare_99(NonSpare):
    name = "029"
    title = "GICB Extracted"
    rule = RuleVariation_379

class NonSpare_20(NonSpare):
    name = "002"
    title = "Time of Day"
    rule = RuleVariation_356

class NonSpare_23(NonSpare):
    name = "006"
    title = "Mode S Address List"
    rule = RuleVariation_1258

class Content_506(ContentTable):
    values = {0: "UC shall be ignored", 1: "UC shall be taken into account"}

class RuleContent_506(RuleContentContextFree):
    variation = Content_506

class Variation_89(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_506

class RuleVariation_89(RuleVariationContextFree):
    variation = Variation_89

class NonSpare_1887(NonSpare):
    name = "UM"
    title = "Uplink Mask"
    rule = RuleVariation_89

class Item_1046(Item):
    non_spare = NonSpare_1887

class Content_68(ContentTable):
    values = {0: "DC shall be ignored", 1: "DC shall be taken into account"}

class RuleContent_68(RuleContentContextFree):
    variation = Content_68

class Variation_395(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_68

class RuleVariation_385(RuleVariationContextFree):
    variation = Variation_395

class NonSpare_837(NonSpare):
    name = "DM"
    title = "Downlink Mask"
    rule = RuleVariation_385

class Item_257(Item):
    non_spare = NonSpare_837

class Content_548(ContentTable):
    values = {0: "the uplink flow shall be enabled", 1: "the uplink flow shall be stopped"}

class RuleContent_548(RuleContentContextFree):
    variation = Content_548

class Variation_563(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_548

class RuleVariation_553(RuleVariationContextFree):
    variation = Variation_563

class NonSpare_1882(NonSpare):
    name = "UC"
    title = "Uplink Command"
    rule = RuleVariation_553

class Item_1041(Item):
    non_spare = NonSpare_1882

class Content_547(ContentTable):
    values = {0: "the downlink flow shall be enabled", 1: "the downlink flow shall be stopped"}

class RuleContent_547(RuleContentContextFree):
    variation = Content_547

class Variation_649(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_547

class RuleVariation_639(RuleVariationContextFree):
    variation = Variation_649

class NonSpare_822(NonSpare):
    name = "DC"
    title = "Downlink Command"
    rule = RuleVariation_639

class Item_244(Item):
    non_spare = NonSpare_822

class Variation_1207(Group):
    bit_size = 8
    items_list = [Item_1046, Item_257, Item_1041, Item_244, Item_22]
    items_dict = {"UM": NonSpare_1887, "DM": NonSpare_837, "UC": NonSpare_1882, "DC": NonSpare_822}

class RuleVariation_1141(RuleVariationContextFree):
    variation = Variation_1207

class NonSpare_24(NonSpare):
    name = "007"
    title = "Aircraft Data Link Command"
    rule = RuleVariation_1141

class Content_471(ContentTable):
    values = {0: "The interrogator is enabled to uplink frames", 1: "The interrogator is disabled to uplink frames"}

class RuleContent_471(RuleContentContextFree):
    variation = Content_471

class Variation_80(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_471

class RuleVariation_80(RuleVariationContextFree):
    variation = Variation_80

class NonSpare_1886(NonSpare):
    name = "UDS"
    title = "Uplink Default Status"
    rule = RuleVariation_80

class Item_1045(Item):
    non_spare = NonSpare_1886

class Content_470(ContentTable):
    values = {0: "The interrogator is enabled to extract frames", 1: "The interrogator is disabled to extract frames"}

class RuleContent_470(RuleContentContextFree):
    variation = Content_470

class Variation_444(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_470

class RuleVariation_434(RuleVariationContextFree):
    variation = Variation_444

class NonSpare_826(NonSpare):
    name = "DDS"
    title = "Downlink Default Status"
    rule = RuleVariation_434

class Item_248(Item):
    non_spare = NonSpare_826

class Variation_556(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_471

class RuleVariation_546(RuleVariationContextFree):
    variation = Variation_556

class NonSpare_1885(NonSpare):
    name = "UCS"
    title = "Uplink Current Status"
    rule = RuleVariation_546

class Item_1044(Item):
    non_spare = NonSpare_1885

class Variation_637(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_470

class RuleVariation_627(RuleVariationContextFree):
    variation = Variation_637

class NonSpare_825(NonSpare):
    name = "DCS"
    title = "Downlink Current Status"
    rule = RuleVariation_627

class Item_247(Item):
    non_spare = NonSpare_825

class Content_468(ContentTable):
    values = {0: "The aircraft is in the Datalink coverage map of the interrogator", 1: "The aircraft is not in the Datalink coverage map of the interrogator"}

class RuleContent_468(RuleContentContextFree):
    variation = Content_468

class Variation_922(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_468

class RuleVariation_893(RuleVariationContextFree):
    variation = Variation_922

class NonSpare_864(NonSpare):
    name = "EI"
    title = "Exit Indication"
    rule = RuleVariation_893

class Item_276(Item):
    non_spare = NonSpare_864

class Content_472(ContentTable):
    values = {0: "The interrogators current ability to uplink/downlink frames (UCS/DCS) and the content of the Aircraft_report could be changed using D_Data_link_command", 1: "The interrogators current ability to uplink/downlink frames (UCS/DCS) and the content of the Aircraft_report cannot be changed using D_Data_link_command"}

class RuleContent_472(RuleContentContextFree):
    variation = Content_472

class Variation_81(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_472

class RuleVariation_81(RuleVariationContextFree):
    variation = Variation_81

class NonSpare_1036(NonSpare):
    name = "IC"
    title = "Interrogator Control"
    rule = RuleVariation_81

class Item_399(Item):
    non_spare = NonSpare_1036

class Variation_1323(Extended):
    items = [Item_1045, Item_248, Item_1044, Item_247, Item_20, Item_276, None, Item_399, Item_9, None]

class RuleVariation_1253(RuleVariationContextFree):
    variation = Variation_1323

class NonSpare_25(NonSpare):
    name = "008"
    title = "Aircraft Data Link Status"
    rule = RuleVariation_1253

class Content_478(ContentTable):
    values = {0: "The next Aircraft_report may not include D_Data_link_status", 1: "The next Aircraft_report shall include D_Data_link_status"}

class RuleContent_478(RuleContentContextFree):
    variation = Content_478

class Variation_83(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_478

class RuleVariation_83(RuleVariationContextFree):
    variation = Variation_83

class NonSpare_1653(NonSpare):
    name = "SR"
    title = ""
    rule = RuleVariation_83

class Item_873(Item):
    non_spare = NonSpare_1653

class Content_474(ContentTable):
    values = {0: "The next Aircraft_report may not include D_COM", 1: "The next Aircraft_report shall include D_COM"}

class RuleContent_474(RuleContentContextFree):
    variation = Content_474

class Variation_445(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_474

class RuleVariation_435(RuleVariationContextFree):
    variation = Variation_445

class NonSpare_577(NonSpare):
    name = "AR"
    title = ""
    rule = RuleVariation_435

class Item_75(Item):
    non_spare = NonSpare_577

class Content_479(ContentTable):
    values = {0: "The next Aircraft_report may not include D_ECA", 1: "The next Aircraft_report shall include D_ECA"}

class RuleContent_479(RuleContentContextFree):
    variation = Content_479

class Variation_557(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_479

class RuleVariation_547(RuleVariationContextFree):
    variation = Variation_557

class NonSpare_890(NonSpare):
    name = "ER"
    title = ""
    rule = RuleVariation_547

class Item_295(Item):
    non_spare = NonSpare_890

class Content_475(ContentTable):
    values = {0: "The next Aircraft_report may not include D_CQF", 1: "The next Aircraft_report shall include D_CQF"}

class RuleContent_475(RuleContentContextFree):
    variation = Content_475

class Variation_638(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_475

class RuleVariation_628(RuleVariationContextFree):
    variation = Variation_638

class NonSpare_925(NonSpare):
    name = "FR"
    title = ""
    rule = RuleVariation_628

class Item_318(Item):
    non_spare = NonSpare_925

class Content_476(ContentTable):
    values = {0: "The next Aircraft_report may not include D_CQF_method", 1: "The next Aircraft_report shall include D_CQF_method"}

class RuleContent_476(RuleContentContextFree):
    variation = Content_476

class Variation_728(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_476

class RuleVariation_718(RuleVariationContextFree):
    variation = Variation_728

class NonSpare_1237(NonSpare):
    name = "MR"
    title = ""
    rule = RuleVariation_718

class Item_548(Item):
    non_spare = NonSpare_1237

class Content_480(ContentTable):
    values = {0: "The next Aircraft_report may not include D_Polar_position", 1: "The next Aircraft_report shall include D_Polar_position"}

class RuleContent_480(RuleContentContextFree):
    variation = Content_480

class Variation_842(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_480

class RuleVariation_813(RuleVariationContextFree):
    variation = Variation_842

class NonSpare_1371(NonSpare):
    name = "PR"
    title = ""
    rule = RuleVariation_813

class Item_659(Item):
    non_spare = NonSpare_1371

class Content_477(ContentTable):
    values = {0: "The next Aircraft_report may not include D_Cartesian_position", 1: "The next Aircraft_report shall include D_Cartesian_position"}

class RuleContent_477(RuleContentContextFree):
    variation = Content_477

class Variation_923(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_477

class RuleVariation_894(RuleVariationContextFree):
    variation = Variation_923

class NonSpare_775(NonSpare):
    name = "CR"
    title = ""
    rule = RuleVariation_894

class Item_205(Item):
    non_spare = NonSpare_775

class Content_473(ContentTable):
    values = {0: "The next Aircraft_report may not include Aircraft_ID", 1: "The next Aircraft_report shall include Aircraft_ID"}

class RuleContent_473(RuleContentContextFree):
    variation = Content_473

class Variation_82(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_473

class RuleVariation_82(RuleVariationContextFree):
    variation = Variation_82

class NonSpare_1038(NonSpare):
    name = "ID"
    title = ""
    rule = RuleVariation_82

class Item_401(Item):
    non_spare = NonSpare_1038

class Content_483(ContentTable):
    values = {0: "The next Aircraft_report may not include Mode_A", 1: "The next Aircraft_report shall include Mode_A"}

class RuleContent_483(RuleContentContextFree):
    variation = Content_483

class Variation_446(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_483

class RuleVariation_436(RuleVariationContextFree):
    variation = Variation_446

class NonSpare_1144(NonSpare):
    name = "MA"
    title = ""
    rule = RuleVariation_436

class Item_491(Item):
    non_spare = NonSpare_1144

class Content_484(ContentTable):
    values = {0: "The next Aircraft_report may not include Speed", 1: "The next Aircraft_report shall include Speed"}

class RuleContent_484(RuleContentContextFree):
    variation = Content_484

class Variation_558(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_484

class RuleVariation_548(RuleVariationContextFree):
    variation = Variation_558

class NonSpare_1640(NonSpare):
    name = "SP"
    title = ""
    rule = RuleVariation_548

class Item_861(Item):
    non_spare = NonSpare_1640

class Content_482(ContentTable):
    values = {0: "The next Aircraft_report may not include Height", 1: "The next Aircraft_report shall include Height"}

class RuleContent_482(RuleContentContextFree):
    variation = Content_482

class Variation_639(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_482

class RuleVariation_629(RuleVariationContextFree):
    variation = Variation_639

class NonSpare_998(NonSpare):
    name = "HG"
    title = ""
    rule = RuleVariation_629

class Item_369(Item):
    non_spare = NonSpare_998

class Content_481(ContentTable):
    values = {0: "The next Aircraft_report may not include Heading", 1: "The next Aircraft_report shall include Heading"}

class RuleContent_481(RuleContentContextFree):
    variation = Content_481

class Variation_729(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_481

class RuleVariation_719(RuleVariationContextFree):
    variation = Variation_729

class NonSpare_992(NonSpare):
    name = "HD"
    title = ""
    rule = RuleVariation_719

class Item_365(Item):
    non_spare = NonSpare_992

class Variation_1314(Extended):
    items = [Item_873, Item_75, Item_295, Item_318, Item_548, Item_659, Item_205, None, Item_401, Item_491, Item_861, Item_369, Item_365, Item_24, None]

class RuleVariation_1244(RuleVariationContextFree):
    variation = Variation_1314

class NonSpare_27(NonSpare):
    name = "009"
    title = "Aircraft Data Link Report Request"
    rule = RuleVariation_1244

class Content_323(ContentTable):
    values = {0: "No communications capability (surveillance only)", 1: "Comm. A and Comm. B capability", 2: "Comm. A, Comm. B and Uplink ELM", 3: "Comm. A, Comm. B and Uplink ELM and Downlink ELM", 4: "Level 5 Transponder capability"}

class RuleContent_323(RuleContentContextFree):
    variation = Content_323

class Variation_859(Element):
    bit_offset8 = 5
    bit_size = 3
    rule = RuleContent_323

class RuleVariation_830(RuleVariationContextFree):
    variation = Variation_859

class NonSpare_736(NonSpare):
    name = "COM"
    title = "Communications Capability of the Transponder"
    rule = RuleVariation_830

class Item_183(Item):
    non_spare = NonSpare_736

class Variation_1002(Group):
    bit_size = 8
    items_list = [Item_4, Item_183]
    items_dict = {"COM": NonSpare_736}

class RuleVariation_971(RuleVariationContextFree):
    variation = Variation_1002

class NonSpare_49(NonSpare):
    name = "010"
    title = "Transponder Communications Capability"
    rule = RuleVariation_971

class NonSpare_50(NonSpare):
    name = "011"
    title = "Capability Report"
    rule = RuleVariation_377

class Content_746(ContentQuantity):
    signedness = Unsigned
    lsb = 3.90625e-3
    unit = "NM"

class RuleContent_745(RuleContentContextFree):
    variation = Content_746

class Variation_327(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_745

class RuleVariation_321(RuleVariationContextFree):
    variation = Variation_327

class NonSpare_1503(NonSpare):
    name = "RHO"
    title = ""
    rule = RuleVariation_321

class Item_774(Item):
    non_spare = NonSpare_1503

class Variation_1141(Group):
    bit_size = 32
    items_list = [Item_774, Item_947]
    items_dict = {"RHO": NonSpare_1503, "THETA": NonSpare_1761}

class RuleVariation_1094(RuleVariationContextFree):
    variation = Variation_1141

class NonSpare_54(NonSpare):
    name = "014"
    title = "Aircraft Position in Polar Co-ordinates"
    rule = RuleVariation_1094

class Content_649(ContentQuantity):
    signedness = Signed
    lsb = 7.8125e-3
    unit = "NM"

class RuleContent_649(RuleContentContextFree):
    variation = Content_649

class Variation_278(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_649

class RuleVariation_272(RuleVariationContextFree):
    variation = Variation_278

class NonSpare_1982(NonSpare):
    name = "X"
    title = "X-Component"
    rule = RuleVariation_272

class Item_1131(Item):
    non_spare = NonSpare_1982

class NonSpare_2035(NonSpare):
    name = "Y"
    title = "Y-Component"
    rule = RuleVariation_272

class Item_1181(Item):
    non_spare = NonSpare_2035

class Variation_1268(Group):
    bit_size = 32
    items_list = [Item_1131, Item_1181]
    items_dict = {"X": NonSpare_1982, "Y": NonSpare_2035}

class RuleVariation_1201(RuleVariationContextFree):
    variation = Variation_1268

class NonSpare_55(NonSpare):
    name = "015"
    title = "Aircraft Position in Cartesian Co-ordinates"
    rule = RuleVariation_1201

class NonSpare_76(NonSpare):
    name = "020"
    title = "Broadcast Number"
    rule = RuleVariation_361

class Variation_132(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_585

class RuleVariation_132(RuleVariationContextFree):
    variation = Variation_132

class NonSpare_1383(NonSpare):
    name = "PRIORITY"
    title = "Priority"
    rule = RuleVariation_132

class Item_671(Item):
    non_spare = NonSpare_1383

class Variation_775(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_585

class RuleVariation_746(RuleVariationContextFree):
    variation = Variation_775

class NonSpare_1370(NonSpare):
    name = "POWER"
    title = "Power"
    rule = RuleVariation_746

class Item_658(Item):
    non_spare = NonSpare_1370

class Variation_214(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_695

class RuleVariation_208(RuleVariationContextFree):
    variation = Variation_214

class NonSpare_859(NonSpare):
    name = "DURATION"
    title = "Duration"
    rule = RuleVariation_208

class Item_273(Item):
    non_spare = NonSpare_859

class Variation_365(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_0

class RuleVariation_359(RuleVariationContextFree):
    variation = Variation_365

class NonSpare_758(NonSpare):
    name = "COVERAGE"
    title = "Coverage"
    rule = RuleVariation_359

class Item_197(Item):
    non_spare = NonSpare_758

class Variation_1127(Group):
    bit_size = 48
    items_list = [Item_671, Item_658, Item_273, Item_197]
    items_dict = {"PRIORITY": NonSpare_1383, "POWER": NonSpare_1370, "DURATION": NonSpare_859, "COVERAGE": NonSpare_758}

class RuleVariation_1081(RuleVariationContextFree):
    variation = Variation_1127

class NonSpare_93(NonSpare):
    name = "021"
    title = "Broadcast Properties"
    rule = RuleVariation_1081

class NonSpare_1378(NonSpare):
    name = "PREFIX"
    title = "Prefix Field"
    rule = RuleVariation_835

class Item_666(Item):
    non_spare = NonSpare_1378

class Variation_1004(Group):
    bit_size = 32
    items_list = [Item_4, Item_666]
    items_dict = {"PREFIX": NonSpare_1378}

class RuleVariation_973(RuleVariationContextFree):
    variation = Variation_1004

class NonSpare_94(NonSpare):
    name = "022"
    title = "Broadcast Prefix"
    rule = RuleVariation_973

class NonSpare_95(NonSpare):
    name = "023"
    title = "Uplink or Downlink Broadcast"
    rule = RuleVariation_377

class NonSpare_1379(NonSpare):
    name = "PREVIOUSII"
    title = "Former II Code"
    rule = RuleVariation_127

class Item_667(Item):
    non_spare = NonSpare_1379

class NonSpare_798(NonSpare):
    name = "CURRENTII"
    title = "Current II Code"
    rule = RuleVariation_741

class Item_223(Item):
    non_spare = NonSpare_798

class Variation_1124(Group):
    bit_size = 8
    items_list = [Item_667, Item_223]
    items_dict = {"PREVIOUSII": NonSpare_1379, "CURRENTII": NonSpare_798}

class RuleVariation_1078(RuleVariationContextFree):
    variation = Variation_1124

class NonSpare_21(NonSpare):
    name = "004"
    title = "II Code"
    rule = RuleVariation_1078

class Variation_381(Element):
    bit_offset8 = 0
    bit_size = 48
    rule = RuleContent_0

class RuleVariation_374(RuleVariationContextFree):
    variation = Variation_381

class NonSpare_116(NonSpare):
    name = "031"
    title = "Aircraft Identity"
    rule = RuleVariation_374

class NonSpare_1221(NonSpare):
    name = "MOD3A"
    title = ""
    rule = RuleVariation_751

class Item_532(Item):
    non_spare = NonSpare_1221

class Variation_1218(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_423, Item_16, Item_532]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "L": NonSpare_1066, "MOD3A": NonSpare_1221}

class RuleVariation_1152(RuleVariationContextFree):
    variation = Variation_1218

class NonSpare_117(NonSpare):
    name = "032"
    title = "Aircraft Mode A"
    rule = RuleVariation_1152

class Variation_1213(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_309]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "FL": NonSpare_908}

class RuleVariation_1147(RuleVariationContextFree):
    variation = Variation_1213

class NonSpare_119(NonSpare):
    name = "033"
    title = "Aircraft Height"
    rule = RuleVariation_1147

class NonSpare_120(NonSpare):
    name = "034"
    title = "Aircraft Speed"
    rule = RuleVariation_324

class NonSpare_122(NonSpare):
    name = "035"
    title = "Aircraft Heading"
    rule = RuleVariation_329

class Content_28(ContentTable):
    values = {0: "Aircraft is airborne", 1: "Aircraft is on the ground"}

class RuleContent_28(RuleContentContextFree):
    variation = Content_28

class Variation_9(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_28

class RuleVariation_9(RuleVariationContextFree):
    variation = Variation_9

class NonSpare_932(NonSpare):
    name = "FS"
    title = "Flight Status"
    rule = RuleVariation_9

class Item_325(Item):
    non_spare = NonSpare_932

class Content_465(ContentTable):
    values = {0: "The CQF calculation method is not supported", 1: "The CQF is minimum", 126: "The CQF is maximum", 127: "The CQF is undefined according to the calculation method"}

class RuleContent_465(RuleContentContextFree):
    variation = Content_465

class Variation_476(Element):
    bit_offset8 = 1
    bit_size = 7
    rule = RuleContent_465

class RuleVariation_466(RuleVariationContextFree):
    variation = Variation_476

class NonSpare_774(NonSpare):
    name = "CQF"
    title = "Aircraft CQF"
    rule = RuleVariation_466

class Item_204(Item):
    non_spare = NonSpare_774

class Variation_1071(Group):
    bit_size = 8
    items_list = [Item_325, Item_204]
    items_dict = {"FS": NonSpare_932, "CQF": NonSpare_774}

class RuleVariation_1030(RuleVariationContextFree):
    variation = Variation_1071

class NonSpare_51(NonSpare):
    name = "012"
    title = "Aircraft Coverage Quality Factor"
    rule = RuleVariation_1030

class NonSpare_53(NonSpare):
    name = "013"
    title = "Aircraft CQF Calculation Method"
    rule = RuleVariation_154

class Record_48(Record):
    items_list = [NonSpare_125, NonSpare_127, NonSpare_0, NonSpare_19, NonSpare_22, NonSpare_69, NonSpare_71, NonSpare_73, NonSpare_74, NonSpare_98, NonSpare_100, NonSpare_96, NonSpare_97, NonSpare_99, NonSpare_20, NonSpare_23, NonSpare_24, NonSpare_25, NonSpare_27, NonSpare_49, NonSpare_50, NonSpare_54, NonSpare_55, NonSpare_76, NonSpare_93, NonSpare_94, NonSpare_95, NonSpare_21, NonSpare_116, NonSpare_117, NonSpare_119, NonSpare_120, NonSpare_122, NonSpare_51, NonSpare_53]
    items_dict = {"036": NonSpare_125, "037": NonSpare_127, "000": NonSpare_0, "001": NonSpare_19, "005": NonSpare_22, "016": NonSpare_69, "017": NonSpare_71, "018": NonSpare_73, "019": NonSpare_74, "028": NonSpare_98, "030": NonSpare_100, "025": NonSpare_96, "027": NonSpare_97, "029": NonSpare_99, "002": NonSpare_20, "006": NonSpare_23, "007": NonSpare_24, "008": NonSpare_25, "009": NonSpare_27, "010": NonSpare_49, "011": NonSpare_50, "014": NonSpare_54, "015": NonSpare_55, "020": NonSpare_76, "021": NonSpare_93, "022": NonSpare_94, "023": NonSpare_95, "004": NonSpare_21, "031": NonSpare_116, "032": NonSpare_117, "033": NonSpare_119, "034": NonSpare_120, "035": NonSpare_122, "012": NonSpare_51, "013": NonSpare_53}

class Uap_44(UapSingle):
    record = Record_48

class Asterix_16(AstCat):
    category = 18
    edition = (1, 7)
    uap = Uap_44

class NonSpare_44(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_568(ContentTable):
    values = {1: "Start of Update Cycle", 2: "Periodic Status Message", 3: "Event-triggered Status Message"}

class RuleContent_568(RuleContentContextFree):
    variation = Content_568

class Variation_186(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_568

class RuleVariation_180(RuleVariationContextFree):
    variation = Variation_186

class NonSpare_9(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_180

class NonSpare_305(NonSpare):
    name = "140"
    title = "Time of Day"
    rule = RuleVariation_356

class Content_394(ContentTable):
    values = {0: "Operational", 1: "Degraded", 2: "NOGO", 3: "Undefined"}

class RuleContent_394(RuleContentContextFree):
    variation = Content_394

class Variation_104(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_394

class RuleVariation_104(RuleVariationContextFree):
    variation = Variation_104

class NonSpare_1292(NonSpare):
    name = "NOGO"
    title = "Operational Release Status of the System"
    rule = RuleVariation_104

class Item_597(Item):
    non_spare = NonSpare_1292

class Variation_727(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_464

class RuleVariation_717(RuleVariationContextFree):
    variation = Variation_727

class NonSpare_1849(NonSpare):
    name = "TTF"
    title = "Test Target"
    rule = RuleVariation_717

class Item_1011(Item):
    non_spare = NonSpare_1849

class Variation_1117(Group):
    bit_size = 8
    items_list = [Item_597, Item_633, Item_1005, Item_1011, Item_25]
    items_dict = {"NOGO": NonSpare_1292, "OVL": NonSpare_1330, "TSV": NonSpare_1841, "TTF": NonSpare_1849}

class RuleVariation_1072(RuleVariationContextFree):
    variation = Variation_1117

class NonSpare_486(NonSpare):
    name = "550"
    title = "System Status"
    rule = RuleVariation_1072

class Content_436(ContentTable):
    values = {0: "Standby", 1: "Exec"}

class RuleContent_436(RuleContentContextFree):
    variation = Content_436

class Variation_71(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_436

class RuleVariation_71(RuleVariationContextFree):
    variation = Variation_71

class NonSpare_1796(NonSpare):
    name = "TP1A"
    title = ""
    rule = RuleVariation_71

class Item_966(Item):
    non_spare = NonSpare_1796

class Content_205(ContentTable):
    values = {0: "Faulted", 1: "Good"}

class RuleContent_205(RuleContentContextFree):
    variation = Content_205

class Variation_416(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_205

class RuleVariation_406(RuleVariationContextFree):
    variation = Variation_416

class NonSpare_1797(NonSpare):
    name = "TP1B"
    title = ""
    rule = RuleVariation_406

class Item_967(Item):
    non_spare = NonSpare_1797

class Variation_553(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_436

class RuleVariation_543(RuleVariationContextFree):
    variation = Variation_553

class NonSpare_1798(NonSpare):
    name = "TP2A"
    title = ""
    rule = RuleVariation_543

class Item_968(Item):
    non_spare = NonSpare_1798

class Variation_615(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_205

class RuleVariation_605(RuleVariationContextFree):
    variation = Variation_615

class NonSpare_1799(NonSpare):
    name = "TP2B"
    title = ""
    rule = RuleVariation_605

class Item_969(Item):
    non_spare = NonSpare_1799

class Variation_724(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_436

class RuleVariation_714(RuleVariationContextFree):
    variation = Variation_724

class NonSpare_1800(NonSpare):
    name = "TP3A"
    title = ""
    rule = RuleVariation_714

class Item_970(Item):
    non_spare = NonSpare_1800

class Variation_820(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_205

class RuleVariation_791(RuleVariationContextFree):
    variation = Variation_820

class NonSpare_1801(NonSpare):
    name = "TP3B"
    title = ""
    rule = RuleVariation_791

class Item_971(Item):
    non_spare = NonSpare_1801

class Variation_918(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_436

class RuleVariation_889(RuleVariationContextFree):
    variation = Variation_918

class NonSpare_1802(NonSpare):
    name = "TP4A"
    title = ""
    rule = RuleVariation_889

class Item_972(Item):
    non_spare = NonSpare_1802

class Variation_941(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_205

class RuleVariation_912(RuleVariationContextFree):
    variation = Variation_941

class NonSpare_1803(NonSpare):
    name = "TP4B"
    title = ""
    rule = RuleVariation_912

class Item_973(Item):
    non_spare = NonSpare_1803

class Variation_1186(Group):
    bit_size = 8
    items_list = [Item_966, Item_967, Item_968, Item_969, Item_970, Item_971, Item_972, Item_973]
    items_dict = {"TP1A": NonSpare_1796, "TP1B": NonSpare_1797, "TP2A": NonSpare_1798, "TP2B": NonSpare_1799, "TP3A": NonSpare_1800, "TP3B": NonSpare_1801, "TP4A": NonSpare_1802, "TP4B": NonSpare_1803}

class RuleVariation_1129(RuleVariationContextFree):
    variation = Variation_1186

class NonSpare_487(NonSpare):
    name = "551"
    title = "Tracking Processor Detailed Status"
    rule = RuleVariation_1129

class NonSpare_1534(NonSpare):
    name = "RSI"
    title = "8-bit Identification Number of RS"
    rule = RuleVariation_154

class Item_794(Item):
    non_spare = NonSpare_1534

class Content_382(ContentTable):
    values = {0: "Not present", 1: "Present"}

class RuleContent_382(RuleContentContextFree):
    variation = Content_382

class Variation_435(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_382

class RuleVariation_425(RuleVariationContextFree):
    variation = Variation_435

class NonSpare_1527(NonSpare):
    name = "RS1090"
    title = "Receiver 1090 MHz"
    rule = RuleVariation_425

class Item_791(Item):
    non_spare = NonSpare_1527

class Variation_547(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_382

class RuleVariation_537(RuleVariationContextFree):
    variation = Variation_547

class NonSpare_1855(NonSpare):
    name = "TX1030"
    title = "Transmitter 1030 MHz"
    rule = RuleVariation_537

class Item_1016(Item):
    non_spare = NonSpare_1855

class Variation_633(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_382

class RuleVariation_623(RuleVariationContextFree):
    variation = Variation_633

class NonSpare_1856(NonSpare):
    name = "TX1090"
    title = "Transmitter 1090 MHz"
    rule = RuleVariation_623

class Item_1017(Item):
    non_spare = NonSpare_1856

class Variation_696(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_205

class RuleVariation_686(RuleVariationContextFree):
    variation = Variation_696

class NonSpare_1538(NonSpare):
    name = "RSS"
    title = "RS Status"
    rule = RuleVariation_686

class Item_796(Item):
    non_spare = NonSpare_1538

class Content_391(ContentTable):
    values = {0: "Offline", 1: "Online"}

class RuleContent_391(RuleContentContextFree):
    variation = Content_391

class Variation_838(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_391

class RuleVariation_809(RuleVariationContextFree):
    variation = Variation_838

class NonSpare_1535(NonSpare):
    name = "RSO"
    title = "RS Operational"
    rule = RuleVariation_809

class Item_795(Item):
    non_spare = NonSpare_1535

class Variation_1149(Group):
    bit_size = 16
    items_list = [Item_794, Item_0, Item_791, Item_1016, Item_1017, Item_796, Item_795, Item_27]
    items_dict = {"RSI": NonSpare_1534, "RS1090": NonSpare_1527, "TX1030": NonSpare_1855, "TX1090": NonSpare_1856, "RSS": NonSpare_1538, "RSO": NonSpare_1535}

class Variation_1352(Repetitive):
    rep_bytes = 1
    variation = Variation_1149

class RuleVariation_1282(RuleVariationContextFree):
    variation = Variation_1352

class NonSpare_488(NonSpare):
    name = "552"
    title = "Remote Sensor Detailed Status"
    rule = RuleVariation_1282

class Content_576(ContentTable):
    values = {1: "Warning", 2: "Faulted", 3: "Good"}

class RuleContent_576(RuleContentContextFree):
    variation = Content_576

class Variation_113(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_576

class RuleVariation_113(RuleVariationContextFree):
    variation = Variation_113

class NonSpare_1490(NonSpare):
    name = "REFTR1"
    title = "Ref Trans 1 Status"
    rule = RuleVariation_113

class Item_761(Item):
    non_spare = NonSpare_1490

class Variation_744(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_576

class RuleVariation_734(RuleVariationContextFree):
    variation = Variation_744

class NonSpare_1491(NonSpare):
    name = "REFTR2"
    title = "Ref Trans 2 Status"
    rule = RuleVariation_734

class Item_762(Item):
    non_spare = NonSpare_1491

class NonSpare_1492(NonSpare):
    name = "REFTR3"
    title = "Ref Trans 3 Status"
    rule = RuleVariation_113

class Item_763(Item):
    non_spare = NonSpare_1492

class NonSpare_1493(NonSpare):
    name = "REFTR4"
    title = "Ref Trans 4 Status"
    rule = RuleVariation_734

class Item_764(Item):
    non_spare = NonSpare_1493

class Variation_1311(Extended):
    items = [Item_761, Item_12, Item_762, Item_26, None, Item_763, Item_12, Item_764, Item_26, None]

class RuleVariation_1241(RuleVariationContextFree):
    variation = Variation_1311

class NonSpare_489(NonSpare):
    name = "553"
    title = "Reference Transponder Detailed Status"
    rule = RuleVariation_1241

class Content_672(ContentQuantity):
    signedness = Signed
    lsb = 1.6763806343078613e-7
    unit = "°"

class RuleContent_672(RuleContentContextFree):
    variation = Content_672

class Variation_372(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_672

class RuleVariation_366(RuleVariationContextFree):
    variation = Variation_372

class NonSpare_1079(NonSpare):
    name = "LAT"
    title = "Latitude"
    rule = RuleVariation_366

class Item_436(Item):
    non_spare = NonSpare_1079

class Content_671(ContentQuantity):
    signedness = Signed
    lsb = 1.6763806343078613e-7
    unit = "°"

class RuleContent_671(RuleContentContextFree):
    variation = Content_671

class Variation_371(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_671

class RuleVariation_365(RuleVariationContextFree):
    variation = Variation_371

class NonSpare_1107(NonSpare):
    name = "LON"
    title = "Longitude"
    rule = RuleVariation_365

class Item_462(Item):
    non_spare = NonSpare_1107

class Variation_1093(Group):
    bit_size = 64
    items_list = [Item_436, Item_462]
    items_dict = {"LAT": NonSpare_1079, "LON": NonSpare_1107}

class RuleVariation_1052(RuleVariationContextFree):
    variation = Variation_1093

class NonSpare_492(NonSpare):
    name = "600"
    title = "Position of the MLT System Reference Point"
    rule = RuleVariation_1052

class Content_631(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "m"

class RuleContent_631(RuleContentContextFree):
    variation = Content_631

class Variation_268(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_631

class RuleVariation_262(RuleVariationContextFree):
    variation = Variation_268

class NonSpare_500(NonSpare):
    name = "610"
    title = "Height of the MLT System Reference Point"
    rule = RuleVariation_262

class Content_598(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = "m"

class RuleContent_598(RuleContentContextFree):
    variation = Content_598

class Variation_199(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_598

class RuleVariation_193(RuleVariationContextFree):
    variation = Variation_199

class NonSpare_503(NonSpare):
    name = "620"
    title = "WGS-84 Undulation"
    rule = RuleVariation_193

class Record_38(Record):
    items_list = [NonSpare_44, NonSpare_9, NonSpare_305, NonSpare_486, NonSpare_487, NonSpare_488, NonSpare_489, NonSpare_492, NonSpare_500, NonSpare_503, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_44, "000": NonSpare_9, "140": NonSpare_305, "550": NonSpare_486, "551": NonSpare_487, "552": NonSpare_488, "553": NonSpare_489, "600": NonSpare_492, "610": NonSpare_500, "620": NonSpare_503, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_34(UapSingle):
    record = Record_38

class Asterix_17(AstCat):
    category = 19
    edition = (1, 3)
    uap = Uap_34

class NonSpare_41(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_365(ContentTable):
    values = {0: "Non-Mode S 1090MHz multilateration", 1: "No Non-Mode S 1090MHz multilat"}

class RuleContent_365(RuleContentContextFree):
    variation = Content_365

class Variation_63(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_365

class RuleVariation_63(RuleVariationContextFree):
    variation = Variation_63

class NonSpare_1665(NonSpare):
    name = "SSR"
    title = ""
    rule = RuleVariation_63

class Item_882(Item):
    non_spare = NonSpare_1665

class Content_280(ContentTable):
    values = {0: "Mode-S 1090 MHz multilateration", 1: "No Mode-S 1090 MHz multilateration"}

class RuleContent_280(RuleContentContextFree):
    variation = Content_280

class Variation_427(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_280

class RuleVariation_417(RuleVariationContextFree):
    variation = Variation_427

class NonSpare_1243(NonSpare):
    name = "MS"
    title = ""
    rule = RuleVariation_417

class Item_554(Item):
    non_spare = NonSpare_1243

class Content_216(ContentTable):
    values = {0: "HF multilateration", 1: "No HF multilateration"}

class RuleContent_216(RuleContentContextFree):
    variation = Content_216

class Variation_516(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_216

class RuleVariation_506(RuleVariationContextFree):
    variation = Variation_516

class NonSpare_997(NonSpare):
    name = "HF"
    title = ""
    rule = RuleVariation_506

class Item_368(Item):
    non_spare = NonSpare_997

class Content_531(ContentTable):
    values = {0: "VDL Mode 4 multilateration", 1: "No VDL Mode 4 multilateration"}

class RuleContent_531(RuleContentContextFree):
    variation = Content_531

class Variation_646(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_531

class RuleVariation_636(RuleVariationContextFree):
    variation = Variation_646

class NonSpare_1916(NonSpare):
    name = "VDL4"
    title = ""
    rule = RuleVariation_636

class Item_1073(Item):
    non_spare = NonSpare_1916

class Content_505(ContentTable):
    values = {0: "UAT multilateration", 1: "No UAT multilateration"}

class RuleContent_505(RuleContentContextFree):
    variation = Content_505

class Variation_731(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_505

class RuleVariation_721(RuleVariationContextFree):
    variation = Variation_731

class NonSpare_1877(NonSpare):
    name = "UAT"
    title = ""
    rule = RuleVariation_721

class Item_1037(Item):
    non_spare = NonSpare_1877

class Content_69(ContentTable):
    values = {0: "DME/TACAN multilateration", 1: "No DME/TACAN multilateration"}

class RuleContent_69(RuleContentContextFree):
    variation = Content_69

class Variation_797(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_69

class RuleVariation_768(RuleVariationContextFree):
    variation = Variation_797

class NonSpare_838(NonSpare):
    name = "DME"
    title = ""
    rule = RuleVariation_768

class Item_258(Item):
    non_spare = NonSpare_838

class Content_398(ContentTable):
    values = {0: "Other Technology Multilateration", 1: "No Other Technology Multilateration"}

class RuleContent_398(RuleContentContextFree):
    variation = Content_398

class Variation_912(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_398

class RuleVariation_883(RuleVariationContextFree):
    variation = Variation_912

class NonSpare_1326(NonSpare):
    name = "OT"
    title = ""
    rule = RuleVariation_883

class Item_629(Item):
    non_spare = NonSpare_1326

class Content_418(ContentTable):
    values = {0: "Report from target transponder", 1: "Report from field monitor (element transponder)"}

class RuleContent_418(RuleContentContextFree):
    variation = Content_418

class Variation_69(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_418

class RuleVariation_69(RuleVariationContextFree):
    variation = Variation_69

class NonSpare_1451(NonSpare):
    name = "RAB"
    title = ""
    rule = RuleVariation_69

class Item_727(Item):
    non_spare = NonSpare_1451

class Variation_392(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_12

class RuleVariation_382(RuleVariationContextFree):
    variation = Variation_392

class NonSpare_1646(NonSpare):
    name = "SPI"
    title = ""
    rule = RuleVariation_382

class Item_866(Item):
    non_spare = NonSpare_1646

class Variation_494(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_50

class RuleVariation_484(RuleVariationContextFree):
    variation = Variation_494

class NonSpare_684(NonSpare):
    name = "CHN"
    title = ""
    rule = RuleVariation_484

class Item_150(Item):
    non_spare = NonSpare_684

class Variation_642(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_498

class RuleVariation_632(RuleVariationContextFree):
    variation = Variation_642

class NonSpare_961(NonSpare):
    name = "GBS"
    title = ""
    rule = RuleVariation_632

class Item_343(Item):
    non_spare = NonSpare_961

class Variation_710(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_292

class RuleVariation_700(RuleVariationContextFree):
    variation = Variation_710

class NonSpare_778(NonSpare):
    name = "CRT"
    title = ""
    rule = RuleVariation_700

class Item_208(Item):
    non_spare = NonSpare_778

class Variation_791(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_18

class RuleVariation_762(RuleVariationContextFree):
    variation = Variation_791

class NonSpare_1636(NonSpare):
    name = "SIM"
    title = ""
    rule = RuleVariation_762

class Item_858(Item):
    non_spare = NonSpare_1636

class Variation_888(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_149

class RuleVariation_859(RuleVariationContextFree):
    variation = Variation_888

class NonSpare_1838(NonSpare):
    name = "TST"
    title = ""
    rule = RuleVariation_859

class Item_1002(Item):
    non_spare = NonSpare_1838

class Variation_1315(Extended):
    items = [Item_882, Item_554, Item_368, Item_1073, Item_1037, Item_258, Item_629, None, Item_727, Item_866, Item_150, Item_343, Item_208, Item_858, Item_1002, None]

class RuleVariation_1245(RuleVariationContextFree):
    variation = Variation_1315

class NonSpare_83(NonSpare):
    name = "020"
    title = "Target Report Descriptor"
    rule = RuleVariation_1245

class NonSpare_304(NonSpare):
    name = "140"
    title = "Time of Day"
    rule = RuleVariation_356

class NonSpare_1078(NonSpare):
    name = "LAT"
    title = "Latitude"
    rule = RuleVariation_364

class Item_435(Item):
    non_spare = NonSpare_1078

class NonSpare_1106(NonSpare):
    name = "LON"
    title = "Longitude"
    rule = RuleVariation_363

class Item_461(Item):
    non_spare = NonSpare_1106

class Variation_1092(Group):
    bit_size = 64
    items_list = [Item_435, Item_461]
    items_dict = {"LAT": NonSpare_1078, "LON": NonSpare_1106}

class RuleVariation_1051(RuleVariationContextFree):
    variation = Variation_1092

class NonSpare_146(NonSpare):
    name = "041"
    title = "Position In WGS-84 Coordinates"
    rule = RuleVariation_1051

class Content_612(ContentQuantity):
    signedness = Signed
    lsb = 0.5
    unit = "m"

class RuleContent_612(RuleContentContextFree):
    variation = Content_612

class Variation_345(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_612

class RuleVariation_339(RuleVariationContextFree):
    variation = Variation_345

class NonSpare_1984(NonSpare):
    name = "X"
    title = "X-coordinate"
    rule = RuleVariation_339

class Item_1133(Item):
    non_spare = NonSpare_1984

class NonSpare_2037(NonSpare):
    name = "Y"
    title = "Y-coordinate"
    rule = RuleVariation_339

class Item_1183(Item):
    non_spare = NonSpare_2037

class Variation_1270(Group):
    bit_size = 48
    items_list = [Item_1133, Item_1183]
    items_dict = {"X": NonSpare_1984, "Y": NonSpare_2037}

class RuleVariation_1202(RuleVariationContextFree):
    variation = Variation_1270

class NonSpare_154(NonSpare):
    name = "042"
    title = "Position in Cartesian Coordinates"
    rule = RuleVariation_1202

class NonSpare_1822(NonSpare):
    name = "TRN"
    title = "Track Number"
    rule = RuleVariation_750

class Item_988(Item):
    non_spare = NonSpare_1822

class Variation_1000(Group):
    bit_size = 16
    items_list = [Item_3, Item_988]
    items_dict = {"TRN": NonSpare_1822}

class RuleVariation_969(RuleVariationContextFree):
    variation = Variation_1000

class NonSpare_336(NonSpare):
    name = "161"
    title = "Track Number"
    rule = RuleVariation_969

class Content_61(ContentTable):
    values = {0: "Confirmed track", 1: "Track in initiation phase"}

class RuleContent_61(RuleContentContextFree):
    variation = Content_61

class Variation_18(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_61

class RuleVariation_18(RuleVariationContextFree):
    variation = Variation_18

class NonSpare_698(NonSpare):
    name = "CNF"
    title = ""
    rule = RuleVariation_18

class Item_160(Item):
    non_spare = NonSpare_698

class Variation_546(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_380

class RuleVariation_536(RuleVariationContextFree):
    variation = Variation_546

class NonSpare_789(NonSpare):
    name = "CST"
    title = ""
    rule = RuleVariation_536

class Item_216(Item):
    non_spare = NonSpare_789

class Variation_652(Element):
    bit_offset8 = 3
    bit_size = 2
    rule = RuleContent_251

class RuleVariation_642(RuleVariationContextFree):
    variation = Variation_652

class NonSpare_665(NonSpare):
    name = "CDM"
    title = ""
    rule = RuleVariation_642

class Item_135(Item):
    non_spare = NonSpare_665

class Variation_801(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_100

class RuleVariation_772(RuleVariationContextFree):
    variation = Variation_801

class NonSpare_1148(NonSpare):
    name = "MAH"
    title = ""
    rule = RuleVariation_772

class Item_493(Item):
    non_spare = NonSpare_1148

class Variation_1285(Extended):
    items = [Item_160, Item_984, Item_216, Item_135, Item_493, Item_904, None, Item_347, Item_9, None]

class RuleVariation_1215(RuleVariationContextFree):
    variation = Variation_1285

class NonSpare_348(NonSpare):
    name = "170"
    title = "Track Status"
    rule = RuleVariation_1215

class Content_277(ContentTable):
    values = {0: "Mode-3/A code derived from the reply of the transponder", 1: "Mode-3/A code not extracted during the last update period"}

class RuleContent_277(RuleContentContextFree):
    variation = Content_277

class Variation_531(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_277

class RuleVariation_521(RuleVariationContextFree):
    variation = Variation_531

class NonSpare_1067(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_521

class Item_424(Item):
    non_spare = NonSpare_1067

class Variation_1230(Group):
    bit_size = 16
    items_list = [Item_1050, Item_335, Item_424, Item_16, Item_543]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "L": NonSpare_1067, "MODE3A": NonSpare_1232}

class RuleVariation_1164(RuleVariationContextFree):
    variation = Variation_1230

class NonSpare_200(NonSpare):
    name = "070"
    title = "Mode-3/A Code in Octal Representation"
    rule = RuleVariation_1164

class NonSpare_1932(NonSpare):
    name = "VX"
    title = ""
    rule = RuleVariation_264

class Item_1086(Item):
    non_spare = NonSpare_1932

class NonSpare_1937(NonSpare):
    name = "VY"
    title = ""
    rule = RuleVariation_264

class Item_1091(Item):
    non_spare = NonSpare_1937

class Variation_1233(Group):
    bit_size = 32
    items_list = [Item_1086, Item_1091]
    items_dict = {"VX": NonSpare_1932, "VY": NonSpare_1937}

class RuleVariation_1167(RuleVariationContextFree):
    variation = Variation_1233

class NonSpare_373(NonSpare):
    name = "202"
    title = "Calculated Track Velocity in Cartesian Coordinates"
    rule = RuleVariation_1167

class NonSpare_233(NonSpare):
    name = "090"
    title = "Flight Level in Binary Representation"
    rule = RuleVariation_1159

class NonSpare_1426(NonSpare):
    name = "QC1"
    title = "Quality Pulse C1"
    rule = RuleVariation_691

class Item_705(Item):
    non_spare = NonSpare_1426

class NonSpare_1405(NonSpare):
    name = "QA1"
    title = "Quality Pulse A1"
    rule = RuleVariation_793

class Item_684(Item):
    non_spare = NonSpare_1405

class NonSpare_1429(NonSpare):
    name = "QC2"
    title = "Quality Pulse C2"
    rule = RuleVariation_871

class Item_708(Item):
    non_spare = NonSpare_1429

class NonSpare_1409(NonSpare):
    name = "QA2"
    title = "Quality Pulse A2"
    rule = RuleVariation_913

class Item_688(Item):
    non_spare = NonSpare_1409

class NonSpare_1432(NonSpare):
    name = "QC4"
    title = "Quality Pulse C4"
    rule = RuleVariation_45

class Item_711(Item):
    non_spare = NonSpare_1432

class NonSpare_1413(NonSpare):
    name = "QA4"
    title = "Quality Pulse A4"
    rule = RuleVariation_411

class Item_692(Item):
    non_spare = NonSpare_1413

class NonSpare_1417(NonSpare):
    name = "QB1"
    title = "Quality Pulse B1"
    rule = RuleVariation_508

class Item_696(Item):
    non_spare = NonSpare_1417

class NonSpare_1435(NonSpare):
    name = "QD1"
    title = "Quality Pulse D1"
    rule = RuleVariation_608

class Item_714(Item):
    non_spare = NonSpare_1435

class NonSpare_1421(NonSpare):
    name = "QB2"
    title = "Quality Pulse B2"
    rule = RuleVariation_690

class Item_700(Item):
    non_spare = NonSpare_1421

class Variation_824(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_223

class RuleVariation_795(RuleVariationContextFree):
    variation = Variation_824

class NonSpare_1438(NonSpare):
    name = "QD2"
    title = "Quality Pulse D2"
    rule = RuleVariation_795

class Item_717(Item):
    non_spare = NonSpare_1438

class NonSpare_1424(NonSpare):
    name = "QB4"
    title = "Quality Pulse B4"
    rule = RuleVariation_870

class Item_703(Item):
    non_spare = NonSpare_1424

class NonSpare_1441(NonSpare):
    name = "QD4"
    title = "Quality Pulse D4"
    rule = RuleVariation_917

class Item_720(Item):
    non_spare = NonSpare_1441

class Variation_1223(Group):
    bit_size = 32
    items_list = [Item_1050, Item_335, Item_12, Item_544, Item_3, Item_705, Item_684, Item_708, Item_688, Item_711, Item_692, Item_696, Item_714, Item_700, Item_717, Item_703, Item_720]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "MODEC": NonSpare_1233, "QC1": NonSpare_1426, "QA1": NonSpare_1405, "QC2": NonSpare_1429, "QA2": NonSpare_1409, "QC4": NonSpare_1432, "QA4": NonSpare_1413, "QB1": NonSpare_1417, "QD1": NonSpare_1435, "QB2": NonSpare_1421, "QD2": NonSpare_1438, "QB4": NonSpare_1424, "QD4": NonSpare_1441}

class RuleVariation_1157(RuleVariationContextFree):
    variation = Variation_1223

class NonSpare_254(NonSpare):
    name = "100"
    title = "Mode C Code"
    rule = RuleVariation_1157

class NonSpare_393(NonSpare):
    name = "220"
    title = "Target Address"
    rule = RuleVariation_335

class Content_49(ContentTable):
    values = {0: "Callsign or registration not downlinked from transponder", 1: "Registration downlinked from transponder", 2: "Callsign downlinked from transponder", 3: "Not defined"}

class RuleContent_49(RuleContentContextFree):
    variation = Content_49

class Variation_94(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_49

class RuleVariation_94(RuleVariationContextFree):
    variation = Variation_94

class NonSpare_1700(NonSpare):
    name = "STI"
    title = ""
    rule = RuleVariation_94

class Item_907(Item):
    non_spare = NonSpare_1700

class NonSpare_687(NonSpare):
    name = "CHR"
    title = "Characters 1-8 (coded on 6 Bits Each) Defining Target Identification"
    rule = RuleVariation_376

class Item_153(Item):
    non_spare = NonSpare_687

class Variation_1179(Group):
    bit_size = 56
    items_list = [Item_907, Item_15, Item_153]
    items_dict = {"STI": NonSpare_1700, "CHR": NonSpare_687}

class RuleVariation_1126(RuleVariationContextFree):
    variation = Variation_1179

class NonSpare_407(NonSpare):
    name = "245"
    title = "Target Identification"
    rule = RuleVariation_1126

class NonSpare_269(NonSpare):
    name = "110"
    title = "Measured Height (Local Cartesian Coordinates)"
    rule = RuleVariation_275

class NonSpare_263(NonSpare):
    name = "105"
    title = "Geometric Height (WGS-84)"
    rule = RuleVariation_275

class NonSpare_608(NonSpare):
    name = "AX"
    title = ""
    rule = RuleVariation_196

class Item_94(Item):
    non_spare = NonSpare_608

class NonSpare_612(NonSpare):
    name = "AY"
    title = ""
    rule = RuleVariation_196

class Item_98(Item):
    non_spare = NonSpare_612

class Variation_1020(Group):
    bit_size = 16
    items_list = [Item_94, Item_98]
    items_dict = {"AX": NonSpare_608, "AY": NonSpare_612}

class RuleVariation_988(RuleVariationContextFree):
    variation = Variation_1020

class NonSpare_375(NonSpare):
    name = "210"
    title = "Calculated Acceleration"
    rule = RuleVariation_988

class NonSpare_1252(NonSpare):
    name = "MSG"
    title = ""
    rule = RuleVariation_467

class Item_561(Item):
    non_spare = NonSpare_1252

class Variation_1188(Group):
    bit_size = 8
    items_list = [Item_979, Item_561]
    items_dict = {"TRB": NonSpare_1809, "MSG": NonSpare_1252}

class RuleVariation_1131(RuleVariationContextFree):
    variation = Variation_1188

class NonSpare_440(NonSpare):
    name = "310"
    title = "Pre-programmed Message"
    rule = RuleVariation_1131

class Content_722(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = ""

class RuleContent_721(RuleContentContextFree):
    variation = Content_722

class Variation_311(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_721

class RuleVariation_305(RuleVariationContextFree):
    variation = Variation_311

class NonSpare_1971(NonSpare):
    name = "X"
    title = "DOP (X-Component)"
    rule = RuleVariation_305

class Item_1120(Item):
    non_spare = NonSpare_1971

class NonSpare_2023(NonSpare):
    name = "Y"
    title = "DOP (Y-Component)"
    rule = RuleVariation_305

class Item_1169(Item):
    non_spare = NonSpare_2023

class NonSpare_2004(NonSpare):
    name = "XY"
    title = "DOP (Correlation XY)"
    rule = RuleVariation_305

class Item_1150(Item):
    non_spare = NonSpare_2004

class Variation_1256(Group):
    bit_size = 48
    items_list = [Item_1120, Item_1169, Item_1150]
    items_dict = {"X": NonSpare_1971, "Y": NonSpare_2023, "XY": NonSpare_2004}

class RuleVariation_1190(RuleVariationContextFree):
    variation = Variation_1256

class NonSpare_839(NonSpare):
    name = "DOP"
    title = "DOP of Position"
    rule = RuleVariation_1190

class Variation_315(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_725

class RuleVariation_309(RuleVariationContextFree):
    variation = Variation_315

class NonSpare_1975(NonSpare):
    name = "X"
    title = "SDP (X-Component)"
    rule = RuleVariation_309

class Item_1124(Item):
    non_spare = NonSpare_1975

class NonSpare_2027(NonSpare):
    name = "Y"
    title = "SDP (Y-Component)"
    rule = RuleVariation_309

class Item_1173(Item):
    non_spare = NonSpare_2027

class NonSpare_2005(NonSpare):
    name = "XY"
    title = "SDP (Correlation XY)"
    rule = RuleVariation_305

class Item_1151(Item):
    non_spare = NonSpare_2005

class Variation_1260(Group):
    bit_size = 48
    items_list = [Item_1124, Item_1173, Item_1151]
    items_dict = {"X": NonSpare_1975, "Y": NonSpare_2027, "XY": NonSpare_2005}

class RuleVariation_1194(RuleVariationContextFree):
    variation = Variation_1260

class NonSpare_1597(NonSpare):
    name = "SDP"
    title = "Standard Deviation of Position"
    rule = RuleVariation_1194

class NonSpare_1592(NonSpare):
    name = "SDH"
    title = "Standard Deviation of Geometric Height (WGS 84)"
    rule = RuleVariation_296

class Variation_1400(Compound):
    items_list = [NonSpare_839, NonSpare_1597, NonSpare_1592]
    items_dict = {"DOP": NonSpare_839, "SDP": NonSpare_1597, "SDH": NonSpare_1592}

class RuleVariation_1330(RuleVariationContextFree):
    variation = Variation_1400

class NonSpare_479(NonSpare):
    name = "500"
    title = "Position Accuracy"
    rule = RuleVariation_1330

class Content_446(ContentTable):
    values = {0: "TU1/RU1 has NOT contributed to the target detection", 1: "TU1/RU1 has contributed to the target detection"}

class RuleContent_446(RuleContentContextFree):
    variation = Content_446

class Variation_76(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_446

class RuleVariation_76(RuleVariationContextFree):
    variation = Variation_76

class NonSpare_632(NonSpare):
    name = "BIT1"
    title = "TU1/RU1 Contribution"
    rule = RuleVariation_76

class Item_114(Item):
    non_spare = NonSpare_632

class Content_447(ContentTable):
    values = {0: "TU2/RU2 has NOT contributed to the target detection", 1: "TU2/RU2 has contributed to the target detection"}

class RuleContent_447(RuleContentContextFree):
    variation = Content_447

class Variation_440(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_447

class RuleVariation_430(RuleVariationContextFree):
    variation = Variation_440

class NonSpare_633(NonSpare):
    name = "BIT2"
    title = "TU2/RU2 Contribution"
    rule = RuleVariation_430

class Item_115(Item):
    non_spare = NonSpare_633

class Content_448(ContentTable):
    values = {0: "TU3/RU3 has NOT contributed to the target detection", 1: "TU3/RU3 has contributed to the target detection"}

class RuleContent_448(RuleContentContextFree):
    variation = Content_448

class Variation_554(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_448

class RuleVariation_544(RuleVariationContextFree):
    variation = Variation_554

class NonSpare_634(NonSpare):
    name = "BIT3"
    title = "TU3/RU3 Contribution"
    rule = RuleVariation_544

class Item_116(Item):
    non_spare = NonSpare_634

class Content_449(ContentTable):
    values = {0: "TU4/RU4 has NOT contributed to the target detection", 1: "TU4/RU4 has contributed to the target detection"}

class RuleContent_449(RuleContentContextFree):
    variation = Content_449

class Variation_636(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_449

class RuleVariation_626(RuleVariationContextFree):
    variation = Variation_636

class NonSpare_635(NonSpare):
    name = "BIT4"
    title = "TU4/RU4 Contribution"
    rule = RuleVariation_626

class Item_117(Item):
    non_spare = NonSpare_635

class Content_450(ContentTable):
    values = {0: "TU5/RU5 has NOT contributed to the target detection", 1: "TU5/RU5 has contributed to the target detection"}

class RuleContent_450(RuleContentContextFree):
    variation = Content_450

class Variation_725(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_450

class RuleVariation_715(RuleVariationContextFree):
    variation = Variation_725

class NonSpare_636(NonSpare):
    name = "BIT5"
    title = "TU5/RU5 Contribution"
    rule = RuleVariation_715

class Item_118(Item):
    non_spare = NonSpare_636

class Content_451(ContentTable):
    values = {0: "TU6/RU6 has NOT contributed to the target detection", 1: "TU6/RU6 has contributed to the target detection"}

class RuleContent_451(RuleContentContextFree):
    variation = Content_451

class Variation_840(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_451

class RuleVariation_811(RuleVariationContextFree):
    variation = Variation_840

class NonSpare_637(NonSpare):
    name = "BIT6"
    title = "TU6/RU6 Contribution"
    rule = RuleVariation_811

class Item_119(Item):
    non_spare = NonSpare_637

class Content_452(ContentTable):
    values = {0: "TU7/RU7 has NOT contributed to the target detection", 1: "TU7/RU7 has contributed to the target detection"}

class RuleContent_452(RuleContentContextFree):
    variation = Content_452

class Variation_921(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_452

class RuleVariation_892(RuleVariationContextFree):
    variation = Variation_921

class NonSpare_638(NonSpare):
    name = "BIT7"
    title = "TU7/RU7 Contribution"
    rule = RuleVariation_892

class Item_120(Item):
    non_spare = NonSpare_638

class Content_453(ContentTable):
    values = {0: "TU8/RU8 has NOT contributed to the target detection", 1: "TU8/RU8 has contributed to the target detection"}

class RuleContent_453(RuleContentContextFree):
    variation = Content_453

class Variation_955(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_453

class RuleVariation_926(RuleVariationContextFree):
    variation = Variation_955

class NonSpare_639(NonSpare):
    name = "BIT8"
    title = "TU8/RU8 Contribution"
    rule = RuleVariation_926

class Item_121(Item):
    non_spare = NonSpare_639

class Variation_1024(Group):
    bit_size = 8
    items_list = [Item_114, Item_115, Item_116, Item_117, Item_118, Item_119, Item_120, Item_121]
    items_dict = {"BIT1": NonSpare_632, "BIT2": NonSpare_633, "BIT3": NonSpare_634, "BIT4": NonSpare_635, "BIT5": NonSpare_636, "BIT6": NonSpare_637, "BIT7": NonSpare_638, "BIT8": NonSpare_639}

class Variation_1339(Repetitive):
    rep_bytes = 1
    variation = Variation_1024

class RuleVariation_1269(RuleVariationContextFree):
    variation = Variation_1339

class NonSpare_459(NonSpare):
    name = "400"
    title = "Contributing Devices"
    rule = RuleVariation_1269

class NonSpare_1159(NonSpare):
    name = "MBDATA"
    title = "56-bit Message Conveying Mode S Comm B Message Data"
    rule = RuleVariation_377

class Item_500(Item):
    non_spare = NonSpare_1159

class NonSpare_628(NonSpare):
    name = "BDS1"
    title = "Comm B Data Buffer Store 1 Address"
    rule = RuleVariation_127

class Item_111(Item):
    non_spare = NonSpare_628

class NonSpare_630(NonSpare):
    name = "BDS2"
    title = "Comm B Data Buffer Store 2 Address"
    rule = RuleVariation_741

class Item_113(Item):
    non_spare = NonSpare_630

class Variation_1107(Group):
    bit_size = 64
    items_list = [Item_500, Item_111, Item_113]
    items_dict = {"MBDATA": NonSpare_1159, "BDS1": NonSpare_628, "BDS2": NonSpare_630}

class Variation_1348(Repetitive):
    rep_bytes = 1
    variation = Variation_1107

class RuleVariation_1278(RuleVariationContextFree):
    variation = Variation_1348

class NonSpare_413(NonSpare):
    name = "250"
    title = "Mode S MB Data"
    rule = RuleVariation_1278

class Content_308(ContentTable):
    values = {0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 6: "Not assigned", 7: "Information not yet extracted"}

class RuleContent_308(RuleContentContextFree):
    variation = Content_308

class Variation_659(Element):
    bit_offset8 = 3
    bit_size = 3
    rule = RuleContent_308

class RuleVariation_649(RuleVariationContextFree):
    variation = Variation_659

class NonSpare_1684(NonSpare):
    name = "STAT"
    title = "Flight Status"
    rule = RuleVariation_649

class Item_892(Item):
    non_spare = NonSpare_1684

class NonSpare_1255(NonSpare):
    name = "MSSC"
    title = "Mode-S Specific Service Capability"
    rule = RuleVariation_53

class Item_564(Item):
    non_spare = NonSpare_1255

class NonSpare_623(NonSpare):
    name = "B1B"
    title = "BDS 1,0 Bits 37/40"
    rule = RuleVariation_741

class Item_106(Item):
    non_spare = NonSpare_623

class Variation_1040(Group):
    bit_size = 16
    items_list = [Item_182, Item_892, Item_27, Item_564, Item_77, Item_54, Item_104, Item_106]
    items_dict = {"COM": NonSpare_735, "STAT": NonSpare_1684, "MSSC": NonSpare_1255, "ARC": NonSpare_581, "AIC": NonSpare_549, "B1A": NonSpare_621, "B1B": NonSpare_623}

class RuleVariation_1001(RuleVariationContextFree):
    variation = Variation_1040

class NonSpare_397(NonSpare):
    name = "230"
    title = "Communications/ACAS Capability and Flight Status"
    rule = RuleVariation_1001

class NonSpare_415(NonSpare):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    rule = RuleVariation_377

class Content_379(ContentTable):
    values = {0: "Not defined; never used", 1: "Multipath Reply (Reflection)", 3: "Split plot", 10: "Phantom SSR plot", 11: "Non-Matching Mode-3/A Code", 12: "Mode C code / Mode S altitude code abnormal value compared to the track", 15: "Transponder anomaly detected", 16: "Duplicated or Illegal Mode S Aircraft Address", 17: "Mode S error correction applied", 18: "Undecodable Mode C code / Mode S altitude code"}

class RuleContent_379(RuleContentContextFree):
    variation = Content_379

class Variation_151(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_379

class Variation_1380(Repetitive):
    rep_bytes = None
    variation = Variation_151

class RuleVariation_1310(RuleVariationContextFree):
    variation = Variation_1380

class NonSpare_112(NonSpare):
    name = "030"
    title = "Warning/Error Conditions"
    rule = RuleVariation_1310

class Content_272(ContentTable):
    values = {0: "Mode-1 code derived from the reply of the transponder", 1: "Smoothed Mode-1 code as provided by a local tracker"}

class RuleContent_272(RuleContentContextFree):
    variation = Content_272

class Variation_526(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_272

class RuleVariation_516(RuleVariationContextFree):
    variation = Variation_526

class NonSpare_1062(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_516

class Item_419(Item):
    non_spare = NonSpare_1062

class Variation_669(Element):
    bit_offset8 = 3
    bit_size = 5
    rule = RuleContent_0

class RuleVariation_659(RuleVariationContextFree):
    variation = Variation_669

class NonSpare_1224(NonSpare):
    name = "MODE1"
    title = "Mode-1 Code in Octal Representation"
    rule = RuleVariation_659

class Item_535(Item):
    non_spare = NonSpare_1224

class Variation_1227(Group):
    bit_size = 8
    items_list = [Item_1050, Item_335, Item_419, Item_535]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "L": NonSpare_1062, "MODE1": NonSpare_1224}

class RuleVariation_1161(RuleVariationContextFree):
    variation = Variation_1227

class NonSpare_176(NonSpare):
    name = "055"
    title = "Mode-1 Code in Octal Representation"
    rule = RuleVariation_1161

class Content_274(ContentTable):
    values = {0: "Mode-2 code derived from the reply of the transponder", 1: "Smoothed Mode-2 code as provided by a local tracker n"}

class RuleContent_274(RuleContentContextFree):
    variation = Content_274

class Variation_528(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_274

class RuleVariation_518(RuleVariationContextFree):
    variation = Variation_528

class NonSpare_1064(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_518

class Item_421(Item):
    non_spare = NonSpare_1064

class NonSpare_1227(NonSpare):
    name = "MODE2"
    title = "Mode-2 Reply in Octal Representation"
    rule = RuleVariation_751

class Item_538(Item):
    non_spare = NonSpare_1227

class Variation_1228(Group):
    bit_size = 16
    items_list = [Item_1050, Item_335, Item_421, Item_16, Item_538]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "L": NonSpare_1064, "MODE2": NonSpare_1227}

class RuleVariation_1162(RuleVariationContextFree):
    variation = Variation_1228

class NonSpare_164(NonSpare):
    name = "050"
    title = "Mode-2 Code in Octal Representation"
    rule = RuleVariation_1162

class Record_35(Record):
    items_list = [NonSpare_41, NonSpare_83, NonSpare_304, NonSpare_146, NonSpare_154, NonSpare_336, NonSpare_348, NonSpare_200, NonSpare_373, NonSpare_233, NonSpare_254, NonSpare_393, NonSpare_407, NonSpare_269, NonSpare_263, NonSpare_375, NonSpare_439, NonSpare_440, NonSpare_479, NonSpare_459, NonSpare_413, NonSpare_397, NonSpare_415, NonSpare_112, NonSpare_176, NonSpare_164, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_41, "020": NonSpare_83, "140": NonSpare_304, "041": NonSpare_146, "042": NonSpare_154, "161": NonSpare_336, "170": NonSpare_348, "070": NonSpare_200, "202": NonSpare_373, "090": NonSpare_233, "100": NonSpare_254, "220": NonSpare_393, "245": NonSpare_407, "110": NonSpare_269, "105": NonSpare_263, "210": NonSpare_375, "300": NonSpare_439, "310": NonSpare_440, "500": NonSpare_479, "400": NonSpare_459, "250": NonSpare_413, "230": NonSpare_397, "260": NonSpare_415, "030": NonSpare_112, "055": NonSpare_176, "050": NonSpare_164, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_31(UapSingle):
    record = Record_35

class Asterix_18(AstCat):
    category = 20
    edition = (1, 9)
    uap = Uap_31

class Content_369(ContentTable):
    values = {0: "Not Coasted", 1: "Coasted"}

class RuleContent_369(RuleContentContextFree):
    variation = Content_369

class Variation_543(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_369

class RuleVariation_533(RuleVariationContextFree):
    variation = Variation_543

class NonSpare_788(NonSpare):
    name = "CST"
    title = ""
    rule = RuleVariation_533

class Item_215(Item):
    non_spare = NonSpare_788

class Variation_1284(Extended):
    items = [Item_160, Item_984, Item_215, Item_135, Item_493, Item_904, None, Item_347, Item_9, None]

class RuleVariation_1214(RuleVariationContextFree):
    variation = Variation_1284

class NonSpare_347(NonSpare):
    name = "170"
    title = "Track Status"
    rule = RuleVariation_1214

class NonSpare_391(NonSpare):
    name = "220"
    title = "Target Address"
    rule = RuleVariation_335

class NonSpare_408(NonSpare):
    name = "250"
    title = "BDS Register Data"
    rule = RuleVariation_1278

class Record_34(Record):
    items_list = [NonSpare_41, NonSpare_83, NonSpare_304, NonSpare_146, NonSpare_154, NonSpare_336, NonSpare_347, NonSpare_200, NonSpare_373, NonSpare_233, NonSpare_254, NonSpare_391, NonSpare_407, NonSpare_269, NonSpare_263, NonSpare_375, NonSpare_439, NonSpare_440, NonSpare_479, NonSpare_459, NonSpare_408, NonSpare_397, NonSpare_415, NonSpare_112, NonSpare_176, NonSpare_164, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_41, "020": NonSpare_83, "140": NonSpare_304, "041": NonSpare_146, "042": NonSpare_154, "161": NonSpare_336, "170": NonSpare_347, "070": NonSpare_200, "202": NonSpare_373, "090": NonSpare_233, "100": NonSpare_254, "220": NonSpare_391, "245": NonSpare_407, "110": NonSpare_269, "105": NonSpare_263, "210": NonSpare_375, "300": NonSpare_439, "310": NonSpare_440, "500": NonSpare_479, "400": NonSpare_459, "250": NonSpare_408, "230": NonSpare_397, "260": NonSpare_415, "030": NonSpare_112, "055": NonSpare_176, "050": NonSpare_164, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_30(UapSingle):
    record = Record_34

class Asterix_19(AstCat):
    category = 20
    edition = (1, 10)
    uap = Uap_30

class NonSpare_28(NonSpare):
    name = "010"
    title = "Data Source Identification"
    rule = RuleVariation_1104

class Variation_58(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_338

class RuleVariation_58(RuleVariationContextFree):
    variation = Variation_58

class NonSpare_824(NonSpare):
    name = "DCR"
    title = "Differential Correction"
    rule = RuleVariation_58

class Item_246(Item):
    non_spare = NonSpare_824

class Content_215(ContentTable):
    values = {0: "Ground Bit not set", 1: "Ground Bit set"}

class RuleContent_215(RuleContentContextFree):
    variation = Content_215

class Variation_419(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_215

class RuleVariation_409(RuleVariationContextFree):
    variation = Variation_419

class NonSpare_964(NonSpare):
    name = "GBS"
    title = "Ground Bit Setting"
    rule = RuleVariation_409

class Item_346(Item):
    non_spare = NonSpare_964

class Variation_490(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_18

class RuleVariation_480(RuleVariationContextFree):
    variation = Variation_490

class NonSpare_1637(NonSpare):
    name = "SIM"
    title = "Simulated Target"
    rule = RuleVariation_480

class Item_859(Item):
    non_spare = NonSpare_1637

class Variation_608(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_149

class RuleVariation_598(RuleVariationContextFree):
    variation = Variation_608

class NonSpare_1839(NonSpare):
    name = "TST"
    title = "Test Target"
    rule = RuleVariation_598

class Item_1003(Item):
    non_spare = NonSpare_1839

class Variation_721(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_419

class RuleVariation_711(RuleVariationContextFree):
    variation = Variation_721

class NonSpare_1456(NonSpare):
    name = "RAB"
    title = "Report Type"
    rule = RuleVariation_711

class Item_732(Item):
    non_spare = NonSpare_1456

class Content_204(ContentTable):
    values = {0: "Equipment capable to provide Selected Altitude", 1: "Equipment not capable to provide Selected Altitude"}

class RuleContent_204(RuleContentContextFree):
    variation = Content_204

class Variation_819(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_204

class RuleVariation_790(RuleVariationContextFree):
    variation = Variation_819

class NonSpare_1559(NonSpare):
    name = "SAA"
    title = "Selected Altitude Available"
    rule = RuleVariation_790

class Item_809(Item):
    non_spare = NonSpare_1559

class Variation_869(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_12

class RuleVariation_840(RuleVariationContextFree):
    variation = Variation_869

class NonSpare_1650(NonSpare):
    name = "SPI"
    title = "Special Position Identification"
    rule = RuleVariation_840

class Item_870(Item):
    non_spare = NonSpare_1650

class Content_364(ContentTable):
    values = {0: "Non unique address", 1: "24-Bit ICAO address", 2: "Surface vehicle address", 3: "Anonymous address", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use"}

class RuleContent_364(RuleContentContextFree):
    variation = Content_364

class Variation_125(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_364

class RuleVariation_125(RuleVariationContextFree):
    variation = Variation_125

class NonSpare_597(NonSpare):
    name = "ATP"
    title = "Address Type"
    rule = RuleVariation_125

class Item_86(Item):
    non_spare = NonSpare_597

class Content_510(ContentTable):
    values = {0: "Unknown", 1: "25 ft", 2: "100 ft"}

class RuleContent_510(RuleContentContextFree):
    variation = Content_510

class Variation_657(Element):
    bit_offset8 = 3
    bit_size = 2
    rule = RuleContent_510

class RuleVariation_647(RuleVariationContextFree):
    variation = Variation_657

class NonSpare_583(NonSpare):
    name = "ARC"
    title = "Altitude Reporting Capability"
    rule = RuleVariation_647

class Item_79(Item):
    non_spare = NonSpare_583

class Variation_1046(Group):
    bit_size = 16
    items_list = [Item_246, Item_346, Item_859, Item_1003, Item_732, Item_809, Item_870, Item_29, Item_86, Item_79, Item_25]
    items_dict = {"DCR": NonSpare_824, "GBS": NonSpare_964, "SIM": NonSpare_1637, "TST": NonSpare_1839, "RAB": NonSpare_1456, "SAA": NonSpare_1559, "SPI": NonSpare_1650, "ATP": NonSpare_597, "ARC": NonSpare_583}

class RuleVariation_1007(RuleVariationContextFree):
    variation = Variation_1046

class NonSpare_136(NonSpare):
    name = "040"
    title = "Target Report Descriptor"
    rule = RuleVariation_1007

class NonSpare_104(NonSpare):
    name = "030"
    title = "Time of Day"
    rule = RuleVariation_356

class Content_666(ContentQuantity):
    signedness = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

class RuleContent_666(RuleContentContextFree):
    variation = Content_666

class Variation_352(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_666

class RuleVariation_346(RuleVariationContextFree):
    variation = Variation_352

class NonSpare_1076(NonSpare):
    name = "LAT"
    title = "Latitude"
    rule = RuleVariation_346

class Item_433(Item):
    non_spare = NonSpare_1076

class Content_664(ContentQuantity):
    signedness = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

class RuleContent_664(RuleContentContextFree):
    variation = Content_664

class Variation_350(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_664

class RuleVariation_344(RuleVariationContextFree):
    variation = Variation_350

class NonSpare_1103(NonSpare):
    name = "LON"
    title = "Longitude"
    rule = RuleVariation_344

class Item_458(Item):
    non_spare = NonSpare_1103

class Variation_1090(Group):
    bit_size = 48
    items_list = [Item_433, Item_458]
    items_dict = {"LAT": NonSpare_1076, "LON": NonSpare_1103}

class RuleVariation_1049(RuleVariationContextFree):
    variation = Variation_1090

class NonSpare_286(NonSpare):
    name = "130"
    title = "Position in WGS-84 Co-ordinates"
    rule = RuleVariation_1049

class NonSpare_221(NonSpare):
    name = "080"
    title = "Target Address"
    rule = RuleVariation_335

class Content_657(ContentQuantity):
    signedness = Signed
    lsb = 6.25
    unit = "ft"

class RuleContent_657(RuleContentContextFree):
    variation = Content_657

class Variation_282(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_657

class RuleVariation_276(RuleVariationContextFree):
    variation = Variation_282

class NonSpare_299(NonSpare):
    name = "140"
    title = "Geometric Altitude"
    rule = RuleVariation_276

class Content_511(ContentTable):
    values = {0: "Unknown", 1: "ACAS not operational", 2: "ACAS operartional", 3: "Invalid"}

class RuleContent_511(RuleContentContextFree):
    variation = Content_511

class Variation_109(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_511

class RuleVariation_109(RuleVariationContextFree):
    variation = Variation_109

class NonSpare_516(NonSpare):
    name = "AC"
    title = "ACAS Capabilities"
    rule = RuleVariation_109

class Item_36(Item):
    non_spare = NonSpare_516

class Content_525(ContentTable):
    values = {0: "Unknown", 1: "Multiple Navigation not operational", 2: "Multiple Navigation operartional", 3: "Invalid"}

class RuleContent_525(RuleContentContextFree):
    variation = Content_525

class Variation_572(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_525

class RuleVariation_562(RuleVariationContextFree):
    variation = Variation_572

class NonSpare_1218(NonSpare):
    name = "MN"
    title = "Multiple Navigation Aids"
    rule = RuleVariation_562

class Item_529(Item):
    non_spare = NonSpare_1218

class Content_519(ContentTable):
    values = {0: "Unknown", 1: "Differencial Correction", 2: "NO Differencial Correction", 3: "Invalid"}

class RuleContent_519(RuleContentContextFree):
    variation = Content_519

class Variation_741(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_519

class RuleVariation_731(RuleVariationContextFree):
    variation = Variation_741

class NonSpare_819(NonSpare):
    name = "DC"
    title = "Differencial Correction"
    rule = RuleVariation_731

class Item_241(Item):
    non_spare = NonSpare_819

class Item_28(Spare):
    bit_offset8 = 6
    bit_size = 6

class Content_594(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = ""

class RuleContent_594(RuleContentContextFree):
    variation = Content_594

class Variation_777(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_594

class RuleVariation_748(RuleVariationContextFree):
    variation = Variation_777

class NonSpare_1339(NonSpare):
    name = "PA"
    title = "Position Accuracy"
    rule = RuleVariation_748

class Item_641(Item):
    non_spare = NonSpare_1339

class Variation_1010(Group):
    bit_size = 16
    items_list = [Item_36, Item_529, Item_241, Item_28, Item_641]
    items_dict = {"AC": NonSpare_516, "MN": NonSpare_1218, "DC": NonSpare_819, "PA": NonSpare_1339}

class RuleVariation_978(RuleVariationContextFree):
    variation = Variation_1010

class NonSpare_230(NonSpare):
    name = "090"
    title = "Figure of Merit"
    rule = RuleVariation_978

class Content_516(ContentTable):
    values = {0: "Unknown", 1: "Aircraft equiped with CDTI"}

class RuleContent_516(RuleContentContextFree):
    variation = Content_516

class Variation_645(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_516

class RuleVariation_635(RuleVariationContextFree):
    variation = Variation_645

class NonSpare_855(NonSpare):
    name = "DTI"
    title = "Cockpit Display of Traffic Information"
    rule = RuleVariation_635

class Item_269(Item):
    non_spare = NonSpare_855

class Content_383(ContentTable):
    values = {0: "Not used", 1: "Used"}

class RuleContent_383(RuleContentContextFree):
    variation = Content_383

class Variation_717(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_383

class RuleVariation_707(RuleVariationContextFree):
    variation = Variation_717

class NonSpare_1185(NonSpare):
    name = "MDS"
    title = "Mode-S Extended Squitter"
    rule = RuleVariation_707

class Item_510(Item):
    non_spare = NonSpare_1185

class Variation_837(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_383

class RuleVariation_808(RuleVariationContextFree):
    variation = Variation_837

class NonSpare_1880(NonSpare):
    name = "UAT"
    title = "UAT"
    rule = RuleVariation_808

class Item_1039(Item):
    non_spare = NonSpare_1880

class Variation_911(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_383

class RuleVariation_882(RuleVariationContextFree):
    variation = Variation_911

class NonSpare_1915(NonSpare):
    name = "VDL"
    title = "VDL Mode 4"
    rule = RuleVariation_882

class Item_1072(Item):
    non_spare = NonSpare_1915

class Variation_951(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_383

class RuleVariation_922(RuleVariationContextFree):
    variation = Variation_951

class NonSpare_1327(NonSpare):
    name = "OTR"
    title = "Other Technology"
    rule = RuleVariation_922

class Item_630(Item):
    non_spare = NonSpare_1327

class Variation_977(Group):
    bit_size = 8
    items_list = [Item_2, Item_269, Item_510, Item_1039, Item_1072, Item_630]
    items_dict = {"DTI": NonSpare_855, "MDS": NonSpare_1185, "UAT": NonSpare_1880, "VDL": NonSpare_1915, "OTR": NonSpare_1327}

class RuleVariation_948(RuleVariationContextFree):
    variation = Variation_977

class NonSpare_379(NonSpare):
    name = "210"
    title = "Link Technology Indicator"
    rule = RuleVariation_948

class Content_621(ContentQuantity):
    signedness = Signed
    lsb = 1.0e-2
    unit = "°"

class RuleContent_621(RuleContentContextFree):
    variation = Content_621

class Variation_260(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_621

class RuleVariation_254(RuleVariationContextFree):
    variation = Variation_260

class NonSpare_399(NonSpare):
    name = "230"
    title = "Roll Angle"
    rule = RuleVariation_254

class Content_625(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "FL"

class RuleContent_625(RuleContentContextFree):
    variation = Content_625

class Variation_263(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_625

class RuleVariation_257(RuleVariationContextFree):
    variation = Variation_263

class NonSpare_314(NonSpare):
    name = "145"
    title = "Flight Level"
    rule = RuleVariation_257

class Content_21(ContentTable):
    values = {0: "Air Speed = IAS, LSB (Bit-1) = 2 -14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001"}

class RuleContent_21(RuleContentContextFree):
    variation = Content_21

class Variation_6(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_21

class RuleVariation_6(RuleVariationContextFree):
    variation = Variation_6

class NonSpare_1050(NonSpare):
    name = "IM"
    title = ""
    rule = RuleVariation_6

class Item_409(Item):
    non_spare = NonSpare_1050

class Content_721(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-3
    unit = "Mach"

class RuleContent_773(RuleContentDependent):
    depends_on = [["150", "IM"]]
    default_variation = Content_0
    cases = [
        ([0], Content_751),
        ([1], Content_721),
    ]

class Variation_487(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_773

class RuleVariation_477(RuleVariationContextFree):
    variation = Variation_487

class NonSpare_587(NonSpare):
    name = "AS"
    title = "Air Speed (IAS or Mach)"
    rule = RuleVariation_477

class Item_81(Item):
    non_spare = NonSpare_587

class Variation_1084(Group):
    bit_size = 16
    items_list = [Item_409, Item_81]
    items_dict = {"IM": NonSpare_1050, "AS": NonSpare_587}

class RuleVariation_1043(RuleVariationContextFree):
    variation = Variation_1084

class NonSpare_320(NonSpare):
    name = "150"
    title = "Air Speed"
    rule = RuleVariation_1043

class Content_686(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "kt"

class RuleContent_686(RuleContentContextFree):
    variation = Content_686

class Variation_292(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_686

class RuleVariation_286(RuleVariationContextFree):
    variation = Variation_292

class NonSpare_323(NonSpare):
    name = "151"
    title = "True Airspeed"
    rule = RuleVariation_286

class NonSpare_325(NonSpare):
    name = "152"
    title = "Magnetic Heading"
    rule = RuleVariation_329

class Content_659(ContentQuantity):
    signedness = Signed
    lsb = 6.25
    unit = "ft/min"

class RuleContent_659(RuleContentContextFree):
    variation = Content_659

class Variation_284(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_659

class RuleVariation_278(RuleVariationContextFree):
    variation = Variation_284

class NonSpare_327(NonSpare):
    name = "155"
    title = "Barometric Vertical Rate"
    rule = RuleVariation_278

class NonSpare_329(NonSpare):
    name = "157"
    title = "Geometric Vertical Rate"
    rule = RuleVariation_278

class Content_654(ContentQuantity):
    signedness = Signed
    lsb = 6.103515625e-5
    unit = "NM/s"

class RuleContent_654(RuleContentContextFree):
    variation = Content_654

class Variation_280(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_654

class RuleVariation_274(RuleVariationContextFree):
    variation = Variation_280

class NonSpare_972(NonSpare):
    name = "GS"
    title = "Ground Speed in Two's Complement Form Referenced to WGS84"
    rule = RuleVariation_274

class Item_351(Item):
    non_spare = NonSpare_972

class NonSpare_1724(NonSpare):
    name = "TA"
    title = "Track Angle"
    rule = RuleVariation_329

class Item_924(Item):
    non_spare = NonSpare_1724

class Variation_1078(Group):
    bit_size = 32
    items_list = [Item_351, Item_924]
    items_dict = {"GS": NonSpare_972, "TA": NonSpare_1724}

class RuleVariation_1037(RuleVariationContextFree):
    variation = Variation_1078

class NonSpare_332(NonSpare):
    name = "160"
    title = "Ground Vector"
    rule = RuleVariation_1037

class Content_375(ContentTable):
    values = {0: "Not available", 1: "Left", 2: "Right", 3: "Straight"}

class RuleContent_375(RuleContentContextFree):
    variation = Content_375

class Variation_101(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_375

class RuleVariation_101(RuleVariationContextFree):
    variation = Variation_101

class NonSpare_1765(NonSpare):
    name = "TI"
    title = "Turn Indicator"
    rule = RuleVariation_101

class Item_951(Item):
    non_spare = NonSpare_1765

class Item_14(Spare):
    bit_offset8 = 2
    bit_size = 5

class Content_638(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "°/s"

class RuleContent_638(RuleContentContextFree):
    variation = Content_638

class Variation_154(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_638

class RuleVariation_149(RuleVariationContextFree):
    variation = Variation_154

class NonSpare_1514(NonSpare):
    name = "ROT"
    title = "Rate of Turn"
    rule = RuleVariation_149

class Item_784(Item):
    non_spare = NonSpare_1514

class Variation_1318(Extended):
    items = [Item_951, Item_14, None, Item_784, None]

class RuleVariation_1248(RuleVariationContextFree):
    variation = Variation_1318

class NonSpare_341(NonSpare):
    name = "165"
    title = "Rate Of Turn"
    rule = RuleVariation_1248

class NonSpare_345(NonSpare):
    name = "170"
    title = "Target Identification"
    rule = RuleVariation_376

class NonSpare_248(NonSpare):
    name = "095"
    title = "Velocity Accuracy"
    rule = RuleVariation_154

class Content_750(ContentQuantity):
    signedness = Unsigned
    lsb = 3.90625e-3
    unit = "s"

class RuleContent_749(RuleContentContextFree):
    variation = Content_750

class Variation_232(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_749

class RuleVariation_226(RuleVariationContextFree):
    variation = Variation_232

class NonSpare_118(NonSpare):
    name = "032"
    title = "Time of Day Accuracy"
    rule = RuleVariation_226

class Content_342(ContentTable):
    values = {0: "No emergency / not reported", 1: "General emergency", 2: "Lifeguard / medical", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference"}

class RuleContent_342(RuleContentContextFree):
    variation = Content_342

class Variation_165(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_342

class RuleVariation_160(RuleVariationContextFree):
    variation = Variation_165

class NonSpare_366(NonSpare):
    name = "200"
    title = "Target Status"
    rule = RuleVariation_160

class Content_555(ContentTable):
    values = {1: "Light aircraft <= 7000 kg", 2: "Reserved", 3: "7000 kg < Medium aircraft < 136000 kg", 4: "Reserved", 5: "136000 kg <= Heavy aircraft", 6: "Highly manoeuvrable (5g acceleration capability) and high speed (>400 knots cruise)", 7: "Reserved", 8: "Reserved", 9: "Reserved", 10: "Rotocraft", 11: "Glider / sailplane", 12: "Lighter-than-air", 13: "Unmanned aerial vehicle", 14: "Space / transatmospheric vehicle", 15: "Ultralight / handglider / paraglider", 16: "Parachutist / skydiver", 17: "Reserved", 18: "Reserved", 19: "Reserved", 20: "Surface emergency vehicle", 21: "Surface service vehicle", 22: "Fixed ground or tethered obstruction", 23: "Reserved", 24: "Reserved"}

class RuleContent_555(RuleContentContextFree):
    variation = Content_555

class Variation_176(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_555

class RuleVariation_170(RuleVariationContextFree):
    variation = Variation_176

class NonSpare_78(NonSpare):
    name = "020"
    title = "Emitter Category"
    rule = RuleVariation_170

class Content_687(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "kt"

class RuleContent_687(RuleContentContextFree):
    variation = Content_687

class Variation_293(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_687

class RuleVariation_287(RuleVariationContextFree):
    variation = Variation_293

class NonSpare_1949(NonSpare):
    name = "WS"
    title = "Wind Speed"
    rule = RuleVariation_287

class Content_698(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "°"

class RuleContent_698(RuleContentContextFree):
    variation = Content_698

class Variation_300(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_698

class RuleVariation_294(RuleVariationContextFree):
    variation = Variation_300

class NonSpare_1942(NonSpare):
    name = "WD"
    title = "Wind Direction"
    rule = RuleVariation_294

class Content_639(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "°C"

class RuleContent_639(RuleContentContextFree):
    variation = Content_639

class Variation_272(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_639

class RuleVariation_266(RuleVariationContextFree):
    variation = Variation_272

class NonSpare_1780(NonSpare):
    name = "TMP"
    title = "Temperature"
    rule = RuleVariation_266

class Content_589(ContentInteger):
    signedness = Unsigned

class RuleContent_589(RuleContentContextFree):
    variation = Content_589

class Variation_196(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_589

class RuleVariation_190(RuleVariationContextFree):
    variation = Variation_196

class NonSpare_1811(NonSpare):
    name = "TRB"
    title = "Turbulence"
    rule = RuleVariation_190

class Variation_1435(Compound):
    items_list = [NonSpare_1949, NonSpare_1942, NonSpare_1780, NonSpare_1811]
    items_dict = {"WS": NonSpare_1949, "WD": NonSpare_1942, "TMP": NonSpare_1780, "TRB": NonSpare_1811}

class RuleVariation_1365(RuleVariationContextFree):
    variation = Variation_1435

class NonSpare_390(NonSpare):
    name = "220"
    title = "Met Information"
    rule = RuleVariation_1365

class Content_358(ContentTable):
    values = {0: "No source information provided", 1: "Source Information provided"}

class RuleContent_358(RuleContentContextFree):
    variation = Content_358

class Variation_61(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_358

class RuleVariation_61(RuleVariationContextFree):
    variation = Variation_61

class NonSpare_1572(NonSpare):
    name = "SAS"
    title = "Source Availability"
    rule = RuleVariation_61

class Item_816(Item):
    non_spare = NonSpare_1572

class Content_514(ContentTable):
    values = {0: "Unknown", 1: "Aircraft Altitude (Holding Altitude)", 2: "MCP/FCU Selected Altitude", 3: "FMS Selected Altitude"}

class RuleContent_514(RuleContentContextFree):
    variation = Content_514

class Variation_463(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_514

class RuleVariation_453(RuleVariationContextFree):
    variation = Variation_463

class NonSpare_1658(NonSpare):
    name = "SRC"
    title = "Source"
    rule = RuleVariation_453

class Item_877(Item):
    non_spare = NonSpare_1658

class Content_607(ContentQuantity):
    signedness = Signed
    lsb = 25.0
    unit = "ft"

class RuleContent_607(RuleContentContextFree):
    variation = Content_607

class Variation_671(Element):
    bit_offset8 = 3
    bit_size = 13
    rule = RuleContent_607

class RuleVariation_661(RuleVariationContextFree):
    variation = Variation_671

class NonSpare_554(NonSpare):
    name = "ALT"
    title = "Altitude"
    rule = RuleVariation_661

class Item_59(Item):
    non_spare = NonSpare_554

class Variation_1162(Group):
    bit_size = 16
    items_list = [Item_816, Item_877, Item_59]
    items_dict = {"SAS": NonSpare_1572, "SRC": NonSpare_1658, "ALT": NonSpare_554}

class RuleVariation_1109(RuleVariationContextFree):
    variation = Variation_1162

class NonSpare_316(NonSpare):
    name = "146"
    title = "Intermediate State Selected Altitude"
    rule = RuleVariation_1109

class Content_371(ContentTable):
    values = {0: "Not active", 1: "Active"}

class RuleContent_371(RuleContentContextFree):
    variation = Content_371

class Variation_64(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_371

class RuleVariation_64(RuleVariationContextFree):
    variation = Variation_64

class NonSpare_1261(NonSpare):
    name = "MV"
    title = "Manage Vertical Mode"
    rule = RuleVariation_64

class Item_569(Item):
    non_spare = NonSpare_1261

class Variation_432(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_371

class RuleVariation_422(RuleVariationContextFree):
    variation = Variation_432

class NonSpare_545(NonSpare):
    name = "AH"
    title = "Altitude Hold Mode"
    rule = RuleVariation_422

class Item_52(Item):
    non_spare = NonSpare_545

class Variation_544(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_371

class RuleVariation_534(RuleVariationContextFree):
    variation = Variation_544

class NonSpare_558(NonSpare):
    name = "AM"
    title = "Approach Mode"
    rule = RuleVariation_534

class Item_63(Item):
    non_spare = NonSpare_558

class Variation_1110(Group):
    bit_size = 16
    items_list = [Item_569, Item_52, Item_63, Item_59]
    items_dict = {"MV": NonSpare_1261, "AH": NonSpare_545, "AM": NonSpare_558, "ALT": NonSpare_554}

class RuleVariation_1065(RuleVariationContextFree):
    variation = Variation_1110

class NonSpare_318(NonSpare):
    name = "148"
    title = "Final State Selected Altitude"
    rule = RuleVariation_1065

class Content_494(ContentTable):
    values = {0: "Trajectory Intent Data is available for this aircraft", 1: "Trajectory Intent Data is not available for this aircraft"}

class RuleContent_494(RuleContentContextFree):
    variation = Content_494

class Variation_87(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_494

class RuleVariation_87(RuleVariationContextFree):
    variation = Variation_87

class NonSpare_1268(NonSpare):
    name = "NAV"
    title = ""
    rule = RuleVariation_87

class Item_575(Item):
    non_spare = NonSpare_1268

class Content_495(ContentTable):
    values = {0: "Trajectory Intent Data is valid", 1: "Trajectory Intent Data is not valid"}

class RuleContent_495(RuleContentContextFree):
    variation = Content_495

class Variation_447(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_495

class RuleVariation_437(RuleVariationContextFree):
    variation = Variation_447

class NonSpare_1308(NonSpare):
    name = "NVB"
    title = ""
    rule = RuleVariation_437

class Item_612(Item):
    non_spare = NonSpare_1308

class Variation_1304(Extended):
    items = [Item_575, Item_612, Item_14, None]

class RuleVariation_1234(RuleVariationContextFree):
    variation = Variation_1304

class NonSpare_1777(NonSpare):
    name = "TIS"
    title = "Trajectory Intent Status"
    rule = RuleVariation_1234

class Content_443(ContentTable):
    values = {0: "TCP number available", 1: "TCP number not available"}

class RuleContent_443(RuleContentContextFree):
    variation = Content_443

class Variation_75(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_443

class RuleVariation_75(RuleVariationContextFree):
    variation = Variation_75

class NonSpare_1743(NonSpare):
    name = "TCA"
    title = ""
    rule = RuleVariation_75

class Item_931(Item):
    non_spare = NonSpare_1743

class Content_442(ContentTable):
    values = {0: "TCP compliance", 1: "TCP non-compliance"}

class RuleContent_442(RuleContentContextFree):
    variation = Content_442

class Variation_439(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_442

class RuleVariation_429(RuleVariationContextFree):
    variation = Variation_439

class NonSpare_1279(NonSpare):
    name = "NC"
    title = ""
    rule = RuleVariation_429

class Item_584(Item):
    non_spare = NonSpare_1279

class Variation_579(Element):
    bit_offset8 = 2
    bit_size = 6
    rule = RuleContent_0

class RuleVariation_569(RuleVariationContextFree):
    variation = Variation_579

class NonSpare_1754(NonSpare):
    name = "TCPN"
    title = ""
    rule = RuleVariation_569

class Item_941(Item):
    non_spare = NonSpare_1754

class Content_604(ContentQuantity):
    signedness = Signed
    lsb = 10.0
    unit = "ft"

class RuleContent_604(RuleContentContextFree):
    variation = Content_604

class Variation_252(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_604

class RuleVariation_246(RuleVariationContextFree):
    variation = Variation_252

class NonSpare_555(NonSpare):
    name = "ALT"
    title = "Altitude in Two's Complement Form"
    rule = RuleVariation_246

class Item_60(Item):
    non_spare = NonSpare_555

class NonSpare_1075(NonSpare):
    name = "LAT"
    title = "In WGS.84 in Two's Complement"
    rule = RuleVariation_346

class Item_432(Item):
    non_spare = NonSpare_1075

class NonSpare_1102(NonSpare):
    name = "LON"
    title = "In WGS.84 in Two's Complement"
    rule = RuleVariation_344

class Item_457(Item):
    non_spare = NonSpare_1102

class Content_522(ContentTable):
    values = {0: "Unknown", 1: "Fly by waypoint (LT)", 2: "Fly over waypoint (LT)", 3: "Hold pattern (LT)", 4: "Procedure hold (LT)", 5: "Procedure turn (LT)", 6: "RF leg (LT)", 7: "Top of climb (VT)", 8: "Top of descent (VT)", 9: "Start of level (VT)", 10: "Cross-over altitude (VT)", 11: "Transition altitude (VT)"}

class RuleContent_522(RuleContentContextFree):
    variation = Content_522

class Variation_130(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_522

class RuleVariation_130(RuleVariationContextFree):
    variation = Variation_130

class NonSpare_1397(NonSpare):
    name = "PT"
    title = "Point Type"
    rule = RuleVariation_130

class Item_679(Item):
    non_spare = NonSpare_1397

class Content_286(ContentTable):
    values = {0: "N/A", 1: "Turn right", 2: "Turn left", 3: "No turn"}

class RuleContent_286(RuleContentContextFree):
    variation = Content_286

class Variation_737(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_286

class RuleVariation_727(RuleVariationContextFree):
    variation = Variation_737

class NonSpare_1756(NonSpare):
    name = "TD"
    title = ""
    rule = RuleVariation_727

class Item_943(Item):
    non_spare = NonSpare_1756

class Content_445(ContentTable):
    values = {0: "TTR not available", 1: "TTR available"}

class RuleContent_445(RuleContentContextFree):
    variation = Content_445

class Variation_920(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_445

class RuleVariation_891(RuleVariationContextFree):
    variation = Variation_920

class NonSpare_1805(NonSpare):
    name = "TRA"
    title = ""
    rule = RuleVariation_891

class Item_975(Item):
    non_spare = NonSpare_1805

class Content_444(ContentTable):
    values = {0: "TOV available", 1: "TOV not available"}

class RuleContent_444(RuleContentContextFree):
    variation = Content_444

class Variation_954(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_444

class RuleVariation_925(RuleVariationContextFree):
    variation = Variation_954

class NonSpare_1784(NonSpare):
    name = "TOA"
    title = ""
    rule = RuleVariation_925

class Item_960(Item):
    non_spare = NonSpare_1784

class Variation_357(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_695

class RuleVariation_351(RuleVariationContextFree):
    variation = Variation_357

class NonSpare_1795(NonSpare):
    name = "TOV"
    title = "Time Over Point"
    rule = RuleVariation_351

class Item_965(Item):
    non_spare = NonSpare_1795

class Content_713(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-2
    unit = "NM"

class RuleContent_713(RuleContentContextFree):
    variation = Content_713

class Variation_305(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_713

class RuleVariation_299(RuleVariationContextFree):
    variation = Variation_305

class NonSpare_1852(NonSpare):
    name = "TTR"
    title = "TCP Turn Radius"
    rule = RuleVariation_299

class Item_1014(Item):
    non_spare = NonSpare_1852

class Variation_1181(Group):
    bit_size = 120
    items_list = [Item_931, Item_584, Item_941, Item_60, Item_432, Item_457, Item_679, Item_943, Item_975, Item_960, Item_965, Item_1014]
    items_dict = {"TCA": NonSpare_1743, "NC": NonSpare_1279, "TCPN": NonSpare_1754, "ALT": NonSpare_555, "LAT": NonSpare_1075, "LON": NonSpare_1102, "PT": NonSpare_1397, "TD": NonSpare_1756, "TRA": NonSpare_1805, "TOA": NonSpare_1784, "TOV": NonSpare_1795, "TTR": NonSpare_1852}

class Variation_1359(Repetitive):
    rep_bytes = 1
    variation = Variation_1181

class RuleVariation_1289(RuleVariationContextFree):
    variation = Variation_1359

class NonSpare_1772(NonSpare):
    name = "TID"
    title = "Trajectory Intent Data"
    rule = RuleVariation_1289

class Variation_1431(Compound):
    items_list = [NonSpare_1777, NonSpare_1772]
    items_dict = {"TIS": NonSpare_1777, "TID": NonSpare_1772}

class RuleVariation_1361(RuleVariationContextFree):
    variation = Variation_1431

class NonSpare_273(NonSpare):
    name = "110"
    title = "Trajectory Intent"
    rule = RuleVariation_1361

class Record_0(Record):
    items_list = [NonSpare_28, NonSpare_136, NonSpare_104, NonSpare_286, NonSpare_221, NonSpare_299, NonSpare_230, NonSpare_379, NonSpare_399, NonSpare_314, NonSpare_320, NonSpare_323, NonSpare_325, NonSpare_327, NonSpare_329, NonSpare_332, NonSpare_341, NonSpare_345, NonSpare_248, NonSpare_118, NonSpare_366, NonSpare_78, NonSpare_390, NonSpare_316, NonSpare_318, NonSpare_273, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_136, "030": NonSpare_104, "130": NonSpare_286, "080": NonSpare_221, "140": NonSpare_299, "090": NonSpare_230, "210": NonSpare_379, "230": NonSpare_399, "145": NonSpare_314, "150": NonSpare_320, "151": NonSpare_323, "152": NonSpare_325, "155": NonSpare_327, "157": NonSpare_329, "160": NonSpare_332, "165": NonSpare_341, "170": NonSpare_345, "095": NonSpare_248, "032": NonSpare_118, "200": NonSpare_366, "020": NonSpare_78, "220": NonSpare_390, "146": NonSpare_316, "148": NonSpare_318, "110": NonSpare_273, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_0(UapSingle):
    record = Record_0

class Asterix_20(AstCat):
    category = 21
    edition = (0, 23)
    uap = Uap_0

class NonSpare_288(NonSpare):
    name = "130"
    title = "Position in WGS-84 Co-ordinates"
    rule = RuleVariation_1051

class Record_1(Record):
    items_list = [NonSpare_28, NonSpare_136, NonSpare_104, NonSpare_288, NonSpare_221, NonSpare_299, NonSpare_230, NonSpare_379, NonSpare_399, NonSpare_314, NonSpare_320, NonSpare_323, NonSpare_325, NonSpare_327, NonSpare_329, NonSpare_332, NonSpare_341, NonSpare_345, NonSpare_248, NonSpare_118, NonSpare_366, NonSpare_78, NonSpare_390, NonSpare_316, NonSpare_318, NonSpare_273, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_136, "030": NonSpare_104, "130": NonSpare_288, "080": NonSpare_221, "140": NonSpare_299, "090": NonSpare_230, "210": NonSpare_379, "230": NonSpare_399, "145": NonSpare_314, "150": NonSpare_320, "151": NonSpare_323, "152": NonSpare_325, "155": NonSpare_327, "157": NonSpare_329, "160": NonSpare_332, "165": NonSpare_341, "170": NonSpare_345, "095": NonSpare_248, "032": NonSpare_118, "200": NonSpare_366, "020": NonSpare_78, "220": NonSpare_390, "146": NonSpare_316, "148": NonSpare_318, "110": NonSpare_273, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_1(UapSingle):
    record = Record_1

class Asterix_21(AstCat):
    category = 21
    edition = (0, 24)
    uap = Uap_1

class NonSpare_300(NonSpare):
    name = "140"
    title = "Geometric Altitude"
    rule = RuleVariation_276

class Record_3(Record):
    items_list = [NonSpare_28, NonSpare_136, NonSpare_104, NonSpare_288, NonSpare_221, NonSpare_300, NonSpare_230, NonSpare_379, NonSpare_399, NonSpare_314, NonSpare_320, NonSpare_323, NonSpare_325, NonSpare_327, NonSpare_329, NonSpare_332, NonSpare_341, NonSpare_345, NonSpare_248, NonSpare_118, NonSpare_366, NonSpare_78, NonSpare_390, NonSpare_316, NonSpare_318, NonSpare_273, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_136, "030": NonSpare_104, "130": NonSpare_288, "080": NonSpare_221, "140": NonSpare_300, "090": NonSpare_230, "210": NonSpare_379, "230": NonSpare_399, "145": NonSpare_314, "150": NonSpare_320, "151": NonSpare_323, "152": NonSpare_325, "155": NonSpare_327, "157": NonSpare_329, "160": NonSpare_332, "165": NonSpare_341, "170": NonSpare_345, "095": NonSpare_248, "032": NonSpare_118, "200": NonSpare_366, "020": NonSpare_78, "220": NonSpare_390, "146": NonSpare_316, "148": NonSpare_318, "110": NonSpare_273, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_3(UapSingle):
    record = Record_3

class Asterix_22(AstCat):
    category = 21
    edition = (0, 25)
    uap = Uap_3

class Content_275(ContentTable):
    values = {0: "Mode-3/A code derived during last update", 1: "Mode-3/A code not extracted during the last update"}

class RuleContent_275(RuleContentContextFree):
    variation = Content_275

class Variation_529(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_275

class RuleVariation_519(RuleVariationContextFree):
    variation = Variation_529

class NonSpare_1065(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_519

class Item_422(Item):
    non_spare = NonSpare_1065

class Variation_1217(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_422, Item_16, Item_543]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "L": NonSpare_1065, "MODE3A": NonSpare_1232}

class RuleVariation_1151(RuleVariationContextFree):
    variation = Variation_1217

class NonSpare_196(NonSpare):
    name = "070"
    title = "Mode 3/A Code in Octal Representation"
    rule = RuleVariation_1151

class NonSpare_294(NonSpare):
    name = "131"
    title = "Signal Amplitude"
    rule = RuleVariation_154

class Record_2(Record):
    items_list = [NonSpare_28, NonSpare_136, NonSpare_104, NonSpare_288, NonSpare_221, NonSpare_300, NonSpare_230, NonSpare_379, NonSpare_399, NonSpare_314, NonSpare_320, NonSpare_323, NonSpare_325, NonSpare_327, NonSpare_329, NonSpare_332, NonSpare_341, NonSpare_345, NonSpare_248, NonSpare_118, NonSpare_366, NonSpare_78, NonSpare_390, NonSpare_316, NonSpare_318, NonSpare_273, NonSpare_196, NonSpare_294, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_136, "030": NonSpare_104, "130": NonSpare_288, "080": NonSpare_221, "140": NonSpare_300, "090": NonSpare_230, "210": NonSpare_379, "230": NonSpare_399, "145": NonSpare_314, "150": NonSpare_320, "151": NonSpare_323, "152": NonSpare_325, "155": NonSpare_327, "157": NonSpare_329, "160": NonSpare_332, "165": NonSpare_341, "170": NonSpare_345, "095": NonSpare_248, "032": NonSpare_118, "200": NonSpare_366, "020": NonSpare_78, "220": NonSpare_390, "146": NonSpare_316, "148": NonSpare_318, "110": NonSpare_273, "070": NonSpare_196, "131": NonSpare_294, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_2(UapSingle):
    record = Record_2

class Asterix_23(AstCat):
    category = 21
    edition = (0, 26)
    uap = Uap_2

class Content_707(ContentQuantity):
    signedness = Unsigned
    lsb = 0.1
    unit = "hPa"

class RuleContent_707(RuleContentContextFree):
    variation = Content_707

class Variation_783(Element):
    bit_offset8 = 4
    bit_size = 12
    rule = RuleContent_707

class RuleVariation_754(RuleVariationContextFree):
    variation = Variation_783

class NonSpare_643(NonSpare):
    name = "BPS"
    title = "Barometric Pressure Setting"
    rule = RuleVariation_754

class Item_125(Item):
    non_spare = NonSpare_643

class Variation_985(Group):
    bit_size = 16
    items_list = [Item_3, Item_125]
    items_dict = {"BPS": NonSpare_643}

class RuleVariation_956(RuleVariationContextFree):
    variation = Variation_985

class NonSpare_645(NonSpare):
    name = "BPS"
    title = "Barometric Pressure Setting"
    rule = RuleVariation_956

class Content_502(ContentTable):
    values = {0: "True North", 1: "Magnetic North"}

class RuleContent_502(RuleContentContextFree):
    variation = Content_502

class Variation_730(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_502

class RuleVariation_720(RuleVariationContextFree):
    variation = Variation_730

class NonSpare_994(NonSpare):
    name = "HDR"
    title = "Horizontal Reference Direction"
    rule = RuleVariation_720

class Item_367(Item):
    non_spare = NonSpare_994

class Content_71(ContentTable):
    values = {0: "Data is either unavailable or invalid", 1: "Data is available and valid"}

class RuleContent_71(RuleContentContextFree):
    variation = Content_71

class Variation_798(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_71

class RuleVariation_769(RuleVariationContextFree):
    variation = Variation_798

class NonSpare_1690(NonSpare):
    name = "STAT"
    title = "Selected Heading Status"
    rule = RuleVariation_769

class Item_898(Item):
    non_spare = NonSpare_1690

class Content_758(ContentQuantity):
    signedness = Unsigned
    lsb = 0.703125
    unit = "°"

class RuleContent_757(RuleContentContextFree):
    variation = Content_758

class Variation_936(Element):
    bit_offset8 = 6
    bit_size = 10
    rule = RuleContent_757

class RuleVariation_907(RuleVariationContextFree):
    variation = Variation_936

class NonSpare_1613(NonSpare):
    name = "SH"
    title = "Selected Heading"
    rule = RuleVariation_907

class Item_838(Item):
    non_spare = NonSpare_1613

class Variation_988(Group):
    bit_size = 16
    items_list = [Item_3, Item_367, Item_898, Item_838]
    items_dict = {"HDR": NonSpare_994, "STAT": NonSpare_1690, "SH": NonSpare_1613}

class RuleVariation_958(RuleVariationContextFree):
    variation = Variation_988

class NonSpare_1614(NonSpare):
    name = "SH"
    title = "Selected Heading"
    rule = RuleVariation_958

class Content_41(ContentTable):
    values = {0: "Autopilot not engaged", 1: "Autopilot engaged"}

class RuleContent_41(RuleContentContextFree):
    variation = Content_41

class Variation_12(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_41

class RuleVariation_12(RuleVariationContextFree):
    variation = Variation_12

class NonSpare_569(NonSpare):
    name = "AP"
    title = "Autopilot"
    rule = RuleVariation_12

class Item_72(Item):
    non_spare = NonSpare_569

class Content_536(ContentTable):
    values = {0: "Vertical Navigation not active", 1: "Vertical Navigation active"}

class RuleContent_536(RuleContentContextFree):
    variation = Content_536

class Variation_451(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_536

class RuleVariation_441(RuleVariationContextFree):
    variation = Variation_451

class NonSpare_1920(NonSpare):
    name = "VN"
    title = "Vertical Navigation"
    rule = RuleVariation_441

class Item_1077(Item):
    non_spare = NonSpare_1920

class Content_31(ContentTable):
    values = {0: "Altitude Hold not engaged", 1: "Altitude Hold engaged"}

class RuleContent_31(RuleContentContextFree):
    variation = Content_31

class Variation_492(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_31

class RuleVariation_482(RuleVariationContextFree):
    variation = Variation_492

class NonSpare_544(NonSpare):
    name = "AH"
    title = "Altitude Hold"
    rule = RuleVariation_482

class Item_51(Item):
    non_spare = NonSpare_544

class Content_34(ContentTable):
    values = {0: "Approach Mode not active", 1: "Approach Mode active"}

class RuleContent_34(RuleContentContextFree):
    variation = Content_34

class Variation_593(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_34

class RuleVariation_583(RuleVariationContextFree):
    variation = Variation_593

class NonSpare_561(NonSpare):
    name = "AM"
    title = "Approach Mode"
    rule = RuleVariation_583

class Item_66(Item):
    non_spare = NonSpare_561

class Variation_1017(Group):
    bit_size = 8
    items_list = [Item_72, Item_1077, Item_51, Item_66, Item_22]
    items_dict = {"AP": NonSpare_569, "VN": NonSpare_1920, "AH": NonSpare_544, "AM": NonSpare_561}

class RuleVariation_985(RuleVariationContextFree):
    variation = Variation_1017

class NonSpare_1269(NonSpare):
    name = "NAV"
    title = "Navigation Mode"
    rule = RuleVariation_985

class NonSpare_956(NonSpare):
    name = "GAO"
    title = "GPS Antenna Offset"
    rule = RuleVariation_154

class Content_26(ContentTable):
    values = {0: "Aircraft has not stopped", 1: "Aircraft has stopped"}

class RuleContent_26(RuleContentContextFree):
    variation = Content_26

class Variation_8(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_26

class RuleVariation_8(RuleVariationContextFree):
    variation = Variation_8

class NonSpare_1703(NonSpare):
    name = "STP"
    title = ""
    rule = RuleVariation_8

class Item_910(Item):
    non_spare = NonSpare_1703

class Content_218(ContentTable):
    values = {0: "Heading/Ground Track data is not valid", 1: "Heading/Ground Track data is valid"}

class RuleContent_218(RuleContentContextFree):
    variation = Content_218

class Variation_420(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_218

class RuleVariation_410(RuleVariationContextFree):
    variation = Variation_420

class NonSpare_1012(NonSpare):
    name = "HTS"
    title = ""
    rule = RuleVariation_410

class Item_380(Item):
    non_spare = NonSpare_1012

class Content_217(ContentTable):
    values = {0: "Heading data provided", 1: "Ground Track provided"}

class RuleContent_217(RuleContentContextFree):
    variation = Content_217

class Variation_517(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_217

class RuleVariation_507(RuleVariationContextFree):
    variation = Variation_517

class NonSpare_1013(NonSpare):
    name = "HTT"
    title = ""
    rule = RuleVariation_507

class Item_381(Item):
    non_spare = NonSpare_1013

class Variation_644(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_502

class RuleVariation_634(RuleVariationContextFree):
    variation = Variation_644

class NonSpare_1011(NonSpare):
    name = "HRD"
    title = ""
    rule = RuleVariation_634

class Item_379(Item):
    non_spare = NonSpare_1011

class Content_733(ContentQuantity):
    signedness = Unsigned
    lsb = 0.125
    unit = "kt"

class RuleContent_732(RuleContentContextFree):
    variation = Content_733

class Variation_778(Element):
    bit_offset8 = 4
    bit_size = 11
    rule = RuleContent_732

class RuleVariation_749(RuleVariationContextFree):
    variation = Variation_778

class NonSpare_976(NonSpare):
    name = "GSS"
    title = "Ground Speed"
    rule = RuleVariation_749

class Item_354(Item):
    non_spare = NonSpare_976

class Content_757(ContentQuantity):
    signedness = Unsigned
    lsb = 2.8125
    unit = "°"

class RuleContent_756(RuleContentContextFree):
    variation = Content_757

class Variation_157(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_756

class RuleVariation_152(RuleVariationContextFree):
    variation = Variation_157

class NonSpare_999(NonSpare):
    name = "HGT"
    title = "Heading/Ground Track Information"
    rule = RuleVariation_152

class Item_370(Item):
    non_spare = NonSpare_999

class Variation_1316(Extended):
    items = [Item_910, Item_380, Item_381, Item_379, Item_354, None, Item_370, None]

class RuleVariation_1246(RuleVariationContextFree):
    variation = Variation_1316

class NonSpare_1612(NonSpare):
    name = "SGV"
    title = "Surface Ground Vector"
    rule = RuleVariation_1246

class Content_456(ContentTable):
    values = {0: "Target is not 1090 ES IN capable", 1: "Target is 1090 ES IN capable"}

class RuleContent_456(RuleContentContextFree):
    variation = Content_456

class Variation_77(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_456

class RuleVariation_77(RuleVariationContextFree):
    variation = Variation_77

class NonSpare_896(NonSpare):
    name = "ES"
    title = ""
    rule = RuleVariation_77

class Item_299(Item):
    non_spare = NonSpare_896

class Content_457(ContentTable):
    values = {0: "Target is not UAT IN capable", 1: "Target is UAT IN capable"}

class RuleContent_457(RuleContentContextFree):
    variation = Content_457

class Variation_441(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_457

class RuleVariation_431(RuleVariationContextFree):
    variation = Variation_441

class NonSpare_1876(NonSpare):
    name = "UAT"
    title = ""
    rule = RuleVariation_431

class Item_1036(Item):
    non_spare = NonSpare_1876

class Variation_1289(Extended):
    items = [Item_299, Item_1036, Item_14, None]

class RuleVariation_1219(RuleVariationContextFree):
    variation = Variation_1289

class NonSpare_1677(NonSpare):
    name = "STA"
    title = "Aircraft Status"
    rule = RuleVariation_1219

class NonSpare_1783(NonSpare):
    name = "TNH"
    title = "True North Heading"
    rule = RuleVariation_329

class Content_299(ContentTable):
    values = {0: "No Mode 5 interrogation", 1: "Mode 5 interrogation"}

class RuleContent_299(RuleContentContextFree):
    variation = Content_299

class Variation_54(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_299

class RuleVariation_54(RuleVariationContextFree):
    variation = Variation_54

class NonSpare_1142(NonSpare):
    name = "M5"
    title = ""
    rule = RuleVariation_54

class Item_490(Item):
    non_spare = NonSpare_1142

class Content_315(ContentTable):
    values = {0: "No authenticated Mode 5 ID reply/report", 1: "Authenticated Mode 5 ID reply/report"}

class RuleContent_315(RuleContentContextFree):
    variation = Content_315

class Variation_431(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_315

class RuleVariation_421(RuleVariationContextFree):
    variation = Variation_431

class NonSpare_1040(NonSpare):
    name = "ID"
    title = ""
    rule = RuleVariation_421

class Item_403(Item):
    non_spare = NonSpare_1040

class Content_312(ContentTable):
    values = {0: "No authenticated Mode 5 Data reply or Report", 1: "Authenticated Mode 5 Data reply or Report (i.e any valid Mode 5 reply type other than ID)"}

class RuleContent_312(RuleContentContextFree):
    variation = Content_312

class Variation_537(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_312

class RuleVariation_527(RuleVariationContextFree):
    variation = Variation_537

class NonSpare_805(NonSpare):
    name = "DA"
    title = ""
    rule = RuleVariation_527

class Item_229(Item):
    non_spare = NonSpare_805

class Content_262(ContentTable):
    values = {0: "Mode 1 code not present or not from Mode 5 reply/report", 1: "Mode 1 code from Mode 5 reply/report"}

class RuleContent_262(RuleContentContextFree):
    variation = Content_262

class Variation_621(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_262

class RuleVariation_611(RuleVariationContextFree):
    variation = Variation_621

class NonSpare_1130(NonSpare):
    name = "M1"
    title = ""
    rule = RuleVariation_611

class Item_483(Item):
    non_spare = NonSpare_1130

class Content_264(ContentTable):
    values = {0: "Mode 2 code not present or not from Mode 5 reply/report", 1: "Mode 2 code from Mode 5 reply/report"}

class RuleContent_264(RuleContentContextFree):
    variation = Content_264

class Variation_707(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_264

class RuleVariation_697(RuleVariationContextFree):
    variation = Variation_707

class NonSpare_1132(NonSpare):
    name = "M2"
    title = ""
    rule = RuleVariation_697

class Item_485(Item):
    non_spare = NonSpare_1132

class Content_266(ContentTable):
    values = {0: "Mode 3 code not present or not from Mode 5 reply/report", 1: "Mode 3 code from Mode 5 reply/report"}

class RuleContent_266(RuleContentContextFree):
    variation = Content_266

class Variation_831(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_266

class RuleVariation_802(RuleVariationContextFree):
    variation = Variation_831

class NonSpare_1136(NonSpare):
    name = "M3"
    title = ""
    rule = RuleVariation_802

class Item_488(Item):
    non_spare = NonSpare_1136

class Content_208(ContentTable):
    values = {0: "Flightlevel not present or not from Mode 5 reply/report", 1: "Flightlevel from Mode 5 reply/report"}

class RuleContent_208(RuleContentContextFree):
    variation = Content_208

class Variation_896(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_208

class RuleVariation_867(RuleVariationContextFree):
    variation = Variation_896

class NonSpare_1161(NonSpare):
    name = "MC"
    title = ""
    rule = RuleVariation_867

class Item_502(Item):
    non_spare = NonSpare_1161

class Content_406(ContentTable):
    values = {0: "Position not from Mode 5 report (ADS-B report)", 1: "Position from Mode 5 report"}

class RuleContent_406(RuleContentContextFree):
    variation = Content_406

class Variation_953(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_406

class RuleVariation_924(RuleVariationContextFree):
    variation = Variation_953

class NonSpare_1358(NonSpare):
    name = "PO"
    title = ""
    rule = RuleVariation_924

class Item_652(Item):
    non_spare = NonSpare_1358

class Variation_1104(Group):
    bit_size = 8
    items_list = [Item_490, Item_403, Item_229, Item_483, Item_485, Item_488, Item_502, Item_652]
    items_dict = {"M5": NonSpare_1142, "ID": NonSpare_1040, "DA": NonSpare_805, "M1": NonSpare_1130, "M2": NonSpare_1132, "M3": NonSpare_1136, "MC": NonSpare_1161, "PO": NonSpare_1358}

class RuleVariation_1062(RuleVariationContextFree):
    variation = Variation_1104

class NonSpare_1717(NonSpare):
    name = "SUM"
    title = "Mode 5 Summary"
    rule = RuleVariation_1062

class Variation_581(Element):
    bit_offset8 = 2
    bit_size = 14
    rule = RuleContent_0

class RuleVariation_571(RuleVariationContextFree):
    variation = Variation_581

class NonSpare_1351(NonSpare):
    name = "PIN"
    title = "PIN Code"
    rule = RuleVariation_571

class Item_650(Item):
    non_spare = NonSpare_1351

class Variation_862(Element):
    bit_offset8 = 5
    bit_size = 11
    rule = RuleContent_0

class RuleVariation_833(RuleVariationContextFree):
    variation = Variation_862

class NonSpare_1284(NonSpare):
    name = "NO"
    title = "National Origin Code"
    rule = RuleVariation_833

class Item_589(Item):
    non_spare = NonSpare_1284

class Variation_973(Group):
    bit_size = 32
    items_list = [Item_1, Item_650, Item_4, Item_589]
    items_dict = {"PIN": NonSpare_1351, "NO": NonSpare_1284}

class RuleVariation_944(RuleVariationContextFree):
    variation = Variation_973

class NonSpare_1357(NonSpare):
    name = "PNO"
    title = "Mode 5 PIN / National Origin"
    rule = RuleVariation_944

class Content_260(ContentTable):
    values = {0: "Mode 1 code as derived from the report of the transponder", 1: "Smoothed Mode 1 code as provided by a local tracker"}

class RuleContent_260(RuleContentContextFree):
    variation = Content_260

class Variation_523(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_260

class RuleVariation_513(RuleVariationContextFree):
    variation = Variation_523

class NonSpare_1059(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_513

class Item_416(Item):
    non_spare = NonSpare_1059

class NonSpare_868(NonSpare):
    name = "EM1"
    title = "Extended Mode 1 Code in Octal Representation"
    rule = RuleVariation_751

class Item_278(Item):
    non_spare = NonSpare_868

class Variation_1209(Group):
    bit_size = 16
    items_list = [Item_1049, Item_7, Item_416, Item_16, Item_278]
    items_dict = {"V": NonSpare_1890, "L": NonSpare_1059, "EM1": NonSpare_868}

class RuleVariation_1143(RuleVariationContextFree):
    variation = Variation_1209

class NonSpare_871(NonSpare):
    name = "EM1"
    title = "Extended Mode 1 Code in Octal Representation"
    rule = RuleVariation_1143

class Content_539(ContentTable):
    values = {0: "X-Pulse not present", 1: "X-pulse present"}

class RuleContent_539(RuleContentContextFree):
    variation = Content_539

class Variation_561(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_539

class RuleVariation_551(RuleVariationContextFree):
    variation = Variation_561

class NonSpare_2002(NonSpare):
    name = "XP"
    title = "X-pulse from Mode 5 PIN Reply/report"
    rule = RuleVariation_551

class Item_1148(Item):
    non_spare = NonSpare_2002

class Content_545(ContentTable):
    values = {0: "X-pulse set to zero or no authenticated Data reply or Report received", 1: "X-pulse set to one (present)"}

class RuleContent_545(RuleContentContextFree):
    variation = Content_545

class Variation_648(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_545

class RuleVariation_638(RuleVariationContextFree):
    variation = Variation_648

class NonSpare_1994(NonSpare):
    name = "X5"
    title = "X-pulse from Mode 5 Data Reply or Report"
    rule = RuleVariation_638

class Item_1143(Item):
    non_spare = NonSpare_1994

class Content_543(ContentTable):
    values = {0: "X-pulse set to zero or no Mode C reply", 1: "X-pulse set to one (present)"}

class RuleContent_543(RuleContentContextFree):
    variation = Content_543

class Variation_733(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_543

class RuleVariation_723(RuleVariationContextFree):
    variation = Variation_733

class NonSpare_1997(NonSpare):
    name = "XC"
    title = "X-pulse from Mode C Reply"
    rule = RuleVariation_723

class Item_1146(Item):
    non_spare = NonSpare_1997

class Content_542(ContentTable):
    values = {0: "X-pulse set to zero or no Mode 3/A reply", 1: "X-pulse set to one (present)"}

class RuleContent_542(RuleContentContextFree):
    variation = Content_542

class Variation_845(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_542

class RuleVariation_816(RuleVariationContextFree):
    variation = Variation_845

class NonSpare_1993(NonSpare):
    name = "X3"
    title = "X-pulse from Mode 3/A Reply"
    rule = RuleVariation_816

class Item_1142(Item):
    non_spare = NonSpare_1993

class Content_1(ContentTable):
    values = {0: "0 X-pulse set to zero or no Mode 2 reply", 1: "X-pulse set to one (present)"}

class RuleContent_1(RuleContentContextFree):
    variation = Content_1

class Variation_868(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_1

class RuleVariation_839(RuleVariationContextFree):
    variation = Variation_868

class NonSpare_1990(NonSpare):
    name = "X2"
    title = "X-pulse from Mode 2 Reply"
    rule = RuleVariation_839

class Item_1139(Item):
    non_spare = NonSpare_1990

class Content_540(ContentTable):
    values = {0: "X-pulse set to zero or no Mode 1 reply", 1: "X-pulse set to one (present)"}

class RuleContent_540(RuleContentContextFree):
    variation = Content_540

class Variation_956(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_540

class RuleVariation_927(RuleVariationContextFree):
    variation = Variation_956

class NonSpare_1987(NonSpare):
    name = "X1"
    title = "X-pulse from Mode 1 Reply"
    rule = RuleVariation_927

class Item_1136(Item):
    non_spare = NonSpare_1987

class Variation_976(Group):
    bit_size = 8
    items_list = [Item_1, Item_1148, Item_1143, Item_1146, Item_1142, Item_1139, Item_1136]
    items_dict = {"XP": NonSpare_2002, "X5": NonSpare_1994, "XC": NonSpare_1997, "X3": NonSpare_1993, "X2": NonSpare_1990, "X1": NonSpare_1987}

class RuleVariation_947(RuleVariationContextFree):
    variation = Variation_976

class NonSpare_1999(NonSpare):
    name = "XP"
    title = "X Pulse Presence"
    rule = RuleVariation_947

class NonSpare_917(NonSpare):
    name = "FOM"
    title = "Figure of Merit"
    rule = RuleVariation_659

class Item_315(Item):
    non_spare = NonSpare_917

class Variation_979(Group):
    bit_size = 8
    items_list = [Item_2, Item_315]
    items_dict = {"FOM": NonSpare_917}

class RuleVariation_950(RuleVariationContextFree):
    variation = Variation_979

class NonSpare_919(NonSpare):
    name = "FOM"
    title = "Figure of Merit"
    rule = RuleVariation_950

class NonSpare_1225(NonSpare):
    name = "MODE2"
    title = "Mode 2 Code in Octal Representation"
    rule = RuleVariation_751

class Item_536(Item):
    non_spare = NonSpare_1225

class Variation_1210(Group):
    bit_size = 16
    items_list = [Item_1049, Item_7, Item_420, Item_16, Item_536]
    items_dict = {"V": NonSpare_1890, "L": NonSpare_1063, "MODE2": NonSpare_1225}

class RuleVariation_1144(RuleVariationContextFree):
    variation = Variation_1210

class NonSpare_1134(NonSpare):
    name = "M2"
    title = "Mode 2 Code in Octal Representation"
    rule = RuleVariation_1144

class Variation_1425(Compound):
    items_list = [NonSpare_1717, NonSpare_1357, NonSpare_871, NonSpare_1999, NonSpare_919, NonSpare_1134]
    items_dict = {"SUM": NonSpare_1717, "PNO": NonSpare_1357, "EM1": NonSpare_871, "XP": NonSpare_1999, "FOM": NonSpare_919, "M2": NonSpare_1134}

class RuleVariation_1355(RuleVariationContextFree):
    variation = Variation_1425

class NonSpare_1193(NonSpare):
    name = "MES"
    title = "Military Extended Squitter"
    rule = RuleVariation_1355

class Expansion_0(Expansion):
    fspec_bytes = 1
    items = [NonSpare_645, NonSpare_1614, NonSpare_1269, NonSpare_956, NonSpare_1612, NonSpare_1677, NonSpare_1783, NonSpare_1193]

class Asterix_24(AstRef):
    category = 21
    edition = (1, 4)
    expansion = Expansion_0

class Content_198(ContentTable):
    values = {0: "Element not populated", 1: "Element populated"}

class RuleContent_198(RuleContentContextFree):
    variation = Content_198

class Variation_694(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_198

class RuleVariation_684(RuleVariationContextFree):
    variation = Variation_694

class NonSpare_885(NonSpare):
    name = "EP"
    title = "Element Populated Bit"
    rule = RuleVariation_684

class Item_290(Item):
    non_spare = NonSpare_885

class Content_248(ContentTable):
    values = {0: "MCP/FCU Mode Bits not populated", 1: "MCP/FCU Mode Bits populated"}

class RuleContent_248(RuleContentContextFree):
    variation = Content_248

class Variation_829(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_248

class RuleVariation_800(RuleVariationContextFree):
    variation = Variation_829

class NonSpare_1908(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_800

class Item_1066(Item):
    non_spare = NonSpare_1908

class Variation_1063(Group):
    bit_size = 2
    items_list = [Item_290, Item_1066]
    items_dict = {"EP": NonSpare_885, "VAL": NonSpare_1908}

class RuleVariation_1022(RuleVariationContextFree):
    variation = Variation_1063

class NonSpare_1200(NonSpare):
    name = "MFM"
    title = "Status of MCP/FCU Mode Bits"
    rule = RuleVariation_1022

class Item_516(Item):
    non_spare = NonSpare_1200

class Variation_1018(Group):
    bit_size = 8
    items_list = [Item_72, Item_1077, Item_51, Item_66, Item_516, Item_27]
    items_dict = {"AP": NonSpare_569, "VN": NonSpare_1920, "AH": NonSpare_544, "AM": NonSpare_561, "MFM": NonSpare_1200}

class RuleVariation_986(RuleVariationContextFree):
    variation = Variation_1018

class NonSpare_1270(NonSpare):
    name = "NAV"
    title = "Navigation Mode"
    rule = RuleVariation_986

class NonSpare_955(NonSpare):
    name = "GAO"
    title = "GPS Antenna Offset"
    rule = RuleVariation_154

class NonSpare_898(NonSpare):
    name = "ES"
    title = "ES IN Capability"
    rule = RuleVariation_77

class Item_300(Item):
    non_spare = NonSpare_898

class NonSpare_1881(NonSpare):
    name = "UAT"
    title = "UAT IN Capability"
    rule = RuleVariation_431

class Item_1040(Item):
    non_spare = NonSpare_1881

class Variation_515(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_198

class RuleVariation_505(RuleVariationContextFree):
    variation = Variation_515

class NonSpare_883(NonSpare):
    name = "EP"
    title = "Element Populated Bit"
    rule = RuleVariation_505

class Item_288(Item):
    non_spare = NonSpare_883

class Content_370(ContentTable):
    values = {0: "Not RCE", 1: "TABS (see Note 2)", 2: "Reserved for future use", 3: "Other RCE"}

class RuleContent_370(RuleContentContextFree):
    variation = Content_370

class Variation_654(Element):
    bit_offset8 = 3
    bit_size = 2
    rule = RuleContent_370

class RuleVariation_644(RuleVariationContextFree):
    variation = Variation_654

class NonSpare_1904(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_644

class Item_1062(Item):
    non_spare = NonSpare_1904

class Variation_1060(Group):
    bit_size = 3
    items_list = [Item_288, Item_1062]
    items_dict = {"EP": NonSpare_883, "VAL": NonSpare_1904}

class RuleVariation_1019(RuleVariationContextFree):
    variation = Variation_1060

class NonSpare_1470(NonSpare):
    name = "RCE"
    title = "Reduced Capability Equipment"
    rule = RuleVariation_1019

class Item_744(Item):
    non_spare = NonSpare_1470

class Variation_818(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_198

class RuleVariation_789(RuleVariationContextFree):
    variation = Variation_818

class NonSpare_886(NonSpare):
    name = "EP"
    title = "Element Populated Bit"
    rule = RuleVariation_789

class Item_291(Item):
    non_spare = NonSpare_886

class Content_415(ContentTable):
    values = {0: "Reply Rate Limiting is not active", 1: "Reply Rate Limiting is active"}

class RuleContent_415(RuleContentContextFree):
    variation = Content_415

class Variation_914(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_415

class RuleVariation_885(RuleVariationContextFree):
    variation = Variation_914

class NonSpare_1911(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_885

class Item_1069(Item):
    non_spare = NonSpare_1911

class Variation_1066(Group):
    bit_size = 2
    items_list = [Item_291, Item_1069]
    items_dict = {"EP": NonSpare_886, "VAL": NonSpare_1911}

class RuleVariation_1025(RuleVariationContextFree):
    variation = Variation_1066

class NonSpare_1525(NonSpare):
    name = "RRL"
    title = "Reply Rate Limiting"
    rule = RuleVariation_1025

class Item_789(Item):
    non_spare = NonSpare_1525

class Variation_40(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_198

class RuleVariation_40(RuleVariationContextFree):
    variation = Variation_40

class NonSpare_882(NonSpare):
    name = "EP"
    title = "Element Populated Bit"
    rule = RuleVariation_40

class Item_287(Item):
    non_spare = NonSpare_882

class Content_344(ContentTable):
    values = {0: "No emergency / not reported", 1: "General emergency", 2: "UAS/RPAS - Lost link", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference", 6: "Aircraft in Distress", 7: "Aircraft in Distress Manual Activation"}

class RuleContent_344(RuleContentContextFree):
    variation = Content_344

class Variation_468(Element):
    bit_offset8 = 1
    bit_size = 3
    rule = RuleContent_344

class RuleVariation_458(RuleVariationContextFree):
    variation = Variation_468

class NonSpare_1900(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_458

class Item_1058(Item):
    non_spare = NonSpare_1900

class Variation_1057(Group):
    bit_size = 4
    items_list = [Item_287, Item_1058]
    items_dict = {"EP": NonSpare_882, "VAL": NonSpare_1900}

class RuleVariation_1016(RuleVariationContextFree):
    variation = Variation_1057

class NonSpare_1386(NonSpare):
    name = "PS3"
    title = "Priority Status for Version 3 ADS-B Systems"
    rule = RuleVariation_1016

class Item_673(Item):
    non_spare = NonSpare_1386

class Content_507(ContentTable):
    values = {0: "Unavailable, Unknown, or less than 70 W", 1: "70 W", 2: "125 W", 3: "200 W"}

class RuleContent_507(RuleContentContextFree):
    variation = Content_507

class Variation_855(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_507

class RuleVariation_826(RuleVariationContextFree):
    variation = Variation_855

class NonSpare_1909(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_826

class Item_1067(Item):
    non_spare = NonSpare_1909

class Variation_1064(Group):
    bit_size = 3
    items_list = [Item_290, Item_1067]
    items_dict = {"EP": NonSpare_885, "VAL": NonSpare_1909}

class RuleVariation_1023(RuleVariationContextFree):
    variation = Variation_1064

class NonSpare_1804(NonSpare):
    name = "TPW"
    title = "Transmit Power"
    rule = RuleVariation_1023

class Item_974(Item):
    non_spare = NonSpare_1804

class Content_528(ContentTable):
    values = {0: "Unknown", 1: "Transponder #1 (left/pilot side or single)", 2: "Transponder #2 (right/co-pilot side)", 3: "Transponder #3 (auxiliary or Backup)"}

class RuleContent_528(RuleContentContextFree):
    variation = Content_528

class Variation_465(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_528

class RuleVariation_455(RuleVariationContextFree):
    variation = Variation_465

class NonSpare_1898(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_455

class Item_1056(Item):
    non_spare = NonSpare_1898

class Variation_1055(Group):
    bit_size = 3
    items_list = [Item_287, Item_1056]
    items_dict = {"EP": NonSpare_882, "VAL": NonSpare_1898}

class RuleVariation_1014(RuleVariationContextFree):
    variation = Variation_1055

class NonSpare_1832(NonSpare):
    name = "TSI"
    title = "Transponder Side Indication"
    rule = RuleVariation_1014

class Item_996(Item):
    non_spare = NonSpare_1832

class Variation_614(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_198

class RuleVariation_604(RuleVariationContextFree):
    variation = Variation_614

class NonSpare_884(NonSpare):
    name = "EP"
    title = "Element Populated Bit"
    rule = RuleVariation_604

class Item_289(Item):
    non_spare = NonSpare_884

class Content_253(ContentTable):
    values = {0: "Manned Operation", 1: "Unmanned Operation"}

class RuleContent_253(RuleContentContextFree):
    variation = Content_253

class Variation_704(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_253

class RuleVariation_694(RuleVariationContextFree):
    variation = Variation_704

class NonSpare_1905(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_694

class Item_1063(Item):
    non_spare = NonSpare_1905

class Variation_1061(Group):
    bit_size = 2
    items_list = [Item_289, Item_1063]
    items_dict = {"EP": NonSpare_884, "VAL": NonSpare_1905}

class RuleVariation_1020(RuleVariationContextFree):
    variation = Variation_1061

class NonSpare_1260(NonSpare):
    name = "MUO"
    title = "Manned / Unmanned Operation"
    rule = RuleVariation_1020

class Item_568(Item):
    non_spare = NonSpare_1260

class Content_412(ContentTable):
    values = {0: "RWC Corrective Alert not active", 1: "RWC Corrective Alert active"}

class RuleContent_412(RuleContentContextFree):
    variation = Content_412

class Variation_913(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_412

class RuleVariation_884(RuleVariationContextFree):
    variation = Variation_913

class NonSpare_1910(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_884

class Item_1068(Item):
    non_spare = NonSpare_1910

class Variation_1065(Group):
    bit_size = 2
    items_list = [Item_291, Item_1068]
    items_dict = {"EP": NonSpare_886, "VAL": NonSpare_1910}

class RuleVariation_1024(RuleVariationContextFree):
    variation = Variation_1065

class NonSpare_1551(NonSpare):
    name = "RWC"
    title = "Remain Well Clear Corrective Alert"
    rule = RuleVariation_1024

class Item_802(Item):
    non_spare = NonSpare_1551

class Content_303(ContentTable):
    values = {0: "No RWC Capability", 1: "RWC/RA/OCM Capability", 2: "RWC/OCM Capability", 3: "Invalid ASTERIX Value"}

class RuleContent_303(RuleContentContextFree):
    variation = Content_303

class Variation_457(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_303

class RuleVariation_447(RuleVariationContextFree):
    variation = Variation_457

class NonSpare_1897(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_447

class Item_1055(Item):
    non_spare = NonSpare_1897

class Variation_1054(Group):
    bit_size = 3
    items_list = [Item_287, Item_1055]
    items_dict = {"EP": NonSpare_882, "VAL": NonSpare_1897}

class RuleVariation_1013(RuleVariationContextFree):
    variation = Variation_1054

class NonSpare_808(NonSpare):
    name = "DAA"
    title = "Detectand Avoid Capabilities"
    rule = RuleVariation_1013

class Item_231(Item):
    non_spare = NonSpare_808

class NonSpare_1906(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_735

class Item_1064(Item):
    non_spare = NonSpare_1906

class Variation_1062(Group):
    bit_size = 4
    items_list = [Item_289, Item_1064]
    items_dict = {"EP": NonSpare_884, "VAL": NonSpare_1906}

class RuleVariation_1021(RuleVariationContextFree):
    variation = Variation_1062

class NonSpare_830(NonSpare):
    name = "DF17CA"
    title = "Transponder Capability"
    rule = RuleVariation_1021

class Item_251(Item):
    non_spare = NonSpare_830

class Content_537(ContentTable):
    values = {0: "Vertical Only", 1: "Horizontal Only", 2: "Blended", 3: "Vertical Only or Horizontal Only per intruder"}

class RuleContent_537(RuleContentContextFree):
    variation = Content_537

class Variation_466(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_537

class RuleVariation_456(RuleVariationContextFree):
    variation = Variation_466

class NonSpare_1899(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_456

class Item_1057(Item):
    non_spare = NonSpare_1899

class Variation_1056(Group):
    bit_size = 3
    items_list = [Item_287, Item_1057]
    items_dict = {"EP": NonSpare_882, "VAL": NonSpare_1899}

class RuleVariation_1015(RuleVariationContextFree):
    variation = Variation_1056

class NonSpare_1721(NonSpare):
    name = "SVH"
    title = "Sense Vertical & Horizontal"
    rule = RuleVariation_1015

class Item_921(Item):
    non_spare = NonSpare_1721

class NonSpare_887(NonSpare):
    name = "EP"
    title = "Element Population Bit"
    rule = RuleVariation_604

class Item_292(Item):
    non_spare = NonSpare_887

class Content_14(ContentTable):
    values = {0: "Active CAS (TCAS II) or no CAS", 1: "Active CAS (not TCAS II)", 2: "Active CAS (not TCAS II) with OCM transmit capability", 3: "Active CAS of Junior Status", 4: "Passive CAS with 1030TCAS Resolution Message receive capability", 5: "Passive CAS with only OCM receive capability", 6: "Reserved for future use", 7: "Reserved for future use"}

class RuleContent_14(RuleContentContextFree):
    variation = Content_14

class Variation_750(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_14

class RuleVariation_737(RuleVariationContextFree):
    variation = Variation_750

class NonSpare_1907(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_737

class Item_1065(Item):
    non_spare = NonSpare_1907

class Variation_1067(Group):
    bit_size = 4
    items_list = [Item_292, Item_1065]
    items_dict = {"EP": NonSpare_887, "VAL": NonSpare_1907}

class RuleVariation_1026(RuleVariationContextFree):
    variation = Variation_1067

class NonSpare_660(NonSpare):
    name = "CATC"
    title = "CAS Type & Capability"
    rule = RuleVariation_1026

class Item_133(Item):
    non_spare = NonSpare_660

class Content_197(ContentTable):
    values = {0: "Element Not Populated", 1: "Element Populated"}

class RuleContent_197(RuleContentContextFree):
    variation = Content_197

class Variation_39(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_197

class RuleVariation_39(RuleVariationContextFree):
    variation = Variation_39

class NonSpare_881(NonSpare):
    name = "EP"
    title = "Element Populated Bit"
    rule = RuleVariation_39

class Item_286(Item):
    non_spare = NonSpare_881

class Content_330(ContentTable):
    values = {0: "No data", 1: "0 ≤ TAO ≤ 1", 2: "1 < TAO ≤ 2", 3: "2 < TAO ≤ 4", 4: "4 < TAO ≤ 6", 5: "6 < TAO ≤ 8", 6: "8 < TAO ≤ 10", 7: "10 < TAO ≤ 12", 8: "12 < TAO ≤ 14", 9: "14 < TAO ≤ 16", 10: "16 < TAO ≤ 18", 11: "18 < TAO ≤ 20", 12: "20 < TAO ≤ 22", 13: "22 < TAO ≤ 24", 14: "24 < TAO ≤ 26", 15: "26 < TAO ≤ 28", 16: "28 < TAO ≤ 30", 17: "30 < TAO ≤ 32", 18: "32 < TAO ≤ 34", 19: "34 < TAO ≤ 36", 20: "36 < TAO ≤ 38", 21: "38 < TAO ≤ 40", 22: "40 < TAO ≤ 42", 23: "42 < TAO ≤ 44", 24: "44 < TAO ≤ 46", 25: "46 < TAO ≤ 48", 26: "48 < TAO ≤ 50", 27: "50 < TAO ≤ 52", 28: "52 < TAO ≤ 54", 29: "54 < TAO ≤ 56", 30: "56 < TAO ≤ 58", 31: "TAO > 58"}

class RuleContent_330(RuleContentContextFree):
    variation = Content_330

class Variation_472(Element):
    bit_offset8 = 1
    bit_size = 5
    rule = RuleContent_330

class RuleVariation_462(RuleVariationContextFree):
    variation = Variation_472

class NonSpare_1901(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_462

class Item_1059(Item):
    non_spare = NonSpare_1901

class Variation_1053(Group):
    bit_size = 7
    items_list = [Item_286, Item_1059, Item_26]
    items_dict = {"EP": NonSpare_881, "VAL": NonSpare_1901}

class RuleVariation_1012(RuleVariationContextFree):
    variation = Variation_1053

class NonSpare_1730(NonSpare):
    name = "TAO"
    title = "Transponder Antenna Offset"
    rule = RuleVariation_1012

class Item_926(Item):
    non_spare = NonSpare_1730

class Variation_1290(Extended):
    items = [Item_300, Item_1040, Item_744, Item_789, None, Item_673, Item_974, None, Item_996, Item_568, Item_802, None, Item_231, Item_251, None, Item_921, Item_133, None, Item_926, None]

class RuleVariation_1220(RuleVariationContextFree):
    variation = Variation_1290

class NonSpare_1678(NonSpare):
    name = "STA"
    title = "Aircraft Status"
    rule = RuleVariation_1220

class Expansion_1(Expansion):
    fspec_bytes = 1
    items = [NonSpare_645, NonSpare_1614, NonSpare_1270, NonSpare_955, NonSpare_1612, NonSpare_1678, NonSpare_1783, NonSpare_1193]

class Asterix_25(AstRef):
    category = 21
    edition = (1, 5)
    expansion = Expansion_1

class Content_5(ContentTable):
    values = {0: "24-Bit ICAO address", 1: "Duplicate address", 2: "Surface vehicle address", 3: "Anonymous address", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use"}

class RuleContent_5(RuleContentContextFree):
    variation = Content_5

class Variation_117(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_5

class RuleVariation_117(RuleVariationContextFree):
    variation = Variation_117

class NonSpare_596(NonSpare):
    name = "ATP"
    title = "Address Type"
    rule = RuleVariation_117

class Item_85(Item):
    non_spare = NonSpare_596

class Content_6(ContentTable):
    values = {0: "25 ft", 1: "100 ft", 2: "Unknown", 3: "Invalid"}

class RuleContent_6(RuleContentContextFree):
    variation = Content_6

class Variation_651(Element):
    bit_offset8 = 3
    bit_size = 2
    rule = RuleContent_6

class RuleVariation_641(RuleVariationContextFree):
    variation = Variation_651

class NonSpare_582(NonSpare):
    name = "ARC"
    title = "Altitude Reporting Capability"
    rule = RuleVariation_641

class Item_78(Item):
    non_spare = NonSpare_582

class Content_139(ContentTable):
    values = {0: "Default", 1: "Range Check passed, CPR Validation pending"}

class RuleContent_139(RuleContentContextFree):
    variation = Content_139

class Variation_805(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_139

class RuleVariation_776(RuleVariationContextFree):
    variation = Variation_805

class NonSpare_1468(NonSpare):
    name = "RC"
    title = "Range Check"
    rule = RuleVariation_776

class Item_742(Item):
    non_spare = NonSpare_1468

class Variation_916(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_419

class RuleVariation_887(RuleVariationContextFree):
    variation = Variation_916

class NonSpare_1457(NonSpare):
    name = "RAB"
    title = "Report Type"
    rule = RuleVariation_887

class Item_733(Item):
    non_spare = NonSpare_1457

class Variation_695(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_204

class RuleVariation_685(RuleVariationContextFree):
    variation = Variation_695

class NonSpare_1558(NonSpare):
    name = "SAA"
    title = "Selected Altitude Available"
    rule = RuleVariation_685

class Item_808(Item):
    non_spare = NonSpare_1558

class Content_421(ContentTable):
    values = {0: "Report valid", 1: "Report suspect", 2: "No information", 3: "Reserved for future use"}

class RuleContent_421(RuleContentContextFree):
    variation = Content_421

class Variation_852(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_421

class RuleVariation_823(RuleVariationContextFree):
    variation = Variation_852

class NonSpare_692(NonSpare):
    name = "CL"
    title = "Confidence Level"
    rule = RuleVariation_823

class Item_155(Item):
    non_spare = NonSpare_692

class Content_164(ContentTable):
    values = {0: "Default (see note)", 1: "Independent Position Check failed"}

class RuleContent_164(RuleContentContextFree):
    variation = Content_164

class Variation_507(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_164

class RuleVariation_497(RuleVariationContextFree):
    variation = Variation_507

class NonSpare_1052(NonSpare):
    name = "IPC"
    title = "Independent Position Check"
    rule = RuleVariation_497

class Item_411(Item):
    non_spare = NonSpare_1052

class Content_287(ContentTable):
    values = {0: "NOGO-bit not set", 1: "NOGO-bit set"}

class RuleContent_287(RuleContentContextFree):
    variation = Content_287

class Variation_624(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_287

class RuleVariation_614(RuleVariationContextFree):
    variation = Variation_624

class NonSpare_1288(NonSpare):
    name = "NOGO"
    title = "No-go Bit Status"
    rule = RuleVariation_614

class Item_593(Item):
    non_spare = NonSpare_1288

class Content_46(ContentTable):
    values = {0: "CPR Validation correct", 1: "CPR Validation failed"}

class RuleContent_46(RuleContentContextFree):
    variation = Content_46

class Variation_676(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_46

class RuleVariation_666(RuleVariationContextFree):
    variation = Variation_676

class NonSpare_771(NonSpare):
    name = "CPR"
    title = "Compact Position Reporting"
    rule = RuleVariation_666

class Item_203(Item):
    non_spare = NonSpare_771

class Content_239(ContentTable):
    values = {0: "LDPJ not detected", 1: "LDPJ detected"}

class RuleContent_239(RuleContentContextFree):
    variation = Content_239

class Variation_828(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_239

class RuleVariation_799(RuleVariationContextFree):
    variation = Variation_828

class NonSpare_1091(NonSpare):
    name = "LDPJ"
    title = "Local Decoding Position Jump"
    rule = RuleVariation_799

class Item_447(Item):
    non_spare = NonSpare_1091

class Content_138(ContentTable):
    values = {0: "Default", 1: "Range Check failed"}

class RuleContent_138(RuleContentContextFree):
    variation = Content_138

class Variation_886(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_138

class RuleVariation_857(RuleVariationContextFree):
    variation = Variation_886

class NonSpare_1471(NonSpare):
    name = "RCF"
    title = "Range Check"
    rule = RuleVariation_857

class Item_745(Item):
    non_spare = NonSpare_1471

class Variation_1281(Extended):
    items = [Item_85, Item_78, Item_742, Item_733, None, Item_246, Item_346, Item_859, Item_1003, Item_808, Item_155, None, Item_1, Item_411, Item_593, Item_203, Item_447, Item_745, None]

class RuleVariation_1211(RuleVariationContextFree):
    variation = Variation_1281

class NonSpare_140(NonSpare):
    name = "040"
    title = "Target Report Descriptor"
    rule = RuleVariation_1211

class NonSpare_1824(NonSpare):
    name = "TRNUM"
    title = "Track Number"
    rule = RuleVariation_750

class Item_989(Item):
    non_spare = NonSpare_1824

class Variation_1001(Group):
    bit_size = 16
    items_list = [Item_3, Item_989]
    items_dict = {"TRNUM": NonSpare_1824}

class RuleVariation_970(RuleVariationContextFree):
    variation = Variation_1001

class NonSpare_338(NonSpare):
    name = "161"
    title = "Track Number"
    rule = RuleVariation_970

class NonSpare_62(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class NonSpare_207(NonSpare):
    name = "071"
    title = "Time of Applicability for Position"
    rule = RuleVariation_356

class NonSpare_287(NonSpare):
    name = "130"
    title = "Position in WGS-84 Co-ordinates"
    rule = RuleVariation_1049

class NonSpare_292(NonSpare):
    name = "131"
    title = "High-Resolution Position in WGS-84 Co-ordinates"
    rule = RuleVariation_1052

class NonSpare_208(NonSpare):
    name = "072"
    title = "Time of Applicability for Velocity"
    rule = RuleVariation_356

class Content_535(ContentTable):
    values = {0: "Value in defined range", 1: "Value exceeds defined range"}

class RuleContent_535(RuleContentContextFree):
    variation = Content_535

class Variation_91(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_535

class RuleVariation_91(RuleVariationContextFree):
    variation = Variation_91

class NonSpare_1480(NonSpare):
    name = "RE"
    title = "Range Exceeded Indicator"
    rule = RuleVariation_91

class Item_752(Item):
    non_spare = NonSpare_1480

class Variation_484(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_686

class RuleVariation_474(RuleVariationContextFree):
    variation = Variation_484

class NonSpare_1735(NonSpare):
    name = "TAS"
    title = "True Air Speed"
    rule = RuleVariation_474

class Item_928(Item):
    non_spare = NonSpare_1735

class Variation_1137(Group):
    bit_size = 16
    items_list = [Item_752, Item_928]
    items_dict = {"RE": NonSpare_1480, "TAS": NonSpare_1735}

class RuleVariation_1090(RuleVariationContextFree):
    variation = Variation_1137

class NonSpare_324(NonSpare):
    name = "151"
    title = "True Airspeed"
    rule = RuleVariation_1090

class NonSpare_209(NonSpare):
    name = "073"
    title = "Time of Message Reception for Position"
    rule = RuleVariation_356

class Content_578(ContentTable):
    values = {3: "Reserved", 2: "TOMRp whole seconds = (I021/073) Whole seconds - 1", 1: "TOMRp whole seconds = (I021/073) Whole seconds + 1", 0: "TOMRp whole seconds = (I021/073) Whole seconds"}

class RuleContent_578(RuleContentContextFree):
    variation = Content_578

class Variation_114(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_578

class RuleVariation_114(RuleVariationContextFree):
    variation = Variation_114

class NonSpare_934(NonSpare):
    name = "FSI"
    title = "Full Second Indication"
    rule = RuleVariation_114

class Item_326(Item):
    non_spare = NonSpare_934

class Content_754(ContentQuantity):
    signedness = Unsigned
    lsb = 9.313225746154785e-10
    unit = "s"

class RuleContent_753(RuleContentContextFree):
    variation = Content_754

class Variation_589(Element):
    bit_offset8 = 2
    bit_size = 30
    rule = RuleContent_753

class RuleVariation_579(RuleVariationContextFree):
    variation = Variation_589

class NonSpare_1791(NonSpare):
    name = "TOMRP"
    title = "Fractional Part of the Time of Message Reception for Position in the Ground Station"
    rule = RuleVariation_579

class Item_963(Item):
    non_spare = NonSpare_1791

class Variation_1072(Group):
    bit_size = 32
    items_list = [Item_326, Item_963]
    items_dict = {"FSI": NonSpare_934, "TOMRP": NonSpare_1791}

class RuleVariation_1031(RuleVariationContextFree):
    variation = Variation_1072

class NonSpare_211(NonSpare):
    name = "074"
    title = "Time of Message Reception of Position-High Precision"
    rule = RuleVariation_1031

class NonSpare_212(NonSpare):
    name = "075"
    title = "Time of Message Reception for Velocity"
    rule = RuleVariation_356

class Content_579(ContentTable):
    values = {3: "Reserved", 2: "TOMRp whole seconds = (I021/075) Whole seconds - 1", 1: "TOMRp whole seconds = (I021/075) Whole seconds + 1", 0: "TOMRp whole seconds = (I021/075) Whole seconds"}

class RuleContent_579(RuleContentContextFree):
    variation = Content_579

class Variation_115(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_579

class RuleVariation_115(RuleVariationContextFree):
    variation = Variation_115

class NonSpare_935(NonSpare):
    name = "FSI"
    title = "Full Second Indication"
    rule = RuleVariation_115

class Item_327(Item):
    non_spare = NonSpare_935

class Variation_1073(Group):
    bit_size = 32
    items_list = [Item_327, Item_963]
    items_dict = {"FSI": NonSpare_935, "TOMRP": NonSpare_1791}

class RuleVariation_1032(RuleVariationContextFree):
    variation = Variation_1073

class NonSpare_214(NonSpare):
    name = "076"
    title = "Time of Message Reception of Velocity-High Precision"
    rule = RuleVariation_1032

class NonSpare_301(NonSpare):
    name = "140"
    title = "Geometric Height"
    rule = RuleVariation_276

class Variation_116(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_0

class RuleVariation_116(RuleVariationContextFree):
    variation = Variation_116

class NonSpare_1307(NonSpare):
    name = "NUCRNACV"
    title = "Navigation Uncertainty Category for Velocity NUCr or the Navigation Accuracy Category for Velocity NACv"
    rule = RuleVariation_116

class Item_611(Item):
    non_spare = NonSpare_1307

class Variation_665(Element):
    bit_offset8 = 3
    bit_size = 4
    rule = RuleContent_0

class RuleVariation_655(RuleVariationContextFree):
    variation = Variation_665

class NonSpare_1306(NonSpare):
    name = "NUCPNIC"
    title = "Navigation Uncertainty Category for Position NUCp or Navigation Integrity Category NIC"
    rule = RuleVariation_655

class Item_610(Item):
    non_spare = NonSpare_1306

class Variation_0(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_0

class RuleVariation_0(RuleVariationContextFree):
    variation = Variation_0

class NonSpare_1282(NonSpare):
    name = "NICBARO"
    title = "Navigation Integrity Category for Barometric Altitude"
    rule = RuleVariation_0

class Item_587(Item):
    non_spare = NonSpare_1282

class Variation_452(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_0

class RuleVariation_442(RuleVariationContextFree):
    variation = Variation_452

class NonSpare_1629(NonSpare):
    name = "SIL"
    title = "Surveillance (version 1) or Source (version 2) Integrity Level"
    rule = RuleVariation_442

class Item_851(Item):
    non_spare = NonSpare_1629

class NonSpare_1265(NonSpare):
    name = "NACP"
    title = "Navigation Accuracy Category for Position"
    rule = RuleVariation_655

class Item_572(Item):
    non_spare = NonSpare_1265

class Content_254(ContentTable):
    values = {0: "Measured per flight-hour", 1: "Measured per sample"}

class RuleContent_254(RuleContentContextFree):
    variation = Content_254

class Variation_521(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_254

class RuleVariation_511(RuleVariationContextFree):
    variation = Variation_521

class NonSpare_1630(NonSpare):
    name = "SILS"
    title = "SIL-Supplement"
    rule = RuleVariation_511

class Item_852(Item):
    non_spare = NonSpare_1630

class Variation_650(Element):
    bit_offset8 = 3
    bit_size = 2
    rule = RuleContent_0

class RuleVariation_640(RuleVariationContextFree):
    variation = Variation_650

class NonSpare_1581(NonSpare):
    name = "SDA"
    title = "Horizontal Position System Design Assurance Level (as Defined in Version 2)"
    rule = RuleVariation_640

class Item_822(Item):
    non_spare = NonSpare_1581

class Variation_846(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_0

class RuleVariation_817(RuleVariationContextFree):
    variation = Variation_846

class NonSpare_980(NonSpare):
    name = "GVA"
    title = "Geometric Altitude Accuracy"
    rule = RuleVariation_817

class Item_357(Item):
    non_spare = NonSpare_980

class NonSpare_1348(NonSpare):
    name = "PIC"
    title = "Position Integrity Category"
    rule = RuleVariation_127

class Item_647(Item):
    non_spare = NonSpare_1348

class Variation_1308(Extended):
    items = [Item_611, Item_610, None, Item_587, Item_851, Item_572, None, Item_1, Item_852, Item_822, Item_357, None, Item_647, Item_21, None]

class RuleVariation_1238(RuleVariationContextFree):
    variation = Variation_1308

class NonSpare_237(NonSpare):
    name = "090"
    title = "Quality Indicators"
    rule = RuleVariation_1238

class Content_467(ContentTable):
    values = {0: "The MOPS Version is supported by the GS", 1: "The MOPS Version is not supported by the GS"}

class RuleContent_467(RuleContentContextFree):
    variation = Content_467

class Variation_443(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_467

class RuleVariation_433(RuleVariationContextFree):
    variation = Variation_443

class NonSpare_1921(NonSpare):
    name = "VNS"
    title = "Version Not Supported"
    rule = RuleVariation_433

class Item_1078(Item):
    non_spare = NonSpare_1921

class Content_196(ContentTable):
    values = {0: "ED102/DO-260 [Ref. 8]", 1: "DO-260A [Ref. 9]", 2: "ED102A/DO-260B [Ref. 11]"}

class RuleContent_196(RuleContentContextFree):
    variation = Content_196

class Variation_576(Element):
    bit_offset8 = 2
    bit_size = 3
    rule = RuleContent_196

class RuleVariation_566(RuleVariationContextFree):
    variation = Variation_576

class NonSpare_1919(NonSpare):
    name = "VN"
    title = "Version Number"
    rule = RuleVariation_566

class Item_1076(Item):
    non_spare = NonSpare_1919

class Content_397(ContentTable):
    values = {0: "Other", 1: "UAT", 2: "1090 ES", 3: "VDL 4", 4: "Not assigned", 5: "Not assigned", 6: "Not assigned", 7: "Not assigned"}

class RuleContent_397(RuleContentContextFree):
    variation = Content_397

class Variation_861(Element):
    bit_offset8 = 5
    bit_size = 3
    rule = RuleContent_397

class RuleVariation_832(RuleVariationContextFree):
    variation = Variation_861

class NonSpare_1124(NonSpare):
    name = "LTT"
    title = "Link Technology Type"
    rule = RuleVariation_832

class Item_477(Item):
    non_spare = NonSpare_1124

class Variation_968(Group):
    bit_size = 8
    items_list = [Item_0, Item_1078, Item_1076, Item_477]
    items_dict = {"VNS": NonSpare_1921, "VN": NonSpare_1919, "LTT": NonSpare_1124}

class RuleVariation_939(RuleVariationContextFree):
    variation = Variation_968

class NonSpare_381(NonSpare):
    name = "210"
    title = "MOPS Version"
    rule = RuleVariation_939

class Variation_994(Group):
    bit_size = 16
    items_list = [Item_3, Item_543]
    items_dict = {"MODE3A": NonSpare_1232}

class RuleVariation_964(RuleVariationContextFree):
    variation = Variation_994

class NonSpare_195(NonSpare):
    name = "070"
    title = "Mode 3/A Code in Octal Representation"
    rule = RuleVariation_964

class NonSpare_398(NonSpare):
    name = "230"
    title = "Roll Angle"
    rule = RuleVariation_254

class NonSpare_326(NonSpare):
    name = "152"
    title = "Magnetic Heading"
    rule = RuleVariation_329

class Content_351(ContentTable):
    values = {0: "No intent change active", 1: "Intent change flag raised"}

class RuleContent_351(RuleContentContextFree):
    variation = Content_351

class Variation_59(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_351

class RuleVariation_59(RuleVariationContextFree):
    variation = Variation_59

class NonSpare_1037(NonSpare):
    name = "ICF"
    title = "Intent Change Flag (see Note)"
    rule = RuleVariation_59

class Item_400(Item):
    non_spare = NonSpare_1037

class Content_240(ContentTable):
    values = {0: "LNAV Mode engaged", 1: "LNAV Mode not engaged"}

class RuleContent_240(RuleContentContextFree):
    variation = Content_240

class Variation_424(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_240

class RuleVariation_414(RuleVariationContextFree):
    variation = Variation_424

class NonSpare_1097(NonSpare):
    name = "LNAV"
    title = "LNAV Mode"
    rule = RuleVariation_414

class Item_452(Item):
    non_spare = NonSpare_1097

class Item_11(Spare):
    bit_offset8 = 2
    bit_size = 1

class Content_343(ContentTable):
    values = {0: "No emergency / not reported", 1: "General emergency", 2: "Lifeguard / medical emergency", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference", 6: "DOWNED Aircraft"}

class RuleContent_343(RuleContentContextFree):
    variation = Content_343

class Variation_662(Element):
    bit_offset8 = 3
    bit_size = 3
    rule = RuleContent_343

class RuleVariation_652(RuleVariationContextFree):
    variation = Variation_662

class NonSpare_1385(NonSpare):
    name = "PS"
    title = "Priority Status"
    rule = RuleVariation_652

class Item_672(Item):
    non_spare = NonSpare_1385

class Content_328(ContentTable):
    values = {0: "No condition reported", 1: "Permanent Alert (Emergency condition)", 2: "Temporary Alert (change in Mode 3/A Code other than emergency)", 3: "SPI set"}

class RuleContent_328(RuleContentContextFree):
    variation = Content_328

class Variation_929(Element):
    bit_offset8 = 6
    bit_size = 2
    rule = RuleContent_328

class RuleVariation_900(RuleVariationContextFree):
    variation = Variation_929

class NonSpare_1663(NonSpare):
    name = "SS"
    title = "Surveillance Status"
    rule = RuleVariation_900

class Item_880(Item):
    non_spare = NonSpare_1663

class Variation_1082(Group):
    bit_size = 8
    items_list = [Item_400, Item_452, Item_11, Item_672, Item_880]
    items_dict = {"ICF": NonSpare_1037, "LNAV": NonSpare_1097, "PS": NonSpare_1385, "SS": NonSpare_1663}

class RuleVariation_1041(RuleVariationContextFree):
    variation = Variation_1082

class NonSpare_367(NonSpare):
    name = "200"
    title = "Target Status"
    rule = RuleVariation_1041

class Variation_483(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_659

class RuleVariation_473(RuleVariationContextFree):
    variation = Variation_483

class NonSpare_649(NonSpare):
    name = "BVR"
    title = "Barometric Vertical Rate"
    rule = RuleVariation_473

class Item_126(Item):
    non_spare = NonSpare_649

class Variation_1134(Group):
    bit_size = 16
    items_list = [Item_752, Item_126]
    items_dict = {"RE": NonSpare_1480, "BVR": NonSpare_649}

class RuleVariation_1087(RuleVariationContextFree):
    variation = Variation_1134

class NonSpare_328(NonSpare):
    name = "155"
    title = "Barometric Vertical Rate"
    rule = RuleVariation_1087

class NonSpare_982(NonSpare):
    name = "GVR"
    title = "Geometric Vertical Rate"
    rule = RuleVariation_473

class Item_358(Item):
    non_spare = NonSpare_982

class Variation_1136(Group):
    bit_size = 16
    items_list = [Item_752, Item_358]
    items_dict = {"RE": NonSpare_1480, "GVR": NonSpare_982}

class RuleVariation_1089(RuleVariationContextFree):
    variation = Variation_1136

class NonSpare_330(NonSpare):
    name = "157"
    title = "Geometric Vertical Rate"
    rule = RuleVariation_1089

class Content_752(ContentQuantity):
    signedness = Unsigned
    lsb = 6.103515625e-5
    unit = "NM/s"

class RuleContent_751(RuleContentContextFree):
    variation = Content_752

class Variation_486(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_751

class RuleVariation_476(RuleVariationContextFree):
    variation = Variation_486

class NonSpare_971(NonSpare):
    name = "GS"
    title = "Ground Speed Referenced to WGS-84"
    rule = RuleVariation_476

class Item_350(Item):
    non_spare = NonSpare_971

class NonSpare_1725(NonSpare):
    name = "TA"
    title = "Track Angle Clockwise Reference to True North"
    rule = RuleVariation_329

class Item_925(Item):
    non_spare = NonSpare_1725

class Variation_1135(Group):
    bit_size = 32
    items_list = [Item_752, Item_350, Item_925]
    items_dict = {"RE": NonSpare_1480, "GS": NonSpare_971, "TA": NonSpare_1725}

class RuleVariation_1088(RuleVariationContextFree):
    variation = Variation_1135

class NonSpare_331(NonSpare):
    name = "160"
    title = "Airborne Ground Vector"
    rule = RuleVariation_1088

class Item_5(Spare):
    bit_offset8 = 0
    bit_size = 6

class Content_643(ContentQuantity):
    signedness = Signed
    lsb = 3.125e-2
    unit = "°/s"

class RuleContent_643(RuleContentContextFree):
    variation = Content_643

class Variation_935(Element):
    bit_offset8 = 6
    bit_size = 10
    rule = RuleContent_643

class RuleVariation_906(RuleVariationContextFree):
    variation = Variation_935

class NonSpare_1731(NonSpare):
    name = "TAR"
    title = "Track Angle Rate"
    rule = RuleVariation_906

class Item_927(Item):
    non_spare = NonSpare_1731

class Variation_1006(Group):
    bit_size = 16
    items_list = [Item_5, Item_927]
    items_dict = {"TAR": NonSpare_1731}

class RuleVariation_975(RuleVariationContextFree):
    variation = Variation_1006

class NonSpare_342(NonSpare):
    name = "165"
    title = "Track Angle Rate"
    rule = RuleVariation_975

class NonSpare_216(NonSpare):
    name = "077"
    title = "Time of ASTERIX Report Transmission"
    rule = RuleVariation_356

class Content_291(ContentTable):
    values = {0: "No ADS-B Emitter Category Information", 1: "Light aircraft <= 15500 lbs", 2: "15500 lbs < small aircraft <75000 lbs", 3: "75000 lbs < medium a/c < 300000 lbs", 4: "High Vortex Large", 5: "300000 lbs <= heavy aircraft", 6: "Highly manoeuvrable (5g acceleration capability) and high speed (>400 knots cruise)", 7: "Reserved", 8: "Reserved", 9: "Reserved", 10: "Rotocraft", 11: "Glider / sailplane", 12: "Lighter-than-air", 13: "Unmanned aerial vehicle", 14: "Space / transatmospheric vehicle", 15: "Ultralight / handglider / paraglider", 16: "Parachutist / skydiver", 17: "Reserved", 18: "Reserved", 19: "Reserved", 20: "Surface emergency vehicle", 21: "Surface service vehicle", 22: "Fixed ground or tethered obstruction", 23: "Cluster obstacle", 24: "Line obstacle"}

class RuleContent_291(RuleContentContextFree):
    variation = Content_291

class Variation_164(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_291

class RuleVariation_159(RuleVariationContextFree):
    variation = Variation_164

class NonSpare_77(NonSpare):
    name = "020"
    title = "Emitter Category"
    rule = RuleVariation_159

class NonSpare_1556(NonSpare):
    name = "S"
    title = "Source"
    rule = RuleVariation_453

class Item_806(Item):
    non_spare = NonSpare_1556

class Variation_1161(Group):
    bit_size = 16
    items_list = [Item_816, Item_806, Item_59]
    items_dict = {"SAS": NonSpare_1572, "S": NonSpare_1556, "ALT": NonSpare_554}

class RuleVariation_1108(RuleVariationContextFree):
    variation = Variation_1161

class NonSpare_317(NonSpare):
    name = "146"
    title = "Selected Altitude"
    rule = RuleVariation_1108

class Content_372(ContentTable):
    values = {0: "Not active or unknown", 1: "Active"}

class RuleContent_372(RuleContentContextFree):
    variation = Content_372

class Variation_65(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_372

class RuleVariation_65(RuleVariationContextFree):
    variation = Variation_65

class NonSpare_1263(NonSpare):
    name = "MV"
    title = "Manage Vertical Mode"
    rule = RuleVariation_65

class Item_571(Item):
    non_spare = NonSpare_1263

class Variation_433(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_372

class RuleVariation_423(RuleVariationContextFree):
    variation = Variation_433

class NonSpare_546(NonSpare):
    name = "AH"
    title = "Altitude Hold Mode"
    rule = RuleVariation_423

class Item_53(Item):
    non_spare = NonSpare_546

class Variation_545(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_372

class RuleVariation_535(RuleVariationContextFree):
    variation = Variation_545

class NonSpare_560(NonSpare):
    name = "AM"
    title = "Approach Mode"
    rule = RuleVariation_535

class Item_65(Item):
    non_spare = NonSpare_560

class Variation_1112(Group):
    bit_size = 16
    items_list = [Item_571, Item_53, Item_65, Item_59]
    items_dict = {"MV": NonSpare_1263, "AH": NonSpare_546, "AM": NonSpare_560, "ALT": NonSpare_554}

class RuleVariation_1067(RuleVariationContextFree):
    variation = Variation_1112

class NonSpare_319(NonSpare):
    name = "148"
    title = "Final State Selected Altitude"
    rule = RuleVariation_1067

class Content_705(ContentQuantity):
    signedness = Unsigned
    lsb = 0.5
    unit = "s"

class RuleContent_705(RuleContentContextFree):
    variation = Content_705

class Variation_218(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_705

class RuleVariation_212(RuleVariationContextFree):
    variation = Variation_218

class NonSpare_70(NonSpare):
    name = "016"
    title = "Service Management"
    rule = RuleVariation_212

class Content_440(ContentTable):
    values = {0: "TCAS II or ACAS RA not active", 1: "TCAS RA active"}

class RuleContent_440(RuleContentContextFree):
    variation = Content_440

class Variation_74(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_440

class RuleVariation_74(RuleVariationContextFree):
    variation = Variation_74

class NonSpare_1450(NonSpare):
    name = "RA"
    title = "TCAS Resolution Advisory Active"
    rule = RuleVariation_74

class Item_726(Item):
    non_spare = NonSpare_1450

class Content_316(ContentTable):
    values = {0: "No capability for Trajectory Change Reports", 1: "Support for TC+0 reports only", 2: "Support for multiple TC reports", 3: "Reserved"}

class RuleContent_316(RuleContentContextFree):
    variation = Content_316

class Variation_458(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_316

class RuleVariation_448(RuleVariationContextFree):
    variation = Variation_458

class NonSpare_1740(NonSpare):
    name = "TC"
    title = "Target Trajectory Change Report Capability"
    rule = RuleVariation_448

class Item_930(Item):
    non_spare = NonSpare_1740

class Content_318(ContentTable):
    values = {0: "No capability to support Target State Reports", 1: "Capable of supporting target State Reports"}

class RuleContent_318(RuleContentContextFree):
    variation = Content_318

class Variation_625(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_318

class RuleVariation_615(RuleVariationContextFree):
    variation = Variation_625

class NonSpare_1825(NonSpare):
    name = "TS"
    title = "Target State Report Capability"
    rule = RuleVariation_615

class Item_990(Item):
    non_spare = NonSpare_1825

class Content_317(ContentTable):
    values = {0: "No capability to generate ARV-reports", 1: "Capable of generate ARV-reports"}

class RuleContent_317(RuleContentContextFree):
    variation = Content_317

class Variation_711(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_317

class RuleVariation_701(RuleVariationContextFree):
    variation = Variation_711

class NonSpare_586(NonSpare):
    name = "ARV"
    title = "Air-Referenced Velocity Report Capability"
    rule = RuleVariation_701

class Item_80(Item):
    non_spare = NonSpare_586

class Content_45(ContentTable):
    values = {0: "CDTI not operational", 1: "CDTI operational"}

class RuleContent_45(RuleContentContextFree):
    variation = Content_45

class Variation_795(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_45

class RuleVariation_766(RuleVariationContextFree):
    variation = Variation_795

class NonSpare_668(NonSpare):
    name = "CDTIA"
    title = "Cockpit Display of Traffic Information Airborne"
    rule = RuleVariation_766

class Item_138(Item):
    non_spare = NonSpare_668

class Content_441(ContentTable):
    values = {0: "TCAS operational", 1: "TCAS not operational"}

class RuleContent_441(RuleContentContextFree):
    variation = Content_441

class Variation_919(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_441

class RuleVariation_890(RuleVariationContextFree):
    variation = Variation_919

class NonSpare_1299(NonSpare):
    name = "NOTTCAS"
    title = "TCAS System Status"
    rule = RuleVariation_890

class Item_604(Item):
    non_spare = NonSpare_1299

class Content_33(ContentTable):
    values = {0: "Antenna Diversity", 1: "Single Antenna only"}

class RuleContent_33(RuleContentContextFree):
    variation = Content_33

class Variation_938(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_33

class RuleVariation_909(RuleVariationContextFree):
    variation = Variation_938

class NonSpare_1557(NonSpare):
    name = "SA"
    title = "Single Antenna"
    rule = RuleVariation_909

class Item_807(Item):
    non_spare = NonSpare_1557

class Variation_1131(Group):
    bit_size = 8
    items_list = [Item_726, Item_930, Item_990, Item_80, Item_138, Item_604, Item_807]
    items_dict = {"RA": NonSpare_1450, "TC": NonSpare_1740, "TS": NonSpare_1825, "ARV": NonSpare_586, "CDTIA": NonSpare_668, "NOTTCAS": NonSpare_1299, "SA": NonSpare_1557}

class RuleVariation_1085(RuleVariationContextFree):
    variation = Variation_1131

class NonSpare_26(NonSpare):
    name = "008"
    title = "Aircraft Operational Status"
    rule = RuleVariation_1085

class Content_407(ContentTable):
    values = {0: "Position transmitted is not ADS-B position reference point", 1: "Position transmitted is the ADS-B position reference point"}

class RuleContent_407(RuleContentContextFree):
    variation = Content_407

class Variation_550(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_407

class RuleVariation_540(RuleVariationContextFree):
    variation = Variation_550

class NonSpare_1359(NonSpare):
    name = "POA"
    title = "Position Offset Applied"
    rule = RuleVariation_540

class Item_653(Item):
    non_spare = NonSpare_1359

class Variation_595(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_45

class RuleVariation_585(RuleVariationContextFree):
    variation = Variation_595

class NonSpare_669(NonSpare):
    name = "CDTIS"
    title = "Cockpit Display of Traffic Information Surface"
    rule = RuleVariation_585

class Item_139(Item):
    non_spare = NonSpare_669

class Content_7(ContentTable):
    values = {0: ">= 70 Watts", 1: "< 70 Watts"}

class RuleContent_7(RuleContentContextFree):
    variation = Content_7

class Variation_674(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_7

class RuleVariation_664(RuleVariationContextFree):
    variation = Variation_674

class NonSpare_625(NonSpare):
    name = "B2LOW"
    title = "Class B2 Transmit Power Less Than 70 Watts"
    rule = RuleVariation_664

class Item_108(Item):
    non_spare = NonSpare_625

class Content_29(ContentTable):
    values = {0: "Aircraft not receiving ATC-services", 1: "Aircraft receiving ATC services"}

class RuleContent_29(RuleContentContextFree):
    variation = Content_29

class Variation_793(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_29

class RuleVariation_764(RuleVariationContextFree):
    variation = Variation_793

class NonSpare_1466(NonSpare):
    name = "RAS"
    title = "Receiving ATC Services"
    rule = RuleVariation_764

class Item_740(Item):
    non_spare = NonSpare_1466

class Content_231(ContentTable):
    values = {0: "IDENT switch not active", 1: "IDENT switch active"}

class RuleContent_231(RuleContentContextFree):
    variation = Content_231

class Variation_902(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_231

class RuleVariation_873(RuleVariationContextFree):
    variation = Variation_902

class NonSpare_1044(NonSpare):
    name = "IDENT"
    title = "Setting of IDENT Switch"
    rule = RuleVariation_873

class Item_406(Item):
    non_spare = NonSpare_1044

class NonSpare_1127(NonSpare):
    name = "LW"
    title = "Length and Width of the Aircraft"
    rule = RuleVariation_741

class Item_480(Item):
    non_spare = NonSpare_1127

class Variation_1275(Extended):
    items = [Item_1, Item_653, Item_139, Item_108, Item_740, Item_406, None, Item_3, Item_480]

class RuleVariation_1205(RuleVariationContextFree):
    variation = Variation_1275

class NonSpare_427(NonSpare):
    name = "271"
    title = "Surface Capabilities and Characteristics"
    rule = RuleVariation_1205

class NonSpare_295(NonSpare):
    name = "132"
    title = "Message Amplitude"
    rule = RuleVariation_192

class NonSpare_411(NonSpare):
    name = "250"
    title = "Mode S MB Data"
    rule = RuleVariation_1262

class Variation_136(Element):
    bit_offset8 = 0
    bit_size = 5
    rule = RuleContent_0

class RuleVariation_136(RuleVariationContextFree):
    variation = Variation_136

class NonSpare_1866(NonSpare):
    name = "TYP"
    title = "Message Type (= 28 for 1090 ES, Version 2)"
    rule = RuleVariation_136

class Item_1026(Item):
    non_spare = NonSpare_1866

class NonSpare_1711(NonSpare):
    name = "STYP"
    title = "Message Sub-type (= 2 for 1090 ES, Version 2)"
    rule = RuleVariation_829

class Item_915(Item):
    non_spare = NonSpare_1711

class Variation_243(Element):
    bit_offset8 = 0
    bit_size = 14
    rule = RuleContent_0

class RuleVariation_237(RuleVariationContextFree):
    variation = Variation_243

class NonSpare_580(NonSpare):
    name = "ARA"
    title = "Active Resolution Advisories"
    rule = RuleVariation_237

class Item_76(Item):
    non_spare = NonSpare_580

class Variation_932(Element):
    bit_offset8 = 6
    bit_size = 4
    rule = RuleContent_0

class RuleVariation_903(RuleVariationContextFree):
    variation = Variation_932

class NonSpare_1458(NonSpare):
    name = "RAC"
    title = "RAC (RA Complement) Record"
    rule = RuleVariation_903

class Item_734(Item):
    non_spare = NonSpare_1458

class Variation_489(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_0

class RuleVariation_479(RuleVariationContextFree):
    variation = Variation_489

class NonSpare_1467(NonSpare):
    name = "RAT"
    title = "RA Terminated"
    rule = RuleVariation_479

class Item_741(Item):
    non_spare = NonSpare_1467

class NonSpare_1257(NonSpare):
    name = "MTE"
    title = "Multiple Threat Encounter"
    rule = RuleVariation_580

class Item_566(Item):
    non_spare = NonSpare_1257

class Variation_734(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_0

class RuleVariation_724(RuleVariationContextFree):
    variation = Variation_734

class NonSpare_1850(NonSpare):
    name = "TTI"
    title = "Threat Type Indicator"
    rule = RuleVariation_724

class Item_1012(Item):
    non_spare = NonSpare_1850

class Variation_937(Element):
    bit_offset8 = 6
    bit_size = 26
    rule = RuleContent_0

class RuleVariation_908(RuleVariationContextFree):
    variation = Variation_937

class NonSpare_1770(NonSpare):
    name = "TID"
    title = "Threat Identity Data"
    rule = RuleVariation_908

class Item_954(Item):
    non_spare = NonSpare_1770

class Variation_1196(Group):
    bit_size = 56
    items_list = [Item_1026, Item_915, Item_76, Item_734, Item_741, Item_566, Item_1012, Item_954]
    items_dict = {"TYP": NonSpare_1866, "STYP": NonSpare_1711, "ARA": NonSpare_580, "RAC": NonSpare_1458, "RAT": NonSpare_1467, "MTE": NonSpare_1257, "TTI": NonSpare_1850, "TID": NonSpare_1770}

class RuleVariation_1137(RuleVariationContextFree):
    variation = Variation_1196

class NonSpare_421(NonSpare):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    rule = RuleVariation_1137

class NonSpare_462(NonSpare):
    name = "400"
    title = "Receiver ID"
    rule = RuleVariation_154

class Content_712(ContentQuantity):
    signedness = Unsigned
    lsb = 0.1
    unit = "s"

class RuleContent_712(RuleContentContextFree):
    variation = Content_712

class Variation_220(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_712

class RuleVariation_214(RuleVariationContextFree):
    variation = Variation_220

class NonSpare_568(NonSpare):
    name = "AOS"
    title = "Aircraft Operational Status Age"
    rule = RuleVariation_214

class NonSpare_1814(NonSpare):
    name = "TRD"
    title = "Target Report Descriptor Age"
    rule = RuleVariation_214

class NonSpare_1140(NonSpare):
    name = "M3A"
    title = "Mode 3/A Age"
    rule = RuleVariation_214

class NonSpare_1442(NonSpare):
    name = "QI"
    title = "Quality Indicators Age"
    rule = RuleVariation_214

class NonSpare_1766(NonSpare):
    name = "TI1"
    title = "Trajectory Intent Age"
    rule = RuleVariation_214

class NonSpare_1151(NonSpare):
    name = "MAM"
    title = "Message Amplitude Age"
    rule = RuleVariation_214

class NonSpare_966(NonSpare):
    name = "GH"
    title = "Geometric Height Age"
    rule = RuleVariation_214

class NonSpare_909(NonSpare):
    name = "FL"
    title = "Flight Level Age"
    rule = RuleVariation_214

class NonSpare_1055(NonSpare):
    name = "ISA"
    title = "Intermediate State Selected Altitude Age"
    rule = RuleVariation_214

class NonSpare_933(NonSpare):
    name = "FSA"
    title = "Final State Selected Altitude Age"
    rule = RuleVariation_214

class NonSpare_588(NonSpare):
    name = "AS"
    title = "Air Speed Age"
    rule = RuleVariation_214

class NonSpare_1736(NonSpare):
    name = "TAS"
    title = "True Air Speed Age"
    rule = RuleVariation_214

class NonSpare_1201(NonSpare):
    name = "MH"
    title = "Magnetic Heading Age"
    rule = RuleVariation_214

class NonSpare_650(NonSpare):
    name = "BVR"
    title = "Barometric Vertical Rate Age"
    rule = RuleVariation_214

class NonSpare_983(NonSpare):
    name = "GVR"
    title = "Geometric Vertical Rate Age"
    rule = RuleVariation_214

class NonSpare_979(NonSpare):
    name = "GV"
    title = "Ground Vector Age"
    rule = RuleVariation_214

class NonSpare_1733(NonSpare):
    name = "TAR"
    title = "Track Angle Rate Age"
    rule = RuleVariation_214

class NonSpare_1767(NonSpare):
    name = "TI2"
    title = "Target Identification Age"
    rule = RuleVariation_214

class NonSpare_1826(NonSpare):
    name = "TS"
    title = "Target Status Age"
    rule = RuleVariation_214

class NonSpare_1194(NonSpare):
    name = "MET"
    title = "Met Information Age"
    rule = RuleVariation_214

class NonSpare_1513(NonSpare):
    name = "ROA"
    title = "Roll Angle Age"
    rule = RuleVariation_214

class NonSpare_579(NonSpare):
    name = "ARA"
    title = "ACAS Resolution Advisory Age"
    rule = RuleVariation_214

class NonSpare_1575(NonSpare):
    name = "SCC"
    title = "Surface Capabilities and Characteristics Age"
    rule = RuleVariation_214

class Variation_1391(Compound):
    items_list = [NonSpare_568, NonSpare_1814, NonSpare_1140, NonSpare_1442, NonSpare_1766, NonSpare_1151, NonSpare_966, NonSpare_909, NonSpare_1055, NonSpare_933, NonSpare_588, NonSpare_1736, NonSpare_1201, NonSpare_650, NonSpare_983, NonSpare_979, NonSpare_1733, NonSpare_1767, NonSpare_1826, NonSpare_1194, NonSpare_1513, NonSpare_579, NonSpare_1575]
    items_dict = {"AOS": NonSpare_568, "TRD": NonSpare_1814, "M3A": NonSpare_1140, "QI": NonSpare_1442, "TI1": NonSpare_1766, "MAM": NonSpare_1151, "GH": NonSpare_966, "FL": NonSpare_909, "ISA": NonSpare_1055, "FSA": NonSpare_933, "AS": NonSpare_588, "TAS": NonSpare_1736, "MH": NonSpare_1201, "BVR": NonSpare_650, "GVR": NonSpare_983, "GV": NonSpare_979, "TAR": NonSpare_1733, "TI2": NonSpare_1767, "TS": NonSpare_1826, "MET": NonSpare_1194, "ROA": NonSpare_1513, "ARA": NonSpare_579, "SCC": NonSpare_1575}

class RuleVariation_1321(RuleVariationContextFree):
    variation = Variation_1391

class NonSpare_433(NonSpare):
    name = "295"
    title = "Data Ages"
    rule = RuleVariation_1321

class Record_7(Record):
    items_list = [NonSpare_28, NonSpare_140, NonSpare_338, NonSpare_62, NonSpare_207, NonSpare_287, NonSpare_292, NonSpare_208, NonSpare_320, NonSpare_324, NonSpare_221, NonSpare_209, NonSpare_211, NonSpare_212, NonSpare_214, NonSpare_301, NonSpare_237, NonSpare_381, NonSpare_195, NonSpare_398, NonSpare_314, NonSpare_326, NonSpare_367, NonSpare_328, NonSpare_330, NonSpare_331, NonSpare_342, NonSpare_216, NonSpare_345, NonSpare_77, NonSpare_390, NonSpare_317, NonSpare_319, NonSpare_273, NonSpare_70, NonSpare_26, NonSpare_427, NonSpare_295, NonSpare_411, NonSpare_421, NonSpare_462, NonSpare_433, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_140, "161": NonSpare_338, "015": NonSpare_62, "071": NonSpare_207, "130": NonSpare_287, "131": NonSpare_292, "072": NonSpare_208, "150": NonSpare_320, "151": NonSpare_324, "080": NonSpare_221, "073": NonSpare_209, "074": NonSpare_211, "075": NonSpare_212, "076": NonSpare_214, "140": NonSpare_301, "090": NonSpare_237, "210": NonSpare_381, "070": NonSpare_195, "230": NonSpare_398, "145": NonSpare_314, "152": NonSpare_326, "200": NonSpare_367, "155": NonSpare_328, "157": NonSpare_330, "160": NonSpare_331, "165": NonSpare_342, "077": NonSpare_216, "170": NonSpare_345, "020": NonSpare_77, "220": NonSpare_390, "146": NonSpare_317, "148": NonSpare_319, "110": NonSpare_273, "016": NonSpare_70, "008": NonSpare_26, "271": NonSpare_427, "132": NonSpare_295, "250": NonSpare_411, "260": NonSpare_421, "400": NonSpare_462, "295": NonSpare_433, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_7(UapSingle):
    record = Record_7

class Asterix_26(AstCat):
    category = 21
    edition = (2, 1)
    uap = Uap_7

class NonSpare_238(NonSpare):
    name = "090"
    title = "Quality Indicators"
    rule = RuleVariation_1238

class Content_352(ContentTable):
    values = {0: "No military emergency", 1: "Military emergency"}

class RuleContent_352(RuleContentContextFree):
    variation = Content_352

class Variation_540(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_352

class RuleVariation_530(RuleVariationContextFree):
    variation = Variation_540

class NonSpare_1192(NonSpare):
    name = "ME"
    title = "Military Emergency"
    rule = RuleVariation_530

class Item_515(Item):
    non_spare = NonSpare_1192

class Variation_1083(Group):
    bit_size = 8
    items_list = [Item_400, Item_452, Item_515, Item_672, Item_880]
    items_dict = {"ICF": NonSpare_1037, "LNAV": NonSpare_1097, "ME": NonSpare_1192, "PS": NonSpare_1385, "SS": NonSpare_1663}

class RuleVariation_1042(RuleVariationContextFree):
    variation = Variation_1083

class NonSpare_368(NonSpare):
    name = "200"
    title = "Target Status"
    rule = RuleVariation_1042

class NonSpare_1126(NonSpare):
    name = "LW"
    title = "Length and Width of the Aircraft"
    rule = RuleVariation_127

class Item_479(Item):
    non_spare = NonSpare_1126

class Variation_1276(Extended):
    items = [Item_1, Item_653, Item_139, Item_108, Item_740, Item_406, None, Item_479, Item_21, None]

class RuleVariation_1206(RuleVariationContextFree):
    variation = Variation_1276

class NonSpare_428(NonSpare):
    name = "271"
    title = "Surface Capabilities and Characteristics"
    rule = RuleVariation_1206

class NonSpare_418(NonSpare):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    rule = RuleVariation_1137

class Record_8(Record):
    items_list = [NonSpare_28, NonSpare_140, NonSpare_338, NonSpare_62, NonSpare_207, NonSpare_287, NonSpare_292, NonSpare_208, NonSpare_320, NonSpare_324, NonSpare_221, NonSpare_209, NonSpare_211, NonSpare_212, NonSpare_214, NonSpare_301, NonSpare_238, NonSpare_381, NonSpare_195, NonSpare_398, NonSpare_314, NonSpare_326, NonSpare_368, NonSpare_328, NonSpare_330, NonSpare_331, NonSpare_342, NonSpare_216, NonSpare_345, NonSpare_77, NonSpare_390, NonSpare_317, NonSpare_319, NonSpare_273, NonSpare_70, NonSpare_26, NonSpare_428, NonSpare_295, NonSpare_411, NonSpare_418, NonSpare_462, NonSpare_433, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_140, "161": NonSpare_338, "015": NonSpare_62, "071": NonSpare_207, "130": NonSpare_287, "131": NonSpare_292, "072": NonSpare_208, "150": NonSpare_320, "151": NonSpare_324, "080": NonSpare_221, "073": NonSpare_209, "074": NonSpare_211, "075": NonSpare_212, "076": NonSpare_214, "140": NonSpare_301, "090": NonSpare_238, "210": NonSpare_381, "070": NonSpare_195, "230": NonSpare_398, "145": NonSpare_314, "152": NonSpare_326, "200": NonSpare_368, "155": NonSpare_328, "157": NonSpare_330, "160": NonSpare_331, "165": NonSpare_342, "077": NonSpare_216, "170": NonSpare_345, "020": NonSpare_77, "220": NonSpare_390, "146": NonSpare_317, "148": NonSpare_319, "110": NonSpare_273, "016": NonSpare_70, "008": NonSpare_26, "271": NonSpare_428, "132": NonSpare_295, "250": NonSpare_411, "260": NonSpare_418, "400": NonSpare_462, "295": NonSpare_433, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_8(UapSingle):
    record = Record_8

class Asterix_27(AstCat):
    category = 21
    edition = (2, 2)
    uap = Uap_8

class Content_108(ContentTable):
    values = {0: "Default", 1: "List Lookup failed (see note)"}

class RuleContent_108(RuleContentContextFree):
    variation = Content_108

class Variation_402(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_108

class RuleVariation_392(RuleVariationContextFree):
    variation = Variation_402

class NonSpare_1095(NonSpare):
    name = "LLC"
    title = "List Lookup Check"
    rule = RuleVariation_392

class Item_450(Item):
    non_spare = NonSpare_1095

class Variation_1279(Extended):
    items = [Item_85, Item_78, Item_742, Item_733, None, Item_246, Item_346, Item_859, Item_1003, Item_808, Item_155, None, Item_0, Item_450, Item_411, Item_593, Item_203, Item_447, Item_745, None]

class RuleVariation_1209(RuleVariationContextFree):
    variation = Variation_1279

class NonSpare_138(NonSpare):
    name = "040"
    title = "Target Report Descriptor"
    rule = RuleVariation_1209

class Record_5(Record):
    items_list = [NonSpare_28, NonSpare_138, NonSpare_338, NonSpare_62, NonSpare_207, NonSpare_287, NonSpare_292, NonSpare_208, NonSpare_320, NonSpare_324, NonSpare_221, NonSpare_209, NonSpare_211, NonSpare_212, NonSpare_214, NonSpare_301, NonSpare_238, NonSpare_381, NonSpare_195, NonSpare_398, NonSpare_314, NonSpare_326, NonSpare_368, NonSpare_328, NonSpare_330, NonSpare_331, NonSpare_342, NonSpare_216, NonSpare_345, NonSpare_77, NonSpare_390, NonSpare_317, NonSpare_319, NonSpare_273, NonSpare_70, NonSpare_26, NonSpare_428, NonSpare_295, NonSpare_411, NonSpare_418, NonSpare_462, NonSpare_433, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_138, "161": NonSpare_338, "015": NonSpare_62, "071": NonSpare_207, "130": NonSpare_287, "131": NonSpare_292, "072": NonSpare_208, "150": NonSpare_320, "151": NonSpare_324, "080": NonSpare_221, "073": NonSpare_209, "074": NonSpare_211, "075": NonSpare_212, "076": NonSpare_214, "140": NonSpare_301, "090": NonSpare_238, "210": NonSpare_381, "070": NonSpare_195, "230": NonSpare_398, "145": NonSpare_314, "152": NonSpare_326, "200": NonSpare_368, "155": NonSpare_328, "157": NonSpare_330, "160": NonSpare_331, "165": NonSpare_342, "077": NonSpare_216, "170": NonSpare_345, "020": NonSpare_77, "220": NonSpare_390, "146": NonSpare_317, "148": NonSpare_319, "110": NonSpare_273, "016": NonSpare_70, "008": NonSpare_26, "271": NonSpare_428, "132": NonSpare_295, "250": NonSpare_411, "260": NonSpare_418, "400": NonSpare_462, "295": NonSpare_433, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_5(UapSingle):
    record = Record_5

class Asterix_28(AstCat):
    category = 21
    edition = (2, 3)
    uap = Uap_5

class Asterix_29(AstCat):
    category = 21
    edition = (2, 4)
    uap = Uap_5

class NonSpare_137(NonSpare):
    name = "040"
    title = "Target Report Descriptor"
    rule = RuleVariation_1209

class NonSpare_222(NonSpare):
    name = "080"
    title = "Target Address"
    rule = RuleVariation_335

class Content_195(ContentTable):
    values = {0: "ED102/DO-260 [Ref. 7]", 1: "DO-260A [Ref. 8]", 2: "ED102A/DO-260B [Ref. 10]", 3: "ED-102B/DO-260C [Ref. 11]"}

class RuleContent_195(RuleContentContextFree):
    variation = Content_195

class Variation_575(Element):
    bit_offset8 = 2
    bit_size = 3
    rule = RuleContent_195

class RuleVariation_565(RuleVariationContextFree):
    variation = Variation_575

class NonSpare_1918(NonSpare):
    name = "VN"
    title = "Version Number"
    rule = RuleVariation_565

class Item_1075(Item):
    non_spare = NonSpare_1918

class Variation_967(Group):
    bit_size = 8
    items_list = [Item_0, Item_1078, Item_1075, Item_477]
    items_dict = {"VNS": NonSpare_1921, "VN": NonSpare_1918, "LTT": NonSpare_1124}

class RuleVariation_938(RuleVariationContextFree):
    variation = Variation_967

class NonSpare_380(NonSpare):
    name = "210"
    title = "MOPS Version"
    rule = RuleVariation_938

class NonSpare_369(NonSpare):
    name = "200"
    title = "Target Status"
    rule = RuleVariation_1042

class NonSpare_419(NonSpare):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    rule = RuleVariation_1137

class Record_4(Record):
    items_list = [NonSpare_28, NonSpare_137, NonSpare_338, NonSpare_62, NonSpare_207, NonSpare_287, NonSpare_292, NonSpare_208, NonSpare_320, NonSpare_324, NonSpare_222, NonSpare_209, NonSpare_211, NonSpare_212, NonSpare_214, NonSpare_301, NonSpare_238, NonSpare_380, NonSpare_195, NonSpare_398, NonSpare_314, NonSpare_326, NonSpare_369, NonSpare_328, NonSpare_330, NonSpare_331, NonSpare_342, NonSpare_216, NonSpare_345, NonSpare_77, NonSpare_390, NonSpare_317, NonSpare_319, NonSpare_273, NonSpare_70, NonSpare_26, NonSpare_428, NonSpare_295, NonSpare_411, NonSpare_419, NonSpare_462, NonSpare_433, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_137, "161": NonSpare_338, "015": NonSpare_62, "071": NonSpare_207, "130": NonSpare_287, "131": NonSpare_292, "072": NonSpare_208, "150": NonSpare_320, "151": NonSpare_324, "080": NonSpare_222, "073": NonSpare_209, "074": NonSpare_211, "075": NonSpare_212, "076": NonSpare_214, "140": NonSpare_301, "090": NonSpare_238, "210": NonSpare_380, "070": NonSpare_195, "230": NonSpare_398, "145": NonSpare_314, "152": NonSpare_326, "200": NonSpare_369, "155": NonSpare_328, "157": NonSpare_330, "160": NonSpare_331, "165": NonSpare_342, "077": NonSpare_216, "170": NonSpare_345, "020": NonSpare_77, "220": NonSpare_390, "146": NonSpare_317, "148": NonSpare_319, "110": NonSpare_273, "016": NonSpare_70, "008": NonSpare_26, "271": NonSpare_428, "132": NonSpare_295, "250": NonSpare_411, "260": NonSpare_419, "400": NonSpare_462, "295": NonSpare_433, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_4(UapSingle):
    record = Record_4

class Asterix_30(AstCat):
    category = 21
    edition = (2, 5)
    uap = Uap_4

class Variation_475(Element):
    bit_offset8 = 1
    bit_size = 6
    rule = RuleContent_585

class RuleVariation_465(RuleVariationContextFree):
    variation = Variation_475

class NonSpare_1903(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_465

class Item_1061(Item):
    non_spare = NonSpare_1903

class Variation_1059(Group):
    bit_size = 7
    items_list = [Item_287, Item_1061]
    items_dict = {"EP": NonSpare_882, "VAL": NonSpare_1903}

class RuleVariation_1018(RuleVariationContextFree):
    variation = Variation_1059

class NonSpare_1739(NonSpare):
    name = "TBC"
    title = "Total Bits Corrected"
    rule = RuleVariation_1018

class Item_929(Item):
    non_spare = NonSpare_1739

class NonSpare_1902(NonSpare):
    name = "VAL"
    title = "Value"
    rule = RuleVariation_465

class Item_1060(Item):
    non_spare = NonSpare_1902

class Variation_1058(Group):
    bit_size = 7
    items_list = [Item_287, Item_1060]
    items_dict = {"EP": NonSpare_882, "VAL": NonSpare_1902}

class RuleVariation_1017(RuleVariationContextFree):
    variation = Variation_1058

class NonSpare_1157(NonSpare):
    name = "MBC"
    title = "Maximum Bits Corrected"
    rule = RuleVariation_1017

class Item_498(Item):
    non_spare = NonSpare_1157

class Variation_1280(Extended):
    items = [Item_85, Item_78, Item_742, Item_733, None, Item_246, Item_346, Item_859, Item_1003, Item_808, Item_155, None, Item_0, Item_450, Item_411, Item_593, Item_203, Item_447, Item_745, None, Item_929, None, Item_498, None]

class RuleVariation_1210(RuleVariationContextFree):
    variation = Variation_1280

class NonSpare_139(NonSpare):
    name = "040"
    title = "Target Report Descriptor"
    rule = RuleVariation_1210

class NonSpare_370(NonSpare):
    name = "200"
    title = "Target Status"
    rule = RuleVariation_1042

class NonSpare_420(NonSpare):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    rule = RuleVariation_1137

class NonSpare_1567(NonSpare):
    name = "SAL"
    title = "Selected Altitude Age"
    rule = RuleVariation_214

class Variation_1392(Compound):
    items_list = [NonSpare_568, NonSpare_1814, NonSpare_1140, NonSpare_1442, NonSpare_1766, NonSpare_1151, NonSpare_966, NonSpare_909, NonSpare_1567, NonSpare_933, NonSpare_588, NonSpare_1736, NonSpare_1201, NonSpare_650, NonSpare_983, NonSpare_979, NonSpare_1733, NonSpare_1767, NonSpare_1826, NonSpare_1194, NonSpare_1513, NonSpare_579, NonSpare_1575]
    items_dict = {"AOS": NonSpare_568, "TRD": NonSpare_1814, "M3A": NonSpare_1140, "QI": NonSpare_1442, "TI1": NonSpare_1766, "MAM": NonSpare_1151, "GH": NonSpare_966, "FL": NonSpare_909, "SAL": NonSpare_1567, "FSA": NonSpare_933, "AS": NonSpare_588, "TAS": NonSpare_1736, "MH": NonSpare_1201, "BVR": NonSpare_650, "GVR": NonSpare_983, "GV": NonSpare_979, "TAR": NonSpare_1733, "TI2": NonSpare_1767, "TS": NonSpare_1826, "MET": NonSpare_1194, "ROA": NonSpare_1513, "ARA": NonSpare_579, "SCC": NonSpare_1575}

class RuleVariation_1322(RuleVariationContextFree):
    variation = Variation_1392

class NonSpare_434(NonSpare):
    name = "295"
    title = "Data Ages"
    rule = RuleVariation_1322

class Record_6(Record):
    items_list = [NonSpare_28, NonSpare_139, NonSpare_338, NonSpare_62, NonSpare_207, NonSpare_287, NonSpare_292, NonSpare_208, NonSpare_320, NonSpare_324, NonSpare_222, NonSpare_209, NonSpare_211, NonSpare_212, NonSpare_214, NonSpare_301, NonSpare_238, NonSpare_380, NonSpare_195, NonSpare_398, NonSpare_314, NonSpare_326, NonSpare_370, NonSpare_328, NonSpare_330, NonSpare_331, NonSpare_342, NonSpare_216, NonSpare_345, NonSpare_77, NonSpare_390, NonSpare_317, NonSpare_319, NonSpare_273, NonSpare_70, NonSpare_26, NonSpare_428, NonSpare_295, NonSpare_411, NonSpare_420, NonSpare_462, NonSpare_434, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_28, "040": NonSpare_139, "161": NonSpare_338, "015": NonSpare_62, "071": NonSpare_207, "130": NonSpare_287, "131": NonSpare_292, "072": NonSpare_208, "150": NonSpare_320, "151": NonSpare_324, "080": NonSpare_222, "073": NonSpare_209, "074": NonSpare_211, "075": NonSpare_212, "076": NonSpare_214, "140": NonSpare_301, "090": NonSpare_238, "210": NonSpare_380, "070": NonSpare_195, "230": NonSpare_398, "145": NonSpare_314, "152": NonSpare_326, "200": NonSpare_370, "155": NonSpare_328, "157": NonSpare_330, "160": NonSpare_331, "165": NonSpare_342, "077": NonSpare_216, "170": NonSpare_345, "020": NonSpare_77, "220": NonSpare_390, "146": NonSpare_317, "148": NonSpare_319, "110": NonSpare_273, "016": NonSpare_70, "008": NonSpare_26, "271": NonSpare_428, "132": NonSpare_295, "250": NonSpare_411, "260": NonSpare_420, "400": NonSpare_462, "295": NonSpare_434, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_6(UapSingle):
    record = Record_6

class Asterix_31(AstCat):
    category = 21
    edition = (2, 6)
    uap = Uap_6

class NonSpare_29(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_552(ContentTable):
    values = {1: "Ground station status report", 2: "Service status report", 3: "Service statistics report"}

class RuleContent_552(RuleContentContextFree):
    variation = Content_552

class Variation_174(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_552

class RuleVariation_168(RuleVariationContextFree):
    variation = Variation_174

class NonSpare_17(NonSpare):
    name = "000"
    title = "Report Type"
    rule = RuleVariation_168

class NonSpare_1622(NonSpare):
    name = "SID"
    title = "Service Identification"
    rule = RuleVariation_127

class Item_845(Item):
    non_spare = NonSpare_1622

class Content_549(ContentTable):
    values = {1: "ADS-B VDL4", 2: "ADS-B Ext Squitter", 3: "ADS-B UAT", 4: "TIS-B VDL4", 5: "TIS-B Ext Squitter", 6: "TIS-B UAT", 7: "FIS-B VDL4", 8: "GRAS VDL4", 9: "MLT"}

class RuleContent_549(RuleContentContextFree):
    variation = Content_549

class Variation_773(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_549

class RuleVariation_744(RuleVariationContextFree):
    variation = Variation_773

class NonSpare_1712(NonSpare):
    name = "STYP"
    title = "Type of Service"
    rule = RuleVariation_744

class Item_916(Item):
    non_spare = NonSpare_1712

class Variation_1171(Group):
    bit_size = 8
    items_list = [Item_845, Item_916]
    items_dict = {"SID": NonSpare_1622, "STYP": NonSpare_1712}

class RuleVariation_1118(RuleVariationContextFree):
    variation = Variation_1171

class NonSpare_67(NonSpare):
    name = "015"
    title = "Service Type and Identification"
    rule = RuleVariation_1118

class NonSpare_205(NonSpare):
    name = "070"
    title = "Time of Day"
    rule = RuleVariation_356

class Content_72(ContentTable):
    values = {0: "Data is released for operational use", 1: "Data must not be used operationally"}

class RuleContent_72(RuleContentContextFree):
    variation = Content_72

class Variation_21(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_72

class RuleVariation_21(RuleVariationContextFree):
    variation = Variation_21

class NonSpare_1289(NonSpare):
    name = "NOGO"
    title = "Operational Release Status of the Data"
    rule = RuleVariation_21

class Item_594(Item):
    non_spare = NonSpare_1289

class Content_191(ContentTable):
    values = {0: "Default, no overload", 1: "Overload in DP"}

class RuleContent_191(RuleContentContextFree):
    variation = Content_191

class Variation_415(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_191

class RuleVariation_405(RuleVariationContextFree):
    variation = Variation_415

class NonSpare_1315(NonSpare):
    name = "ODP"
    title = "Data Processor Overload Indicator"
    rule = RuleVariation_405

class Item_619(Item):
    non_spare = NonSpare_1315

class Content_193(ContentTable):
    values = {0: "Default, no overload", 1: "Overload in transmission subsystem"}

class RuleContent_193(RuleContentContextFree):
    variation = Content_193

class Variation_514(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_193

class RuleVariation_504(RuleVariationContextFree):
    variation = Variation_514

class NonSpare_1336(NonSpare):
    name = "OXT"
    title = "Ground Interface Data Communications Overload"
    rule = RuleVariation_504

class Item_639(Item):
    non_spare = NonSpare_1336

class Content_282(ContentTable):
    values = {0: "Monitoring system not connected or unknown", 1: "Monitoring system connected"}

class RuleContent_282(RuleContentContextFree):
    variation = Content_282

class Variation_623(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_282

class RuleVariation_613(RuleVariationContextFree):
    variation = Variation_623

class NonSpare_1248(NonSpare):
    name = "MSC"
    title = "Monitoring System Connected Status"
    rule = RuleVariation_613

class Item_557(Item):
    non_spare = NonSpare_1248

class Variation_732(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_534

class RuleVariation_722(RuleVariationContextFree):
    variation = Variation_732

class NonSpare_1842(NonSpare):
    name = "TSV"
    title = "Time Source Validity"
    rule = RuleVariation_722

class Item_1006(Item):
    non_spare = NonSpare_1842

class Content_360(ContentTable):
    values = {0: "No spoofing detected", 1: "Potential spoofing attack"}

class RuleContent_360(RuleContentContextFree):
    variation = Content_360

class Variation_834(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_360

class RuleVariation_805(RuleVariationContextFree):
    variation = Variation_834

class NonSpare_1651(NonSpare):
    name = "SPO"
    title = "Indication of Spoofing Attack"
    rule = RuleVariation_805

class Item_871(Item):
    non_spare = NonSpare_1651

class Content_152(ContentTable):
    values = {0: "Default", 1: "Track numbering has restarted"}

class RuleContent_152(RuleContentContextFree):
    variation = Content_152

class Variation_889(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_152

class RuleVariation_860(RuleVariationContextFree):
    variation = Variation_889

class NonSpare_1511(NonSpare):
    name = "RN"
    title = "Renumbering Indication for Track ID"
    rule = RuleVariation_860

class Item_782(Item):
    non_spare = NonSpare_1511

class Content_697(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "s"

class RuleContent_697(RuleContentContextFree):
    variation = Content_697

class Variation_156(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_697

class RuleVariation_151(RuleVariationContextFree):
    variation = Variation_156

class NonSpare_977(NonSpare):
    name = "GSSP"
    title = "Ground Station Status Reporting Period"
    rule = RuleVariation_151

class Item_355(Item):
    non_spare = NonSpare_977

class Variation_1307(Extended):
    items = [Item_594, Item_619, Item_639, Item_557, Item_1006, Item_871, Item_782, None, Item_355, None]

class RuleVariation_1237(RuleVariationContextFree):
    variation = Variation_1307

class NonSpare_253(NonSpare):
    name = "100"
    title = "Ground Station Status"
    rule = RuleVariation_1237

class NonSpare_1516(NonSpare):
    name = "RP"
    title = "Report Period for Category 021 Reports"
    rule = RuleVariation_212

class Item_786(Item):
    non_spare = NonSpare_1516

class Content_350(ContentTable):
    values = {0: "No information", 1: "NRA class", 2: "Reserved for future use", 3: "Reserved for future use", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use"}

class RuleContent_350(RuleContentContextFree):
    variation = Content_350

class Variation_123(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_350

class RuleVariation_123(RuleVariationContextFree):
    variation = Variation_123

class NonSpare_1574(NonSpare):
    name = "SC"
    title = "Service Class"
    rule = RuleVariation_123

class Item_817(Item):
    non_spare = NonSpare_1574

class Item_18(Spare):
    bit_offset8 = 3
    bit_size = 4

class NonSpare_1674(NonSpare):
    name = "SSRP"
    title = "Service Status Reporting Period"
    rule = RuleVariation_151

class Item_886(Item):
    non_spare = NonSpare_1674

class Variation_1312(Extended):
    items = [Item_786, Item_817, Item_18, None, Item_886, None]

class RuleVariation_1242(RuleVariationContextFree):
    variation = Variation_1312

class NonSpare_261(NonSpare):
    name = "101"
    title = "Service Configuration"
    rule = RuleVariation_1242

class Content_684(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "NM"

class RuleContent_684(RuleContentContextFree):
    variation = Content_684

class Variation_213(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_684

class RuleVariation_207(RuleVariationContextFree):
    variation = Variation_213

class NonSpare_363(NonSpare):
    name = "200"
    title = "Operational Range"
    rule = RuleVariation_207

class Content_521(ContentTable):
    values = {0: "Unknown", 1: "Failed", 2: "Disabled", 3: "Degraded", 4: "Normal", 5: "Initialisation"}

class RuleContent_521(RuleContentContextFree):
    variation = Content_521

class Variation_765(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_521

class RuleVariation_740(RuleVariationContextFree):
    variation = Variation_765

class NonSpare_1692(NonSpare):
    name = "STAT"
    title = "Status of the Service"
    rule = RuleVariation_740

class Item_900(Item):
    non_spare = NonSpare_1692

class Variation_1277(Extended):
    items = [Item_3, Item_900, None]

class RuleVariation_1207(RuleVariationContextFree):
    variation = Variation_1277

class NonSpare_271(NonSpare):
    name = "110"
    title = "Service Status"
    rule = RuleVariation_1207

class Content_388(ContentTable):
    values = {0: "Number of unknown messages received", 1: "Number of too old messages received", 2: "Number of failed message conversions", 3: "Total Number of messages received", 4: "Total Number of messages transmitted", 20: "Number of TIS-B management messages received", 21: "Number of Basic messages received", 22: "Number of High Dynamic messages received", 23: "Number of Full Position messages received", 24: "Number of Basic Ground  messages received", 25: "Number of TCP messages received", 26: "Number of UTC time  messages received", 27: "Number of Data messages received", 28: "Number of High Resolution messages received", 29: "Number of Aircraft Target Airborne messages received", 30: "Number of Aircraft Target Ground messages received", 31: "Number of Ground Vehicle Target messages received", 32: "Number of 2 slots TCP messages received"}

class RuleContent_388(RuleContentContextFree):
    variation = Content_388

class Variation_167(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_388

class RuleVariation_161(RuleVariationContextFree):
    variation = Variation_167

class NonSpare_1873(NonSpare):
    name = "TYPE"
    title = "Type of Report Counter"
    rule = RuleVariation_161

class Item_1033(Item):
    non_spare = NonSpare_1873

class Content_211(ContentTable):
    values = {0: "From midnight", 1: "From the last report"}

class RuleContent_211(RuleContentContextFree):
    variation = Content_211

class Variation_43(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_211

class RuleVariation_43(RuleVariationContextFree):
    variation = Variation_43

class NonSpare_1489(NonSpare):
    name = "REF"
    title = "Reference from which the Messages Are Countered"
    rule = RuleVariation_43

class Item_760(Item):
    non_spare = NonSpare_1489

class Item_10(Spare):
    bit_offset8 = 1
    bit_size = 7

class NonSpare_800(NonSpare):
    name = "CV"
    title = "32-bit Counter Value"
    rule = RuleVariation_359

class Item_225(Item):
    non_spare = NonSpare_800

class Variation_1203(Group):
    bit_size = 48
    items_list = [Item_1033, Item_760, Item_10, Item_225]
    items_dict = {"TYPE": NonSpare_1873, "REF": NonSpare_1489, "CV": NonSpare_800}

class Variation_1369(Repetitive):
    rep_bytes = 1
    variation = Variation_1203

class RuleVariation_1299(RuleVariationContextFree):
    variation = Variation_1369

class NonSpare_281(NonSpare):
    name = "120"
    title = "Service Statistics"
    rule = RuleVariation_1299

class Record_9(Record):
    items_list = [NonSpare_29, NonSpare_17, NonSpare_67, NonSpare_205, NonSpare_253, NonSpare_261, NonSpare_363, NonSpare_271, NonSpare_281, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_29, "000": NonSpare_17, "015": NonSpare_67, "070": NonSpare_205, "100": NonSpare_253, "101": NonSpare_261, "200": NonSpare_363, "110": NonSpare_271, "120": NonSpare_281, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_9(UapSingle):
    record = Record_9

class Asterix_32(AstCat):
    category = 23
    edition = (1, 2)
    uap = Uap_9

class Asterix_33(AstCat):
    category = 23
    edition = (1, 3)
    uap = Uap_9

class NonSpare_31(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class RuleVariation_145(RuleVariationContextFree):
    variation = Variation_145

class NonSpare_1545(NonSpare):
    name = "RTYP"
    title = "Report Type"
    rule = RuleVariation_145

class Item_798(Item):
    non_spare = NonSpare_1545

class Variation_1150(Group):
    bit_size = 8
    items_list = [Item_798, Item_770]
    items_dict = {"RTYP": NonSpare_1545, "RG": NonSpare_1499}

class RuleVariation_1101(RuleVariationContextFree):
    variation = Variation_1150

class NonSpare_18(NonSpare):
    name = "000"
    title = "Report Type"
    rule = RuleVariation_1101

class NonSpare_361(NonSpare):
    name = "200"
    title = "Message Identification"
    rule = RuleVariation_337

class NonSpare_66(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class NonSpare_81(NonSpare):
    name = "020"
    title = "Service Designator"
    rule = RuleVariation_376

class NonSpare_204(NonSpare):
    name = "070"
    title = "Time of Day"
    rule = RuleVariation_356

class NonSpare_1286(NonSpare):
    name = "NOGO"
    title = ""
    rule = RuleVariation_21

class Item_591(Item):
    non_spare = NonSpare_1286

class Content_396(ContentTable):
    values = {0: "Operational", 1: "Operational but in Standby", 2: "Maintenance", 3: "Reserved for future use"}

class RuleContent_396(RuleContentContextFree):
    variation = Content_396

class Variation_462(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_396

class RuleVariation_452(RuleVariationContextFree):
    variation = Variation_462

class NonSpare_1320(NonSpare):
    name = "OPS"
    title = ""
    rule = RuleVariation_452

class Item_624(Item):
    non_spare = NonSpare_1320

class Content_422(ContentTable):
    values = {0: "Running", 1: "Failed", 2: "Degraded", 3: "Undefined", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use", 8: "Reserved for future use", 9: "Reserved for future use", 10: "Reserved for future use", 11: "Reserved for future use", 12: "Reserved for future use", 13: "Reserved for future use", 14: "Reserved for future use", 15: "Reserved for future use"}

class RuleContent_422(RuleContentContextFree):
    variation = Content_422

class Variation_668(Element):
    bit_offset8 = 3
    bit_size = 4
    rule = RuleContent_422

class RuleVariation_658(RuleVariationContextFree):
    variation = Variation_668

class NonSpare_1676(NonSpare):
    name = "SSTAT"
    title = ""
    rule = RuleVariation_658

class Item_888(Item):
    non_spare = NonSpare_1676

class Content_424(ContentTable):
    values = {0: "Running / OK", 1: "Failed", 2: "Degraded", 3: "Undefined", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use"}

class RuleContent_424(RuleContentContextFree):
    variation = Content_424

class Variation_470(Element):
    bit_offset8 = 1
    bit_size = 3
    rule = RuleContent_424

class RuleVariation_460(RuleVariationContextFree):
    variation = Variation_470

class NonSpare_1723(NonSpare):
    name = "SYSTAT"
    title = ""
    rule = RuleVariation_460

class Item_923(Item):
    non_spare = NonSpare_1723

class Content_390(ContentTable):
    values = {0: "OK", 1: "Failed", 2: "Degraded", 3: "Undefined", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use"}

class RuleContent_390(RuleContentContextFree):
    variation = Content_390

class Variation_759(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_390

class RuleVariation_739(RuleVariationContextFree):
    variation = Variation_759

class NonSpare_1610(NonSpare):
    name = "SESTAT"
    title = ""
    rule = RuleVariation_739

class Item_836(Item):
    non_spare = NonSpare_1610

class Variation_1306(Extended):
    items = [Item_591, Item_624, Item_888, None, Item_0, Item_923, Item_836, None]

class RuleVariation_1236(RuleVariationContextFree):
    variation = Variation_1306

class NonSpare_259(NonSpare):
    name = "100"
    title = "System and Service Status"
    rule = RuleVariation_1236

class Content_345(ContentTable):
    values = {0: "No error detected (shall not be sent)", 1: "Error Code Undefined", 2: "Time Source Invalid", 3: "Time Source Coasting", 4: "Track ID numbering has restarted", 5: "Data Processor Overload", 6: "Ground Interface Data Communications Overload", 7: "System stopped by operator", 8: "CBIT failed", 9: "Test Target Failure", 10: "Reserved for allocation by the AMG", 11: "Reserved for allocation by the AMG", 12: "Reserved for allocation by the AMG", 13: "Reserved for allocation by the AMG", 14: "Reserved for allocation by the AMG", 15: "Reserved for allocation by the AMG", 16: "Reserved for allocation by the AMG", 17: "Reserved for allocation by the AMG", 18: "Reserved for allocation by the AMG", 19: "Reserved for allocation by the AMG", 20: "Reserved for allocation by the AMG", 21: "Reserved for allocation by the AMG", 22: "Reserved for allocation by the AMG", 23: "Reserved for allocation by the AMG", 24: "Reserved for allocation by the AMG", 25: "Reserved for allocation by the AMG", 26: "Reserved for allocation by the AMG", 27: "Reserved for allocation by the AMG", 28: "Reserved for allocation by the AMG", 29: "Reserved for allocation by the AMG", 30: "Reserved for allocation by the AMG", 31: "Reserved for allocation by the AMG", 32: "Reserved for allocation by system manufacturers", 33: "Reserved for allocation by system manufacturers", 34: "Reserved for allocation by system manufacturers", 35: "Reserved for allocation by system manufacturers", 36: "Reserved for allocation by system manufacturers", 37: "Reserved for allocation by system manufacturers", 38: "Reserved for allocation by system manufacturers", 39: "Reserved for allocation by system manufacturers", 40: "Reserved for allocation by system manufacturers", 41: "Reserved for allocation by system manufacturers", 42: "Reserved for allocation by system manufacturers", 43: "Reserved for allocation by system manufacturers", 44: "Reserved for allocation by system manufacturers", 45: "Reserved for allocation by system manufacturers", 46: "Reserved for allocation by system manufacturers", 47: "Reserved for allocation by system manufacturers", 48: "Reserved for allocation by system manufacturers", 49: "Reserved for allocation by system manufacturers", 50: "Reserved for allocation by system manufacturers", 51: "Reserved for allocation by system manufacturers", 52: "Reserved for allocation by system manufacturers", 53: "Reserved for allocation by system manufacturers", 54: "Reserved for allocation by system manufacturers", 55: "Reserved for allocation by system manufacturers", 56: "Reserved for allocation by system manufacturers", 57: "Reserved for allocation by system manufacturers", 58: "Reserved for allocation by system manufacturers", 59: "Reserved for allocation by system manufacturers", 60: "Reserved for allocation by system manufacturers", 61: "Reserved for allocation by system manufacturers", 62: "Reserved for allocation by system manufacturers", 63: "Reserved for allocation by system manufacturers", 64: "Reserved for allocation by system manufacturers", 65: "Reserved for allocation by system manufacturers", 66: "Reserved for allocation by system manufacturers", 67: "Reserved for allocation by system manufacturers", 68: "Reserved for allocation by system manufacturers", 69: "Reserved for allocation by system manufacturers", 70: "Reserved for allocation by system manufacturers", 71: "Reserved for allocation by system manufacturers", 72: "Reserved for allocation by system manufacturers", 73: "Reserved for allocation by system manufacturers", 74: "Reserved for allocation by system manufacturers", 75: "Reserved for allocation by system manufacturers", 76: "Reserved for allocation by system manufacturers", 77: "Reserved for allocation by system manufacturers", 78: "Reserved for allocation by system manufacturers", 79: "Reserved for allocation by system manufacturers", 80: "Reserved for allocation by system manufacturers", 81: "Reserved for allocation by system manufacturers", 82: "Reserved for allocation by system manufacturers", 83: "Reserved for allocation by system manufacturers", 84: "Reserved for allocation by system manufacturers", 85: "Reserved for allocation by system manufacturers", 86: "Reserved for allocation by system manufacturers", 87: "Reserved for allocation by system manufacturers", 88: "Reserved for allocation by system manufacturers", 89: "Reserved for allocation by system manufacturers", 90: "Reserved for allocation by system manufacturers", 91: "Reserved for allocation by system manufacturers", 92: "Reserved for allocation by system manufacturers", 93: "Reserved for allocation by system manufacturers", 94: "Reserved for allocation by system manufacturers", 95: "Reserved for allocation by system manufacturers", 96: "Reserved for allocation by system manufacturers", 97: "Reserved for allocation by system manufacturers", 98: "Reserved for allocation by system manufacturers", 99: "Reserved for allocation by system manufacturers", 100: "Reserved for allocation by system manufacturers", 101: "Reserved for allocation by system manufacturers", 102: "Reserved for allocation by system manufacturers", 103: "Reserved for allocation by system manufacturers", 104: "Reserved for allocation by system manufacturers", 105: "Reserved for allocation by system manufacturers", 106: "Reserved for allocation by system manufacturers", 107: "Reserved for allocation by system manufacturers", 108: "Reserved for allocation by system manufacturers", 109: "Reserved for allocation by system manufacturers", 110: "Reserved for allocation by system manufacturers", 111: "Reserved for allocation by system manufacturers", 112: "Reserved for allocation by system manufacturers", 113: "Reserved for allocation by system manufacturers", 114: "Reserved for allocation by system manufacturers", 115: "Reserved for allocation by system manufacturers", 116: "Reserved for allocation by system manufacturers", 117: "Reserved for allocation by system manufacturers", 118: "Reserved for allocation by system manufacturers", 119: "Reserved for allocation by system manufacturers", 120: "Reserved for allocation by system manufacturers", 121: "Reserved for allocation by system manufacturers", 122: "Reserved for allocation by system manufacturers", 123: "Reserved for allocation by system manufacturers", 124: "Reserved for allocation by system manufacturers", 125: "Reserved for allocation by system manufacturers", 126: "Reserved for allocation by system manufacturers", 127: "Reserved for allocation by system manufacturers", 128: "Reserved for allocation by system manufacturers", 129: "Reserved for allocation by system manufacturers", 130: "Reserved for allocation by system manufacturers", 131: "Reserved for allocation by system manufacturers", 132: "Reserved for allocation by system manufacturers", 133: "Reserved for allocation by system manufacturers", 134: "Reserved for allocation by system manufacturers", 135: "Reserved for allocation by system manufacturers", 136: "Reserved for allocation by system manufacturers", 137: "Reserved for allocation by system manufacturers", 138: "Reserved for allocation by system manufacturers", 139: "Reserved for allocation by system manufacturers", 140: "Reserved for allocation by system manufacturers", 141: "Reserved for allocation by system manufacturers", 142: "Reserved for allocation by system manufacturers", 143: "Reserved for allocation by system manufacturers", 144: "Reserved for allocation by system manufacturers", 145: "Reserved for allocation by system manufacturers", 146: "Reserved for allocation by system manufacturers", 147: "Reserved for allocation by system manufacturers", 148: "Reserved for allocation by system manufacturers", 149: "Reserved for allocation by system manufacturers", 150: "Reserved for allocation by system manufacturers", 151: "Reserved for allocation by system manufacturers", 152: "Reserved for allocation by system manufacturers", 153: "Reserved for allocation by system manufacturers", 154: "Reserved for allocation by system manufacturers", 155: "Reserved for allocation by system manufacturers", 156: "Reserved for allocation by system manufacturers", 157: "Reserved for allocation by system manufacturers", 158: "Reserved for allocation by system manufacturers", 159: "Reserved for allocation by system manufacturers", 160: "Reserved for allocation by system manufacturers", 161: "Reserved for allocation by system manufacturers", 162: "Reserved for allocation by system manufacturers", 163: "Reserved for allocation by system manufacturers", 164: "Reserved for allocation by system manufacturers", 165: "Reserved for allocation by system manufacturers", 166: "Reserved for allocation by system manufacturers", 167: "Reserved for allocation by system manufacturers", 168: "Reserved for allocation by system manufacturers", 169: "Reserved for allocation by system manufacturers", 170: "Reserved for allocation by system manufacturers", 171: "Reserved for allocation by system manufacturers", 172: "Reserved for allocation by system manufacturers", 173: "Reserved for allocation by system manufacturers", 174: "Reserved for allocation by system manufacturers", 175: "Reserved for allocation by system manufacturers", 176: "Reserved for allocation by system manufacturers", 177: "Reserved for allocation by system manufacturers", 178: "Reserved for allocation by system manufacturers", 179: "Reserved for allocation by system manufacturers", 180: "Reserved for allocation by system manufacturers", 181: "Reserved for allocation by system manufacturers", 182: "Reserved for allocation by system manufacturers", 183: "Reserved for allocation by system manufacturers", 184: "Reserved for allocation by system manufacturers", 185: "Reserved for allocation by system manufacturers", 186: "Reserved for allocation by system manufacturers", 187: "Reserved for allocation by system manufacturers", 188: "Reserved for allocation by system manufacturers", 189: "Reserved for allocation by system manufacturers", 190: "Reserved for allocation by system manufacturers", 191: "Reserved for allocation by system manufacturers", 192: "Reserved for allocation by system manufacturers", 193: "Reserved for allocation by system manufacturers", 194: "Reserved for allocation by system manufacturers", 195: "Reserved for allocation by system manufacturers", 196: "Reserved for allocation by system manufacturers", 197: "Reserved for allocation by system manufacturers", 198: "Reserved for allocation by system manufacturers", 199: "Reserved for allocation by system manufacturers", 200: "Reserved for allocation by system manufacturers", 201: "Reserved for allocation by system manufacturers", 202: "Reserved for allocation by system manufacturers", 203: "Reserved for allocation by system manufacturers", 204: "Reserved for allocation by system manufacturers", 205: "Reserved for allocation by system manufacturers", 206: "Reserved for allocation by system manufacturers", 207: "Reserved for allocation by system manufacturers", 208: "Reserved for allocation by system manufacturers", 209: "Reserved for allocation by system manufacturers", 210: "Reserved for allocation by system manufacturers", 211: "Reserved for allocation by system manufacturers", 212: "Reserved for allocation by system manufacturers", 213: "Reserved for allocation by system manufacturers", 214: "Reserved for allocation by system manufacturers", 215: "Reserved for allocation by system manufacturers", 216: "Reserved for allocation by system manufacturers", 217: "Reserved for allocation by system manufacturers", 218: "Reserved for allocation by system manufacturers", 219: "Reserved for allocation by system manufacturers", 220: "Reserved for allocation by system manufacturers", 221: "Reserved for allocation by system manufacturers", 222: "Reserved for allocation by system manufacturers", 223: "Reserved for allocation by system manufacturers", 224: "Reserved for allocation by system manufacturers", 225: "Reserved for allocation by system manufacturers", 226: "Reserved for allocation by system manufacturers", 227: "Reserved for allocation by system manufacturers", 228: "Reserved for allocation by system manufacturers", 229: "Reserved for allocation by system manufacturers", 230: "Reserved for allocation by system manufacturers", 231: "Reserved for allocation by system manufacturers", 232: "Reserved for allocation by system manufacturers", 233: "Reserved for allocation by system manufacturers", 234: "Reserved for allocation by system manufacturers", 235: "Reserved for allocation by system manufacturers", 236: "Reserved for allocation by system manufacturers", 237: "Reserved for allocation by system manufacturers", 238: "Reserved for allocation by system manufacturers", 239: "Reserved for allocation by system manufacturers", 240: "Reserved for allocation by system manufacturers", 241: "Reserved for allocation by system manufacturers", 242: "Reserved for allocation by system manufacturers", 243: "Reserved for allocation by system manufacturers", 244: "Reserved for allocation by system manufacturers", 245: "Reserved for allocation by system manufacturers", 246: "Reserved for allocation by system manufacturers", 247: "Reserved for allocation by system manufacturers", 248: "Reserved for allocation by system manufacturers", 249: "Reserved for allocation by system manufacturers", 250: "Reserved for allocation by system manufacturers", 251: "Reserved for allocation by system manufacturers", 252: "Reserved for allocation by system manufacturers", 253: "Reserved for allocation by system manufacturers", 254: "Reserved for allocation by system manufacturers", 255: "Reserved for allocation by system manufacturers"}

class RuleContent_345(RuleContentContextFree):
    variation = Content_345

class Variation_166(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_345

class Variation_1325(Repetitive):
    rep_bytes = 1
    variation = Variation_166

class RuleVariation_1255(RuleVariationContextFree):
    variation = Variation_1325

class NonSpare_264(NonSpare):
    name = "105"
    title = "System and Service Error Codes"
    rule = RuleVariation_1255

class NonSpare_691(NonSpare):
    name = "CID"
    title = "Component ID"
    rule = RuleVariation_240

class Item_154(Item):
    non_spare = NonSpare_691

class Content_293(ContentTable):
    values = {0: "No Error Detected", 1: "Error Code Undefined", 2: "Reserved for allocation by the AMG", 3: "Reserved for allocation by the AMG", 4: "Reserved for allocation by the AMG", 5: "Reserved for allocation by the AMG", 6: "Reserved for allocation by the AMG", 7: "Reserved for allocation by the AMG", 8: "Reserved for allocation by the AMG", 9: "Reserved for allocation by the AMG", 10: "Reserved for allocation by the AMG", 11: "Reserved for allocation by the AMG", 12: "Reserved for allocation by the AMG", 13: "Reserved for allocation by the AMG", 14: "Reserved for allocation by the AMG", 15: "Reserved for allocation by the AMG", 16: "Reserved for allocation by system manufacturers", 17: "Reserved for allocation by system manufacturers", 18: "Reserved for allocation by system manufacturers", 19: "Reserved for allocation by system manufacturers", 20: "Reserved for allocation by system manufacturers", 21: "Reserved for allocation by system manufacturers", 22: "Reserved for allocation by system manufacturers", 23: "Reserved for allocation by system manufacturers", 24: "Reserved for allocation by system manufacturers", 25: "Reserved for allocation by system manufacturers", 26: "Reserved for allocation by system manufacturers", 27: "Reserved for allocation by system manufacturers", 28: "Reserved for allocation by system manufacturers", 29: "Reserved for allocation by system manufacturers", 30: "Reserved for allocation by system manufacturers", 31: "Reserved for allocation by system manufacturers", 32: "Reserved for allocation by system manufacturers", 33: "Reserved for allocation by system manufacturers", 34: "Reserved for allocation by system manufacturers", 35: "Reserved for allocation by system manufacturers", 36: "Reserved for allocation by system manufacturers", 37: "Reserved for allocation by system manufacturers", 38: "Reserved for allocation by system manufacturers", 39: "Reserved for allocation by system manufacturers", 40: "Reserved for allocation by system manufacturers", 41: "Reserved for allocation by system manufacturers", 42: "Reserved for allocation by system manufacturers", 43: "Reserved for allocation by system manufacturers", 44: "Reserved for allocation by system manufacturers", 45: "Reserved for allocation by system manufacturers", 46: "Reserved for allocation by system manufacturers", 47: "Reserved for allocation by system manufacturers", 48: "Reserved for allocation by system manufacturers", 49: "Reserved for allocation by system manufacturers", 50: "Reserved for allocation by system manufacturers", 51: "Reserved for allocation by system manufacturers", 52: "Reserved for allocation by system manufacturers", 53: "Reserved for allocation by system manufacturers", 54: "Reserved for allocation by system manufacturers", 55: "Reserved for allocation by system manufacturers", 56: "Reserved for allocation by system manufacturers", 57: "Reserved for allocation by system manufacturers", 58: "Reserved for allocation by system manufacturers", 59: "Reserved for allocation by system manufacturers", 60: "Reserved for allocation by system manufacturers", 61: "Reserved for allocation by system manufacturers", 62: "Reserved for allocation by system manufacturers", 63: "Reserved for allocation by system manufacturers"}

class RuleContent_293(RuleContentContextFree):
    variation = Content_293

class Variation_143(Element):
    bit_offset8 = 0
    bit_size = 6
    rule = RuleContent_293

class RuleVariation_143(RuleVariationContextFree):
    variation = Variation_143

class NonSpare_895(NonSpare):
    name = "ERRC"
    title = "Error Code"
    rule = RuleVariation_143

class Item_298(Item):
    non_spare = NonSpare_895

class Content_423(ContentTable):
    values = {0: "Running", 1: "Failed", 2: "Maintenance", 3: "Reserved"}

class RuleContent_423(RuleContentContextFree):
    variation = Content_423

class Variation_930(Element):
    bit_offset8 = 6
    bit_size = 2
    rule = RuleContent_423

class RuleVariation_901(RuleVariationContextFree):
    variation = Variation_930

class NonSpare_781(NonSpare):
    name = "CS"
    title = "Component State/Mode"
    rule = RuleVariation_901

class Item_210(Item):
    non_spare = NonSpare_781

class Variation_1033(Group):
    bit_size = 24
    items_list = [Item_154, Item_298, Item_210]
    items_dict = {"CID": NonSpare_691, "ERRC": NonSpare_895, "CS": NonSpare_781}

class Variation_1343(Repetitive):
    rep_bytes = 1
    variation = Variation_1033

class RuleVariation_1273(RuleVariationContextFree):
    variation = Variation_1343

class NonSpare_275(NonSpare):
    name = "120"
    title = "Component Status"
    rule = RuleVariation_1273

class Content_389(ContentTable):
    values = {0: "Number of unknown messages received", 1: "Number of too old messages received", 2: "Number of failed message conversions", 3: "Total Number of messages received", 4: "Total number of messages transmitted", 5: "Reserved for AMG", 6: "Reserved for AMG", 7: "Reserved for AMG", 8: "Reserved for AMG", 9: "Reserved for AMG", 10: "Reserved for AMG", 11: "Reserved for AMG", 12: "Reserved for AMG", 13: "Reserved for AMG", 14: "Reserved for AMG", 15: "Reserved for AMG", 16: "Reserved for AMG", 17: "Reserved for AMG", 18: "Reserved for AMG", 19: "Reserved for AMG", 20: "Implementation specific", 21: "Implementation specific", 22: "Implementation specific", 23: "Implementation specific", 24: "Implementation specific", 25: "Implementation specific", 26: "Implementation specific", 27: "Implementation specific", 28: "Implementation specific", 29: "Implementation specific", 30: "Implementation specific", 31: "Implementation specific", 32: "Implementation specific", 33: "Implementation specific", 34: "Implementation specific", 35: "Implementation specific", 36: "Implementation specific", 37: "Implementation specific", 38: "Implementation specific", 39: "Implementation specific", 40: "Implementation specific", 41: "Implementation specific", 42: "Implementation specific", 43: "Implementation specific", 44: "Implementation specific", 45: "Implementation specific", 46: "Implementation specific", 47: "Implementation specific", 48: "Implementation specific", 49: "Implementation specific", 50: "Implementation specific", 51: "Implementation specific", 52: "Implementation specific", 53: "Implementation specific", 54: "Implementation specific", 55: "Implementation specific", 56: "Implementation specific", 57: "Implementation specific", 58: "Implementation specific", 59: "Implementation specific", 60: "Implementation specific", 61: "Implementation specific", 62: "Implementation specific", 63: "Implementation specific", 64: "Implementation specific", 65: "Implementation specific", 66: "Implementation specific", 67: "Implementation specific", 68: "Implementation specific", 69: "Implementation specific", 70: "Implementation specific", 71: "Implementation specific", 72: "Implementation specific", 73: "Implementation specific", 74: "Implementation specific", 75: "Implementation specific", 76: "Implementation specific", 77: "Implementation specific", 78: "Implementation specific", 79: "Implementation specific", 80: "Implementation specific", 81: "Implementation specific", 82: "Implementation specific", 83: "Implementation specific", 84: "Implementation specific", 85: "Implementation specific", 86: "Implementation specific", 87: "Implementation specific", 88: "Implementation specific", 89: "Implementation specific", 90: "Implementation specific", 91: "Implementation specific", 92: "Implementation specific", 93: "Implementation specific", 94: "Implementation specific", 95: "Implementation specific", 96: "Implementation specific", 97: "Implementation specific", 98: "Implementation specific", 99: "Implementation specific", 100: "Implementation specific", 101: "Implementation specific", 102: "Implementation specific", 103: "Implementation specific", 104: "Implementation specific", 105: "Implementation specific", 106: "Implementation specific", 107: "Implementation specific", 108: "Implementation specific", 109: "Implementation specific", 110: "Implementation specific", 111: "Implementation specific", 112: "Implementation specific", 113: "Implementation specific", 114: "Implementation specific", 115: "Implementation specific", 116: "Implementation specific", 117: "Implementation specific", 118: "Implementation specific", 119: "Implementation specific", 120: "Implementation specific", 121: "Implementation specific", 122: "Implementation specific", 123: "Implementation specific", 124: "Implementation specific", 125: "Implementation specific", 126: "Implementation specific", 127: "Implementation specific", 128: "Implementation specific", 129: "Implementation specific", 130: "Implementation specific", 131: "Implementation specific", 132: "Implementation specific", 133: "Implementation specific", 134: "Implementation specific", 135: "Implementation specific", 136: "Implementation specific", 137: "Implementation specific", 138: "Implementation specific", 139: "Implementation specific", 140: "Implementation specific", 141: "Implementation specific", 142: "Implementation specific", 143: "Implementation specific", 144: "Implementation specific", 145: "Implementation specific", 146: "Implementation specific", 147: "Implementation specific", 148: "Implementation specific", 149: "Implementation specific", 150: "Implementation specific", 151: "Implementation specific", 152: "Implementation specific", 153: "Implementation specific", 154: "Implementation specific", 155: "Implementation specific", 156: "Implementation specific", 157: "Implementation specific", 158: "Implementation specific", 159: "Implementation specific", 160: "Implementation specific", 161: "Implementation specific", 162: "Implementation specific", 163: "Implementation specific", 164: "Implementation specific", 165: "Implementation specific", 166: "Implementation specific", 167: "Implementation specific", 168: "Implementation specific", 169: "Implementation specific", 170: "Implementation specific", 171: "Implementation specific", 172: "Implementation specific", 173: "Implementation specific", 174: "Implementation specific", 175: "Implementation specific", 176: "Implementation specific", 177: "Implementation specific", 178: "Implementation specific", 179: "Implementation specific", 180: "Implementation specific", 181: "Implementation specific", 182: "Implementation specific", 183: "Implementation specific", 184: "Implementation specific", 185: "Implementation specific", 186: "Implementation specific", 187: "Implementation specific", 188: "Implementation specific", 189: "Implementation specific", 190: "Implementation specific", 191: "Implementation specific", 192: "Implementation specific", 193: "Implementation specific", 194: "Implementation specific", 195: "Implementation specific", 196: "Implementation specific", 197: "Implementation specific", 198: "Implementation specific", 199: "Implementation specific", 200: "Implementation specific", 201: "Implementation specific", 202: "Implementation specific", 203: "Implementation specific", 204: "Implementation specific", 205: "Implementation specific", 206: "Implementation specific", 207: "Implementation specific", 208: "Implementation specific", 209: "Implementation specific", 210: "Implementation specific", 211: "Implementation specific", 212: "Implementation specific", 213: "Implementation specific", 214: "Implementation specific", 215: "Implementation specific", 216: "Implementation specific", 217: "Implementation specific", 218: "Implementation specific", 219: "Implementation specific", 220: "Implementation specific", 221: "Implementation specific", 222: "Implementation specific", 223: "Implementation specific", 224: "Implementation specific", 225: "Implementation specific", 226: "Implementation specific", 227: "Implementation specific", 228: "Implementation specific", 229: "Implementation specific", 230: "Implementation specific", 231: "Implementation specific", 232: "Implementation specific", 233: "Implementation specific", 234: "Implementation specific", 235: "Implementation specific", 236: "Implementation specific", 237: "Implementation specific", 238: "Implementation specific", 239: "Implementation specific", 240: "Implementation specific", 241: "Implementation specific", 242: "Implementation specific", 243: "Implementation specific", 244: "Implementation specific", 245: "Implementation specific", 246: "Implementation specific", 247: "Implementation specific", 248: "Implementation specific", 249: "Implementation specific", 250: "Implementation specific", 251: "Implementation specific", 252: "Implementation specific", 253: "Implementation specific", 254: "Implementation specific", 255: "Implementation specific"}

class RuleContent_389(RuleContentContextFree):
    variation = Content_389

class Variation_168(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_389

class RuleVariation_162(RuleVariationContextFree):
    variation = Variation_168

class NonSpare_1874(NonSpare):
    name = "TYPE"
    title = "Type of Report Counter"
    rule = RuleVariation_162

class Item_1034(Item):
    non_spare = NonSpare_1874

class Content_210(ContentTable):
    values = {0: "From UTC midnight", 1: "From the previous report"}

class RuleContent_210(RuleContentContextFree):
    variation = Content_210

class Variation_42(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_210

class RuleVariation_42(RuleVariationContextFree):
    variation = Variation_42

class NonSpare_1488(NonSpare):
    name = "REF"
    title = "Reference from which the Messages Are Counted"
    rule = RuleVariation_42

class Item_759(Item):
    non_spare = NonSpare_1488

class NonSpare_750(NonSpare):
    name = "COUNT"
    title = "Counter Value"
    rule = RuleVariation_361

class Item_193(Item):
    non_spare = NonSpare_750

class Variation_1204(Group):
    bit_size = 48
    items_list = [Item_1034, Item_759, Item_10, Item_193]
    items_dict = {"TYPE": NonSpare_1874, "REF": NonSpare_1488, "COUNT": NonSpare_750}

class Variation_1370(Repetitive):
    rep_bytes = 1
    variation = Variation_1204

class RuleVariation_1300(RuleVariationContextFree):
    variation = Variation_1370

class NonSpare_302(NonSpare):
    name = "140"
    title = "Service Statistics"
    rule = RuleVariation_1300

class Content_678(ContentQuantity):
    signedness = Signed
    lsb = 4.190951585769653e-8
    unit = "°"

class RuleContent_678(RuleContentContextFree):
    variation = Content_678

class Variation_377(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_678

class RuleVariation_371(RuleVariationContextFree):
    variation = Variation_377

class NonSpare_1081(NonSpare):
    name = "LAT"
    title = "Latitude"
    rule = RuleVariation_371

class Item_438(Item):
    non_spare = NonSpare_1081

class Content_677(ContentQuantity):
    signedness = Signed
    lsb = 4.190951585769653e-8
    unit = "°"

class RuleContent_677(RuleContentContextFree):
    variation = Content_677

class Variation_376(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_677

class RuleVariation_370(RuleVariationContextFree):
    variation = Variation_376

class NonSpare_1109(NonSpare):
    name = "LON"
    title = "Longitude"
    rule = RuleVariation_370

class Item_464(Item):
    non_spare = NonSpare_1109

class Variation_1095(Group):
    bit_size = 64
    items_list = [Item_438, Item_464]
    items_dict = {"LAT": NonSpare_1081, "LON": NonSpare_1109}

class RuleVariation_1054(RuleVariationContextFree):
    variation = Variation_1095

class NonSpare_493(NonSpare):
    name = "600"
    title = "Position of the System Reference Point"
    rule = RuleVariation_1054

class Content_632(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "m"

class RuleContent_632(RuleContentContextFree):
    variation = Content_632

class Variation_269(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_632

class RuleVariation_263(RuleVariationContextFree):
    variation = Variation_269

class NonSpare_501(NonSpare):
    name = "610"
    title = "Height of the System Reference Point"
    rule = RuleVariation_263

class Record_11(Record):
    items_list = [NonSpare_31, NonSpare_18, NonSpare_361, NonSpare_66, NonSpare_81, NonSpare_204, NonSpare_259, NonSpare_264, NonSpare_275, NonSpare_302, NonSpare_1641, NonSpare_493, NonSpare_501]
    items_dict = {"010": NonSpare_31, "000": NonSpare_18, "200": NonSpare_361, "015": NonSpare_66, "020": NonSpare_81, "070": NonSpare_204, "100": NonSpare_259, "105": NonSpare_264, "120": NonSpare_275, "140": NonSpare_302, "SP": NonSpare_1641, "600": NonSpare_493, "610": NonSpare_501}

class Uap_11(UapSingle):
    record = Record_11

class Asterix_34(AstCat):
    category = 25
    edition = (1, 5)
    uap = Uap_11

class NonSpare_48(NonSpare):
    name = "010"
    title = "Server Identification Tag"
    rule = RuleVariation_1104

class NonSpare_68(NonSpare):
    name = "015"
    title = "User Number"
    rule = RuleVariation_241

class NonSpare_72(NonSpare):
    name = "018"
    title = "Data Source Identification Tag"
    rule = RuleVariation_1104

class Content_553(ContentTable):
    values = {1: "Information sent by an FPPS"}

class RuleContent_553(RuleContentContextFree):
    variation = Content_553

class Variation_131(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_553

class RuleVariation_131(RuleVariationContextFree):
    variation = Variation_131

class NonSpare_904(NonSpare):
    name = "FAMILY"
    title = ""
    rule = RuleVariation_131

class Item_306(Item):
    non_spare = NonSpare_904

class Content_551(ContentTable):
    values = {1: "Flight Plan to track initial correlation", 2: "Miniplan update", 3: "End of correlation", 4: "Miniplan Cancellation", 5: "Retained Miniplan"}

class RuleContent_551(RuleContentContextFree):
    variation = Content_551

class Variation_774(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_551

class RuleVariation_745(RuleVariationContextFree):
    variation = Variation_774

class NonSpare_1267(NonSpare):
    name = "NATURE"
    title = ""
    rule = RuleVariation_745

class Item_574(Item):
    non_spare = NonSpare_1267

class Variation_1070(Group):
    bit_size = 8
    items_list = [Item_306, Item_574]
    items_dict = {"FAMILY": NonSpare_904, "NATURE": NonSpare_1267}

class RuleVariation_1029(RuleVariationContextFree):
    variation = Variation_1070

class NonSpare_124(NonSpare):
    name = "035"
    title = "Type of Message"
    rule = RuleVariation_1029

class NonSpare_88(NonSpare):
    name = "020"
    title = "Time of ASTERIX Report Generation"
    rule = RuleVariation_356

class NonSpare_142(NonSpare):
    name = "040"
    title = "Track Number"
    rule = RuleVariation_241

class Variation_195(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_585

class RuleVariation_189(RuleVariationContextFree):
    variation = Variation_195

class NonSpare_1715(NonSpare):
    name = "SUI"
    title = "System Unit Identification"
    rule = RuleVariation_189

class Item_919(Item):
    non_spare = NonSpare_1715

class Variation_245(Element):
    bit_offset8 = 0
    bit_size = 15
    rule = RuleContent_585

class RuleVariation_239(RuleVariationContextFree):
    variation = Variation_245

class NonSpare_1701(NonSpare):
    name = "STN"
    title = "System Track Number"
    rule = RuleVariation_239

class Item_908(Item):
    non_spare = NonSpare_1701

class Variation_1317(Extended):
    items = [Item_919, Item_908, None]

class RuleVariation_1247(RuleVariationContextFree):
    variation = Variation_1317

class NonSpare_159(NonSpare):
    name = "050"
    title = "Composed Track Number"
    rule = RuleVariation_1247

class NonSpare_1228(NonSpare):
    name = "MODE3A"
    title = "(Mode 3/A Code) 4 Digits, Octal Representation"
    rule = RuleVariation_751

class Item_539(Item):
    non_spare = NonSpare_1228

class Variation_991(Group):
    bit_size = 16
    items_list = [Item_3, Item_539]
    items_dict = {"MODE3A": NonSpare_1228}

class RuleVariation_961(RuleVariationContextFree):
    variation = Variation_991

class NonSpare_188(NonSpare):
    name = "060"
    title = "Track Mode 3/A"
    rule = RuleVariation_961

class NonSpare_458(NonSpare):
    name = "400"
    title = "Callsign"
    rule = RuleVariation_378

class NonSpare_464(NonSpare):
    name = "410"
    title = "Plan Number"
    rule = RuleVariation_241

class NonSpare_957(NonSpare):
    name = "GATOAT"
    title = ""
    rule = RuleVariation_111

class Item_339(Item):
    non_spare = NonSpare_957

class NonSpare_927(NonSpare):
    name = "FR1FR2"
    title = ""
    rule = RuleVariation_559

class Item_320(Item):
    non_spare = NonSpare_927

class NonSpare_1644(NonSpare):
    name = "SP3"
    title = ""
    rule = RuleVariation_663

class Item_864(Item):
    non_spare = NonSpare_1644

class Variation_788(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_0

class RuleVariation_759(RuleVariationContextFree):
    variation = Variation_788

class NonSpare_1643(NonSpare):
    name = "SP2"
    title = ""
    rule = RuleVariation_759

class Item_863(Item):
    non_spare = NonSpare_1643

class Variation_867(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_0

class RuleVariation_838(RuleVariationContextFree):
    variation = Variation_867

class NonSpare_1642(NonSpare):
    name = "SP1"
    title = ""
    rule = RuleVariation_838

class Item_862(Item):
    non_spare = NonSpare_1642

class Variation_1075(Group):
    bit_size = 8
    items_list = [Item_339, Item_320, Item_864, Item_863, Item_862, Item_29]
    items_dict = {"GATOAT": NonSpare_957, "FR1FR2": NonSpare_927, "SP3": NonSpare_1644, "SP2": NonSpare_1643, "SP1": NonSpare_1642}

class RuleVariation_1034(RuleVariationContextFree):
    variation = Variation_1075

class NonSpare_466(NonSpare):
    name = "420"
    title = "Flight Category"
    rule = RuleVariation_1034

class NonSpare_471(NonSpare):
    name = "440"
    title = "Departure Aerodrome"
    rule = RuleVariation_360

class NonSpare_472(NonSpare):
    name = "450"
    title = "Destination Aerodrome"
    rule = RuleVariation_360

class Content_724(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "FL"

class RuleContent_723(RuleContentContextFree):
    variation = Content_724

class Variation_313(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_723

class RuleVariation_307(RuleVariationContextFree):
    variation = Variation_313

class NonSpare_475(NonSpare):
    name = "480"
    title = "Current Cleared Flight Level"
    rule = RuleVariation_307

class NonSpare_673(NonSpare):
    name = "CEN"
    title = "Centre"
    rule = RuleVariation_154

class Item_143(Item):
    non_spare = NonSpare_673

class NonSpare_1366(NonSpare):
    name = "POS"
    title = "Position"
    rule = RuleVariation_154

class Item_656(Item):
    non_spare = NonSpare_1366

class Variation_1031(Group):
    bit_size = 16
    items_list = [Item_143, Item_656]
    items_dict = {"CEN": NonSpare_673, "POS": NonSpare_1366}

class RuleVariation_994(RuleVariationContextFree):
    variation = Variation_1031

class NonSpare_476(NonSpare):
    name = "490"
    title = "Current Control Position"
    rule = RuleVariation_994

class NonSpare_469(NonSpare):
    name = "430"
    title = "Type of Aircraft"
    rule = RuleVariation_360

class NonSpare_470(NonSpare):
    name = "435"
    title = "Wake Turbulence Category"
    rule = RuleVariation_187

class NonSpare_1311(NonSpare):
    name = "OCT1"
    title = "1st Octal Digit"
    rule = RuleVariation_735

class Item_615(Item):
    non_spare = NonSpare_1311

class Variation_958(Element):
    bit_offset8 = 7
    bit_size = 3
    rule = RuleContent_0

class RuleVariation_929(RuleVariationContextFree):
    variation = Variation_958

class NonSpare_1312(NonSpare):
    name = "OCT2"
    title = "2nd Octal Digit"
    rule = RuleVariation_929

class Item_616(Item):
    non_spare = NonSpare_1312

class Variation_574(Element):
    bit_offset8 = 2
    bit_size = 3
    rule = RuleContent_0

class RuleVariation_564(RuleVariationContextFree):
    variation = Variation_574

class NonSpare_1313(NonSpare):
    name = "OCT3"
    title = "3rd Octal Digit"
    rule = RuleVariation_564

class Item_617(Item):
    non_spare = NonSpare_1313

class NonSpare_1314(NonSpare):
    name = "OCT4"
    title = "4th Octal Digit"
    rule = RuleVariation_829

class Item_618(Item):
    non_spare = NonSpare_1314

class Variation_995(Group):
    bit_size = 16
    items_list = [Item_3, Item_615, Item_616, Item_617, Item_618]
    items_dict = {"OCT1": NonSpare_1311, "OCT2": NonSpare_1312, "OCT3": NonSpare_1313, "OCT4": NonSpare_1314}

class Variation_1336(Repetitive):
    rep_bytes = 1
    variation = Variation_995

class RuleVariation_1266(RuleVariationContextFree):
    variation = Variation_1336

class NonSpare_473(NonSpare):
    name = "460"
    title = "Allocated SSR Codes"
    rule = RuleVariation_1266

class Content_403(ContentTable):
    values = {0: "Plan Number", 1: "Unit 1 internal flight number", 2: "Unit 2 internal flight number", 3: "Unit 3 internal flight number"}

class RuleContent_403(RuleContentContextFree):
    variation = Content_403

class Variation_106(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_403

class RuleVariation_106(RuleVariationContextFree):
    variation = Variation_106

class NonSpare_1858(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_106

class Item_1019(Item):
    non_spare = NonSpare_1858

class Content_592(ContentInteger):
    signedness = Unsigned

class RuleContent_592(RuleContentContextFree):
    variation = Content_592

class Variation_865(Element):
    bit_offset8 = 5
    bit_size = 27
    rule = RuleContent_592

class RuleVariation_836(RuleVariationContextFree):
    variation = Variation_865

class NonSpare_1274(NonSpare):
    name = "NBR"
    title = ""
    rule = RuleVariation_836

class Item_579(Item):
    non_spare = NonSpare_1274

class Variation_1191(Group):
    bit_size = 32
    items_list = [Item_1019, Item_13, Item_579]
    items_dict = {"TYP": NonSpare_1858, "NBR": NonSpare_1274}

class RuleVariation_1134(RuleVariationContextFree):
    variation = Variation_1191

class NonSpare_1046(NonSpare):
    name = "IFI"
    title = "IFPS FLIGHT ID"
    rule = RuleVariation_1134

class Content_518(ContentTable):
    values = {0: "Unknown", 1: "Approved", 2: "Exempt", 3: "Not approved"}

class RuleContent_518(RuleContentContextFree):
    variation = Content_518

class Variation_857(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_518

class RuleVariation_828(RuleVariationContextFree):
    variation = Variation_857

class NonSpare_1548(NonSpare):
    name = "RVSM"
    title = ""
    rule = RuleVariation_828

class Item_800(Item):
    non_spare = NonSpare_1548

class Variation_950(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_367

class RuleVariation_921(RuleVariationContextFree):
    variation = Variation_950

class NonSpare_1008(NonSpare):
    name = "HPR"
    title = ""
    rule = RuleVariation_921

class Item_377(Item):
    non_spare = NonSpare_1008

class Variation_1005(Group):
    bit_size = 8
    items_list = [Item_4, Item_800, Item_377]
    items_dict = {"RVSM": NonSpare_1548, "HPR": NonSpare_1008}

class RuleVariation_974(RuleVariationContextFree):
    variation = Variation_1005

class NonSpare_1546(NonSpare):
    name = "RVP"
    title = "RVSM & Flight Priority"
    rule = RuleVariation_974

class Variation_194(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_581

class RuleVariation_188(RuleVariationContextFree):
    variation = Variation_194

class NonSpare_1304(NonSpare):
    name = "NU1"
    title = "First Number"
    rule = RuleVariation_188

class Item_608(Item):
    non_spare = NonSpare_1304

class NonSpare_1305(NonSpare):
    name = "NU2"
    title = "Second Number"
    rule = RuleVariation_188

class Item_609(Item):
    non_spare = NonSpare_1305

class NonSpare_1123(NonSpare):
    name = "LTR"
    title = "Letter"
    rule = RuleVariation_188

class Item_476(Item):
    non_spare = NonSpare_1123

class Variation_1118(Group):
    bit_size = 24
    items_list = [Item_608, Item_609, Item_476]
    items_dict = {"NU1": NonSpare_1304, "NU2": NonSpare_1305, "LTR": NonSpare_1123}

class RuleVariation_1073(RuleVariationContextFree):
    variation = Variation_1118

class NonSpare_1478(NonSpare):
    name = "RDS"
    title = "Runway Designation"
    rule = RuleVariation_1073

class Content_431(ContentTable):
    values = {0: "Scheduled Off-Block Time", 1: "Estimated Off-Block Time", 2: "Estimated Take-Off Time", 3: "Actual Off-Block Time", 4: "Predicted Time at Runway Hold", 5: "Actual Time at Runway Hold", 6: "Actual Line-Up Time", 7: "Actual Take-Off Time", 8: "Estimated Time of Arrival", 9: "Predicted Landing Time", 10: "Actual Landing Time", 11: "Actual Time off Runway", 12: "Predicted Time to Gate", 13: "Actual On-Block Time"}

class RuleContent_431(RuleContentContextFree):
    variation = Content_431

class Variation_139(Element):
    bit_offset8 = 0
    bit_size = 5
    rule = RuleContent_431

class RuleVariation_139(RuleVariationContextFree):
    variation = Variation_139

class NonSpare_1861(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_139

class Item_1022(Item):
    non_spare = NonSpare_1861

class Content_487(ContentTable):
    values = {0: "Today", 1: "Yesterday", 2: "Tomorrow", 3: "Invalid"}

class RuleContent_487(RuleContentContextFree):
    variation = Content_487

class Variation_854(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_487

class RuleVariation_825(RuleVariationContextFree):
    variation = Variation_854

class NonSpare_811(NonSpare):
    name = "DAY"
    title = ""
    rule = RuleVariation_825

class Item_233(Item):
    non_spare = NonSpare_811

class NonSpare_1003(NonSpare):
    name = "HOR"
    title = ""
    rule = RuleVariation_660

class Item_373(Item):
    non_spare = NonSpare_1003

class NonSpare_1211(NonSpare):
    name = "MIN"
    title = ""
    rule = RuleVariation_570

class Item_523(Item):
    non_spare = NonSpare_1211

class NonSpare_603(NonSpare):
    name = "AVS"
    title = ""
    rule = RuleVariation_70

class Item_90(Item):
    non_spare = NonSpare_603

class NonSpare_1607(NonSpare):
    name = "SEC"
    title = ""
    rule = RuleVariation_570

class Item_833(Item):
    non_spare = NonSpare_1607

class Variation_1193(Group):
    bit_size = 32
    items_list = [Item_1022, Item_233, Item_30, Item_373, Item_1, Item_523, Item_90, Item_7, Item_833]
    items_dict = {"TYP": NonSpare_1861, "DAY": NonSpare_811, "HOR": NonSpare_1003, "MIN": NonSpare_1211, "AVS": NonSpare_603, "SEC": NonSpare_1607}

class Variation_1362(Repetitive):
    rep_bytes = 1
    variation = Variation_1193

class RuleVariation_1292(RuleVariationContextFree):
    variation = Variation_1362

class NonSpare_1788(NonSpare):
    name = "TOD"
    title = "Time of Departure / Arrival"
    rule = RuleVariation_1292

class Content_200(ContentTable):
    values = {0: "Empty", 1: "Occupied", 2: "Unknown", 3: "Invalid"}

class RuleContent_200(RuleContentContextFree):
    variation = Content_200

class Variation_98(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_200

class RuleVariation_98(RuleVariationContextFree):
    variation = Variation_98

class NonSpare_875(NonSpare):
    name = "EMP"
    title = ""
    rule = RuleVariation_98

class Item_280(Item):
    non_spare = NonSpare_875

class Content_43(ContentTable):
    values = {0: "Available", 1: "Not available", 2: "Unknown", 3: "Invalid"}

class RuleContent_43(RuleContentContextFree):
    variation = Content_43

class Variation_566(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_43

class RuleVariation_556(RuleVariationContextFree):
    variation = Variation_566

class NonSpare_601(NonSpare):
    name = "AVL"
    title = ""
    rule = RuleVariation_556

class Item_88(Item):
    non_spare = NonSpare_601

class Variation_1050(Group):
    bit_size = 8
    items_list = [Item_280, Item_88, Item_22]
    items_dict = {"EMP": NonSpare_875, "AVL": NonSpare_601}

class RuleVariation_1009(RuleVariationContextFree):
    variation = Variation_1050

class NonSpare_1707(NonSpare):
    name = "STS"
    title = "Stand Status"
    rule = RuleVariation_1009

class NonSpare_1623(NonSpare):
    name = "SID"
    title = "Standard Instrument Departure"
    rule = RuleVariation_378

class NonSpare_1680(NonSpare):
    name = "STAR"
    title = "Standard Instrument Arrival"
    rule = RuleVariation_378

class Variation_1409(Compound):
    items_list = [NonSpare_1046, NonSpare_1546, NonSpare_1478, NonSpare_1788, NonSpare_591, NonSpare_1707, NonSpare_1623, NonSpare_1680]
    items_dict = {"IFI": NonSpare_1046, "RVP": NonSpare_1546, "RDS": NonSpare_1478, "TOD": NonSpare_1788, "AST": NonSpare_591, "STS": NonSpare_1707, "SID": NonSpare_1623, "STAR": NonSpare_1680}

class RuleVariation_1339(RuleVariationContextFree):
    variation = Variation_1409

class NonSpare_481(NonSpare):
    name = "500"
    title = "Supplementary Flight Data"
    rule = RuleVariation_1339

class Record_47(Record):
    items_list = [NonSpare_48, NonSpare_68, NonSpare_72, NonSpare_124, NonSpare_88, NonSpare_142, NonSpare_159, NonSpare_188, NonSpare_458, NonSpare_464, NonSpare_466, NonSpare_471, NonSpare_472, NonSpare_475, NonSpare_476, NonSpare_469, NonSpare_470, NonSpare_473, NonSpare_481, UapItemSpare, NonSpare_1481]
    items_dict = {"010": NonSpare_48, "015": NonSpare_68, "018": NonSpare_72, "035": NonSpare_124, "020": NonSpare_88, "040": NonSpare_142, "050": NonSpare_159, "060": NonSpare_188, "400": NonSpare_458, "410": NonSpare_464, "420": NonSpare_466, "440": NonSpare_471, "450": NonSpare_472, "480": NonSpare_475, "490": NonSpare_476, "430": NonSpare_469, "435": NonSpare_470, "460": NonSpare_473, "500": NonSpare_481, "RE": NonSpare_1481}

class Uap_43(UapSingle):
    record = Record_47

class Asterix_35(AstCat):
    category = 32
    edition = (1, 1)
    uap = Uap_43

class NonSpare_34(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_560(ContentTable):
    values = {1: "North marker message", 2: "Sector crossing message", 3: "Geographical filtering message", 4: "Jamming strobe message"}

class RuleContent_560(RuleContentContextFree):
    variation = Content_560

class Variation_179(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_560

class RuleVariation_173(RuleVariationContextFree):
    variation = Variation_179

class NonSpare_3(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_173

class NonSpare_102(NonSpare):
    name = "030"
    title = "Time of Day"
    rule = RuleVariation_356

class NonSpare_80(NonSpare):
    name = "020"
    title = "Sector Number"
    rule = RuleVariation_230

class NonSpare_145(NonSpare):
    name = "041"
    title = "Antenna Rotation Speed"
    rule = RuleVariation_320

class Content_439(ContentTable):
    values = {0: "System is released for operational use", 1: "Operational use of System is inhibited, i.e. the data shall be discarded by an operational SDPS"}

class RuleContent_439(RuleContentContextFree):
    variation = Content_439

class Variation_73(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_439

class RuleVariation_73(RuleVariationContextFree):
    variation = Variation_73

class NonSpare_1290(NonSpare):
    name = "NOGO"
    title = "Operational Release Status of the System"
    rule = RuleVariation_73

class Item_595(Item):
    non_spare = NonSpare_1290

class Content_411(ContentTable):
    values = {0: "RDPC-1 selected", 1: "RDPC-2 selected"}

class RuleContent_411(RuleContentContextFree):
    variation = Content_411

class Variation_438(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_411

class RuleVariation_428(RuleVariationContextFree):
    variation = Variation_438

class NonSpare_1475(NonSpare):
    name = "RDPC"
    title = "Radar Data Processor Chain Selection Status"
    rule = RuleVariation_428

class Item_749(Item):
    non_spare = NonSpare_1475

class Content_165(ContentTable):
    values = {0: "Default situation", 1: "Reset of RDPC"}

class RuleContent_165(RuleContentContextFree):
    variation = Content_165

class Variation_508(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_165

class RuleVariation_498(RuleVariationContextFree):
    variation = Variation_508

class NonSpare_1476(NonSpare):
    name = "RDPR"
    title = "Event to Signal a Reset/restart of the Selected Radar Data Processor Chain, I.e. Expect a New Assignment of Track Numbers"
    rule = RuleVariation_498

class Item_750(Item):
    non_spare = NonSpare_1476

class Content_192(ContentTable):
    values = {0: "Default, no overload", 1: "Overload in RDP"}

class RuleContent_192(RuleContentContextFree):
    variation = Content_192

class Variation_613(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_192

class RuleVariation_603(RuleVariationContextFree):
    variation = Variation_613

class NonSpare_1332(NonSpare):
    name = "OVLRDP"
    title = "Radar Data Processor Overload Indicator"
    rule = RuleVariation_603

class Item_635(Item):
    non_spare = NonSpare_1332

class Variation_693(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_193

class RuleVariation_683(RuleVariationContextFree):
    variation = Variation_693

class NonSpare_1335(NonSpare):
    name = "OVLXMT"
    title = "Transmission Subsystem Overload Status"
    rule = RuleVariation_683

class Item_638(Item):
    non_spare = NonSpare_1335

class Content_281(ContentTable):
    values = {0: "Monitoring system connected", 1: "Monitoring system disconnected"}

class RuleContent_281(RuleContentContextFree):
    variation = Content_281

class Variation_832(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_281

class RuleVariation_803(RuleVariationContextFree):
    variation = Variation_832

class NonSpare_1250(NonSpare):
    name = "MSC"
    title = "Monitoring System Connected Status"
    rule = RuleVariation_803

class Item_559(Item):
    non_spare = NonSpare_1250

class Variation_925(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_534

class RuleVariation_896(RuleVariationContextFree):
    variation = Variation_925

class NonSpare_1843(NonSpare):
    name = "TSV"
    title = "Time Source Validity"
    rule = RuleVariation_896

class Item_1007(Item):
    non_spare = NonSpare_1843

class Variation_1115(Group):
    bit_size = 8
    items_list = [Item_595, Item_749, Item_750, Item_635, Item_638, Item_559, Item_1007, Item_29]
    items_dict = {"NOGO": NonSpare_1290, "RDPC": NonSpare_1475, "RDPR": NonSpare_1476, "OVLRDP": NonSpare_1332, "OVLXMT": NonSpare_1335, "MSC": NonSpare_1250, "TSV": NonSpare_1843}

class RuleVariation_1070(RuleVariationContextFree):
    variation = Variation_1115

class NonSpare_732(NonSpare):
    name = "COM"
    title = "Common Part"
    rule = RuleVariation_1070

class Content_32(ContentTable):
    values = {0: "Antenna 1", 1: "Antenna 2"}

class RuleContent_32(RuleContentContextFree):
    variation = Content_32

class Variation_11(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_32

class RuleVariation_11(RuleVariationContextFree):
    variation = Variation_11

class NonSpare_567(NonSpare):
    name = "ANT"
    title = "Selected Antenna"
    rule = RuleVariation_11

class Item_71(Item):
    non_spare = NonSpare_567

class Content_320(ContentTable):
    values = {0: "No channel selected", 1: "Channel A only selected", 2: "Channel B only selected", 3: "Diversity mode ; Channel A and B selected"}

class RuleContent_320(RuleContentContextFree):
    variation = Content_320

class Variation_459(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_320

class RuleVariation_449(RuleVariationContextFree):
    variation = Variation_459

class NonSpare_680(NonSpare):
    name = "CHAB"
    title = "Channel A/B Selection Status"
    rule = RuleVariation_449

class Item_146(Item):
    non_spare = NonSpare_680

class Variation_628(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_354

class RuleVariation_618(RuleVariationContextFree):
    variation = Variation_628

class NonSpare_1329(NonSpare):
    name = "OVL"
    title = "Overload Condition"
    rule = RuleVariation_618

class Item_632(Item):
    non_spare = NonSpare_1329

class Variation_708(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_281

class RuleVariation_698(RuleVariationContextFree):
    variation = Variation_708

class NonSpare_1249(NonSpare):
    name = "MSC"
    title = "Monitoring System Connected Status"
    rule = RuleVariation_698

class Item_558(Item):
    non_spare = NonSpare_1249

class Variation_1014(Group):
    bit_size = 8
    items_list = [Item_71, Item_146, Item_632, Item_558, Item_25]
    items_dict = {"ANT": NonSpare_567, "CHAB": NonSpare_680, "OVL": NonSpare_1329, "MSC": NonSpare_1249}

class RuleVariation_982(RuleVariationContextFree):
    variation = Variation_1014

class NonSpare_1394(NonSpare):
    name = "PSR"
    title = "Specific Status Information for a PSR Sensor"
    rule = RuleVariation_982

class Content_322(ContentTable):
    values = {0: "No channel selected", 1: "Channel A only selected", 2: "Channel B only selected", 3: "Invalid combination"}

class RuleContent_322(RuleContentContextFree):
    variation = Content_322

class Variation_461(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_322

class RuleVariation_451(RuleVariationContextFree):
    variation = Variation_461

class NonSpare_682(NonSpare):
    name = "CHAB"
    title = "Channel A/B Selection Status"
    rule = RuleVariation_451

class Item_148(Item):
    non_spare = NonSpare_682

class NonSpare_1251(NonSpare):
    name = "MSC"
    title = "Monitoring System Connected Status:"
    rule = RuleVariation_698

class Item_560(Item):
    non_spare = NonSpare_1251

class Variation_1016(Group):
    bit_size = 8
    items_list = [Item_71, Item_148, Item_632, Item_560, Item_25]
    items_dict = {"ANT": NonSpare_567, "CHAB": NonSpare_682, "OVL": NonSpare_1329, "MSC": NonSpare_1251}

class RuleVariation_984(RuleVariationContextFree):
    variation = Variation_1016

class NonSpare_1673(NonSpare):
    name = "SSR"
    title = "Specific Status Information for a SSR Sensor"
    rule = RuleVariation_984

class Content_321(ContentTable):
    values = {0: "No channel selected", 1: "Channel A only selected", 2: "Channel B only selected", 3: "Illegal combination"}

class RuleContent_321(RuleContentContextFree):
    variation = Content_321

class Variation_460(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_321

class RuleVariation_450(RuleVariationContextFree):
    variation = Variation_460

class NonSpare_681(NonSpare):
    name = "CHAB"
    title = "Channel A/B Selection Status"
    rule = RuleVariation_450

class Item_147(Item):
    non_spare = NonSpare_681

class NonSpare_1334(NonSpare):
    name = "OVLSUR"
    title = "Overload Condition"
    rule = RuleVariation_618

class Item_637(Item):
    non_spare = NonSpare_1334

class Content_51(ContentTable):
    values = {0: "Channel A in use", 1: "Channel B in use"}

class RuleContent_51(RuleContentContextFree):
    variation = Content_51

class Variation_796(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_51

class RuleVariation_767(RuleVariationContextFree):
    variation = Variation_796

class NonSpare_1577(NonSpare):
    name = "SCF"
    title = "Channel A/B Selection Status for Surveillance Co-ordination Function"
    rule = RuleVariation_767

class Item_819(Item):
    non_spare = NonSpare_1577

class Variation_872(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_51

class RuleVariation_843(RuleVariationContextFree):
    variation = Variation_872

class NonSpare_835(NonSpare):
    name = "DLF"
    title = "Channel A/B Selection Status for Data Link Function"
    rule = RuleVariation_843

class Item_256(Item):
    non_spare = NonSpare_835

class Variation_949(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_354

class RuleVariation_920(RuleVariationContextFree):
    variation = Variation_949

class NonSpare_1333(NonSpare):
    name = "OVLSCF"
    title = "Overload in Surveillance Co-ordination Function"
    rule = RuleVariation_920

class Item_636(Item):
    non_spare = NonSpare_1333

class Variation_60(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_354

class RuleVariation_60(RuleVariationContextFree):
    variation = Variation_60

class NonSpare_1331(NonSpare):
    name = "OVLDLF"
    title = "Overload in Data Link Function"
    rule = RuleVariation_60

class Item_634(Item):
    non_spare = NonSpare_1331

class Variation_1015(Group):
    bit_size = 16
    items_list = [Item_71, Item_147, Item_637, Item_560, Item_819, Item_256, Item_636, Item_634, Item_10]
    items_dict = {"ANT": NonSpare_567, "CHAB": NonSpare_681, "OVLSUR": NonSpare_1334, "MSC": NonSpare_1251, "SCF": NonSpare_1577, "DLF": NonSpare_835, "OVLSCF": NonSpare_1333, "OVLDLF": NonSpare_1331}

class RuleVariation_983(RuleVariationContextFree):
    variation = Variation_1015

class NonSpare_1187(NonSpare):
    name = "MDS"
    title = "Specific Status Information for a Mode S Sensor"
    rule = RuleVariation_983

class Variation_1399(Compound):
    items_list = [NonSpare_732, None, None, NonSpare_1394, NonSpare_1673, NonSpare_1187]
    items_dict = {"COM": NonSpare_732, "PSR": NonSpare_1394, "SSR": NonSpare_1673, "MDS": NonSpare_1187}

class RuleVariation_1329(RuleVariationContextFree):
    variation = Variation_1399

class NonSpare_170(NonSpare):
    name = "050"
    title = "System Configuration and Status"
    rule = RuleVariation_1329

class Content_355(ContentTable):
    values = {0: "No reduction active", 1: "Reduction step 1 active", 2: "Reduction step 2 active", 3: "Reduction step 3 active", 4: "Reduction step 4 active", 5: "Reduction step 5 active", 6: "Reduction step 6 active", 7: "Reduction step 7 active"}

class RuleContent_355(RuleContentContextFree):
    variation = Content_355

class Variation_469(Element):
    bit_offset8 = 1
    bit_size = 3
    rule = RuleContent_355

class RuleVariation_459(RuleVariationContextFree):
    variation = Variation_469

class NonSpare_1486(NonSpare):
    name = "REDRDP"
    title = "Reduction Steps in Use for An Overload of the RDP"
    rule = RuleVariation_459

class Item_757(Item):
    non_spare = NonSpare_1486

class Variation_758(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_355

class RuleVariation_738(RuleVariationContextFree):
    variation = Variation_758

class NonSpare_1487(NonSpare):
    name = "REDXMT"
    title = "Reduction Steps in Use for An Overload of the Transmission Subsystem"
    rule = RuleVariation_738

class Item_758(Item):
    non_spare = NonSpare_1487

class Variation_963(Group):
    bit_size = 8
    items_list = [Item_0, Item_757, Item_758, Item_29]
    items_dict = {"REDRDP": NonSpare_1486, "REDXMT": NonSpare_1487}

class RuleVariation_934(RuleVariationContextFree):
    variation = Variation_963

class NonSpare_731(NonSpare):
    name = "COM"
    title = "Common Part"
    rule = RuleVariation_934

class Content_246(ContentTable):
    values = {0: "Linear polarization", 1: "Circular polarization"}

class RuleContent_246(RuleContentContextFree):
    variation = Content_246

class Variation_48(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_246

class RuleVariation_48(RuleVariationContextFree):
    variation = Variation_48

class NonSpare_1360(NonSpare):
    name = "POL"
    title = "Polarization in Use by PSR"
    rule = RuleVariation_48

class Item_654(Item):
    non_spare = NonSpare_1360

class NonSpare_1484(NonSpare):
    name = "REDRAD"
    title = "Reduction Steps in Use as Result of An Overload Within the PSR Subsystem"
    rule = RuleVariation_459

class Item_755(Item):
    non_spare = NonSpare_1484

class Content_429(ContentTable):
    values = {0: "STC Map-1", 1: "STC Map-2", 2: "STC Map-3", 3: "STC Map-4"}

class RuleContent_429(RuleContentContextFree):
    variation = Content_429

class Variation_739(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_429

class RuleVariation_729(RuleVariationContextFree):
    variation = Variation_739

class NonSpare_1694(NonSpare):
    name = "STC"
    title = "Sensitivity Time Control Map in Use"
    rule = RuleVariation_729

class Item_902(Item):
    non_spare = NonSpare_1694

class Variation_1122(Group):
    bit_size = 8
    items_list = [Item_654, Item_755, Item_902, Item_27]
    items_dict = {"POL": NonSpare_1360, "REDRAD": NonSpare_1484, "STC": NonSpare_1694}

class RuleVariation_1076(RuleVariationContextFree):
    variation = Variation_1122

class NonSpare_1393(NonSpare):
    name = "PSR"
    title = "Specific Processing Mode Information for a PSR Sensor"
    rule = RuleVariation_1076

class Variation_124(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_355

class RuleVariation_124(RuleVariationContextFree):
    variation = Variation_124

class NonSpare_1485(NonSpare):
    name = "REDRAD"
    title = "Reduction Steps in Use as Result of An Overload Within the SSR Subsystem"
    rule = RuleVariation_124

class Item_756(Item):
    non_spare = NonSpare_1485

class Variation_1139(Group):
    bit_size = 8
    items_list = [Item_756, Item_19]
    items_dict = {"REDRAD": NonSpare_1485}

class RuleVariation_1092(RuleVariationContextFree):
    variation = Variation_1139

class NonSpare_1672(NonSpare):
    name = "SSR"
    title = "Specific Processing Mode Information for a SSR Sensor"
    rule = RuleVariation_1092

class NonSpare_1483(NonSpare):
    name = "REDRAD"
    title = "Reduction Steps in Use as Result of An Overload Within the Mode S Subsystem"
    rule = RuleVariation_124

class Item_754(Item):
    non_spare = NonSpare_1483

class Content_40(ContentTable):
    values = {0: "Autonomous", 1: "Not autonomous"}

class RuleContent_40(RuleContentContextFree):
    variation = Content_40

class Variation_594(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_40

class RuleVariation_584(RuleVariationContextFree):
    variation = Variation_594

class NonSpare_695(NonSpare):
    name = "CLU"
    title = "Cluster State"
    rule = RuleVariation_584

class Item_158(Item):
    non_spare = NonSpare_695

class Variation_1138(Group):
    bit_size = 8
    items_list = [Item_754, Item_158, Item_22]
    items_dict = {"REDRAD": NonSpare_1483, "CLU": NonSpare_695}

class RuleVariation_1091(RuleVariationContextFree):
    variation = Variation_1138

class NonSpare_1186(NonSpare):
    name = "MDS"
    title = "Specific Processing Mode Information for a Mode S Sensor"
    rule = RuleVariation_1091

class Variation_1398(Compound):
    items_list = [NonSpare_731, None, None, NonSpare_1393, NonSpare_1672, NonSpare_1186]
    items_dict = {"COM": NonSpare_731, "PSR": NonSpare_1393, "SSR": NonSpare_1672, "MDS": NonSpare_1186}

class RuleVariation_1328(RuleVariationContextFree):
    variation = Variation_1398

class NonSpare_187(NonSpare):
    name = "060"
    title = "System Processing Mode"
    rule = RuleVariation_1328

class Content_335(ContentTable):
    values = {0: "No detection (number of misses)", 1: "Single PSR target reports", 2: "Single SSR target reports (Non-Mode S)", 3: "SSR+PSR target reports (Non-Mode S)", 4: "Single All-Call target reports (Mode S)", 5: "Single Roll-Call target reports (Mode S)", 6: "All-Call + PSR (Mode S) target reports", 7: "Roll-Call + PSR (Mode S) target reports", 8: "Filter for Weather data", 9: "Filter for Jamming Strobe", 10: "Filter for PSR data", 11: "Filter for SSR/Mode S data", 12: "Filter for SSR/Mode S+PSR data", 13: "Filter for Enhanced Surveillance data", 14: "Filter for PSR+Enhanced Surveillance", 15: "Filter for PSR+Enhanced Surveillance + SSR/Mode S data not in Area of Prime Interest", 16: "Filter for PSR+Enhanced Surveillance + all SSR/Mode S data"}

class RuleContent_335(RuleContentContextFree):
    variation = Content_335

class Variation_137(Element):
    bit_offset8 = 0
    bit_size = 5
    rule = RuleContent_335

class RuleVariation_137(RuleVariationContextFree):
    variation = Variation_137

class NonSpare_1869(NonSpare):
    name = "TYP"
    title = "Type of Message Counter"
    rule = RuleVariation_137

class Item_1029(Item):
    non_spare = NonSpare_1869

class Variation_863(Element):
    bit_offset8 = 5
    bit_size = 11
    rule = RuleContent_585

class RuleVariation_834(RuleVariationContextFree):
    variation = Variation_863

class NonSpare_749(NonSpare):
    name = "COUNT"
    title = "COUNTER"
    rule = RuleVariation_834

class Item_192(Item):
    non_spare = NonSpare_749

class Variation_1199(Group):
    bit_size = 16
    items_list = [Item_1029, Item_192]
    items_dict = {"TYP": NonSpare_1869, "COUNT": NonSpare_749}

class Variation_1365(Repetitive):
    rep_bytes = 1
    variation = Variation_1199

class RuleVariation_1295(RuleVariationContextFree):
    variation = Variation_1365

class NonSpare_193(NonSpare):
    name = "070"
    title = "Message Count Values"
    rule = RuleVariation_1295

class Content_747(ContentQuantity):
    signedness = Unsigned
    lsb = 3.90625e-3
    unit = "NM"

class RuleContent_746(RuleContentContextFree):
    variation = Content_747

class Variation_328(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_746

class RuleVariation_322(RuleVariationContextFree):
    variation = Variation_328

class NonSpare_1507(NonSpare):
    name = "RHOST"
    title = "Rho Start"
    rule = RuleVariation_322

class Item_778(Item):
    non_spare = NonSpare_1507

class NonSpare_1506(NonSpare):
    name = "RHOEND"
    title = "Rho End"
    rule = RuleVariation_322

class Item_777(Item):
    non_spare = NonSpare_1506

class NonSpare_1764(NonSpare):
    name = "THETAST"
    title = "Theta Start"
    rule = RuleVariation_329

class Item_950(Item):
    non_spare = NonSpare_1764

class NonSpare_1763(NonSpare):
    name = "THETAEND"
    title = "Theta End"
    rule = RuleVariation_329

class Item_949(Item):
    non_spare = NonSpare_1763

class Variation_1144(Group):
    bit_size = 64
    items_list = [Item_778, Item_777, Item_950, Item_949]
    items_dict = {"RHOST": NonSpare_1507, "RHOEND": NonSpare_1506, "THETAST": NonSpare_1764, "THETAEND": NonSpare_1763}

class RuleVariation_1097(RuleVariationContextFree):
    variation = Variation_1144

class NonSpare_252(NonSpare):
    name = "100"
    title = "Generic Polar Window"
    rule = RuleVariation_1097

class Content_238(ContentTable):
    values = {0: "Invalid value", 1: "Filter for Weather data", 2: "Filter for Jamming Strobe", 3: "Filter for PSR data", 4: "Filter for SSR/Mode S data", 5: "Filter for SSR/Mode S + PSR data", 6: "Enhanced Surveillance data", 7: "Filter for PSR+Enhanced Surveillance data", 8: "Filter for PSR+Enhanced Surveillance + SSR/Mode S data not in Area of Prime Interest", 9: "Filter for PSR+Enhanced Surveillance + all SSR/Mode S data"}

class RuleContent_238(RuleContentContextFree):
    variation = Content_238

class Variation_162(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_238

class RuleVariation_157(RuleVariationContextFree):
    variation = Variation_162

class NonSpare_265(NonSpare):
    name = "110"
    title = "Data Filter"
    rule = RuleVariation_157

class Variation_249(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_598

class RuleVariation_243(RuleVariationContextFree):
    variation = Variation_249

class NonSpare_1000(NonSpare):
    name = "HGT"
    title = "Height of Data Source"
    rule = RuleVariation_243

class Item_371(Item):
    non_spare = NonSpare_1000

class Content_665(ContentQuantity):
    signedness = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

class RuleContent_665(RuleContentContextFree):
    variation = Content_665

class Variation_351(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_665

class RuleVariation_345(RuleVariationContextFree):
    variation = Variation_351

class NonSpare_1104(NonSpare):
    name = "LON"
    title = "Longitude"
    rule = RuleVariation_345

class Item_459(Item):
    non_spare = NonSpare_1104

class Variation_1081(Group):
    bit_size = 64
    items_list = [Item_371, Item_433, Item_459]
    items_dict = {"HGT": NonSpare_1000, "LAT": NonSpare_1076, "LON": NonSpare_1104}

class RuleVariation_1040(RuleVariationContextFree):
    variation = Variation_1081

class NonSpare_274(NonSpare):
    name = "120"
    title = "3D-Position Of Data Source"
    rule = RuleVariation_1040

class NonSpare_1512(NonSpare):
    name = "RNG"
    title = "Range Error"
    rule = RuleVariation_200

class Item_783(Item):
    non_spare = NonSpare_1512

class NonSpare_619(NonSpare):
    name = "AZM"
    title = "Azimuth Error"
    rule = RuleVariation_205

class Item_103(Item):
    non_spare = NonSpare_619

class Variation_1146(Group):
    bit_size = 16
    items_list = [Item_783, Item_103]
    items_dict = {"RNG": NonSpare_1512, "AZM": NonSpare_619}

class RuleVariation_1098(RuleVariationContextFree):
    variation = Variation_1146

class NonSpare_229(NonSpare):
    name = "090"
    title = "Collimation Error"
    rule = RuleVariation_1098

class Record_14(Record):
    items_list = [NonSpare_34, NonSpare_3, NonSpare_102, NonSpare_80, NonSpare_145, NonSpare_170, NonSpare_187, NonSpare_193, NonSpare_252, NonSpare_265, NonSpare_274, NonSpare_229, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_34, "000": NonSpare_3, "030": NonSpare_102, "020": NonSpare_80, "041": NonSpare_145, "050": NonSpare_170, "060": NonSpare_187, "070": NonSpare_193, "100": NonSpare_252, "110": NonSpare_265, "120": NonSpare_274, "090": NonSpare_229, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_14(UapSingle):
    record = Record_14

class Asterix_36(AstCat):
    category = 34
    edition = (1, 27)
    uap = Uap_14

class Content_561(ContentTable):
    values = {1: "North marker message", 2: "Sector crossing message", 3: "Geographical filtering message", 4: "Jamming strobe message", 5: "Solar Storm Message"}

class RuleContent_561(RuleContentContextFree):
    variation = Content_561

class Variation_180(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_561

class RuleVariation_174(RuleVariationContextFree):
    variation = Variation_180

class NonSpare_4(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_174

class Content_336(ContentTable):
    values = {0: "No detection (number of misses)", 1: "Single PSR target reports", 2: "Single SSR target reports (Non-Mode S)", 3: "SSR+PSR target reports (Non-Mode S)", 4: "Single All-Call target reports (Mode S)", 5: "Single Roll-Call target reports (Mode S)", 6: "All-Call + PSR (Mode S) target reports", 7: "Roll-Call + PSR (Mode S) target reports", 8: "Filter for Weather data", 9: "Filter for Jamming Strobe", 10: "Filter for PSR data", 11: "Filter for SSR/Mode S data", 12: "Filter for SSR/Mode S+PSR data", 13: "Filter for Enhanced Surveillance data", 14: "Filter for PSR+Enhanced Surveillance", 15: "Filter for PSR+Enhanced Surveillance + SSR/Mode S data not in Area of Prime Interest", 16: "Filter for PSR+Enhanced Surveillance + all SSR/Mode S data", 17: "Re-Interrogations (per sector)", 18: "BDS Swap and wrong DF replies(per sector)", 19: "Mode A/C FRUIT (per sector)", 20: "Mode S FRUIT (per sector)"}

class RuleContent_336(RuleContentContextFree):
    variation = Content_336

class Variation_138(Element):
    bit_offset8 = 0
    bit_size = 5
    rule = RuleContent_336

class RuleVariation_138(RuleVariationContextFree):
    variation = Variation_138

class NonSpare_1870(NonSpare):
    name = "TYP"
    title = "Type of Message Counter"
    rule = RuleVariation_138

class Item_1030(Item):
    non_spare = NonSpare_1870

class Variation_1200(Group):
    bit_size = 16
    items_list = [Item_1030, Item_192]
    items_dict = {"TYP": NonSpare_1870, "COUNT": NonSpare_749}

class Variation_1366(Repetitive):
    rep_bytes = 1
    variation = Variation_1200

class RuleVariation_1296(RuleVariationContextFree):
    variation = Variation_1366

class NonSpare_194(NonSpare):
    name = "070"
    title = "Message Count Values"
    rule = RuleVariation_1296

class Record_15(Record):
    items_list = [NonSpare_34, NonSpare_4, NonSpare_102, NonSpare_80, NonSpare_145, NonSpare_170, NonSpare_187, NonSpare_194, NonSpare_252, NonSpare_265, NonSpare_274, NonSpare_229, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_34, "000": NonSpare_4, "030": NonSpare_102, "020": NonSpare_80, "041": NonSpare_145, "050": NonSpare_170, "060": NonSpare_187, "070": NonSpare_194, "100": NonSpare_252, "110": NonSpare_265, "120": NonSpare_274, "090": NonSpare_229, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_15(UapSingle):
    record = Record_15

class Asterix_37(AstCat):
    category = 34
    edition = (1, 28)
    uap = Uap_15

class Content_562(ContentTable):
    values = {1: "North marker message", 2: "Sector crossing message", 3: "Geographical filtering message", 4: "Jamming strobe message", 5: "Solar Storm Message", 6: "SSR Jamming Strobe Message", 7: "Mode S Jamming Strobe Message"}

class RuleContent_562(RuleContentContextFree):
    variation = Content_562

class Variation_181(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_562

class RuleVariation_175(RuleVariationContextFree):
    variation = Variation_181

class NonSpare_5(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_175

class Record_16(Record):
    items_list = [NonSpare_34, NonSpare_5, NonSpare_102, NonSpare_80, NonSpare_145, NonSpare_170, NonSpare_187, NonSpare_194, NonSpare_252, NonSpare_265, NonSpare_274, NonSpare_229, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_34, "000": NonSpare_5, "030": NonSpare_102, "020": NonSpare_80, "041": NonSpare_145, "050": NonSpare_170, "060": NonSpare_187, "070": NonSpare_194, "100": NonSpare_252, "110": NonSpare_265, "120": NonSpare_274, "090": NonSpare_229, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_16(UapSingle):
    record = Record_16

class Asterix_38(AstCat):
    category = 34
    edition = (1, 29)
    uap = Uap_16

class Content_313(ContentTable):
    values = {0: "No authenticated Mode 5 Data reply/report", 1: "Authenticated Mode 5 Data reply/report (i.e any valid Mode 5 reply type other than ID)"}

class RuleContent_313(RuleContentContextFree):
    variation = Content_313

class Variation_538(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_313

class RuleVariation_528(RuleVariationContextFree):
    variation = Variation_538

class NonSpare_806(NonSpare):
    name = "DA"
    title = ""
    rule = RuleVariation_528

class Item_230(Item):
    non_spare = NonSpare_806

class Content_269(ContentTable):
    values = {0: "Mode C altitude not present or not from Mode 5 reply/report", 1: "Mode C altitude from Mode 5 reply/report"}

class RuleContent_269(RuleContentContextFree):
    variation = Content_269

class Variation_907(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_269

class RuleVariation_878(RuleVariationContextFree):
    variation = Variation_907

class NonSpare_1163(NonSpare):
    name = "MC"
    title = ""
    rule = RuleVariation_878

class Item_504(Item):
    non_spare = NonSpare_1163

class Variation_1105(Group):
    bit_size = 8
    items_list = [Item_490, Item_403, Item_230, Item_483, Item_485, Item_488, Item_504, Item_29]
    items_dict = {"M5": NonSpare_1142, "ID": NonSpare_1040, "DA": NonSpare_806, "M1": NonSpare_1130, "M2": NonSpare_1132, "M3": NonSpare_1136, "MC": NonSpare_1163}

class RuleVariation_1063(RuleVariationContextFree):
    variation = Variation_1105

class NonSpare_1718(NonSpare):
    name = "SUM"
    title = "Mode 5 Summary"
    rule = RuleVariation_1063

class Content_288(ContentTable):
    values = {0: "National Origin is valid", 1: "National Origin is invalid"}

class RuleContent_288(RuleContentContextFree):
    variation = Content_288

class Variation_534(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_288

class RuleVariation_524(RuleVariationContextFree):
    variation = Variation_534

class NonSpare_1272(NonSpare):
    name = "NAV"
    title = "Validity of NAT"
    rule = RuleVariation_524

class Item_577(Item):
    non_spare = NonSpare_1272

class NonSpare_1266(NonSpare):
    name = "NAT"
    title = "National Origin"
    rule = RuleVariation_659

class Item_573(Item):
    non_spare = NonSpare_1266

class NonSpare_1214(NonSpare):
    name = "MIS"
    title = "Mission Code"
    rule = RuleVariation_569

class Item_526(Item):
    non_spare = NonSpare_1214

class Variation_970(Group):
    bit_size = 32
    items_list = [Item_1, Item_650, Item_1, Item_577, Item_573, Item_1, Item_526]
    items_dict = {"PIN": NonSpare_1351, "NAV": NonSpare_1272, "NAT": NonSpare_1266, "MIS": NonSpare_1214}

class RuleVariation_941(RuleVariationContextFree):
    variation = Variation_970

class NonSpare_1354(NonSpare):
    name = "PMN"
    title = "PIN/ National Origin/Mission Code"
    rule = RuleVariation_941

class Content_668(ContentQuantity):
    signedness = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

class RuleContent_668(RuleContentContextFree):
    variation = Content_668

class Variation_354(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_668

class RuleVariation_348(RuleVariationContextFree):
    variation = Variation_354

class NonSpare_1082(NonSpare):
    name = "LAT"
    title = "Latitude in WGS 84"
    rule = RuleVariation_348

class Item_439(Item):
    non_spare = NonSpare_1082

class Content_667(ContentQuantity):
    signedness = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

class RuleContent_667(RuleContentContextFree):
    variation = Content_667

class Variation_353(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_667

class RuleVariation_347(RuleVariationContextFree):
    variation = Variation_353

class NonSpare_1110(NonSpare):
    name = "LON"
    title = "Longitude in WGS 84"
    rule = RuleVariation_347

class Item_465(Item):
    non_spare = NonSpare_1110

class Variation_1096(Group):
    bit_size = 48
    items_list = [Item_439, Item_465]
    items_dict = {"LAT": NonSpare_1082, "LON": NonSpare_1110}

class RuleVariation_1055(RuleVariationContextFree):
    variation = Variation_1096

class NonSpare_1365(NonSpare):
    name = "POS"
    title = "Mode 5 Reported Position"
    rule = RuleVariation_1055

class Content_213(ContentTable):
    values = {0: "GA reported in 100 ft increments", 1: "GA reported in 25 ft increments"}

class RuleContent_213(RuleContentContextFree):
    variation = Content_213

class Variation_417(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_213

class RuleVariation_407(RuleVariationContextFree):
    variation = Variation_417

class NonSpare_1495(NonSpare):
    name = "RES"
    title = ""
    rule = RuleVariation_407

class Item_766(Item):
    non_spare = NonSpare_1495

class Content_609(ContentQuantity):
    signedness = Signed
    lsb = 25.0
    unit = "ft"

class RuleContent_609(RuleContentContextFree):
    variation = Content_609

class Variation_584(Element):
    bit_offset8 = 2
    bit_size = 14
    rule = RuleContent_609

class RuleVariation_574(RuleVariationContextFree):
    variation = Variation_584

class NonSpare_947(NonSpare):
    name = "GA"
    title = ""
    rule = RuleVariation_574

class Item_336(Item):
    non_spare = NonSpare_947

class Variation_964(Group):
    bit_size = 16
    items_list = [Item_0, Item_766, Item_336]
    items_dict = {"RES": NonSpare_1495, "GA": NonSpare_947}

class RuleVariation_935(RuleVariationContextFree):
    variation = Variation_964

class NonSpare_950(NonSpare):
    name = "GA"
    title = "Mode 5 GNSS-derived Altitude"
    rule = RuleVariation_935

class Content_52(ContentTable):
    values = {0: "Code not validated", 1: "Code validated"}

class RuleContent_52(RuleContentContextFree):
    variation = Content_52

class Variation_13(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_52

class RuleVariation_13(RuleVariationContextFree):
    variation = Variation_13

class NonSpare_1889(NonSpare):
    name = "V"
    title = ""
    rule = RuleVariation_13

class Item_1048(Item):
    non_spare = NonSpare_1889

class Content_259(ContentTable):
    values = {0: "Mode 1 Code derived from the reply of the transponder", 1: "Mode 1 Code not extracted during the last scan"}

class RuleContent_259(RuleContentContextFree):
    variation = Content_259

class Variation_522(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_259

class RuleVariation_512(RuleVariationContextFree):
    variation = Variation_522

class NonSpare_1058(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_512

class Item_415(Item):
    non_spare = NonSpare_1058

class Variation_1208(Group):
    bit_size = 16
    items_list = [Item_1048, Item_333, Item_415, Item_16, Item_278]
    items_dict = {"V": NonSpare_1889, "G": NonSpare_943, "L": NonSpare_1058, "EM1": NonSpare_868}

class RuleVariation_1142(RuleVariationContextFree):
    variation = Variation_1208

class NonSpare_870(NonSpare):
    name = "EM1"
    title = "Extended Mode 1 Code in Octal Representation"
    rule = RuleVariation_1142

class Variation_231(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_742

class RuleVariation_225(RuleVariationContextFree):
    variation = Variation_231

class NonSpare_1793(NonSpare):
    name = "TOS"
    title = "Time Offset for POS and GA"
    rule = RuleVariation_225

class NonSpare_2001(NonSpare):
    name = "XP"
    title = "X-pulse from Mode 5 PIN Reply/Report"
    rule = RuleVariation_551

class Item_1147(Item):
    non_spare = NonSpare_2001

class Content_541(ContentTable):
    values = {0: "X-pulse set to zero or no Mode 2 reply", 1: "X-pulse set to one (present)"}

class RuleContent_541(RuleContentContextFree):
    variation = Content_541

class Variation_926(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_541

class RuleVariation_897(RuleVariationContextFree):
    variation = Variation_926

class NonSpare_1991(NonSpare):
    name = "X2"
    title = "X-pulse from Mode 2 Reply"
    rule = RuleVariation_897

class Item_1140(Item):
    non_spare = NonSpare_1991

class Variation_975(Group):
    bit_size = 8
    items_list = [Item_1, Item_1147, Item_1143, Item_1146, Item_1142, Item_1140, Item_1136]
    items_dict = {"XP": NonSpare_2001, "X5": NonSpare_1994, "XC": NonSpare_1997, "X3": NonSpare_1993, "X2": NonSpare_1991, "X1": NonSpare_1987}

class RuleVariation_946(RuleVariationContextFree):
    variation = Variation_975

class NonSpare_1998(NonSpare):
    name = "XP"
    title = "X Pulse Presence"
    rule = RuleVariation_946

class Variation_1426(Compound):
    items_list = [NonSpare_1718, NonSpare_1354, NonSpare_1365, NonSpare_950, NonSpare_870, NonSpare_1793, NonSpare_1998]
    items_dict = {"SUM": NonSpare_1718, "PMN": NonSpare_1354, "POS": NonSpare_1365, "GA": NonSpare_950, "EM1": NonSpare_870, "TOS": NonSpare_1793, "XP": NonSpare_1998}

class RuleVariation_1356(RuleVariationContextFree):
    variation = Variation_1426

class NonSpare_1173(NonSpare):
    name = "MD5"
    title = "Mode 5 Reports"
    rule = RuleVariation_1356

class NonSpare_1719(NonSpare):
    name = "SUM"
    title = "Mode 5 Summary"
    rule = RuleVariation_1063

class Variation_709(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_288

class RuleVariation_699(RuleVariationContextFree):
    variation = Variation_709

class NonSpare_1300(NonSpare):
    name = "NOV"
    title = "Validity of NO"
    rule = RuleVariation_699

class Item_605(Item):
    non_spare = NonSpare_1300

class NonSpare_1283(NonSpare):
    name = "NO"
    title = "National Origin"
    rule = RuleVariation_833

class Item_588(Item):
    non_spare = NonSpare_1283

class Variation_972(Group):
    bit_size = 32
    items_list = [Item_1, Item_650, Item_3, Item_605, Item_588]
    items_dict = {"PIN": NonSpare_1351, "NOV": NonSpare_1300, "NO": NonSpare_1283}

class RuleVariation_943(RuleVariationContextFree):
    variation = Variation_972

class NonSpare_1355(NonSpare):
    name = "PMN"
    title = "PIN/ National Origin/Mission Code"
    rule = RuleVariation_943

class NonSpare_1364(NonSpare):
    name = "POS"
    title = "Mode 5 Reported Position"
    rule = RuleVariation_1055

class NonSpare_1496(NonSpare):
    name = "RES"
    title = ""
    rule = RuleVariation_407

class Item_767(Item):
    non_spare = NonSpare_1496

class NonSpare_948(NonSpare):
    name = "GA"
    title = ""
    rule = RuleVariation_574

class Item_337(Item):
    non_spare = NonSpare_948

class Variation_965(Group):
    bit_size = 16
    items_list = [Item_0, Item_767, Item_337]
    items_dict = {"RES": NonSpare_1496, "GA": NonSpare_948}

class RuleVariation_936(RuleVariationContextFree):
    variation = Variation_965

class NonSpare_951(NonSpare):
    name = "GA"
    title = "Mode 5 GNSS-derived Altitude"
    rule = RuleVariation_936

class NonSpare_916(NonSpare):
    name = "FOM"
    title = ""
    rule = RuleVariation_659

class Item_314(Item):
    non_spare = NonSpare_916

class Variation_978(Group):
    bit_size = 8
    items_list = [Item_2, Item_314]
    items_dict = {"FOM": NonSpare_916}

class RuleVariation_949(RuleVariationContextFree):
    variation = Variation_978

class NonSpare_918(NonSpare):
    name = "FOM"
    title = "Figure of Merit"
    rule = RuleVariation_949

class Variation_1427(Compound):
    items_list = [NonSpare_1719, NonSpare_1355, NonSpare_1364, NonSpare_951, NonSpare_870, NonSpare_1793, NonSpare_1998, NonSpare_918]
    items_dict = {"SUM": NonSpare_1719, "PMN": NonSpare_1355, "POS": NonSpare_1364, "GA": NonSpare_951, "EM1": NonSpare_870, "TOS": NonSpare_1793, "XP": NonSpare_1998, "FOM": NonSpare_918}

class RuleVariation_1357(RuleVariationContextFree):
    variation = Variation_1427

class NonSpare_1143(NonSpare):
    name = "M5N"
    title = "Mode 5 Reports, New Format"
    rule = RuleVariation_1357

class Content_296(ContentTable):
    values = {0: "No Mode 4 interrogation", 1: "Possibly friendly target", 2: "Probably friendly target", 3: "Friendly target"}

class RuleContent_296(RuleContentContextFree):
    variation = Content_296

class Variation_850(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_296

class RuleVariation_821(RuleVariationContextFree):
    variation = Variation_850

class NonSpare_915(NonSpare):
    name = "FOEFRI"
    title = "Indication Foe/Friend (Mode4)"
    rule = RuleVariation_821

class Item_313(Item):
    non_spare = NonSpare_915

class Variation_1278(Extended):
    items = [Item_4, Item_313, None]

class RuleVariation_1208(RuleVariationContextFree):
    variation = Variation_1278

class NonSpare_1141(NonSpare):
    name = "M4E"
    title = "Extended Mode 4 Report"
    rule = RuleVariation_1208

class NonSpare_1580(NonSpare):
    name = "SCO"
    title = "Score"
    rule = RuleVariation_189

class Content_706(ContentQuantity):
    signedness = Unsigned
    lsb = 0.1
    unit = "dB"

class RuleContent_706(RuleContentContextFree):
    variation = Content_706

class Variation_304(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_706

class RuleVariation_298(RuleVariationContextFree):
    variation = Variation_304

class NonSpare_1657(NonSpare):
    name = "SRC"
    title = "Signal/Clutter Ratio"
    rule = RuleVariation_298

class Content_749(ContentQuantity):
    signedness = Unsigned
    lsb = 3.90625e-3
    unit = "NM"

class RuleContent_748(RuleContentContextFree):
    variation = Content_749

class Variation_329(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_748

class RuleVariation_323(RuleVariationContextFree):
    variation = Variation_329

class NonSpare_1550(NonSpare):
    name = "RW"
    title = "Range Width"
    rule = RuleVariation_323

class NonSpare_578(NonSpare):
    name = "AR"
    title = "Ambiguous Range"
    rule = RuleVariation_323

class Variation_1420(Compound):
    items_list = [NonSpare_1580, NonSpare_1657, NonSpare_1550, NonSpare_578]
    items_dict = {"SCO": NonSpare_1580, "SRC": NonSpare_1657, "RW": NonSpare_1550, "AR": NonSpare_578}

class RuleVariation_1350(RuleVariationContextFree):
    variation = Variation_1420

class NonSpare_1517(NonSpare):
    name = "RPC"
    title = "Radar Plot Characteristics"
    rule = RuleVariation_1350

class Content_748(ContentQuantity):
    signedness = Unsigned
    lsb = 3.90625e-3
    unit = "NM"

class RuleContent_747(RuleContentContextFree):
    variation = Content_748

class Variation_364(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_747

class RuleVariation_358(RuleVariationContextFree):
    variation = Variation_364

class NonSpare_894(NonSpare):
    name = "ERR"
    title = "Extended Range Report"
    rule = RuleVariation_358

class Content_489(ContentTable):
    values = {0: "Track is not associated with an SCN Plot", 1: "Track is associated with an SCN Plot"}

class RuleContent_489(RuleContentContextFree):
    variation = Content_489

class Variation_640(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_489

class RuleVariation_630(RuleVariationContextFree):
    variation = Variation_640

class NonSpare_1579(NonSpare):
    name = "SCN"
    title = "Track / SCN Association"
    rule = RuleVariation_630

class Item_821(Item):
    non_spare = NonSpare_1579

class Content_37(ContentTable):
    values = {0: "Associated Plot does not contain a Roll Call component", 1: "Associated Plot contains at least a Roll Call component"}

class RuleContent_37(RuleContentContextFree):
    variation = Content_37

class Variation_675(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_37

class RuleVariation_665(RuleVariationContextFree):
    variation = Variation_675

class NonSpare_1469(NonSpare):
    name = "RC"
    title = "Roll Call Component"
    rule = RuleVariation_665

class Item_743(Item):
    non_spare = NonSpare_1469

class Content_38(ContentTable):
    values = {0: "Associated Plot does not contain an All Call component", 1: "Associated Plot contains at least an All Call component"}

class RuleContent_38(RuleContentContextFree):
    variation = Content_38

class Variation_794(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_38

class RuleVariation_765(RuleVariationContextFree):
    variation = Variation_794

class NonSpare_519(NonSpare):
    name = "AC"
    title = "All Call Component"
    rule = RuleVariation_765

class Item_39(Item):
    non_spare = NonSpare_519

class Content_39(ContentTable):
    values = {0: "Associated Plot does not contain an SSR component", 1: "Associated Plot contains at least an SSR component"}

class RuleContent_39(RuleContentContextFree):
    variation = Content_39

class Variation_871(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_39

class RuleVariation_842(RuleVariationContextFree):
    variation = Variation_871

class NonSpare_1671(NonSpare):
    name = "SSR"
    title = "SSR Component"
    rule = RuleVariation_842

class Item_885(Item):
    non_spare = NonSpare_1671

class Content_36(ContentTable):
    values = {0: "Associated Plot does not contain a PSR component", 1: "Associated Plot contains at least a PSR component"}

class RuleContent_36(RuleContentContextFree):
    variation = Content_36

class Variation_939(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_36

class RuleVariation_910(RuleVariationContextFree):
    variation = Variation_939

class NonSpare_1392(NonSpare):
    name = "PSR"
    title = "PSR Component"
    rule = RuleVariation_910

class Item_676(Item):
    non_spare = NonSpare_1392

class NonSpare_1352(NonSpare):
    name = "PLOTNR"
    title = ""
    rule = RuleVariation_240

class Item_651(Item):
    non_spare = NonSpare_1352

class Variation_981(Group):
    bit_size = 24
    items_list = [Item_2, Item_821, Item_743, Item_39, Item_885, Item_676, Item_651]
    items_dict = {"SCN": NonSpare_1579, "RC": NonSpare_1469, "AC": NonSpare_519, "SSR": NonSpare_1671, "PSR": NonSpare_1392, "PLOTNR": NonSpare_1352}

class RuleVariation_952(RuleVariationContextFree):
    variation = Variation_981

class NonSpare_1398(NonSpare):
    name = "PTL"
    title = "Plot/Track Link"
    rule = RuleVariation_952

class Variation_1327(Repetitive):
    rep_bytes = 1
    variation = Variation_246

class RuleVariation_1257(RuleVariationContextFree):
    variation = Variation_1327

class NonSpare_594(NonSpare):
    name = "ATL"
    title = "ADS-B/Track Link"
    rule = RuleVariation_1257

class Content_682(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "%"

class RuleContent_682(RuleContentContextFree):
    variation = Content_682

class Variation_212(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_682

class RuleVariation_206(RuleVariationContextFree):
    variation = Variation_212

class NonSpare_1823(NonSpare):
    name = "TRN"
    title = "Turn State"
    rule = RuleVariation_206

class Content_739(ContentQuantity):
    signedness = Unsigned
    lsb = 7.8125e-3
    unit = "NM"

class RuleContent_738(RuleContentContextFree):
    variation = Content_739

class Variation_322(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_738

class RuleVariation_316(RuleVariationContextFree):
    variation = Variation_322

class NonSpare_1375(NonSpare):
    name = "PREDRHO"
    title = "Predicted Range"
    rule = RuleVariation_316

class Item_663(Item):
    non_spare = NonSpare_1375

class NonSpare_1376(NonSpare):
    name = "PREDTHETA"
    title = "Predicted Azimuth"
    rule = RuleVariation_329

class Item_664(Item):
    non_spare = NonSpare_1376

class NonSpare_900(NonSpare):
    name = "EVOLRHOSTART"
    title = "Predicted Closest Range"
    rule = RuleVariation_316

class Item_302(Item):
    non_spare = NonSpare_900

class NonSpare_899(NonSpare):
    name = "EVOLRHOEND"
    title = "Predicted Largest Range"
    rule = RuleVariation_316

class Item_301(Item):
    non_spare = NonSpare_899

class NonSpare_902(NonSpare):
    name = "EVOLTHETASTART"
    title = "Predicted Smallest Azimuth"
    rule = RuleVariation_329

class Item_304(Item):
    non_spare = NonSpare_902

class NonSpare_901(NonSpare):
    name = "EVOLTHETAEND"
    title = "Predicted Largest Azimuth"
    rule = RuleVariation_329

class Item_303(Item):
    non_spare = NonSpare_901

class NonSpare_1295(NonSpare):
    name = "NOISERHOSTART"
    title = "Predicted Closest Range"
    rule = RuleVariation_316

class Item_600(Item):
    non_spare = NonSpare_1295

class NonSpare_1294(NonSpare):
    name = "NOISERHOEND"
    title = "Predicted Largest Range"
    rule = RuleVariation_316

class Item_599(Item):
    non_spare = NonSpare_1294

class NonSpare_1297(NonSpare):
    name = "NOISETHETASTART"
    title = "Predicted Smallest Azimuth"
    rule = RuleVariation_329

class Item_602(Item):
    non_spare = NonSpare_1297

class NonSpare_1296(NonSpare):
    name = "NOISETHETAEND"
    title = "Predicted Largest Azimuth"
    rule = RuleVariation_329

class Item_601(Item):
    non_spare = NonSpare_1296

class NonSpare_1377(NonSpare):
    name = "PREDTIME"
    title = "Predicted Detection Time"
    rule = RuleVariation_320

class Item_665(Item):
    non_spare = NonSpare_1377

class Variation_1123(Group):
    bit_size = 176
    items_list = [Item_663, Item_664, Item_302, Item_301, Item_304, Item_303, Item_600, Item_599, Item_602, Item_601, Item_665]
    items_dict = {"PREDRHO": NonSpare_1375, "PREDTHETA": NonSpare_1376, "EVOLRHOSTART": NonSpare_900, "EVOLRHOEND": NonSpare_899, "EVOLTHETASTART": NonSpare_902, "EVOLTHETAEND": NonSpare_901, "NOISERHOSTART": NonSpare_1295, "NOISERHOEND": NonSpare_1294, "NOISETHETASTART": NonSpare_1297, "NOISETHETAEND": NonSpare_1296, "PREDTIME": NonSpare_1377}

class RuleVariation_1077(RuleVariationContextFree):
    variation = Variation_1123

class NonSpare_1301(NonSpare):
    name = "NPP"
    title = "Next Predicted Position"
    rule = RuleVariation_1077

class Content_437(ContentTable):
    values = {0: "Surveillance Mode A (alert bit or periodic)", 1: "Comm-A", 2: "Ground Initiated Comm-B", 3: "Air Initiated Comm-B", 4: "Broadcast Comm-B", 5: "Comm-C", 6: "Comm-D", 7: "Reserved for future use", 8: "Reserved for future use", 9: "Reserved for future use", 10: "Reserved for future use", 11: "Reserved for future use", 12: "Reserved for future use", 13: "Reserved for future use", 14: "Reserved for future use", 15: "Reserved for future use"}

class RuleContent_437(RuleContentContextFree):
    variation = Content_437

class Variation_129(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_437

class RuleVariation_129(RuleVariationContextFree):
    variation = Variation_129

class NonSpare_1871(NonSpare):
    name = "TYPE"
    title = ""
    rule = RuleVariation_129

class Item_1031(Item):
    non_spare = NonSpare_1871

class Content_212(ContentTable):
    values = {0: "From previous scan", 1: "New in current scan", 2: "Requested in the beam by transponder", 3: "Invalid ASTERIX value"}

class RuleContent_212(RuleContentContextFree):
    variation = Content_212

class Variation_735(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_212

class RuleVariation_725(RuleVariationContextFree):
    variation = Variation_735

class NonSpare_1324(NonSpare):
    name = "ORIGIN"
    title = ""
    rule = RuleVariation_725

class Item_628(Item):
    non_spare = NonSpare_1324

class Content_233(ContentTable):
    values = {0: "In progress", 1: "Completed", 2: "Cancelled", 3: "Invalid ASTERIX value"}

class RuleContent_233(RuleContentContextFree):
    variation = Content_233

class Variation_927(Element):
    bit_offset8 = 6
    bit_size = 2
    rule = RuleContent_233

class RuleVariation_898(RuleVariationContextFree):
    variation = Variation_927

class NonSpare_1693(NonSpare):
    name = "STATE"
    title = ""
    rule = RuleVariation_898

class Item_901(Item):
    non_spare = NonSpare_1693

class Variation_1201(Group):
    bit_size = 8
    items_list = [Item_1031, Item_628, Item_901]
    items_dict = {"TYPE": NonSpare_1871, "ORIGIN": NonSpare_1324, "STATE": NonSpare_1693}

class Variation_1367(Repetitive):
    rep_bytes = 1
    variation = Variation_1201

class RuleVariation_1297(RuleVariationContextFree):
    variation = Variation_1367

class NonSpare_836(NonSpare):
    name = "DLK"
    title = "Data Link Characteristics"
    rule = RuleVariation_1297

class Content_460(ContentTable):
    values = {0: "Target not locked out by this radar", 1: "Target locked out by this radar"}

class RuleContent_460(RuleContentContextFree):
    variation = Content_460

class Variation_79(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_460

class RuleVariation_79(RuleVariationContextFree):
    variation = Variation_79

class NonSpare_1121(NonSpare):
    name = "LS"
    title = "Lockout State"
    rule = RuleVariation_79

class Item_474(Item):
    non_spare = NonSpare_1121

class Content_693(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "ms"

class RuleContent_693(RuleContentContextFree):
    variation = Content_693

class Variation_485(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_693

class RuleVariation_475(RuleVariationContextFree):
    variation = Variation_485

class NonSpare_1098(NonSpare):
    name = "LOCTIM"
    title = "Lockout Time"
    rule = RuleVariation_475

class Item_453(Item):
    non_spare = NonSpare_1098

class Variation_1102(Group):
    bit_size = 16
    items_list = [Item_474, Item_453]
    items_dict = {"LS": NonSpare_1121, "LOCTIM": NonSpare_1098}

class RuleVariation_1060(RuleVariationContextFree):
    variation = Variation_1102

class NonSpare_1090(NonSpare):
    name = "LCK"
    title = "Lockout Characteristics"
    rule = RuleVariation_1060

class Variation_959(Element):
    bit_offset8 = 7
    bit_size = 4
    rule = RuleContent_585

class RuleVariation_930(RuleVariationContextFree):
    variation = Variation_959

class NonSpare_1751(NonSpare):
    name = "TCOUNT1"
    title = ""
    rule = RuleVariation_930

class Item_938(Item):
    non_spare = NonSpare_1751

class NonSpare_1748(NonSpare):
    name = "TCODE1"
    title = ""
    rule = RuleVariation_659

class Item_935(Item):
    non_spare = NonSpare_1748

class NonSpare_1752(NonSpare):
    name = "TCOUNT2"
    title = ""
    rule = RuleVariation_132

class Item_939(Item):
    non_spare = NonSpare_1752

class NonSpare_1749(NonSpare):
    name = "TCODE2"
    title = ""
    rule = RuleVariation_751

class Item_936(Item):
    non_spare = NonSpare_1749

class NonSpare_1753(NonSpare):
    name = "TCOUNT3"
    title = ""
    rule = RuleVariation_132

class Item_940(Item):
    non_spare = NonSpare_1753

class NonSpare_1750(NonSpare):
    name = "TCODE3"
    title = ""
    rule = RuleVariation_751

class Item_937(Item):
    non_spare = NonSpare_1750

class Variation_1008(Group):
    bit_size = 48
    items_list = [Item_6, Item_938, Item_935, Item_939, Item_936, Item_940, Item_937]
    items_dict = {"TCOUNT1": NonSpare_1751, "TCODE1": NonSpare_1748, "TCOUNT2": NonSpare_1752, "TCODE2": NonSpare_1749, "TCOUNT3": NonSpare_1753, "TCODE3": NonSpare_1750}

class RuleVariation_977(RuleVariationContextFree):
    variation = Variation_1008

class NonSpare_1742(NonSpare):
    name = "TC"
    title = "Transition Code"
    rule = RuleVariation_977

class Content_463(ContentTable):
    values = {0: "Tentative Track with One Plot", 1: "Tentative Track with at least Two Plots", 2: "Pre-Confirmed Track", 3: "Confirmed Track"}

class RuleContent_463(RuleContentContextFree):
    variation = Content_463

class Variation_108(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_463

class RuleVariation_108(RuleVariationContextFree):
    variation = Variation_108

class NonSpare_524(NonSpare):
    name = "ACQI"
    title = ""
    rule = RuleVariation_108

class Item_42(Item):
    non_spare = NonSpare_524

class Variation_582(Element):
    bit_offset8 = 2
    bit_size = 14
    rule = RuleContent_585

class RuleVariation_572(RuleVariationContextFree):
    variation = Variation_582

class NonSpare_1821(NonSpare):
    name = "TRKUPDCTR"
    title = ""
    rule = RuleVariation_572

class Item_987(Item):
    non_spare = NonSpare_1821

class Variation_298(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_693

class RuleVariation_292(RuleVariationContextFree):
    variation = Variation_298

class NonSpare_1071(NonSpare):
    name = "LASTTRKUPD"
    title = ""
    rule = RuleVariation_292

class Item_428(Item):
    non_spare = NonSpare_1071

class Variation_1013(Group):
    bit_size = 32
    items_list = [Item_42, Item_987, Item_428]
    items_dict = {"ACQI": NonSpare_524, "TRKUPDCTR": NonSpare_1821, "LASTTRKUPD": NonSpare_1071}

class RuleVariation_981(RuleVariationContextFree):
    variation = Variation_1013

class NonSpare_1779(NonSpare):
    name = "TLC"
    title = "Track Life Cycle"
    rule = RuleVariation_981

class NonSpare_1565(NonSpare):
    name = "SACADJS"
    title = "SAC of the Adjacent Sensor"
    rule = RuleVariation_154

class Item_813(Item):
    non_spare = NonSpare_1565

class NonSpare_1620(NonSpare):
    name = "SICADJS"
    title = "SIC of the Adjacent Sensor"
    rule = RuleVariation_154

class Item_844(Item):
    non_spare = NonSpare_1620

class NonSpare_1776(NonSpare):
    name = "TIMEOFDAYSCN"
    title = "Absolute Timestamp in UTC Provided by the SCN"
    rule = RuleVariation_320

class Item_957(Item):
    non_spare = NonSpare_1776

class Content_73(ContentTable):
    values = {0: "Data used by Tracker", 1: "Data not used by Tracker", 2: "2-127: Reserved for future use"}

class RuleContent_73(RuleContentContextFree):
    variation = Content_73

class Variation_146(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_73

class RuleVariation_146(RuleVariationContextFree):
    variation = Variation_146

class NonSpare_809(NonSpare):
    name = "DATAUSE"
    title = "Use of Adjacent Sensor Data"
    rule = RuleVariation_146

class Item_232(Item):
    non_spare = NonSpare_809

class Content_70(ContentTable):
    values = {0: "DRN not available", 1: "DRN available"}

class RuleContent_70(RuleContentContextFree):
    variation = Content_70

class Variation_940(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_70

class RuleVariation_911(RuleVariationContextFree):
    variation = Variation_940

class NonSpare_848(NonSpare):
    name = "DRNA"
    title = "DRN Availability"
    rule = RuleVariation_911

class Item_265(Item):
    non_spare = NonSpare_848

class NonSpare_847(NonSpare):
    name = "DRN"
    title = ""
    rule = RuleVariation_240

class Item_264(Item):
    non_spare = NonSpare_847

class Variation_1159(Group):
    bit_size = 56
    items_list = [Item_813, Item_844, Item_957, Item_232, Item_265, Item_264]
    items_dict = {"SACADJS": NonSpare_1565, "SICADJS": NonSpare_1620, "TIMEOFDAYSCN": NonSpare_1776, "DATAUSE": NonSpare_809, "DRNA": NonSpare_848, "DRN": NonSpare_847}

class Variation_1357(Repetitive):
    rep_bytes = 1
    variation = Variation_1159

class RuleVariation_1287(RuleVariationContextFree):
    variation = Variation_1357

class NonSpare_589(NonSpare):
    name = "ASI"
    title = "Adjacent Sensor Information"
    rule = RuleVariation_1287

class Content_413(ContentTable):
    values = {0: "Radar tracker calculation", 1: "Integrated ADS-B", 2: "External ADS-B", 3: "SCN"}

class RuleContent_413(RuleContentContextFree):
    variation = Content_413

class Variation_170(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_413

class RuleVariation_164(RuleVariationContextFree):
    variation = Variation_170

class NonSpare_1759(NonSpare):
    name = "TES"
    title = "Track Extrapolation Source"
    rule = RuleVariation_164

class Content_232(ContentTable):
    values = {0: "Identity not requested", 1: "Identity requested"}

class RuleContent_232(RuleContentContextFree):
    variation = Content_232

class Variation_46(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_232

class RuleVariation_46(RuleVariationContextFree):
    variation = Variation_46

class NonSpare_1053(NonSpare):
    name = "IR"
    title = ""
    rule = RuleVariation_46

class Item_412(Item):
    non_spare = NonSpare_1053

class Variation_480(Element):
    bit_offset8 = 1
    bit_size = 7
    rule = RuleContent_695

class RuleVariation_470(RuleVariationContextFree):
    variation = Variation_480

class NonSpare_1139(NonSpare):
    name = "M3A"
    title = "Age of Mode 3/A Code (I048/070)"
    rule = RuleVariation_470

class Item_489(Item):
    non_spare = NonSpare_1139

class Variation_1086(Group):
    bit_size = 8
    items_list = [Item_412, Item_489]
    items_dict = {"IR": NonSpare_1053, "M3A": NonSpare_1139}

class RuleVariation_1045(RuleVariationContextFree):
    variation = Variation_1086

class NonSpare_1054(NonSpare):
    name = "IR"
    title = "Identity Requested"
    rule = RuleVariation_1045

class Variation_1418(Compound):
    items_list = [NonSpare_1398, NonSpare_594, NonSpare_1823, NonSpare_1301, NonSpare_836, NonSpare_1090, NonSpare_1742, NonSpare_1779, NonSpare_589, NonSpare_1759, NonSpare_1054]
    items_dict = {"PTL": NonSpare_1398, "ATL": NonSpare_594, "TRN": NonSpare_1823, "NPP": NonSpare_1301, "DLK": NonSpare_836, "LCK": NonSpare_1090, "TC": NonSpare_1742, "TLC": NonSpare_1779, "ASI": NonSpare_589, "TES": NonSpare_1759, "IR": NonSpare_1054}

class RuleVariation_1348(RuleVariationContextFree):
    variation = Variation_1418

class NonSpare_1543(NonSpare):
    name = "RTC"
    title = "Radar Track Characteristics"
    rule = RuleVariation_1348

class NonSpare_1356(NonSpare):
    name = "PNB"
    title = "Plot Number"
    rule = RuleVariation_240

class Content_400(ContentTable):
    values = {0: "PSR Echo", 1: "SSR Reply", 2: "All Call Reply", 3: "Roll Call Reply"}

class RuleContent_400(RuleContentContextFree):
    variation = Content_400

class Variation_169(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_400

class RuleVariation_163(RuleVariationContextFree):
    variation = Variation_169

class NonSpare_1872(NonSpare):
    name = "TYPE"
    title = "Reply Type"
    rule = RuleVariation_163

class Item_1032(Item):
    non_spare = NonSpare_1872

class NonSpare_1494(NonSpare):
    name = "REPLYNBR"
    title = ""
    rule = RuleVariation_240

class Item_765(Item):
    non_spare = NonSpare_1494

class Variation_1202(Group):
    bit_size = 24
    items_list = [Item_1032, Item_765]
    items_dict = {"TYPE": NonSpare_1872, "REPLYNBR": NonSpare_1494}

class Variation_1368(Repetitive):
    rep_bytes = 1
    variation = Variation_1202

class RuleVariation_1298(RuleVariationContextFree):
    variation = Variation_1368

class NonSpare_1519(NonSpare):
    name = "RPL"
    title = "Replies/Plot Link"
    rule = RuleVariation_1298

class NonSpare_1639(NonSpare):
    name = "SNB"
    title = "Scan Number"
    rule = RuleVariation_189

class Content_588(ContentInteger):
    signedness = Unsigned

class RuleContent_588(RuleContentContextFree):
    variation = Content_588

class Variation_135(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_588

class RuleVariation_135(RuleVariationContextFree):
    variation = Variation_135

class NonSpare_2038(NonSpare):
    name = "Y1"
    title = ""
    rule = RuleVariation_135

class Item_1184(Item):
    non_spare = NonSpare_2038

class Variation_776(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_588

class RuleVariation_747(RuleVariationContextFree):
    variation = Variation_776

class NonSpare_2041(NonSpare):
    name = "Y2"
    title = ""
    rule = RuleVariation_747

class Item_1187(Item):
    non_spare = NonSpare_2041

class NonSpare_2043(NonSpare):
    name = "Y3"
    title = ""
    rule = RuleVariation_135

class Item_1189(Item):
    non_spare = NonSpare_2043

class NonSpare_2044(NonSpare):
    name = "Y4"
    title = ""
    rule = RuleVariation_747

class Item_1190(Item):
    non_spare = NonSpare_2044

class Content_586(ContentInteger):
    signedness = Unsigned

class RuleContent_586(RuleContentContextFree):
    variation = Content_586

class Variation_133(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_586

class RuleVariation_133(RuleVariationContextFree):
    variation = Variation_133

class NonSpare_1128(NonSpare):
    name = "M1"
    title = ""
    rule = RuleVariation_133

class Item_481(Item):
    non_spare = NonSpare_1128

class NonSpare_1133(NonSpare):
    name = "M2"
    title = ""
    rule = RuleVariation_747

class Item_486(Item):
    non_spare = NonSpare_1133

class Content_587(ContentInteger):
    signedness = Unsigned

class RuleContent_587(RuleContentContextFree):
    variation = Content_587

class Variation_134(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_587

class RuleVariation_134(RuleVariationContextFree):
    variation = Variation_134

class NonSpare_803(NonSpare):
    name = "D1"
    title = ""
    rule = RuleVariation_134

class Item_227(Item):
    non_spare = NonSpare_803

class NonSpare_804(NonSpare):
    name = "D2"
    title = ""
    rule = RuleVariation_747

class Item_228(Item):
    non_spare = NonSpare_804

class Variation_1274(Group):
    bit_size = 32
    items_list = [Item_1184, Item_1187, Item_1189, Item_1190, Item_481, Item_486, Item_227, Item_228]
    items_dict = {"Y1": NonSpare_2038, "Y2": NonSpare_2041, "Y3": NonSpare_2043, "Y4": NonSpare_2044, "M1": NonSpare_1128, "M2": NonSpare_1133, "D1": NonSpare_803, "D2": NonSpare_804}

class RuleVariation_1204(RuleVariationContextFree):
    variation = Variation_1274

class NonSpare_810(NonSpare):
    name = "DATE"
    title = "Common and Plot Characteristics Date"
    rule = RuleVariation_1204

class Variation_1415(Compound):
    items_list = [NonSpare_1356, NonSpare_1519, NonSpare_1639, NonSpare_810]
    items_dict = {"PNB": NonSpare_1356, "RPL": NonSpare_1519, "SNB": NonSpare_1639, "DATE": NonSpare_810}

class RuleVariation_1345(RuleVariationContextFree):
    variation = Variation_1415

class NonSpare_766(NonSpare):
    name = "CPC"
    title = "Common and Plot Characteristics"
    rule = RuleVariation_1345

class Expansion_3(Expansion):
    fspec_bytes = 1
    items = [NonSpare_1173, NonSpare_1143, NonSpare_1141, NonSpare_1517, NonSpare_894, NonSpare_1543, NonSpare_766]

class Asterix_39(AstRef):
    category = 48
    edition = (1, 11)
    expansion = Expansion_3

class NonSpare_38(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class NonSpare_309(NonSpare):
    name = "140"
    title = "Time of Day"
    rule = RuleVariation_357

class Content_332(ContentTable):
    values = {0: "No detection", 1: "Single PSR detection", 2: "Single SSR detection", 3: "SSR + PSR detection", 4: "Single ModeS All-Call", 5: "Single ModeS Roll-Call", 6: "ModeS All-Call + PSR", 7: "ModeS Roll-Call +PSR"}

class RuleContent_332(RuleContentContextFree):
    variation = Content_332

class Variation_122(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_332

class RuleVariation_122(RuleVariationContextFree):
    variation = Variation_122

class NonSpare_1859(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_122

class Item_1020(Item):
    non_spare = NonSpare_1859

class Variation_591(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_18

class RuleVariation_581(RuleVariationContextFree):
    variation = Variation_591

class NonSpare_1635(NonSpare):
    name = "SIM"
    title = ""
    rule = RuleVariation_581

class Item_857(Item):
    non_spare = NonSpare_1635

class Content_416(ContentTable):
    values = {0: "Report from RDP Chain 1", 1: "Report from RDP Chain 2"}

class RuleContent_416(RuleContentContextFree):
    variation = Content_416

class Variation_720(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_416

class RuleVariation_710(RuleVariationContextFree):
    variation = Variation_720

class NonSpare_1473(NonSpare):
    name = "RDP"
    title = ""
    rule = RuleVariation_710

class Item_747(Item):
    non_spare = NonSpare_1473

class Variation_790(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_12

class RuleVariation_761(RuleVariationContextFree):
    variation = Variation_790

class NonSpare_1648(NonSpare):
    name = "SPI"
    title = ""
    rule = RuleVariation_761

class Item_868(Item):
    non_spare = NonSpare_1648

class Content_417(ContentTable):
    values = {0: "Report from aircraft transponder", 1: "Report from field monitor (fixed transponder)"}

class RuleContent_417(RuleContentContextFree):
    variation = Content_417

class Variation_915(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_417

class RuleVariation_886(RuleVariationContextFree):
    variation = Variation_915

class NonSpare_1455(NonSpare):
    name = "RAB"
    title = ""
    rule = RuleVariation_886

class Item_731(Item):
    non_spare = NonSpare_1455

class Content_414(ContentTable):
    values = {0: "Real target report", 1: "Test target report"}

class RuleContent_414(RuleContentContextFree):
    variation = Content_414

class Variation_68(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_414

class RuleVariation_68(RuleVariationContextFree):
    variation = Variation_68

class NonSpare_1834(NonSpare):
    name = "TST"
    title = ""
    rule = RuleVariation_68

class Item_998(Item):
    non_spare = NonSpare_1834

class Content_294(ContentTable):
    values = {0: "No Extended Range", 1: "Extended Range present"}

class RuleContent_294(RuleContentContextFree):
    variation = Content_294

class Variation_429(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_294

class RuleVariation_419(RuleVariationContextFree):
    variation = Variation_429

class NonSpare_893(NonSpare):
    name = "ERR"
    title = ""
    rule = RuleVariation_419

class Item_297(Item):
    non_spare = NonSpare_893

class Content_304(ContentTable):
    values = {0: "No X-Pulse present", 1: "X-Pulse present"}

class RuleContent_304(RuleContentContextFree):
    variation = Content_304

class Variation_536(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_304

class RuleVariation_526(RuleVariationContextFree):
    variation = Variation_536

class NonSpare_2003(NonSpare):
    name = "XPP"
    title = ""
    rule = RuleVariation_526

class Item_1149(Item):
    non_spare = NonSpare_2003

class Variation_627(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_352

class RuleVariation_617(RuleVariationContextFree):
    variation = Variation_627

class NonSpare_1190(NonSpare):
    name = "ME"
    title = ""
    rule = RuleVariation_617

class Item_513(Item):
    non_spare = NonSpare_1190

class Content_353(ContentTable):
    values = {0: "No military identification", 1: "Military identification"}

class RuleContent_353(RuleContentContextFree):
    variation = Content_353

class Variation_714(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_353

class RuleVariation_704(RuleVariationContextFree):
    variation = Variation_714

class NonSpare_1208(NonSpare):
    name = "MI"
    title = ""
    rule = RuleVariation_704

class Item_520(Item):
    non_spare = NonSpare_1208

class Content_295(ContentTable):
    values = {0: "No Mode 4 interrogation", 1: "Friendly target", 2: "Unknown target", 3: "No reply"}

class RuleContent_295(RuleContentContextFree):
    variation = Content_295

class Variation_849(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_295

class RuleVariation_820(RuleVariationContextFree):
    variation = Variation_849

class NonSpare_914(NonSpare):
    name = "FOEFRI"
    title = ""
    rule = RuleVariation_820

class Item_312(Item):
    non_spare = NonSpare_914

class Variation_1320(Extended):
    items = [Item_1020, Item_857, Item_747, Item_868, Item_731, None, Item_998, Item_297, Item_1149, Item_513, Item_520, Item_312, None]

class RuleVariation_1250(RuleVariationContextFree):
    variation = Variation_1320

class NonSpare_85(NonSpare):
    name = "020"
    title = "Target Report Descriptor"
    rule = RuleVariation_1250

class NonSpare_132(NonSpare):
    name = "040"
    title = "Measured Position in Polar Co-ordinates"
    rule = RuleVariation_1094

class Variation_1219(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_423, Item_16, Item_543]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "L": NonSpare_1066, "MODE3A": NonSpare_1232}

class RuleVariation_1153(RuleVariationContextFree):
    variation = Variation_1219

class NonSpare_198(NonSpare):
    name = "070"
    title = "Mode-3/A Code in Octal Representation"
    rule = RuleVariation_1153

class NonSpare_907(NonSpare):
    name = "FL"
    title = ""
    rule = RuleVariation_577

class Item_308(Item):
    non_spare = NonSpare_907

class Variation_1212(Group):
    bit_size = 16
    items_list = [Item_1049, Item_333, Item_308]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "FL": NonSpare_907}

class RuleVariation_1146(RuleVariationContextFree):
    variation = Variation_1212

class NonSpare_231(NonSpare):
    name = "090"
    title = "Flight Level in Binary Representation"
    rule = RuleVariation_1146

class Content_765(ContentQuantity):
    signedness = Unsigned
    lsb = 4.39453125e-2
    unit = "°"

class RuleContent_764(RuleContentContextFree):
    variation = Content_765

class Variation_238(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_764

class RuleVariation_232(RuleVariationContextFree):
    variation = Variation_238

class NonSpare_1661(NonSpare):
    name = "SRL"
    title = "SSR Plot Runlength"
    rule = RuleVariation_232

class NonSpare_1662(NonSpare):
    name = "SRR"
    title = "Number of Received Replies for (M)SSR"
    rule = RuleVariation_189

class NonSpare_1570(NonSpare):
    name = "SAM"
    title = "Amplitude of (M)SSR Reply"
    rule = RuleVariation_192

class NonSpare_1384(NonSpare):
    name = "PRL"
    title = "Primary Plot Runlength"
    rule = RuleVariation_232

class NonSpare_1342(NonSpare):
    name = "PAM"
    title = "Amplitude of Primary Plot"
    rule = RuleVariation_192

class Content_651(ContentQuantity):
    signedness = Signed
    lsb = 3.90625e-3
    unit = "NM"

class RuleContent_651(RuleContentContextFree):
    variation = Content_651

class Variation_208(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_651

class RuleVariation_202(RuleVariationContextFree):
    variation = Variation_208

class NonSpare_1518(NonSpare):
    name = "RPD"
    title = "Difference in Range Between PSR and SSR Plot"
    rule = RuleVariation_202

class NonSpare_572(NonSpare):
    name = "APD"
    title = "Difference in Azimuth Between PSR and SSR Plot"
    rule = RuleVariation_205

class Variation_1423(Compound):
    items_list = [NonSpare_1661, NonSpare_1662, NonSpare_1570, NonSpare_1384, NonSpare_1342, NonSpare_1518, NonSpare_572]
    items_dict = {"SRL": NonSpare_1661, "SRR": NonSpare_1662, "SAM": NonSpare_1570, "PRL": NonSpare_1384, "PAM": NonSpare_1342, "RPD": NonSpare_1518, "APD": NonSpare_572}

class RuleVariation_1353(RuleVariationContextFree):
    variation = Variation_1423

class NonSpare_290(NonSpare):
    name = "130"
    title = "Radar Plot Characteristics"
    rule = RuleVariation_1353

class NonSpare_387(NonSpare):
    name = "220"
    title = "Aircraft Address"
    rule = RuleVariation_335

class NonSpare_401(NonSpare):
    name = "240"
    title = "Aircraft Identification"
    rule = RuleVariation_376

class NonSpare_1160(NonSpare):
    name = "MBDATA"
    title = "Mode S Comm B Message Data"
    rule = RuleVariation_377

class Item_501(Item):
    non_spare = NonSpare_1160

class Variation_1108(Group):
    bit_size = 64
    items_list = [Item_501, Item_111, Item_113]
    items_dict = {"MBDATA": NonSpare_1160, "BDS1": NonSpare_628, "BDS2": NonSpare_630}

class Variation_1349(Repetitive):
    rep_bytes = 1
    variation = Variation_1108

class RuleVariation_1279(RuleVariationContextFree):
    variation = Variation_1349

class NonSpare_414(NonSpare):
    name = "250"
    title = "Mode S MB Data"
    rule = RuleVariation_1279

class NonSpare_337(NonSpare):
    name = "161"
    title = "Track Number"
    rule = RuleVariation_969

class NonSpare_2029(NonSpare):
    name = "Y"
    title = "X-Component"
    rule = RuleVariation_272

class Item_1175(Item):
    non_spare = NonSpare_2029

class Variation_1267(Group):
    bit_size = 32
    items_list = [Item_1131, Item_1175]
    items_dict = {"X": NonSpare_1982, "Y": NonSpare_2029}

class RuleVariation_1200(RuleVariationContextFree):
    variation = Variation_1267

class NonSpare_152(NonSpare):
    name = "042"
    title = "Calculated Position in Cartesian Co-ordinates"
    rule = RuleVariation_1200

class NonSpare_359(NonSpare):
    name = "200"
    title = "Calculated Track Velocity in Polar Co-ordinates"
    rule = RuleVariation_1038

class Variation_15(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_57

class RuleVariation_15(RuleVariationContextFree):
    variation = Variation_15

class NonSpare_700(NonSpare):
    name = "CNF"
    title = "Confirmed Vs. Tentative Track"
    rule = RuleVariation_15

class Item_162(Item):
    non_spare = NonSpare_700

class Content_55(ContentTable):
    values = {0: "Combined Track", 1: "PSR Track", 2: "SSR/Mode S Track", 3: "Invalid"}

class RuleContent_55(RuleContentContextFree):
    variation = Content_55

class Variation_453(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_55

class RuleVariation_443(RuleVariationContextFree):
    variation = Variation_453

class NonSpare_1460(NonSpare):
    name = "RAD"
    title = "Type of Sensor(s) Maintaining Track"
    rule = RuleVariation_443

class Item_736(Item):
    non_spare = NonSpare_1460

class Content_368(ContentTable):
    values = {0: "Normal confidence", 1: "Low confidence in plot to track association"}

class RuleContent_368(RuleContentContextFree):
    variation = Content_368

class Variation_630(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_368

class RuleVariation_620(RuleVariationContextFree):
    variation = Variation_630

class NonSpare_843(NonSpare):
    name = "DOU"
    title = "Signals Level of Confidence in Plot to Track Association Process"
    rule = RuleVariation_620

class Item_262(Item):
    non_spare = NonSpare_843

class Content_347(ContentTable):
    values = {0: "No horizontal man.sensed", 1: "Horizontal man. sensed"}

class RuleContent_347(RuleContentContextFree):
    variation = Content_347

class Variation_712(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_347

class RuleVariation_702(RuleVariationContextFree):
    variation = Variation_712

class NonSpare_1149(NonSpare):
    name = "MAH"
    title = "Manoeuvre Detection in Horizontal Sense"
    rule = RuleVariation_702

class Item_494(Item):
    non_spare = NonSpare_1149

class Content_252(ContentTable):
    values = {0: "Maintaining", 1: "Climbing", 2: "Descending", 3: "Unknown"}

class RuleContent_252(RuleContentContextFree):
    variation = Content_252

class Variation_847(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_252

class RuleVariation_818(RuleVariationContextFree):
    variation = Variation_847

class NonSpare_666(NonSpare):
    name = "CDM"
    title = "Climbing / Descending Mode"
    rule = RuleVariation_818

class Item_136(Item):
    non_spare = NonSpare_666

class Content_491(ContentTable):
    values = {0: "Track still alive", 1: "End of track lifetime(last report for this track)"}

class RuleContent_491(RuleContentContextFree):
    variation = Content_491

class Variation_86(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_491

class RuleVariation_86(RuleVariationContextFree):
    variation = Variation_86

class NonSpare_1817(NonSpare):
    name = "TRE"
    title = "Signal for End_of_Track"
    rule = RuleVariation_86

class Item_985(Item):
    non_spare = NonSpare_1817

class Content_503(ContentTable):
    values = {0: "True target track", 1: "Ghost target track"}

class RuleContent_503(RuleContentContextFree):
    variation = Content_503

class Variation_450(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_503

class RuleVariation_440(RuleVariationContextFree):
    variation = Variation_450

class NonSpare_969(NonSpare):
    name = "GHO"
    title = "Ghost Vs. True Target"
    rule = RuleVariation_440

class Item_349(Item):
    non_spare = NonSpare_969

class NonSpare_1720(NonSpare):
    name = "SUP"
    title = "Track Maintained with Track Information from Neighbouring Node B on the Cluster, or Network"
    rule = RuleVariation_525

class Item_920(Item):
    non_spare = NonSpare_1720

class Content_493(ContentTable):
    values = {0: "Tracking performed in so-called 'Radar Plane', i.e. neither slant range correction nor stereographical projection was applied", 1: "Slant range correction and a suitable projection technique are used to track in a 2D.reference plane, tangential to the earth model at the Radar Site co-ordinates"}

class RuleContent_493(RuleContentContextFree):
    variation = Content_493

class Variation_641(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_493

class RuleVariation_631(RuleVariationContextFree):
    variation = Variation_641

class NonSpare_1747(NonSpare):
    name = "TCC"
    title = "Type of Plot Coordinate Transformation Mechanism:"
    rule = RuleVariation_631

class Item_934(Item):
    non_spare = NonSpare_1747

class Variation_1286(Extended):
    items = [Item_162, Item_736, Item_262, Item_494, Item_136, None, Item_985, Item_349, Item_920, Item_934, Item_21, None]

class RuleVariation_1216(RuleVariationContextFree):
    variation = Variation_1286

class NonSpare_349(NonSpare):
    name = "170"
    title = "Track Status"
    rule = RuleVariation_1216

class Variation_230(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_738

class RuleVariation_224(RuleVariationContextFree):
    variation = Variation_230

class NonSpare_1627(NonSpare):
    name = "SIGX"
    title = "Sigma (X)) Standard Deviation on the Horizontal Axis of the Local Grid System"
    rule = RuleVariation_224

class Item_849(Item):
    non_spare = NonSpare_1627

class NonSpare_1628(NonSpare):
    name = "SIGY"
    title = "Sigma (Y)) Standard Deviation on the Vertical Axis of the Local Grid System"
    rule = RuleVariation_224

class Item_850(Item):
    non_spare = NonSpare_1628

class Variation_233(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_750

class RuleVariation_227(RuleVariationContextFree):
    variation = Variation_233

class NonSpare_1626(NonSpare):
    name = "SIGV"
    title = "Sigma (V)) Standard Deviation on the Groundspeed Within the Local Grid System"
    rule = RuleVariation_227

class Item_848(Item):
    non_spare = NonSpare_1626

class Content_764(ContentQuantity):
    signedness = Unsigned
    lsb = 8.7890625e-2
    unit = "°"

class RuleContent_763(RuleContentContextFree):
    variation = Content_764

class Variation_237(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_763

class RuleVariation_231(RuleVariationContextFree):
    variation = Variation_237

class NonSpare_1625(NonSpare):
    name = "SIGH"
    title = "Sigma (H)) Standard Deviation on the Heading Within the Local Grid System"
    rule = RuleVariation_231

class Item_847(Item):
    non_spare = NonSpare_1625

class Variation_1172(Group):
    bit_size = 32
    items_list = [Item_849, Item_850, Item_848, Item_847]
    items_dict = {"SIGX": NonSpare_1627, "SIGY": NonSpare_1628, "SIGV": NonSpare_1626, "SIGH": NonSpare_1625}

class RuleVariation_1119(RuleVariationContextFree):
    variation = Variation_1172

class NonSpare_383(NonSpare):
    name = "210"
    title = "Track Quality"
    rule = RuleVariation_1119

class Content_376(ContentTable):
    values = {0: "Not defined; never used", 1: "Multipath Reply (Reflection)", 2: "Reply due to sidelobe interrogation/reception", 3: "Split plot", 4: "Second time around reply", 5: "Angel", 6: "Slow moving target correlated with road infrastructure (terrestrial vehicle)", 7: "Fixed PSR plot", 8: "Slow PSR target", 9: "Low quality PSR plot", 10: "Phantom SSR plot", 11: "Non-Matching Mode-3/A Code", 12: "Mode C code / Mode S altitude code abnormal value compared to the track", 13: "Target in Clutter Area", 14: "Maximum Doppler Response in Zero Filter", 15: "Transponder anomaly detected", 16: "Duplicated or Illegal Mode S Aircraft Address", 17: "Mode S error correction applied", 18: "Undecodable Mode C code / Mode S altitude code", 19: "Birds", 20: "Flock of Birds", 21: "Mode-1 was present in original reply", 22: "Mode-2 was present in original reply", 23: "Plot potentially caused by Wind Turbine", 24: "Helicopter", 25: "Maximum number of re-interrogations reached (surveillance information)", 26: "Maximum number of re-interrogations reached (BDS Extractions)", 27: "BDS Overlay Incoherence", 28: "Potential BDS Swap Detected", 29: "Track Update in the Zenithal Gap", 30: "Mode S Track re-acquired", 31: "Duplicated Mode 5 Pair NO/PIN detected"}

class RuleContent_376(RuleContentContextFree):
    variation = Content_376

class Variation_148(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_376

class Variation_1377(Repetitive):
    rep_bytes = None
    variation = Variation_148

class RuleVariation_1307(RuleVariationContextFree):
    variation = Variation_1377

class NonSpare_113(NonSpare):
    name = "030"
    title = "Warning/Error Conditions and Target Classification"
    rule = RuleVariation_1307

class NonSpare_255(NonSpare):
    name = "100"
    title = "Mode-C Code and Code Confidence Indicator"
    rule = RuleVariation_1145

class Variation_583(Element):
    bit_offset8 = 2
    bit_size = 14
    rule = RuleContent_605

class RuleVariation_573(RuleVariationContextFree):
    variation = Variation_583

class NonSpare_457(NonSpare):
    name = "3DH"
    title = "3D Height, in Binary Notation. Negative Values Are Expressed in Two's Complement"
    rule = RuleVariation_573

class Item_32(Item):
    non_spare = NonSpare_457

class Variation_969(Group):
    bit_size = 16
    items_list = [Item_1, Item_32]
    items_dict = {"3DH": NonSpare_457}

class RuleVariation_940(RuleVariationContextFree):
    variation = Variation_969

class NonSpare_268(NonSpare):
    name = "110"
    title = "Height Measured by a 3D Radar"
    rule = RuleVariation_940

class Content_194(ContentTable):
    values = {0: "Doppler speed is valid", 1: "Doppler speed is doubtful"}

class RuleContent_194(RuleContentContextFree):
    variation = Content_194

class Variation_38(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_194

class RuleVariation_38(RuleVariationContextFree):
    variation = Variation_38

class NonSpare_802(NonSpare):
    name = "D"
    title = ""
    rule = RuleVariation_38

class Item_226(Item):
    non_spare = NonSpare_802

class Item_8(Spare):
    bit_offset8 = 1
    bit_size = 5

class Content_601(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = "m/s"

class RuleContent_601(RuleContentContextFree):
    variation = Content_601

class Variation_934(Element):
    bit_offset8 = 6
    bit_size = 10
    rule = RuleContent_601

class RuleVariation_905(RuleVariationContextFree):
    variation = Variation_934

class NonSpare_655(NonSpare):
    name = "CAL"
    title = "Calculated Doppler Speed, Coded in Two's Complement"
    rule = RuleVariation_905

class Item_129(Item):
    non_spare = NonSpare_655

class Variation_1045(Group):
    bit_size = 16
    items_list = [Item_226, Item_8, Item_129]
    items_dict = {"D": NonSpare_802, "CAL": NonSpare_655}

class RuleVariation_1006(RuleVariationContextFree):
    variation = Variation_1045

class NonSpare_654(NonSpare):
    name = "CAL"
    title = "Calculated Doppler Speed"
    rule = RuleVariation_1006

class Content_692(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "m/s"

class RuleContent_692(RuleContentContextFree):
    variation = Content_692

class Variation_297(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_692

class RuleVariation_291(RuleVariationContextFree):
    variation = Variation_297

class NonSpare_840(NonSpare):
    name = "DOP"
    title = "Doppler Speed"
    rule = RuleVariation_291

class Item_259(Item):
    non_spare = NonSpare_840

class NonSpare_563(NonSpare):
    name = "AMB"
    title = "Ambiguity Range"
    rule = RuleVariation_291

class Item_68(Item):
    non_spare = NonSpare_563

class Content_683(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "MHz"

class RuleContent_683(RuleContentContextFree):
    variation = Content_683

class Variation_291(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_683

class RuleVariation_285(RuleVariationContextFree):
    variation = Variation_291

class NonSpare_931(NonSpare):
    name = "FRQ"
    title = "Transmitter Frequency"
    rule = RuleVariation_285

class Item_324(Item):
    non_spare = NonSpare_931

class Variation_1048(Group):
    bit_size = 48
    items_list = [Item_259, Item_68, Item_324]
    items_dict = {"DOP": NonSpare_840, "AMB": NonSpare_563, "FRQ": NonSpare_931}

class Variation_1345(Repetitive):
    rep_bytes = 1
    variation = Variation_1048

class RuleVariation_1275(RuleVariationContextFree):
    variation = Variation_1345

class NonSpare_1477(NonSpare):
    name = "RDS"
    title = "Raw Doppler Speed"
    rule = RuleVariation_1275

class Variation_1396(Compound):
    items_list = [NonSpare_654, NonSpare_1477]
    items_dict = {"CAL": NonSpare_654, "RDS": NonSpare_1477}

class RuleVariation_1326(RuleVariationContextFree):
    variation = Variation_1396

class NonSpare_279(NonSpare):
    name = "120"
    title = "Radial Doppler Speed"
    rule = RuleVariation_1326

class Content_324(ContentTable):
    values = {0: "No communications capability (surveillance only)", 1: "Comm. A and Comm. B capability", 2: "Comm. A, Comm. B and Uplink ELM", 3: "Comm. A, Comm. B, Uplink ELM and Downlink ELM", 4: "Level 5 Transponder capability"}

class RuleContent_324(RuleContentContextFree):
    variation = Content_324

class Variation_118(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_324

class RuleVariation_118(RuleVariationContextFree):
    variation = Variation_118

class NonSpare_734(NonSpare):
    name = "COM"
    title = "Communications Capability of the Transponder"
    rule = RuleVariation_118

class Item_181(Item):
    non_spare = NonSpare_734

class Content_310(ContentTable):
    values = {0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 7: "Unknown"}

class RuleContent_310(RuleContentContextFree):
    variation = Content_310

class Variation_661(Element):
    bit_offset8 = 3
    bit_size = 3
    rule = RuleContent_310

class RuleVariation_651(RuleVariationContextFree):
    variation = Variation_661

class NonSpare_1686(NonSpare):
    name = "STAT"
    title = "Flight Status"
    rule = RuleVariation_651

class Item_894(Item):
    non_spare = NonSpare_1686

class Content_426(ContentTable):
    values = {0: "SI-Code Capable", 1: "II-Code Capable"}

class RuleContent_426(RuleContentContextFree):
    variation = Content_426

class Variation_917(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_426

class RuleVariation_888(RuleVariationContextFree):
    variation = Variation_917

class NonSpare_1615(NonSpare):
    name = "SI"
    title = "SI/II Transponder Capability"
    rule = RuleVariation_888

class Item_839(Item):
    non_spare = NonSpare_1615

class Variation_1038(Group):
    bit_size = 16
    items_list = [Item_181, Item_894, Item_839, Item_29, Item_564, Item_77, Item_54, Item_104, Item_106]
    items_dict = {"COM": NonSpare_734, "STAT": NonSpare_1686, "SI": NonSpare_1615, "MSSC": NonSpare_1255, "ARC": NonSpare_581, "AIC": NonSpare_549, "B1A": NonSpare_621, "B1B": NonSpare_623}

class RuleVariation_999(RuleVariationContextFree):
    variation = Variation_1038

class NonSpare_395(NonSpare):
    name = "230"
    title = "Communications/ACAS Capability and Flight Status"
    rule = RuleVariation_999

class NonSpare_416(NonSpare):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    rule = RuleVariation_377

class Content_271(ContentTable):
    values = {0: "Mode-1 code as derived from the reply of the transponder", 1: "Smoothed Mode-1 code as provided by a local tracker"}

class RuleContent_271(RuleContentContextFree):
    variation = Content_271

class Variation_525(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_271

class RuleVariation_515(RuleVariationContextFree):
    variation = Variation_525

class NonSpare_1061(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_515

class Item_418(Item):
    non_spare = NonSpare_1061

class NonSpare_1223(NonSpare):
    name = "MODE1"
    title = "Mode-1 Code"
    rule = RuleVariation_659

class Item_534(Item):
    non_spare = NonSpare_1223

class Variation_1215(Group):
    bit_size = 8
    items_list = [Item_1049, Item_333, Item_418, Item_534]
    items_dict = {"V": NonSpare_1890, "G": NonSpare_943, "L": NonSpare_1061, "MODE1": NonSpare_1223}

class RuleVariation_1149(RuleVariationContextFree):
    variation = Variation_1215

class NonSpare_175(NonSpare):
    name = "055"
    title = "Mode-1 Code in Octal Representation"
    rule = RuleVariation_1149

class NonSpare_162(NonSpare):
    name = "050"
    title = "Mode-2 Code in Octal Representation"
    rule = RuleVariation_1150

class Variation_616(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_221

class RuleVariation_606(RuleVariationContextFree):
    variation = Variation_616

class NonSpare_1411(NonSpare):
    name = "QA4"
    title = ""
    rule = RuleVariation_606

class Item_690(Item):
    non_spare = NonSpare_1411

class Variation_698(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_220

class RuleVariation_688(RuleVariationContextFree):
    variation = Variation_698

class NonSpare_1406(NonSpare):
    name = "QA2"
    title = ""
    rule = RuleVariation_688

class Item_685(Item):
    non_spare = NonSpare_1406

class Variation_898(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_223

class RuleVariation_869(RuleVariationContextFree):
    variation = Variation_898

class NonSpare_1420(NonSpare):
    name = "QB2"
    title = ""
    rule = RuleVariation_869

class Item_699(Item):
    non_spare = NonSpare_1420

class Variation_943(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_222

class RuleVariation_914(RuleVariationContextFree):
    variation = Variation_943

class NonSpare_1416(NonSpare):
    name = "QB1"
    title = ""
    rule = RuleVariation_914

class Item_695(Item):
    non_spare = NonSpare_1416

class Variation_980(Group):
    bit_size = 8
    items_list = [Item_2, Item_690, Item_685, Item_682, Item_699, Item_695]
    items_dict = {"QA4": NonSpare_1411, "QA2": NonSpare_1406, "QA1": NonSpare_1403, "QB2": NonSpare_1420, "QB1": NonSpare_1416}

class RuleVariation_951(RuleVariationContextFree):
    variation = Variation_980

class NonSpare_190(NonSpare):
    name = "065"
    title = "Mode-1 Code Confidence Indicator"
    rule = RuleVariation_951

class NonSpare_177(NonSpare):
    name = "060"
    title = "Mode-2 Code Confidence Indicator"
    rule = RuleVariation_966

class Record_28(Record):
    items_list = [NonSpare_38, NonSpare_309, NonSpare_85, NonSpare_132, NonSpare_198, NonSpare_231, NonSpare_290, NonSpare_387, NonSpare_401, NonSpare_414, NonSpare_337, NonSpare_152, NonSpare_359, NonSpare_349, NonSpare_383, NonSpare_113, NonSpare_217, NonSpare_255, NonSpare_268, NonSpare_279, NonSpare_395, NonSpare_416, NonSpare_175, NonSpare_162, NonSpare_190, NonSpare_177, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_38, "140": NonSpare_309, "020": NonSpare_85, "040": NonSpare_132, "070": NonSpare_198, "090": NonSpare_231, "130": NonSpare_290, "220": NonSpare_387, "240": NonSpare_401, "250": NonSpare_414, "161": NonSpare_337, "042": NonSpare_152, "200": NonSpare_359, "170": NonSpare_349, "210": NonSpare_383, "030": NonSpare_113, "080": NonSpare_217, "100": NonSpare_255, "110": NonSpare_268, "120": NonSpare_279, "230": NonSpare_395, "260": NonSpare_416, "055": NonSpare_175, "050": NonSpare_162, "065": NonSpare_190, "060": NonSpare_177, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_24(UapSingle):
    record = Record_28

class Asterix_40(AstCat):
    category = 48
    edition = (1, 27)
    uap = Uap_24

class Content_377(ContentTable):
    values = {0: "Not defined; never used", 1: "Multipath Reply (Reflection)", 2: "Reply due to sidelobe interrogation/reception", 3: "Split plot", 4: "Second time around reply", 5: "Angel", 6: "Slow moving target correlated with road infrastructure (terrestrial vehicle)", 7: "Fixed PSR plot", 8: "Slow PSR target", 9: "Low quality PSR plot", 10: "Phantom SSR plot", 11: "Non-Matching Mode-3/A Code", 12: "Mode C code / Mode S altitude code abnormal value compared to the track", 13: "Target in Clutter Area", 14: "Maximum Doppler Response in Zero Filter", 15: "Transponder anomaly detected", 16: "Duplicated or Illegal Mode S Aircraft Address", 17: "Mode S error correction applied", 18: "Undecodable Mode C code / Mode S altitude code", 19: "Birds", 20: "Flock of Birds", 21: "Mode-1 was present in original reply", 22: "Mode-2 was present in original reply", 23: "Plot potentially caused by Wind Turbine", 24: "Helicopter", 25: "Maximum number of re-interrogations reached (surveillance information)", 26: "Maximum number of re-interrogations reached (BDS Extractions)", 27: "BDS Overlay Incoherence", 28: "Potential BDS Swap Detected", 29: "Track Update in the Zenithal Gap", 30: "Mode S Track re-acquired", 31: "Duplicated Mode 5 Pair NO/PIN detected", 32: "Wrong DF reply format detected", 33: "Transponder anomaly (MS XPD replies with Mode A/C to Mode A/C-only all-call)", 34: "Transponder anomaly (SI capability report wrong)"}

class RuleContent_377(RuleContentContextFree):
    variation = Content_377

class Variation_149(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_377

class Variation_1378(Repetitive):
    rep_bytes = None
    variation = Variation_149

class RuleVariation_1308(RuleVariationContextFree):
    variation = Variation_1378

class NonSpare_114(NonSpare):
    name = "030"
    title = "Warning/Error Conditions and Target Classification"
    rule = RuleVariation_1308

class Record_29(Record):
    items_list = [NonSpare_38, NonSpare_309, NonSpare_85, NonSpare_132, NonSpare_198, NonSpare_231, NonSpare_290, NonSpare_387, NonSpare_401, NonSpare_414, NonSpare_337, NonSpare_152, NonSpare_359, NonSpare_349, NonSpare_383, NonSpare_114, NonSpare_217, NonSpare_255, NonSpare_268, NonSpare_279, NonSpare_395, NonSpare_416, NonSpare_175, NonSpare_162, NonSpare_190, NonSpare_177, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_38, "140": NonSpare_309, "020": NonSpare_85, "040": NonSpare_132, "070": NonSpare_198, "090": NonSpare_231, "130": NonSpare_290, "220": NonSpare_387, "240": NonSpare_401, "250": NonSpare_414, "161": NonSpare_337, "042": NonSpare_152, "200": NonSpare_359, "170": NonSpare_349, "210": NonSpare_383, "030": NonSpare_114, "080": NonSpare_217, "100": NonSpare_255, "110": NonSpare_268, "120": NonSpare_279, "230": NonSpare_395, "260": NonSpare_416, "055": NonSpare_175, "050": NonSpare_162, "065": NonSpare_190, "060": NonSpare_177, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_25(UapSingle):
    record = Record_29

class Asterix_41(AstCat):
    category = 48
    edition = (1, 28)
    uap = Uap_25

class NonSpare_410(NonSpare):
    name = "250"
    title = "BDS Register Data"
    rule = RuleVariation_1279

class NonSpare_417(NonSpare):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    rule = RuleVariation_377

class Record_27(Record):
    items_list = [NonSpare_38, NonSpare_309, NonSpare_85, NonSpare_132, NonSpare_198, NonSpare_231, NonSpare_290, NonSpare_387, NonSpare_401, NonSpare_410, NonSpare_337, NonSpare_152, NonSpare_359, NonSpare_349, NonSpare_383, NonSpare_114, NonSpare_217, NonSpare_255, NonSpare_268, NonSpare_279, NonSpare_395, NonSpare_417, NonSpare_175, NonSpare_162, NonSpare_190, NonSpare_177, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_38, "140": NonSpare_309, "020": NonSpare_85, "040": NonSpare_132, "070": NonSpare_198, "090": NonSpare_231, "130": NonSpare_290, "220": NonSpare_387, "240": NonSpare_401, "250": NonSpare_410, "161": NonSpare_337, "042": NonSpare_152, "200": NonSpare_359, "170": NonSpare_349, "210": NonSpare_383, "030": NonSpare_114, "080": NonSpare_217, "100": NonSpare_255, "110": NonSpare_268, "120": NonSpare_279, "230": NonSpare_395, "260": NonSpare_417, "055": NonSpare_175, "050": NonSpare_162, "065": NonSpare_190, "060": NonSpare_177, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_23(UapSingle):
    record = Record_27

class Asterix_42(AstCat):
    category = 48
    edition = (1, 29)
    uap = Uap_23

class NonSpare_388(NonSpare):
    name = "220"
    title = "Aircraft Address"
    rule = RuleVariation_335

class NonSpare_402(NonSpare):
    name = "240"
    title = "Aircraft Identification"
    rule = RuleVariation_376

class NonSpare_409(NonSpare):
    name = "250"
    title = "BDS Register Data"
    rule = RuleVariation_1279

class NonSpare_396(NonSpare):
    name = "230"
    title = "Communications/ACAS Capability and Flight Status"
    rule = RuleVariation_999

class NonSpare_161(NonSpare):
    name = "050"
    title = "Mode-2 Code in Octal Representation"
    rule = RuleVariation_1150

class Record_30(Record):
    items_list = [NonSpare_38, NonSpare_309, NonSpare_85, NonSpare_132, NonSpare_198, NonSpare_231, NonSpare_290, NonSpare_388, NonSpare_402, NonSpare_409, NonSpare_337, NonSpare_152, NonSpare_359, NonSpare_349, NonSpare_383, NonSpare_114, NonSpare_217, NonSpare_255, NonSpare_268, NonSpare_279, NonSpare_396, NonSpare_417, NonSpare_175, NonSpare_161, NonSpare_190, NonSpare_177, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_38, "140": NonSpare_309, "020": NonSpare_85, "040": NonSpare_132, "070": NonSpare_198, "090": NonSpare_231, "130": NonSpare_290, "220": NonSpare_388, "240": NonSpare_402, "250": NonSpare_409, "161": NonSpare_337, "042": NonSpare_152, "200": NonSpare_359, "170": NonSpare_349, "210": NonSpare_383, "030": NonSpare_114, "080": NonSpare_217, "100": NonSpare_255, "110": NonSpare_268, "120": NonSpare_279, "230": NonSpare_396, "260": NonSpare_417, "055": NonSpare_175, "050": NonSpare_161, "065": NonSpare_190, "060": NonSpare_177, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_26(UapSingle):
    record = Record_30

class Asterix_43(AstCat):
    category = 48
    edition = (1, 30)
    uap = Uap_26

class Content_9(ContentTable):
    values = {0: "ADSB not populated", 1: "ADSB populated"}

class RuleContent_9(RuleContentContextFree):
    variation = Content_9

class Variation_1(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_9

class RuleVariation_1(RuleVariationContextFree):
    variation = Variation_1

class NonSpare_880(NonSpare):
    name = "EP"
    title = "ADSB Element Populated Bit"
    rule = RuleVariation_1

class Item_285(Item):
    non_spare = NonSpare_880

class Content_374(ContentTable):
    values = {0: "Not available", 1: "Available"}

class RuleContent_374(RuleContentContextFree):
    variation = Content_374

class Variation_434(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_374

class RuleVariation_424(RuleVariationContextFree):
    variation = Variation_434

class NonSpare_1894(NonSpare):
    name = "VAL"
    title = "On-Site ADS-B Information"
    rule = RuleVariation_424

class Item_1052(Item):
    non_spare = NonSpare_1894

class Variation_1052(Group):
    bit_size = 2
    items_list = [Item_285, Item_1052]
    items_dict = {"EP": NonSpare_880, "VAL": NonSpare_1894}

class RuleVariation_1011(RuleVariationContextFree):
    variation = Variation_1052

class NonSpare_539(NonSpare):
    name = "ADSB"
    title = "On-Site ADS-B Information"
    rule = RuleVariation_1011

class Item_47(Item):
    non_spare = NonSpare_539

class Content_425(ContentTable):
    values = {0: "SCN not populated", 1: "SCN populated"}

class RuleContent_425(RuleContentContextFree):
    variation = Content_425

class Variation_552(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_425

class RuleVariation_542(RuleVariationContextFree):
    variation = Variation_552

class NonSpare_889(NonSpare):
    name = "EP"
    title = "SCN Element Populated Bit"
    rule = RuleVariation_542

class Item_294(Item):
    non_spare = NonSpare_889

class Variation_631(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_374

class RuleVariation_621(RuleVariationContextFree):
    variation = Variation_631

class NonSpare_1896(NonSpare):
    name = "VAL"
    title = "Surveillance Cluster Network Information"
    rule = RuleVariation_621

class Item_1054(Item):
    non_spare = NonSpare_1896

class Variation_1069(Group):
    bit_size = 2
    items_list = [Item_294, Item_1054]
    items_dict = {"EP": NonSpare_889, "VAL": NonSpare_1896}

class RuleVariation_1028(RuleVariationContextFree):
    variation = Variation_1069

class NonSpare_1578(NonSpare):
    name = "SCN"
    title = "Surveillance Cluster Network Information"
    rule = RuleVariation_1028

class Item_820(Item):
    non_spare = NonSpare_1578

class Content_399(ContentTable):
    values = {0: "PAI not populated", 1: "PAI populated"}

class RuleContent_399(RuleContentContextFree):
    variation = Content_399

class Variation_718(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_399

class RuleVariation_708(RuleVariationContextFree):
    variation = Variation_718

class NonSpare_888(NonSpare):
    name = "EP"
    title = "PAI Element Populated Bit"
    rule = RuleVariation_708

class Item_293(Item):
    non_spare = NonSpare_888

class Variation_835(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_374

class RuleVariation_806(RuleVariationContextFree):
    variation = Variation_835

class NonSpare_1895(NonSpare):
    name = "VAL"
    title = "Passive Acquisition Interface Information"
    rule = RuleVariation_806

class Item_1053(Item):
    non_spare = NonSpare_1895

class Variation_1068(Group):
    bit_size = 2
    items_list = [Item_293, Item_1053]
    items_dict = {"EP": NonSpare_888, "VAL": NonSpare_1895}

class RuleVariation_1027(RuleVariationContextFree):
    variation = Variation_1068

class NonSpare_1340(NonSpare):
    name = "PAI"
    title = "Passive Acquisition Interface Information"
    rule = RuleVariation_1027

class Item_642(Item):
    non_spare = NonSpare_1340

class Variation_1321(Extended):
    items = [Item_1020, Item_857, Item_747, Item_868, Item_731, None, Item_998, Item_297, Item_1149, Item_513, Item_520, Item_312, None, Item_47, Item_820, Item_642, Item_26, None]

class RuleVariation_1251(RuleVariationContextFree):
    variation = Variation_1321

class NonSpare_86(NonSpare):
    name = "020"
    title = "Target Report Descriptor"
    rule = RuleVariation_1251

class Content_378(ContentTable):
    values = {0: "Not defined; never used", 1: "Multipath Reply (Reflection)", 2: "Reply due to sidelobe interrogation/reception", 3: "Split plot", 4: "Second time around reply", 5: "Angel", 6: "Slow moving target correlated with road infrastructure (terrestrial vehicle)", 7: "Fixed PSR plot", 8: "Slow PSR target", 9: "Low quality PSR plot", 10: "Phantom SSR plot", 11: "Non-Matching Mode-3/A Code", 12: "Mode C code / Mode S altitude code abnormal value compared to the track", 13: "Target in Clutter Area", 14: "Maximum Doppler Response in Zero Filter", 15: "Transponder anomaly detected", 16: "Duplicated or Illegal Mode S Aircraft Address", 17: "Mode S error correction applied", 18: "Undecodable Mode C code / Mode S altitude code", 19: "Birds", 20: "Flock of Birds", 21: "Mode-1 was present in original reply", 22: "Mode-2 was present in original reply", 23: "Plot potentially caused by Wind Turbine", 24: "Helicopter", 25: "Maximum number of re-interrogations reached (surveillance information)", 26: "Maximum number of re-interrogations reached (BDS Extractions)", 27: "BDS Overlay Incoherence", 28: "Potential BDS Swap Detected", 29: "Track Update in the Zenithal Gap", 30: "Mode S Track re-acquired", 31: "Duplicated Mode 5 Pair NO/PIN detected", 32: "Wrong DF reply format detected", 33: "Transponder anomaly (MS XPD replies with Mode A/C to Mode A/C-only all-call)", 34: "Transponder anomaly (SI capability report wrong)", 35: "Potential IC Conflict", 36: "IC Conflict detection possible-no conflict currently detected"}

class RuleContent_378(RuleContentContextFree):
    variation = Content_378

class Variation_150(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_378

class Variation_1379(Repetitive):
    rep_bytes = None
    variation = Variation_150

class RuleVariation_1309(RuleVariationContextFree):
    variation = Variation_1379

class NonSpare_115(NonSpare):
    name = "030"
    title = "Warning/Error Conditions and Target Classification"
    rule = RuleVariation_1309

class NonSpare_280(NonSpare):
    name = "120"
    title = "Radial Doppler Speed"
    rule = RuleVariation_1326

class Record_31(Record):
    items_list = [NonSpare_38, NonSpare_309, NonSpare_86, NonSpare_132, NonSpare_198, NonSpare_231, NonSpare_290, NonSpare_388, NonSpare_402, NonSpare_409, NonSpare_337, NonSpare_152, NonSpare_359, NonSpare_349, NonSpare_383, NonSpare_115, NonSpare_217, NonSpare_255, NonSpare_268, NonSpare_280, NonSpare_396, NonSpare_417, NonSpare_175, NonSpare_161, NonSpare_190, NonSpare_177, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_38, "140": NonSpare_309, "020": NonSpare_86, "040": NonSpare_132, "070": NonSpare_198, "090": NonSpare_231, "130": NonSpare_290, "220": NonSpare_388, "240": NonSpare_402, "250": NonSpare_409, "161": NonSpare_337, "042": NonSpare_152, "200": NonSpare_359, "170": NonSpare_349, "210": NonSpare_383, "030": NonSpare_115, "080": NonSpare_217, "100": NonSpare_255, "110": NonSpare_268, "120": NonSpare_280, "230": NonSpare_396, "260": NonSpare_417, "055": NonSpare_175, "050": NonSpare_161, "065": NonSpare_190, "060": NonSpare_177, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_27(UapSingle):
    record = Record_31

class Asterix_44(AstCat):
    category = 48
    edition = (1, 31)
    uap = Uap_27

class Content_333(ContentTable):
    values = {0: "No detection", 1: "Single PSR detection", 2: "Single SSR detection", 3: "SSR+PSR detection", 4: "Single Mode S All-Call", 5: "Single Mode S Roll-Call", 6: "Mode S All-Call + PSR", 7: "Mode S Roll-Call + PSR", 8: "ADS-B", 9: "WAM"}

class RuleContent_333(RuleContentContextFree):
    variation = Content_333

class Variation_771(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_333

class RuleVariation_742(RuleVariationContextFree):
    variation = Variation_771

class NonSpare_1863(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_742

class Item_1024(Item):
    non_spare = NonSpare_1863

class NonSpare_1122(NonSpare):
    name = "LTN"
    title = "Local Track Number"
    rule = RuleVariation_240

class Item_475(Item):
    non_spare = NonSpare_1122

class Variation_1156(Group):
    bit_size = 40
    items_list = [Item_811, Item_842, Item_3, Item_1024, Item_475]
    items_dict = {"SAC": NonSpare_1563, "SIC": NonSpare_1618, "TYP": NonSpare_1863, "LTN": NonSpare_1122}

class Variation_1356(Repetitive):
    rep_bytes = 1
    variation = Variation_1156

class RuleVariation_1286(RuleVariationContextFree):
    variation = Variation_1356

class NonSpare_791(NonSpare):
    name = "CST"
    title = "Contributing Sensors With Local Tracknumbers"
    rule = RuleVariation_1286

class Variation_1155(Group):
    bit_size = 24
    items_list = [Item_811, Item_842, Item_3, Item_1024]
    items_dict = {"SAC": NonSpare_1563, "SIC": NonSpare_1618, "TYP": NonSpare_1863}

class Variation_1355(Repetitive):
    rep_bytes = 1
    variation = Variation_1155

class RuleVariation_1285(RuleVariationContextFree):
    variation = Variation_1355

class NonSpare_785(NonSpare):
    name = "CSN"
    title = "Contributing Sensors No Local Tracknumbers"
    rule = RuleVariation_1285

class Content_634(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "m/s"

class RuleContent_634(RuleContentContextFree):
    variation = Content_634

class Variation_271(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_634

class RuleVariation_265(RuleVariationContextFree):
    variation = Variation_271

class NonSpare_1933(NonSpare):
    name = "VX"
    title = ""
    rule = RuleVariation_265

class Item_1087(Item):
    non_spare = NonSpare_1933

class NonSpare_1938(NonSpare):
    name = "VY"
    title = ""
    rule = RuleVariation_265

class Item_1092(Item):
    non_spare = NonSpare_1938

class Variation_1234(Group):
    bit_size = 32
    items_list = [Item_1087, Item_1092]
    items_dict = {"VX": NonSpare_1933, "VY": NonSpare_1938}

class RuleVariation_1168(RuleVariationContextFree):
    variation = Variation_1234

class NonSpare_1854(NonSpare):
    name = "TVS"
    title = "Calculated Track Velocity Relative to System Reference Point"
    rule = RuleVariation_1168

class Content_207(ContentTable):
    values = {0: "Flight plan data from active FDPS", 1: "Flight plan data retained from no longer active FDPS"}

class RuleContent_207(RuleContentContextFree):
    variation = Content_207

class Variation_41(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_207

class RuleVariation_41(RuleVariationContextFree):
    variation = Variation_41

class NonSpare_906(NonSpare):
    name = "FDR"
    title = "Flight Data Retained"
    rule = RuleVariation_41

class Item_307(Item):
    non_spare = NonSpare_906

class Variation_1292(Extended):
    items = [Item_307, Item_9, None]

class RuleVariation_1222(RuleVariationContextFree):
    variation = Variation_1292

class NonSpare_1709(NonSpare):
    name = "STS"
    title = "Supplementary Track Status"
    rule = RuleVariation_1222

class Expansion_2(Expansion):
    fspec_bytes = 1
    items = [NonSpare_791, NonSpare_785, NonSpare_1854, NonSpare_1709]

class Asterix_45(AstRef):
    category = 62
    edition = (1, 2)
    expansion = Expansion_2

class NonSpare_45(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class NonSpare_57(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class NonSpare_202(NonSpare):
    name = "070"
    title = "Time Of Track Information"
    rule = RuleVariation_356

class NonSpare_262(NonSpare):
    name = "105"
    title = "Calculated Position In WGS-84 Co-ordinates"
    rule = RuleVariation_1051

class NonSpare_1978(NonSpare):
    name = "X"
    title = "X Coordinate"
    rule = RuleVariation_338

class Item_1127(Item):
    non_spare = NonSpare_1978

class NonSpare_2031(NonSpare):
    name = "Y"
    title = "Y Coordinate"
    rule = RuleVariation_338

class Item_1177(Item):
    non_spare = NonSpare_2031

class Variation_1263(Group):
    bit_size = 48
    items_list = [Item_1127, Item_1177]
    items_dict = {"X": NonSpare_1978, "Y": NonSpare_2031}

class RuleVariation_1197(RuleVariationContextFree):
    variation = Variation_1263

class NonSpare_250(NonSpare):
    name = "100"
    title = "Calculated Track Position (Cartesian)"
    rule = RuleVariation_1197

class NonSpare_1934(NonSpare):
    name = "VX"
    title = "Velocity (X-component)"
    rule = RuleVariation_265

class Item_1088(Item):
    non_spare = NonSpare_1934

class NonSpare_1939(NonSpare):
    name = "VY"
    title = "Velocity (Y-component)"
    rule = RuleVariation_265

class Item_1093(Item):
    non_spare = NonSpare_1939

class Variation_1235(Group):
    bit_size = 32
    items_list = [Item_1088, Item_1093]
    items_dict = {"VX": NonSpare_1934, "VY": NonSpare_1939}

class RuleVariation_1169(RuleVariationContextFree):
    variation = Variation_1235

class NonSpare_356(NonSpare):
    name = "185"
    title = "Calculated Track Velocity (Cartesian)"
    rule = RuleVariation_1169

class Content_635(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "m/s²"

class RuleContent_635(RuleContentContextFree):
    variation = Content_635

class Variation_201(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_635

class RuleVariation_195(RuleVariationContextFree):
    variation = Variation_201

class NonSpare_607(NonSpare):
    name = "AX"
    title = ""
    rule = RuleVariation_195

class Item_93(Item):
    non_spare = NonSpare_607

class NonSpare_611(NonSpare):
    name = "AY"
    title = ""
    rule = RuleVariation_195

class Item_97(Item):
    non_spare = NonSpare_611

class Variation_1019(Group):
    bit_size = 16
    items_list = [Item_93, Item_97]
    items_dict = {"AX": NonSpare_607, "AY": NonSpare_611}

class RuleVariation_987(RuleVariationContextFree):
    variation = Variation_1019

class NonSpare_378(NonSpare):
    name = "210"
    title = "Calculated Acceleration (Cartesian)"
    rule = RuleVariation_987

class Content_319(ContentTable):
    values = {0: "No change", 1: "Mode 3/A has changed"}

class RuleContent_319(RuleContentContextFree):
    variation = Content_319

class Variation_539(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_319

class RuleVariation_529(RuleVariationContextFree):
    variation = Variation_539

class NonSpare_679(NonSpare):
    name = "CH"
    title = "Change in Mode 3/A"
    rule = RuleVariation_529

class Item_145(Item):
    non_spare = NonSpare_679

class Variation_1224(Group):
    bit_size = 16
    items_list = [Item_1050, Item_335, Item_145, Item_16, Item_543]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "CH": NonSpare_679, "MODE3A": NonSpare_1232}

class RuleVariation_1158(RuleVariationContextFree):
    variation = Variation_1224

class NonSpare_189(NonSpare):
    name = "060"
    title = "Track Mode 3/A Code"
    rule = RuleVariation_1158

class Content_47(ContentTable):
    values = {0: "Callsign or registration downlinked from target", 1: "Callsign not downlinked from target", 2: "Registration not downlinked from target", 3: "Invalid"}

class RuleContent_47(RuleContentContextFree):
    variation = Content_47

class Variation_92(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_47

class RuleVariation_92(RuleVariationContextFree):
    variation = Variation_92

class NonSpare_1698(NonSpare):
    name = "STI"
    title = ""
    rule = RuleVariation_92

class Item_905(Item):
    non_spare = NonSpare_1698

class Variation_1176(Group):
    bit_size = 56
    items_list = [Item_905, Item_15, Item_152]
    items_dict = {"STI": NonSpare_1698, "CHR": NonSpare_686}

class RuleVariation_1123(RuleVariationContextFree):
    variation = Variation_1176

class NonSpare_404(NonSpare):
    name = "245"
    title = "Target Identification"
    rule = RuleVariation_1123

class NonSpare_533(NonSpare):
    name = "ADR"
    title = "Target Address"
    rule = RuleVariation_335

class NonSpare_1041(NonSpare):
    name = "ID"
    title = "Target Identification"
    rule = RuleVariation_376

class NonSpare_1203(NonSpare):
    name = "MHG"
    title = "Magnetic Heading"
    rule = RuleVariation_329

class Content_22(ContentTable):
    values = {0: "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001"}

class RuleContent_22(RuleContentContextFree):
    variation = Content_22

class Variation_7(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_22

class RuleVariation_7(RuleVariationContextFree):
    variation = Variation_7

class NonSpare_1051(NonSpare):
    name = "IM"
    title = ""
    rule = RuleVariation_7

class Item_410(Item):
    non_spare = NonSpare_1051

class RuleContent_774(RuleContentDependent):
    depends_on = [["380", "IAS", "IM"]]
    default_variation = Content_0
    cases = [
        ([0], Content_751),
        ([1], Content_721),
    ]

class Variation_488(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_774

class RuleVariation_478(RuleVariationContextFree):
    variation = Variation_488

class NonSpare_1032(NonSpare):
    name = "IAS"
    title = ""
    rule = RuleVariation_478

class Item_397(Item):
    non_spare = NonSpare_1032

class Variation_1085(Group):
    bit_size = 16
    items_list = [Item_410, Item_397]
    items_dict = {"IM": NonSpare_1051, "IAS": NonSpare_1032}

class RuleVariation_1044(RuleVariationContextFree):
    variation = Variation_1085

class NonSpare_1034(NonSpare):
    name = "IAS"
    title = "Indicated Airspeed/Mach No"
    rule = RuleVariation_1044

class Content_689(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "kt"

class RuleContent_689(RuleContentContextFree):
    variation = Content_689

class Variation_295(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_689

class RuleVariation_289(RuleVariationContextFree):
    variation = Variation_295

class NonSpare_1737(NonSpare):
    name = "TAS"
    title = "True Airspeed"
    rule = RuleVariation_289

class Content_359(ContentTable):
    values = {0: "No source information provided", 1: "Source information provided"}

class RuleContent_359(RuleContentContextFree):
    variation = Content_359

class Variation_62(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_359

class RuleVariation_62(RuleVariationContextFree):
    variation = Variation_62

class NonSpare_1571(NonSpare):
    name = "SAS"
    title = ""
    rule = RuleVariation_62

class Item_815(Item):
    non_spare = NonSpare_1571

class Content_515(ContentTable):
    values = {0: "Unknown", 1: "Aircraft altitude", 2: "FCU/MCP selected altitude", 3: "FMS selected altitude"}

class RuleContent_515(RuleContentContextFree):
    variation = Content_515

class Variation_464(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_515

class RuleVariation_454(RuleVariationContextFree):
    variation = Variation_464

class NonSpare_1655(NonSpare):
    name = "SRC"
    title = ""
    rule = RuleVariation_454

class Item_875(Item):
    non_spare = NonSpare_1655

class Content_608(ContentQuantity):
    signedness = Signed
    lsb = 25.0
    unit = "ft"

class RuleContent_608(RuleContentContextFree):
    variation = Content_608

class Variation_672(Element):
    bit_offset8 = 3
    bit_size = 13
    rule = RuleContent_608

class RuleVariation_662(RuleVariationContextFree):
    variation = Variation_672

class NonSpare_556(NonSpare):
    name = "ALT"
    title = "Altitude in Two's Complement Form"
    rule = RuleVariation_662

class Item_61(Item):
    non_spare = NonSpare_556

class Variation_1160(Group):
    bit_size = 16
    items_list = [Item_815, Item_875, Item_61]
    items_dict = {"SAS": NonSpare_1571, "SRC": NonSpare_1655, "ALT": NonSpare_556}

class RuleVariation_1107(RuleVariationContextFree):
    variation = Variation_1160

class NonSpare_1566(NonSpare):
    name = "SAL"
    title = "Selected Altitude"
    rule = RuleVariation_1107

class NonSpare_1262(NonSpare):
    name = "MV"
    title = "Manage Vertical Mode"
    rule = RuleVariation_64

class Item_570(Item):
    non_spare = NonSpare_1262

class NonSpare_543(NonSpare):
    name = "AH"
    title = "Altitude Hold"
    rule = RuleVariation_422

class Item_50(Item):
    non_spare = NonSpare_543

class NonSpare_559(NonSpare):
    name = "AM"
    title = "Approach Mode"
    rule = RuleVariation_534

class Item_64(Item):
    non_spare = NonSpare_559

class Variation_1111(Group):
    bit_size = 16
    items_list = [Item_570, Item_50, Item_64, Item_61]
    items_dict = {"MV": NonSpare_1262, "AH": NonSpare_543, "AM": NonSpare_559, "ALT": NonSpare_556}

class RuleVariation_1066(RuleVariationContextFree):
    variation = Variation_1111

class NonSpare_936(NonSpare):
    name = "FSS"
    title = "Final State Selected Altitude"
    rule = RuleVariation_1066

class Content_496(ContentTable):
    values = {0: "Trajectory intent data is available for this aircraft", 1: "Trajectory intent data is not available for this aircraft"}

class RuleContent_496(RuleContentContextFree):
    variation = Content_496

class Variation_88(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_496

class RuleVariation_88(RuleVariationContextFree):
    variation = Variation_88

class NonSpare_1271(NonSpare):
    name = "NAV"
    title = "TID Available"
    rule = RuleVariation_88

class Item_576(Item):
    non_spare = NonSpare_1271

class Content_497(ContentTable):
    values = {0: "Trajectory intent data is valid", 1: "Trajectory intent data is not valid"}

class RuleContent_497(RuleContentContextFree):
    variation = Content_497

class Variation_448(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_497

class RuleVariation_438(RuleVariationContextFree):
    variation = Variation_448

class NonSpare_1309(NonSpare):
    name = "NVB"
    title = "TID Valid"
    rule = RuleVariation_438

class Item_613(Item):
    non_spare = NonSpare_1309

class Variation_1305(Extended):
    items = [Item_576, Item_613, Item_14, None]

class RuleVariation_1235(RuleVariationContextFree):
    variation = Variation_1305

class NonSpare_1778(NonSpare):
    name = "TIS"
    title = "Trajectory Intent Status"
    rule = RuleVariation_1235

class NonSpare_1744(NonSpare):
    name = "TCA"
    title = "TCP Number Availability"
    rule = RuleVariation_75

class Item_932(Item):
    non_spare = NonSpare_1744

class NonSpare_1280(NonSpare):
    name = "NC"
    title = "TCP Compliance"
    rule = RuleVariation_429

class Item_585(Item):
    non_spare = NonSpare_1280

class NonSpare_1755(NonSpare):
    name = "TCPN"
    title = "Trajectory Change Point Number"
    rule = RuleVariation_569

class Item_942(Item):
    non_spare = NonSpare_1755

class NonSpare_1085(NonSpare):
    name = "LAT"
    title = "Latitude in WGS.84 in Two's Complement"
    rule = RuleVariation_346

class Item_442(Item):
    non_spare = NonSpare_1085

class NonSpare_1113(NonSpare):
    name = "LON"
    title = "Longitude in WGS.84 in Two's Complement"
    rule = RuleVariation_344

class Item_468(Item):
    non_spare = NonSpare_1113

class NonSpare_1757(NonSpare):
    name = "TD"
    title = "Turn Direction"
    rule = RuleVariation_727

class Item_944(Item):
    non_spare = NonSpare_1757

class NonSpare_1807(NonSpare):
    name = "TRA"
    title = "Turn Radius Availability"
    rule = RuleVariation_891

class Item_977(Item):
    non_spare = NonSpare_1807

class NonSpare_1785(NonSpare):
    name = "TOA"
    title = "TOV Available"
    rule = RuleVariation_925

class Item_961(Item):
    non_spare = NonSpare_1785

class Variation_1182(Group):
    bit_size = 120
    items_list = [Item_932, Item_585, Item_942, Item_60, Item_442, Item_468, Item_679, Item_944, Item_977, Item_961, Item_965, Item_1014]
    items_dict = {"TCA": NonSpare_1744, "NC": NonSpare_1280, "TCPN": NonSpare_1755, "ALT": NonSpare_555, "LAT": NonSpare_1085, "LON": NonSpare_1113, "PT": NonSpare_1397, "TD": NonSpare_1757, "TRA": NonSpare_1807, "TOA": NonSpare_1785, "TOV": NonSpare_1795, "TTR": NonSpare_1852}

class Variation_1360(Repetitive):
    rep_bytes = 1
    variation = Variation_1182

class RuleVariation_1290(RuleVariationContextFree):
    variation = Variation_1360

class NonSpare_1773(NonSpare):
    name = "TID"
    title = "Trajectory Intent Data"
    rule = RuleVariation_1290

class Content_305(ContentTable):
    values = {0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground"}

class RuleContent_305(RuleContentContextFree):
    variation = Content_305

class Variation_658(Element):
    bit_offset8 = 3
    bit_size = 3
    rule = RuleContent_305

class RuleVariation_648(RuleVariationContextFree):
    variation = Variation_658

class NonSpare_1683(NonSpare):
    name = "STAT"
    title = "Flight Status"
    rule = RuleVariation_648

class Item_891(Item):
    non_spare = NonSpare_1683

class NonSpare_624(NonSpare):
    name = "B1B"
    title = "BDS BDS 1,0 Bits 37/40"
    rule = RuleVariation_741

class Item_107(Item):
    non_spare = NonSpare_624

class Variation_1039(Group):
    bit_size = 16
    items_list = [Item_182, Item_891, Item_27, Item_881, Item_77, Item_54, Item_104, Item_107]
    items_dict = {"COM": NonSpare_735, "STAT": NonSpare_1683, "SSC": NonSpare_1664, "ARC": NonSpare_581, "AIC": NonSpare_549, "B1A": NonSpare_621, "B1B": NonSpare_624}

class RuleVariation_1000(RuleVariationContextFree):
    variation = Variation_1039

class NonSpare_737(NonSpare):
    name = "COM"
    title = "Communications/ACAS Capability and Flight Status"
    rule = RuleVariation_1000

class Content_512(ContentTable):
    values = {0: "Unknown", 1: "ACAS not operational", 2: "ACAS operational", 3: "Invalid"}

class RuleContent_512(RuleContentContextFree):
    variation = Content_512

class Variation_110(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_512

class RuleVariation_110(RuleVariationContextFree):
    variation = Variation_110

class NonSpare_518(NonSpare):
    name = "AC"
    title = "ACAS Status"
    rule = RuleVariation_110

class Item_38(Item):
    non_spare = NonSpare_518

class Content_526(ContentTable):
    values = {0: "Unknown", 1: "Multiple navigational aids not operating", 2: "Multiple navigational aids operating", 3: "Invalid"}

class RuleContent_526(RuleContentContextFree):
    variation = Content_526

class Variation_573(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_526

class RuleVariation_563(RuleVariationContextFree):
    variation = Variation_573

class NonSpare_1220(NonSpare):
    name = "MN"
    title = "Multiple Navigational Aids Status"
    rule = RuleVariation_563

class Item_531(Item):
    non_spare = NonSpare_1220

class Content_520(ContentTable):
    values = {0: "Unknown", 1: "Differential correction", 2: "No differential correction", 3: "Invalid"}

class RuleContent_520(RuleContentContextFree):
    variation = Content_520

class Variation_742(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_520

class RuleVariation_732(RuleVariationContextFree):
    variation = Variation_742

class NonSpare_821(NonSpare):
    name = "DC"
    title = "Differential Correction Status"
    rule = RuleVariation_732

class Item_243(Item):
    non_spare = NonSpare_821

class Content_501(ContentTable):
    values = {0: "Transponder ground bit not set or unknown", 1: "Transponder Ground Bit set"}

class RuleContent_501(RuleContentContextFree):
    variation = Content_501

class Variation_924(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_501

class RuleVariation_895(RuleVariationContextFree):
    variation = Variation_924

class NonSpare_963(NonSpare):
    name = "GBS"
    title = "Ground Bit Set"
    rule = RuleVariation_895

class Item_345(Item):
    non_spare = NonSpare_963

class Item_31(Spare):
    bit_offset8 = 7
    bit_size = 6

class Content_341(ContentTable):
    values = {0: "No emergency", 1: "General emergency", 2: "Lifeguard / medical", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference", 6: "Downed Aircraft", 7: "Unknown"}

class RuleContent_341(RuleContentContextFree):
    variation = Content_341

class Variation_860(Element):
    bit_offset8 = 5
    bit_size = 3
    rule = RuleContent_341

class RuleVariation_831(RuleVariationContextFree):
    variation = Variation_860

class NonSpare_1689(NonSpare):
    name = "STAT"
    title = "Flight Status"
    rule = RuleVariation_831

class Item_897(Item):
    non_spare = NonSpare_1689

class Variation_1011(Group):
    bit_size = 16
    items_list = [Item_38, Item_531, Item_243, Item_345, Item_31, Item_897]
    items_dict = {"AC": NonSpare_518, "MN": NonSpare_1220, "DC": NonSpare_821, "GBS": NonSpare_963, "STAT": NonSpare_1689}

class RuleVariation_979(RuleVariationContextFree):
    variation = Variation_1011

class NonSpare_1560(NonSpare):
    name = "SAB"
    title = "Status Reported by ADS-B"
    rule = RuleVariation_979

class Content_773(ContentBds):
    bds_type = (BdsAt, 48)

class RuleContent_772(RuleContentContextFree):
    variation = Content_773

class Variation_387(Element):
    bit_offset8 = 0
    bit_size = 56
    rule = RuleContent_772

class RuleVariation_380(RuleVariationContextFree):
    variation = Variation_387

class NonSpare_525(NonSpare):
    name = "ACS"
    title = "ACAS Resolution Advisory Report"
    rule = RuleVariation_380

class NonSpare_648(NonSpare):
    name = "BVR"
    title = "Barometric Vertical Rate"
    rule = RuleVariation_278

class NonSpare_981(NonSpare):
    name = "GVR"
    title = "Geometric Vertical Rate"
    rule = RuleVariation_278

class NonSpare_1463(NonSpare):
    name = "RAN"
    title = "Roll Angle"
    rule = RuleVariation_254

class Content_637(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "°/s"

class RuleContent_637(RuleContentContextFree):
    variation = Content_637

class Variation_153(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_637

class RuleVariation_148(RuleVariationContextFree):
    variation = Variation_153

class NonSpare_1515(NonSpare):
    name = "ROT"
    title = "Rate of Turn in Two's Complement Form"
    rule = RuleVariation_148

class Item_785(Item):
    non_spare = NonSpare_1515

class Variation_1183(Group):
    bit_size = 16
    items_list = [Item_951, Item_15, Item_785, Item_29]
    items_dict = {"TI": NonSpare_1765, "ROT": NonSpare_1515}

class RuleVariation_1127(RuleVariationContextFree):
    variation = Variation_1183

class NonSpare_1732(NonSpare):
    name = "TAR"
    title = "Track Angle Rate"
    rule = RuleVariation_1127

class NonSpare_1728(NonSpare):
    name = "TAN"
    title = "Track Angle"
    rule = RuleVariation_329

class Content_653(ContentQuantity):
    signedness = Signed
    lsb = 6.103515625e-5
    unit = "NM/s"

class RuleContent_653(RuleContentContextFree):
    variation = Content_653

class Variation_279(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_653

class RuleVariation_273(RuleVariationContextFree):
    variation = Variation_279

class NonSpare_970(NonSpare):
    name = "GS"
    title = "Ground Speed"
    rule = RuleVariation_273

class NonSpare_1929(NonSpare):
    name = "VUN"
    title = "Velocity Uncertainty"
    rule = RuleVariation_154

class Content_387(ContentTable):
    values = {0: "Not valid Wind Speed", 1: "Valid Wind Speed"}

class RuleContent_387(RuleContentContextFree):
    variation = Content_387

class Variation_66(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_387

class RuleVariation_66(RuleVariationContextFree):
    variation = Variation_66

class NonSpare_1950(NonSpare):
    name = "WS"
    title = "Wind Speed Valid Flag"
    rule = RuleVariation_66

class Item_1101(Item):
    non_spare = NonSpare_1950

class Content_386(ContentTable):
    values = {0: "Not valid Wind Direction", 1: "Valid Wind Direction"}

class RuleContent_386(RuleContentContextFree):
    variation = Content_386

class Variation_436(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_386

class RuleVariation_426(RuleVariationContextFree):
    variation = Variation_436

class NonSpare_1943(NonSpare):
    name = "WD"
    title = "Wind Direction Valid Flag"
    rule = RuleVariation_426

class Item_1096(Item):
    non_spare = NonSpare_1943

class Content_384(ContentTable):
    values = {0: "Not valid Temperature", 1: "Valid Temperature"}

class RuleContent_384(RuleContentContextFree):
    variation = Content_384

class Variation_548(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_384

class RuleVariation_538(RuleVariationContextFree):
    variation = Variation_548

class NonSpare_1781(NonSpare):
    name = "TMP"
    title = "Temperature Valid Flag"
    rule = RuleVariation_538

class Item_958(Item):
    non_spare = NonSpare_1781

class Content_385(ContentTable):
    values = {0: "Not valid Turbulence", 1: "Valid Turbulence"}

class RuleContent_385(RuleContentContextFree):
    variation = Content_385

class Variation_634(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_385

class RuleVariation_624(RuleVariationContextFree):
    variation = Variation_634

class NonSpare_1812(NonSpare):
    name = "TRB"
    title = "Turbulence Valid Flag"
    rule = RuleVariation_624

class Item_981(Item):
    non_spare = NonSpare_1812

class NonSpare_1951(NonSpare):
    name = "WSD"
    title = "Wind Speed"
    rule = RuleVariation_287

class Item_1102(Item):
    non_spare = NonSpare_1951

class NonSpare_1944(NonSpare):
    name = "WDD"
    title = "Wind Direction"
    rule = RuleVariation_294

class Item_1097(Item):
    non_spare = NonSpare_1944

class NonSpare_1782(NonSpare):
    name = "TMPD"
    title = "Temperature in Degrees Celsius"
    rule = RuleVariation_266

class Item_959(Item):
    non_spare = NonSpare_1782

class NonSpare_1813(NonSpare):
    name = "TRBD"
    title = "Turbulence"
    rule = RuleVariation_190

class Item_982(Item):
    non_spare = NonSpare_1813

class Variation_1238(Group):
    bit_size = 64
    items_list = [Item_1101, Item_1096, Item_958, Item_981, Item_22, Item_1102, Item_1097, Item_959, Item_982]
    items_dict = {"WS": NonSpare_1950, "WD": NonSpare_1943, "TMP": NonSpare_1781, "TRB": NonSpare_1812, "WSD": NonSpare_1951, "WDD": NonSpare_1944, "TMPD": NonSpare_1782, "TRBD": NonSpare_1813}

class RuleVariation_1172(RuleVariationContextFree):
    variation = Variation_1238

class NonSpare_1195(NonSpare):
    name = "MET"
    title = "Meteorological Data"
    rule = RuleVariation_1172

class Content_556(ContentTable):
    values = {1: "Light aircraft =< 7000 kg", 2: "Reserved", 3: "7000 kg < medium aircraft < 136000 kg", 4: "Reserved", 5: "136000 kg <= heavy aircraft", 6: "Highly manoeuvrable (5g acceleration capability) and high speed (>400 knots cruise)", 7: "Reserved", 8: "Reserved", 9: "Reserved", 10: "Rotocraft", 11: "Glider / sailplane", 12: "Lighter-than-air", 13: "Unmanned aerial vehicle", 14: "Space / transatmospheric vehicle", 15: "Ultralight / handglider / paraglider", 16: "Parachutist / skydiver", 17: "Reserved", 18: "Reserved", 19: "Reserved", 20: "Surface emergency vehicle", 21: "Surface service vehicle", 22: "Fixed ground or tethered obstruction", 23: "Reserved", 24: "Reserved"}

class RuleContent_556(RuleContentContextFree):
    variation = Content_556

class Variation_177(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_556

class RuleVariation_171(RuleVariationContextFree):
    variation = Variation_177

class NonSpare_873(NonSpare):
    name = "EMC"
    title = "Emitter Category"
    rule = RuleVariation_171

class NonSpare_1086(NonSpare):
    name = "LAT"
    title = "Latitude in WGS.84 in Two's Complement Form"
    rule = RuleVariation_346

class Item_443(Item):
    non_spare = NonSpare_1086

class NonSpare_1114(NonSpare):
    name = "LON"
    title = "Longitude in WGS.84 in Two's Complement Form"
    rule = RuleVariation_344

class Item_469(Item):
    non_spare = NonSpare_1114

class Variation_1099(Group):
    bit_size = 48
    items_list = [Item_443, Item_469]
    items_dict = {"LAT": NonSpare_1086, "LON": NonSpare_1114}

class RuleVariation_1058(RuleVariationContextFree):
    variation = Variation_1099

class NonSpare_1367(NonSpare):
    name = "POS"
    title = "Position"
    rule = RuleVariation_1058

class NonSpare_953(NonSpare):
    name = "GAL"
    title = "Geometric Altitude"
    rule = RuleVariation_277

class NonSpare_1399(NonSpare):
    name = "PUN"
    title = "Position Uncertainty"
    rule = RuleVariation_741

class Item_680(Item):
    non_spare = NonSpare_1399

class Variation_996(Group):
    bit_size = 8
    items_list = [Item_3, Item_680]
    items_dict = {"PUN": NonSpare_1399}

class RuleVariation_965(RuleVariationContextFree):
    variation = Variation_996

class NonSpare_1400(NonSpare):
    name = "PUN"
    title = "Position Uncertainty"
    rule = RuleVariation_965

class NonSpare_1155(NonSpare):
    name = "MB"
    title = "MODE S MB DATA"
    rule = RuleVariation_1262

class Content_688(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "kt"

class RuleContent_688(RuleContentContextFree):
    variation = Content_688

class Variation_294(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_688

class RuleVariation_288(RuleVariationContextFree):
    variation = Variation_294

class NonSpare_1030(NonSpare):
    name = "IAR"
    title = "Indicated Airspeed"
    rule = RuleVariation_288

class Content_720(ContentQuantity):
    signedness = Unsigned
    lsb = 8.0e-3
    unit = "Mach"

class RuleContent_720(RuleContentContextFree):
    variation = Content_720

class Variation_310(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_720

class RuleVariation_304(RuleVariationContextFree):
    variation = Variation_310

class NonSpare_1145(NonSpare):
    name = "MAC"
    title = "Mach Number"
    rule = RuleVariation_304

class Content_711(ContentQuantity):
    signedness = Unsigned
    lsb = 0.1
    unit = "mb"

class RuleContent_711(RuleContentContextFree):
    variation = Content_711

class Variation_784(Element):
    bit_offset8 = 4
    bit_size = 12
    rule = RuleContent_711

class RuleVariation_755(RuleVariationContextFree):
    variation = Variation_784

class NonSpare_642(NonSpare):
    name = "BPS"
    title = ""
    rule = RuleVariation_755

class Item_124(Item):
    non_spare = NonSpare_642

class Variation_984(Group):
    bit_size = 16
    items_list = [Item_3, Item_124]
    items_dict = {"BPS": NonSpare_642}

class RuleVariation_955(RuleVariationContextFree):
    variation = Variation_984

class NonSpare_646(NonSpare):
    name = "BPS"
    title = "Barometric Pressure Setting (derived from Mode S BDS 4,0)"
    rule = RuleVariation_955

class Variation_1384(Compound):
    items_list = [NonSpare_533, NonSpare_1041, NonSpare_1203, NonSpare_1034, NonSpare_1737, NonSpare_1566, NonSpare_936, NonSpare_1778, NonSpare_1773, NonSpare_737, NonSpare_1560, NonSpare_525, NonSpare_648, NonSpare_981, NonSpare_1463, NonSpare_1732, NonSpare_1728, NonSpare_970, NonSpare_1929, NonSpare_1195, NonSpare_873, NonSpare_1367, NonSpare_953, NonSpare_1400, NonSpare_1155, NonSpare_1030, NonSpare_1145, NonSpare_646]
    items_dict = {"ADR": NonSpare_533, "ID": NonSpare_1041, "MHG": NonSpare_1203, "IAS": NonSpare_1034, "TAS": NonSpare_1737, "SAL": NonSpare_1566, "FSS": NonSpare_936, "TIS": NonSpare_1778, "TID": NonSpare_1773, "COM": NonSpare_737, "SAB": NonSpare_1560, "ACS": NonSpare_525, "BVR": NonSpare_648, "GVR": NonSpare_981, "RAN": NonSpare_1463, "TAR": NonSpare_1732, "TAN": NonSpare_1728, "GS": NonSpare_970, "VUN": NonSpare_1929, "MET": NonSpare_1195, "EMC": NonSpare_873, "POS": NonSpare_1367, "GAL": NonSpare_953, "PUN": NonSpare_1400, "MB": NonSpare_1155, "IAR": NonSpare_1030, "MAC": NonSpare_1145, "BPS": NonSpare_646}

class RuleVariation_1314(RuleVariationContextFree):
    variation = Variation_1384

class NonSpare_447(NonSpare):
    name = "380"
    title = "Aircraft Derived Data"
    rule = RuleVariation_1314

class NonSpare_141(NonSpare):
    name = "040"
    title = "Track Number"
    rule = RuleVariation_240

class Content_285(ContentTable):
    values = {0: "Multisensor track", 1: "Monosensor track"}

class RuleContent_285(RuleContentContextFree):
    variation = Content_285

class Variation_52(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_285

class RuleVariation_52(RuleVariationContextFree):
    variation = Variation_52

class NonSpare_1236(NonSpare):
    name = "MON"
    title = ""
    rule = RuleVariation_52

class Item_547(Item):
    non_spare = NonSpare_1236

class NonSpare_1239(NonSpare):
    name = "MRH"
    title = "Most Reliable Height"
    rule = RuleVariation_483

class Item_550(Item):
    non_spare = NonSpare_1239

class Content_356(ContentTable):
    values = {0: "No source", 1: "GNSS", 2: "3D radar", 3: "Triangulation", 4: "Height from coverage", 5: "Speed look-up table", 6: "Default height", 7: "Multilateration"}

class RuleContent_356(RuleContentContextFree):
    variation = Content_356

class Variation_663(Element):
    bit_offset8 = 3
    bit_size = 3
    rule = RuleContent_356

class RuleVariation_653(RuleVariationContextFree):
    variation = Variation_663

class NonSpare_1659(NonSpare):
    name = "SRC"
    title = "Source of Calculated Track Altitude for I062/130"
    rule = RuleVariation_653

class Item_878(Item):
    non_spare = NonSpare_1659

class Content_19(ContentTable):
    values = {0: "Actual track", 1: "Simulated track"}

class RuleContent_19(RuleContentContextFree):
    variation = Content_19

class Variation_5(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_19

class RuleVariation_5(RuleVariationContextFree):
    variation = Variation_5

class NonSpare_1633(NonSpare):
    name = "SIM"
    title = ""
    rule = RuleVariation_5

class Item_855(Item):
    non_spare = NonSpare_1633

class Content_181(ContentTable):
    values = {0: "Default value", 1: "Last message transmitted to the user for the track"}

class RuleContent_181(RuleContentContextFree):
    variation = Content_181

class Variation_412(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_181

class RuleVariation_402(RuleVariationContextFree):
    variation = Variation_412

class NonSpare_1830(NonSpare):
    name = "TSE"
    title = ""
    rule = RuleVariation_402

class Item_994(Item):
    non_spare = NonSpare_1830

class Content_179(ContentTable):
    values = {0: "Default value", 1: "First message transmitted to the user for the track"}

class RuleContent_179(RuleContentContextFree):
    variation = Content_179

class Variation_512(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_179

class RuleVariation_502(RuleVariationContextFree):
    variation = Variation_512

class NonSpare_1828(NonSpare):
    name = "TSB"
    title = ""
    rule = RuleVariation_502

class Item_992(Item):
    non_spare = NonSpare_1828

class Content_185(ContentTable):
    values = {0: "Default value", 1: "Slave Track Promotion"}

class RuleContent_185(RuleContentContextFree):
    variation = Content_185

class Variation_816(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_185

class RuleVariation_787(RuleVariationContextFree):
    variation = Variation_816

class NonSpare_1704(NonSpare):
    name = "STP"
    title = ""
    rule = RuleVariation_787

class Item_911(Item):
    non_spare = NonSpare_1704

class Content_56(ContentTable):
    values = {0: "Complementary service used", 1: "Background service used"}

class RuleContent_56(RuleContentContextFree):
    variation = Content_56

class Variation_873(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_56

class RuleVariation_844(RuleVariationContextFree):
    variation = Variation_873

class NonSpare_1057(NonSpare):
    name = "KOS"
    title = ""
    rule = RuleVariation_844

class Item_414(Item):
    non_spare = NonSpare_1057

class Variation_456(Element):
    bit_offset8 = 1
    bit_size = 2
    rule = RuleContent_295

class RuleVariation_446(RuleVariationContextFree):
    variation = Variation_456

class NonSpare_1169(NonSpare):
    name = "MD4"
    title = ""
    rule = RuleVariation_446

class Item_505(Item):
    non_spare = NonSpare_1169

class Variation_611(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_182

class RuleVariation_601(RuleVariationContextFree):
    variation = Variation_611

class NonSpare_1189(NonSpare):
    name = "ME"
    title = ""
    rule = RuleVariation_601

class Item_512(Item):
    non_spare = NonSpare_1189

class Content_183(ContentTable):
    values = {0: "Default value", 1: "Military Identification present in the last report received from a sensor capable of decoding this data"}

class RuleContent_183(RuleContentContextFree):
    variation = Content_183

class Variation_692(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_183

class RuleVariation_682(RuleVariationContextFree):
    variation = Variation_692

class NonSpare_1207(NonSpare):
    name = "MI"
    title = ""
    rule = RuleVariation_682

class Item_519(Item):
    non_spare = NonSpare_1207

class Content_298(ContentTable):
    values = {0: "No Mode 5 interrogation", 1: "Friendly target", 2: "Unknown target", 3: "No reply"}

class RuleContent_298(RuleContentContextFree):
    variation = Content_298

class Variation_851(Element):
    bit_offset8 = 5
    bit_size = 2
    rule = RuleContent_298

class RuleVariation_822(RuleVariationContextFree):
    variation = Variation_851

class NonSpare_1171(NonSpare):
    name = "MD5"
    title = ""
    rule = RuleVariation_822

class Item_506(Item):
    non_spare = NonSpare_1171

class Variation_36(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_173

class RuleVariation_36(RuleVariationContextFree):
    variation = Variation_36

class NonSpare_786(NonSpare):
    name = "CST"
    title = ""
    rule = RuleVariation_36

class Item_213(Item):
    non_spare = NonSpare_786

class Content_168(ContentTable):
    values = {0: "Default value", 1: "Age of the last received ADS-B track update is higher than system dependent threshold"}

class RuleContent_168(RuleContentContextFree):
    variation = Content_168

class Variation_691(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_168

class RuleVariation_681(RuleVariationContextFree):
    variation = Variation_691

class NonSpare_535(NonSpare):
    name = "ADS"
    title = ""
    rule = RuleVariation_681

class Item_45(Item):
    non_spare = NonSpare_535

class Content_174(ContentTable):
    values = {0: "Default value", 1: "Assigned Mode A Code Conflict (same discrete Mode A Code assigned to another track)"}

class RuleContent_174(RuleContentContextFree):
    variation = Content_174

class Variation_891(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_174

class RuleVariation_862(RuleVariationContextFree):
    variation = Variation_891

class NonSpare_512(NonSpare):
    name = "AAC"
    title = ""
    rule = RuleVariation_862

class Item_34(Item):
    non_spare = NonSpare_512

class Content_54(ContentTable):
    values = {0: "Combined", 1: "Co-operative only", 2: "Non-Cooperative only", 3: "Not defined"}

class RuleContent_54(RuleContentContextFree):
    variation = Content_54

class Variation_95(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_54

class RuleVariation_95(RuleVariationContextFree):
    variation = Variation_95

class NonSpare_1603(NonSpare):
    name = "SDS"
    title = ""
    rule = RuleVariation_95

class Item_830(Item):
    non_spare = NonSpare_1603

class Content_340(ContentTable):
    values = {0: "No emergency", 1: "General emergency", 2: "Lifeguard / medical", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference", 6: "Downed Aircraft", 7: "Undefined"}

class RuleContent_340(RuleContentContextFree):
    variation = Content_340

class Variation_578(Element):
    bit_offset8 = 2
    bit_size = 3
    rule = RuleContent_340

class RuleVariation_568(RuleVariationContextFree):
    variation = Variation_578

class NonSpare_877(NonSpare):
    name = "EMS"
    title = ""
    rule = RuleVariation_568

class Item_282(Item):
    non_spare = NonSpare_877

class Content_349(ContentTable):
    values = {0: "No indication", 1: "Potential False Track Indication"}

class RuleContent_349(RuleContentContextFree):
    variation = Content_349

class Variation_833(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_349

class RuleVariation_804(RuleVariationContextFree):
    variation = Variation_833

class NonSpare_1347(NonSpare):
    name = "PFT"
    title = ""
    rule = RuleVariation_804

class Item_646(Item):
    non_spare = NonSpare_1347

class Content_188(ContentTable):
    values = {0: "Default value", 1: "Track created / updated with FPL data"}

class RuleContent_188(RuleContentContextFree):
    variation = Content_188

class Variation_893(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_188

class RuleVariation_864(RuleVariationContextFree):
    variation = Variation_893

class NonSpare_923(NonSpare):
    name = "FPLT"
    title = ""
    rule = RuleVariation_864

class Item_317(Item):
    non_spare = NonSpare_923

class Content_178(ContentTable):
    values = {0: "Default value", 1: "Duplicate Mode 3/A Code"}

class RuleContent_178(RuleContentContextFree):
    variation = Content_178

class Variation_37(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_178

class RuleVariation_37(RuleVariationContextFree):
    variation = Variation_37

class NonSpare_858(NonSpare):
    name = "DUPT"
    title = ""
    rule = RuleVariation_37

class Item_272(Item):
    non_spare = NonSpare_858

class Content_176(ContentTable):
    values = {0: "Default value", 1: "Duplicate Flight Plan"}

class RuleContent_176(RuleContentContextFree):
    variation = Content_176

class Variation_411(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_176

class RuleVariation_401(RuleVariationContextFree):
    variation = Variation_411

class NonSpare_856(NonSpare):
    name = "DUPF"
    title = ""
    rule = RuleVariation_401

class Item_270(Item):
    non_spare = NonSpare_856

class Content_177(ContentTable):
    values = {0: "Default value", 1: "Duplicate Flight Plan due to manual correlation"}

class RuleContent_177(RuleContentContextFree):
    variation = Content_177

class Variation_511(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_177

class RuleVariation_501(RuleVariationContextFree):
    variation = Variation_511

class NonSpare_857(NonSpare):
    name = "DUPM"
    title = ""
    rule = RuleVariation_501

class Item_271(Item):
    non_spare = NonSpare_857

class Variation_1300(Extended):
    items = [Item_547, Item_867, Item_550, Item_878, Item_161, None, Item_855, Item_994, Item_992, Item_316, Item_49, Item_911, Item_414, None, Item_67, Item_505, Item_512, Item_519, Item_506, None, Item_213, Item_674, Item_883, Item_507, Item_45, Item_918, Item_34, None, Item_830, Item_282, Item_646, Item_317, None, Item_272, Item_270, Item_271, Item_18, None]

class RuleVariation_1230(RuleVariationContextFree):
    variation = Variation_1300

class NonSpare_223(NonSpare):
    name = "080"
    title = "Track Status"
    rule = RuleVariation_1230

class Content_731(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "s"

class RuleContent_730(RuleContentContextFree):
    variation = Content_731

class Variation_227(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_730

class RuleVariation_221(RuleVariationContextFree):
    variation = Variation_227

class NonSpare_1819(NonSpare):
    name = "TRK"
    title = "Track Age"
    rule = RuleVariation_221

class NonSpare_1391(NonSpare):
    name = "PSR"
    title = "PSR Age"
    rule = RuleVariation_221

class NonSpare_1670(NonSpare):
    name = "SSR"
    title = "SSR Age"
    rule = RuleVariation_221

class NonSpare_1184(NonSpare):
    name = "MDS"
    title = "Mode S Age"
    rule = RuleVariation_221

class Content_732(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "s"

class RuleContent_731(RuleContentContextFree):
    variation = Content_732

class Variation_318(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_731

class RuleVariation_312(RuleVariationContextFree):
    variation = Variation_318

class NonSpare_537(NonSpare):
    name = "ADS"
    title = "ADS-C Age"
    rule = RuleVariation_312

class NonSpare_897(NonSpare):
    name = "ES"
    title = "ADS-B Extended Squitter Age"
    rule = RuleVariation_221

class NonSpare_1913(NonSpare):
    name = "VDL"
    title = "ADS-B VDL Mode 4 Age"
    rule = RuleVariation_221

class NonSpare_1878(NonSpare):
    name = "UAT"
    title = "ADS-B UAT Age"
    rule = RuleVariation_221

class NonSpare_1119(NonSpare):
    name = "LOP"
    title = "Loop Age"
    rule = RuleVariation_221

class NonSpare_1217(NonSpare):
    name = "MLT"
    title = "Multilateration Age"
    rule = RuleVariation_221

class Variation_1432(Compound):
    items_list = [NonSpare_1819, NonSpare_1391, NonSpare_1670, NonSpare_1184, NonSpare_537, NonSpare_897, NonSpare_1913, NonSpare_1878, NonSpare_1119, NonSpare_1217]
    items_dict = {"TRK": NonSpare_1819, "PSR": NonSpare_1391, "SSR": NonSpare_1670, "MDS": NonSpare_1184, "ADS": NonSpare_537, "ES": NonSpare_897, "VDL": NonSpare_1913, "UAT": NonSpare_1878, "LOP": NonSpare_1119, "MLT": NonSpare_1217}

class RuleVariation_1362(RuleVariationContextFree):
    variation = Variation_1432

class NonSpare_432(NonSpare):
    name = "290"
    title = "System Track Update Ages"
    rule = RuleVariation_1362

class Content_64(ContentTable):
    values = {0: "Constant course", 1: "Right turn", 2: "Left turn", 3: "Undetermined"}

class RuleContent_64(RuleContentContextFree):
    variation = Content_64

class Variation_96(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_64

class RuleVariation_96(RuleVariationContextFree):
    variation = Variation_96

class NonSpare_1808(NonSpare):
    name = "TRANS"
    title = "Transversal Acceleration"
    rule = RuleVariation_96

class Item_978(Item):
    non_spare = NonSpare_1808

class Content_65(ContentTable):
    values = {0: "Constant groundspeed", 1: "Increasing groundspeed", 2: "Decreasing groundspeed", 3: "Undetermined"}

class RuleContent_65(RuleContentContextFree):
    variation = Content_65

class Variation_567(Element):
    bit_offset8 = 2
    bit_size = 2
    rule = RuleContent_65

class RuleVariation_557(RuleVariationContextFree):
    variation = Variation_567

class NonSpare_1115(NonSpare):
    name = "LONG"
    title = "Longitudinal Acceleration"
    rule = RuleVariation_557

class Item_470(Item):
    non_spare = NonSpare_1115

class Content_244(ContentTable):
    values = {0: "Level", 1: "Climb", 2: "Descent", 3: "Undetermined"}

class RuleContent_244(RuleContentContextFree):
    variation = Content_244

class Variation_736(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_244

class RuleVariation_726(RuleVariationContextFree):
    variation = Variation_736

class NonSpare_1917(NonSpare):
    name = "VERT"
    title = "Transversal Acceleration"
    rule = RuleVariation_726

class Item_1074(Item):
    non_spare = NonSpare_1917

class Content_311(ContentTable):
    values = {0: "No altitude discrepancy", 1: "Altitude discrepancy"}

class RuleContent_311(RuleContentContextFree):
    variation = Content_311

class Variation_909(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_311

class RuleVariation_880(RuleVariationContextFree):
    variation = Variation_909

class NonSpare_531(NonSpare):
    name = "ADF"
    title = "Altitude Discrepancy Flag"
    rule = RuleVariation_880

class Item_43(Item):
    non_spare = NonSpare_531

class Variation_1187(Group):
    bit_size = 8
    items_list = [Item_978, Item_470, Item_1074, Item_43, Item_29]
    items_dict = {"TRANS": NonSpare_1808, "LONG": NonSpare_1115, "VERT": NonSpare_1917, "ADF": NonSpare_531}

class RuleVariation_1130(RuleVariationContextFree):
    variation = Variation_1187

class NonSpare_362(NonSpare):
    name = "200"
    title = "Mode of Movement"
    rule = RuleVariation_1130

class NonSpare_1199(NonSpare):
    name = "MFL"
    title = "Measured Flight Level Age"
    rule = RuleVariation_221

class NonSpare_1165(NonSpare):
    name = "MD1"
    title = "Mode 1 Age"
    rule = RuleVariation_221

class NonSpare_1168(NonSpare):
    name = "MD2"
    title = "Mode 2 Age"
    rule = RuleVariation_221

class NonSpare_1177(NonSpare):
    name = "MDA"
    title = "Mode 3/A Age"
    rule = RuleVariation_221

class NonSpare_1170(NonSpare):
    name = "MD4"
    title = "Mode 4 Age"
    rule = RuleVariation_221

class NonSpare_1172(NonSpare):
    name = "MD5"
    title = "Mode 5 Age"
    rule = RuleVariation_221

class NonSpare_1204(NonSpare):
    name = "MHG"
    title = "Magnetic Heading Age"
    rule = RuleVariation_221

class NonSpare_1033(NonSpare):
    name = "IAS"
    title = "Indicated Airspeed / Mach Nb Age"
    rule = RuleVariation_221

class NonSpare_1738(NonSpare):
    name = "TAS"
    title = "True Airspeed Age"
    rule = RuleVariation_221

class NonSpare_1568(NonSpare):
    name = "SAL"
    title = "Selected Altitude Age"
    rule = RuleVariation_221

class NonSpare_937(NonSpare):
    name = "FSS"
    title = "Final State Selected Altitude Age"
    rule = RuleVariation_221

class NonSpare_1771(NonSpare):
    name = "TID"
    title = "Trajectory Intent Age"
    rule = RuleVariation_221

class NonSpare_733(NonSpare):
    name = "COM"
    title = "Communication/ACAS Capability and Flight Status Age"
    rule = RuleVariation_221

class NonSpare_1561(NonSpare):
    name = "SAB"
    title = "Status Reported by ADS-B Age"
    rule = RuleVariation_221

class NonSpare_526(NonSpare):
    name = "ACS"
    title = "ACAS Resolution Advisory Report Age"
    rule = RuleVariation_221

class NonSpare_651(NonSpare):
    name = "BVR"
    title = "Barometric Vertical Rate Age"
    rule = RuleVariation_221

class NonSpare_984(NonSpare):
    name = "GVR"
    title = "Geometrical Vertical Rate Age"
    rule = RuleVariation_221

class NonSpare_1464(NonSpare):
    name = "RAN"
    title = "Roll Angle Age"
    rule = RuleVariation_221

class NonSpare_1734(NonSpare):
    name = "TAR"
    title = "Track Angle Rate Age"
    rule = RuleVariation_221

class NonSpare_1729(NonSpare):
    name = "TAN"
    title = "Track Angle Age"
    rule = RuleVariation_221

class NonSpare_975(NonSpare):
    name = "GSP"
    title = "Ground Speed Age"
    rule = RuleVariation_221

class NonSpare_1930(NonSpare):
    name = "VUN"
    title = "Velocity Uncertainty Age"
    rule = RuleVariation_221

class NonSpare_1196(NonSpare):
    name = "MET"
    title = "Meteorological Data Age"
    rule = RuleVariation_221

class NonSpare_874(NonSpare):
    name = "EMC"
    title = "Emitter Category Age"
    rule = RuleVariation_221

class NonSpare_1368(NonSpare):
    name = "POS"
    title = "Position Age"
    rule = RuleVariation_221

class NonSpare_954(NonSpare):
    name = "GAL"
    title = "Geometric Altitude Age"
    rule = RuleVariation_221

class NonSpare_1401(NonSpare):
    name = "PUN"
    title = "Position Uncertainty Age"
    rule = RuleVariation_221

class NonSpare_1156(NonSpare):
    name = "MB"
    title = "Mode S MB Data Age"
    rule = RuleVariation_221

class NonSpare_1031(NonSpare):
    name = "IAR"
    title = "Indicated Airspeed Data Age"
    rule = RuleVariation_221

class NonSpare_1146(NonSpare):
    name = "MAC"
    title = "Mach Number Data Age"
    rule = RuleVariation_221

class NonSpare_647(NonSpare):
    name = "BPS"
    title = "Barometric Pressure Setting Data Age"
    rule = RuleVariation_221

class Variation_1413(Compound):
    items_list = [NonSpare_1199, NonSpare_1165, NonSpare_1168, NonSpare_1177, NonSpare_1170, NonSpare_1172, NonSpare_1204, NonSpare_1033, NonSpare_1738, NonSpare_1568, NonSpare_937, NonSpare_1771, NonSpare_733, NonSpare_1561, NonSpare_526, NonSpare_651, NonSpare_984, NonSpare_1464, NonSpare_1734, NonSpare_1729, NonSpare_975, NonSpare_1930, NonSpare_1196, NonSpare_874, NonSpare_1368, NonSpare_954, NonSpare_1401, NonSpare_1156, NonSpare_1031, NonSpare_1146, NonSpare_647]
    items_dict = {"MFL": NonSpare_1199, "MD1": NonSpare_1165, "MD2": NonSpare_1168, "MDA": NonSpare_1177, "MD4": NonSpare_1170, "MD5": NonSpare_1172, "MHG": NonSpare_1204, "IAS": NonSpare_1033, "TAS": NonSpare_1738, "SAL": NonSpare_1568, "FSS": NonSpare_937, "TID": NonSpare_1771, "COM": NonSpare_733, "SAB": NonSpare_1561, "ACS": NonSpare_526, "BVR": NonSpare_651, "GVR": NonSpare_984, "RAN": NonSpare_1464, "TAR": NonSpare_1734, "TAN": NonSpare_1729, "GSP": NonSpare_975, "VUN": NonSpare_1930, "MET": NonSpare_1196, "EMC": NonSpare_874, "POS": NonSpare_1368, "GAL": NonSpare_954, "PUN": NonSpare_1401, "MB": NonSpare_1156, "IAR": NonSpare_1031, "MAC": NonSpare_1146, "BPS": NonSpare_647}

class RuleVariation_1343(RuleVariationContextFree):
    variation = Variation_1413

class NonSpare_435(NonSpare):
    name = "295"
    title = "Track Data Ages"
    rule = RuleVariation_1343

class Variation_264(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_626

class RuleVariation_258(RuleVariationContextFree):
    variation = Variation_264

class NonSpare_297(NonSpare):
    name = "136"
    title = "Measured Flight Level"
    rule = RuleVariation_258

class NonSpare_284(NonSpare):
    name = "130"
    title = "Calculated Track Geometric Altitude"
    rule = RuleVariation_277

class NonSpare_1443(NonSpare):
    name = "QNH"
    title = ""
    rule = RuleVariation_56

class Item_721(Item):
    non_spare = NonSpare_1443

class NonSpare_795(NonSpare):
    name = "CTB"
    title = "Calculated Track Barometric Altitude"
    rule = RuleVariation_472

class Item_221(Item):
    non_spare = NonSpare_795

class Variation_1128(Group):
    bit_size = 16
    items_list = [Item_721, Item_221]
    items_dict = {"QNH": NonSpare_1443, "CTB": NonSpare_795}

class RuleVariation_1082(RuleVariationContextFree):
    variation = Variation_1128

class NonSpare_296(NonSpare):
    name = "135"
    title = "Calculated Track Barometric Altitude"
    rule = RuleVariation_1082

class NonSpare_389(NonSpare):
    name = "220"
    title = "Calculated Rate of Climb/Descent"
    rule = RuleVariation_278

class NonSpare_1727(NonSpare):
    name = "TAG"
    title = "FPPS Identification Tag"
    rule = RuleVariation_1104

class NonSpare_780(NonSpare):
    name = "CS"
    title = "Callsign"
    rule = RuleVariation_378

class NonSpare_1277(NonSpare):
    name = "NBR"
    title = "Number from 0 to 99 999 999"
    rule = RuleVariation_836

class Item_582(Item):
    non_spare = NonSpare_1277

class Variation_1192(Group):
    bit_size = 32
    items_list = [Item_1019, Item_13, Item_582]
    items_dict = {"TYP": NonSpare_1858, "NBR": NonSpare_1277}

class RuleVariation_1135(RuleVariationContextFree):
    variation = Variation_1192

class NonSpare_1047(NonSpare):
    name = "IFI"
    title = "IFPS_FLIGHT_ID"
    rule = RuleVariation_1135

class NonSpare_926(NonSpare):
    name = "FR1FR2"
    title = ""
    rule = RuleVariation_558

class Item_319(Item):
    non_spare = NonSpare_926

class Variation_1074(Group):
    bit_size = 8
    items_list = [Item_339, Item_319, Item_799, Item_376, Item_29]
    items_dict = {"GATOAT": NonSpare_957, "FR1FR2": NonSpare_926, "RVSM": NonSpare_1547, "HPR": NonSpare_1007}

class RuleVariation_1033(RuleVariationContextFree):
    variation = Variation_1074

class NonSpare_905(NonSpare):
    name = "FCT"
    title = "Flight Category"
    rule = RuleVariation_1033

class NonSpare_1726(NonSpare):
    name = "TAC"
    title = "Type of Aircraft"
    rule = RuleVariation_360

class NonSpare_1953(NonSpare):
    name = "WTC"
    title = "Wake Turbulence Category"
    rule = RuleVariation_188

class NonSpare_827(NonSpare):
    name = "DEP"
    title = "Departure Airport"
    rule = RuleVariation_360

class NonSpare_851(NonSpare):
    name = "DST"
    title = "Destination Airport"
    rule = RuleVariation_360

class NonSpare_797(NonSpare):
    name = "CTL"
    title = "Current Control Position"
    rule = RuleVariation_995

class NonSpare_1862(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_140

class Item_1023(Item):
    non_spare = NonSpare_1862

class NonSpare_1004(NonSpare):
    name = "HOR"
    title = "Hours"
    rule = RuleVariation_660

class Item_374(Item):
    non_spare = NonSpare_1004

class NonSpare_1212(NonSpare):
    name = "MIN"
    title = "Minutes"
    rule = RuleVariation_570

class Item_524(Item):
    non_spare = NonSpare_1212

class NonSpare_605(NonSpare):
    name = "AVS"
    title = "Seconds Available Flag"
    rule = RuleVariation_70

class Item_92(Item):
    non_spare = NonSpare_605

class NonSpare_1608(NonSpare):
    name = "SEC"
    title = "Seconds"
    rule = RuleVariation_570

class Item_834(Item):
    non_spare = NonSpare_1608

class Variation_1194(Group):
    bit_size = 32
    items_list = [Item_1023, Item_233, Item_30, Item_374, Item_1, Item_524, Item_92, Item_7, Item_834]
    items_dict = {"TYP": NonSpare_1862, "DAY": NonSpare_811, "HOR": NonSpare_1004, "MIN": NonSpare_1212, "AVS": NonSpare_605, "SEC": NonSpare_1608}

class Variation_1363(Repetitive):
    rep_bytes = 1
    variation = Variation_1194

class RuleVariation_1293(RuleVariationContextFree):
    variation = Variation_1363

class NonSpare_1789(NonSpare):
    name = "TOD"
    title = "Time of Departure / Arrival"
    rule = RuleVariation_1293

class NonSpare_1696(NonSpare):
    name = "STD"
    title = "Standard Instrument Departure"
    rule = RuleVariation_378

class NonSpare_1679(NonSpare):
    name = "STA"
    title = "Standard Instrument Arrival"
    rule = RuleVariation_378

class Content_361(ContentTable):
    values = {0: "No valid Mode 3/A available", 1: "Valid Mode 3/A available"}

class RuleContent_361(RuleContentContextFree):
    variation = Content_361

class Variation_629(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_361

class RuleVariation_619(RuleVariationContextFree):
    variation = Variation_629

class NonSpare_1892(NonSpare):
    name = "VA"
    title = ""
    rule = RuleVariation_619

class Item_1051(Item):
    non_spare = NonSpare_1892

class Variation_982(Group):
    bit_size = 16
    items_list = [Item_2, Item_1051, Item_543]
    items_dict = {"VA": NonSpare_1892, "MODE3A": NonSpare_1232}

class RuleVariation_953(RuleVariationContextFree):
    variation = Variation_982

class NonSpare_1346(NonSpare):
    name = "PEM"
    title = "Pre-Emergency Mode 3/A"
    rule = RuleVariation_953

class NonSpare_1345(NonSpare):
    name = "PEC"
    title = "Pre-Emergency Callsign"
    rule = RuleVariation_378

class Variation_1428(Compound):
    items_list = [NonSpare_1727, NonSpare_780, NonSpare_1047, NonSpare_905, NonSpare_1726, NonSpare_1953, NonSpare_827, NonSpare_851, NonSpare_1478, NonSpare_677, NonSpare_797, NonSpare_1789, NonSpare_591, NonSpare_1707, NonSpare_1696, NonSpare_1679, NonSpare_1346, NonSpare_1345]
    items_dict = {"TAG": NonSpare_1727, "CS": NonSpare_780, "IFI": NonSpare_1047, "FCT": NonSpare_905, "TAC": NonSpare_1726, "WTC": NonSpare_1953, "DEP": NonSpare_827, "DST": NonSpare_851, "RDS": NonSpare_1478, "CFL": NonSpare_677, "CTL": NonSpare_797, "TOD": NonSpare_1789, "AST": NonSpare_591, "STS": NonSpare_1707, "STD": NonSpare_1696, "STA": NonSpare_1679, "PEM": NonSpare_1346, "PEC": NonSpare_1345}

class RuleVariation_1358(RuleVariationContextFree):
    variation = Variation_1428

class NonSpare_455(NonSpare):
    name = "390"
    title = "Flight Plan Related Data"
    rule = RuleVariation_1358

class NonSpare_424(NonSpare):
    name = "270"
    title = "Target Size and Orientation"
    rule = RuleVariation_1224

class Content_314(ContentTable):
    values = {0: "No authenticated Mode 5 ID reply", 1: "Authenticated Mode 5 ID reply"}

class RuleContent_314(RuleContentContextFree):
    variation = Content_314

class Variation_430(Element):
    bit_offset8 = 1
    bit_size = 1
    rule = RuleContent_314

class RuleVariation_420(RuleVariationContextFree):
    variation = Variation_430

class NonSpare_1039(NonSpare):
    name = "ID"
    title = ""
    rule = RuleVariation_420

class Item_402(Item):
    non_spare = NonSpare_1039

class Content_261(ContentTable):
    values = {0: "Mode 1 code not present or not from Mode 5 reply", 1: "Mode 1 code from Mode 5 reply"}

class RuleContent_261(RuleContentContextFree):
    variation = Content_261

class Variation_620(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_261

class RuleVariation_610(RuleVariationContextFree):
    variation = Variation_620

class NonSpare_1129(NonSpare):
    name = "M1"
    title = ""
    rule = RuleVariation_610

class Item_482(Item):
    non_spare = NonSpare_1129

class Content_263(ContentTable):
    values = {0: "Mode 2 code not present or not from Mode 5 reply", 1: "Mode 2 code from Mode 5 reply"}

class RuleContent_263(RuleContentContextFree):
    variation = Content_263

class Variation_706(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_263

class RuleVariation_696(RuleVariationContextFree):
    variation = Variation_706

class NonSpare_1131(NonSpare):
    name = "M2"
    title = ""
    rule = RuleVariation_696

class Item_484(Item):
    non_spare = NonSpare_1131

class Content_265(ContentTable):
    values = {0: "Mode 3 code not present or not from Mode 5 reply", 1: "Mode 3 code from Mode 5 reply"}

class RuleContent_265(RuleContentContextFree):
    variation = Content_265

class Variation_830(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_265

class RuleVariation_801(RuleVariationContextFree):
    variation = Variation_830

class NonSpare_1135(NonSpare):
    name = "M3"
    title = ""
    rule = RuleVariation_801

class Item_487(Item):
    non_spare = NonSpare_1135

class Content_268(ContentTable):
    values = {0: "Mode C altitude code not present or not from Mode 5 reply", 1: "Mode C altitude from Mode 5 reply"}

class RuleContent_268(RuleContentContextFree):
    variation = Content_268

class Variation_906(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_268

class RuleVariation_877(RuleVariationContextFree):
    variation = Variation_906

class NonSpare_1162(NonSpare):
    name = "MC"
    title = ""
    rule = RuleVariation_877

class Item_503(Item):
    non_spare = NonSpare_1162

class Content_544(ContentTable):
    values = {0: "X-pulse set to zero or no authenticated Data reply or Report received", 1: "X-pulse set to one"}

class RuleContent_544(RuleContentContextFree):
    variation = Content_544

class Variation_957(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_544

class RuleVariation_928(RuleVariationContextFree):
    variation = Variation_957

class NonSpare_1985(NonSpare):
    name = "X"
    title = "X-pulse from Mode 5 Data Reply or Report"
    rule = RuleVariation_928

class Item_1134(Item):
    non_spare = NonSpare_1985

class Variation_1103(Group):
    bit_size = 8
    items_list = [Item_490, Item_402, Item_229, Item_482, Item_484, Item_487, Item_503, Item_1134]
    items_dict = {"M5": NonSpare_1142, "ID": NonSpare_1039, "DA": NonSpare_805, "M1": NonSpare_1129, "M2": NonSpare_1131, "M3": NonSpare_1135, "MC": NonSpare_1162, "X": NonSpare_1985}

class RuleVariation_1061(RuleVariationContextFree):
    variation = Variation_1103

class NonSpare_1716(NonSpare):
    name = "SUM"
    title = "Mode 5 Summary"
    rule = RuleVariation_1061

class Variation_971(Group):
    bit_size = 32
    items_list = [Item_1, Item_650, Item_2, Item_573, Item_1, Item_526]
    items_dict = {"PIN": NonSpare_1351, "NAT": NonSpare_1266, "MIS": NonSpare_1214}

class RuleVariation_942(RuleVariationContextFree):
    variation = Variation_971

class NonSpare_1353(NonSpare):
    name = "PMN"
    title = "Mode 5 PIN/ National Origin/Mission Code"
    rule = RuleVariation_942

class NonSpare_1363(NonSpare):
    name = "POS"
    title = "Mode 5 Reported Position"
    rule = RuleVariation_1049

class NonSpare_1498(NonSpare):
    name = "RES"
    title = "Resolution with which the GNSS-derived Altitude (GA) is Reported"
    rule = RuleVariation_407

class Item_769(Item):
    non_spare = NonSpare_1498

class NonSpare_949(NonSpare):
    name = "GA"
    title = "GNSS-derived Altitude of Target, Expressed as Height Above WGS 84 Ellipsoid"
    rule = RuleVariation_574

class Item_338(Item):
    non_spare = NonSpare_949

class Variation_966(Group):
    bit_size = 16
    items_list = [Item_0, Item_769, Item_338]
    items_dict = {"RES": NonSpare_1498, "GA": NonSpare_949}

class RuleVariation_937(RuleVariationContextFree):
    variation = Variation_966

class NonSpare_952(NonSpare):
    name = "GA"
    title = "Mode 5 GNSS-derived Altitude"
    rule = RuleVariation_937

class NonSpare_872(NonSpare):
    name = "EM1"
    title = "Extended Mode 1 Reply in Octal Representation"
    rule = RuleVariation_751

class Item_279(Item):
    non_spare = NonSpare_872

class Variation_986(Group):
    bit_size = 16
    items_list = [Item_3, Item_279]
    items_dict = {"EM1": NonSpare_872}

class RuleVariation_957(RuleVariationContextFree):
    variation = Variation_986

class NonSpare_869(NonSpare):
    name = "EM1"
    title = "Extended Mode 1 Code in Octal Representation"
    rule = RuleVariation_957

class Content_650(ContentQuantity):
    signedness = Signed
    lsb = 7.8125e-3
    unit = "s"

class RuleContent_650(RuleContentContextFree):
    variation = Content_650

class Variation_207(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_650

class RuleVariation_201(RuleVariationContextFree):
    variation = Variation_207

class NonSpare_1792(NonSpare):
    name = "TOS"
    title = "Time Offset for POS and GA"
    rule = RuleVariation_201

class Variation_983(Group):
    bit_size = 8
    items_list = [Item_2, Item_1143, Item_1146, Item_1142, Item_1140, Item_1136]
    items_dict = {"X5": NonSpare_1994, "XC": NonSpare_1997, "X3": NonSpare_1993, "X2": NonSpare_1991, "X1": NonSpare_1987}

class RuleVariation_954(RuleVariationContextFree):
    variation = Variation_983

class NonSpare_2000(NonSpare):
    name = "XP"
    title = "X Pulse Presence"
    rule = RuleVariation_954

class Variation_1424(Compound):
    items_list = [NonSpare_1716, NonSpare_1353, NonSpare_1363, NonSpare_952, NonSpare_869, NonSpare_1792, NonSpare_2000]
    items_dict = {"SUM": NonSpare_1716, "PMN": NonSpare_1353, "POS": NonSpare_1363, "GA": NonSpare_952, "EM1": NonSpare_869, "TOS": NonSpare_1792, "XP": NonSpare_2000}

class RuleVariation_1354(RuleVariationContextFree):
    variation = Variation_1424

class NonSpare_270(NonSpare):
    name = "110"
    title = "Mode 5 Data Reports and Extended Mode 1 Code"
    rule = RuleVariation_1354

class Variation_990(Group):
    bit_size = 16
    items_list = [Item_3, Item_537]
    items_dict = {"MODE2": NonSpare_1226}

class RuleVariation_960(RuleVariationContextFree):
    variation = Variation_990

class NonSpare_283(NonSpare):
    name = "120"
    title = "Track Mode 2 Code"
    rule = RuleVariation_960

class NonSpare_1210(NonSpare):
    name = "MIDENT"
    title = "Master System Unit Identification"
    rule = RuleVariation_154

class Item_522(Item):
    non_spare = NonSpare_1210

class NonSpare_1258(NonSpare):
    name = "MTRACK"
    title = "Master System Track Number"
    rule = RuleVariation_238

class Item_567(Item):
    non_spare = NonSpare_1258

class NonSpare_1624(NonSpare):
    name = "SIDENT"
    title = "Slave System Unit Identification"
    rule = RuleVariation_154

class Item_846(Item):
    non_spare = NonSpare_1624

class NonSpare_1706(NonSpare):
    name = "STRACK"
    title = "Slave System Track Number"
    rule = RuleVariation_238

class Item_913(Item):
    non_spare = NonSpare_1706

class Variation_1296(Extended):
    items = [Item_522, Item_567, None, Item_846, Item_913, None]

class RuleVariation_1226(RuleVariationContextFree):
    variation = Variation_1296

class NonSpare_482(NonSpare):
    name = "510"
    title = "Composed Track Number"
    rule = RuleVariation_1226

class NonSpare_1969(NonSpare):
    name = "X"
    title = "APC (X-Component)"
    rule = RuleVariation_296

class Item_1118(Item):
    non_spare = NonSpare_1969

class NonSpare_2021(NonSpare):
    name = "Y"
    title = "APC (Y-Component)"
    rule = RuleVariation_296

class Item_1167(Item):
    non_spare = NonSpare_2021

class Variation_1254(Group):
    bit_size = 32
    items_list = [Item_1118, Item_1167]
    items_dict = {"X": NonSpare_1969, "Y": NonSpare_2021}

class RuleVariation_1188(RuleVariationContextFree):
    variation = Variation_1254

class NonSpare_570(NonSpare):
    name = "APC"
    title = "Estimated Accuracy Of Track Position (Cartesian)"
    rule = RuleVariation_1188

class NonSpare_752(NonSpare):
    name = "COV"
    title = "XY Covariance Component"
    rule = RuleVariation_250

class Content_761(ContentQuantity):
    signedness = Unsigned
    lsb = 5.364418029785156e-6
    unit = "°"

class RuleContent_760(RuleContentContextFree):
    variation = Content_761

class Variation_334(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_760

class RuleVariation_328(RuleVariationContextFree):
    variation = Variation_334

class NonSpare_1072(NonSpare):
    name = "LAT"
    title = "APW (Latitude Component)"
    rule = RuleVariation_328

class Item_429(Item):
    non_spare = NonSpare_1072

class NonSpare_1099(NonSpare):
    name = "LON"
    title = "APW (Longitude Component)"
    rule = RuleVariation_328

class Item_454(Item):
    non_spare = NonSpare_1099

class Variation_1087(Group):
    bit_size = 32
    items_list = [Item_429, Item_454]
    items_dict = {"LAT": NonSpare_1072, "LON": NonSpare_1099}

class RuleVariation_1046(RuleVariationContextFree):
    variation = Variation_1087

class NonSpare_575(NonSpare):
    name = "APW"
    title = "Estimated Accuracy Of Track Position (WGS-84)"
    rule = RuleVariation_1046

class Content_755(ContentQuantity):
    signedness = Unsigned
    lsb = 6.25
    unit = "ft"

class RuleContent_754(RuleContentContextFree):
    variation = Content_755

class Variation_234(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_754

class RuleVariation_228(RuleVariationContextFree):
    variation = Variation_234

class NonSpare_542(NonSpare):
    name = "AGA"
    title = "Estimated Accuracy Of Calculated Track Geometric Altitude"
    rule = RuleVariation_228

class Variation_222(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_722

class RuleVariation_216(RuleVariationContextFree):
    variation = Variation_222

class NonSpare_515(NonSpare):
    name = "ABA"
    title = "Estimated Accuracy Of Calculated Track Barometric Altitude"
    rule = RuleVariation_216

class Content_728(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "m/s"

class RuleContent_727(RuleContentContextFree):
    variation = Content_728

class Variation_224(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_727

class RuleVariation_218(RuleVariationContextFree):
    variation = Variation_224

class NonSpare_1970(NonSpare):
    name = "X"
    title = "ATV (X-Component)"
    rule = RuleVariation_218

class Item_1119(Item):
    non_spare = NonSpare_1970

class NonSpare_2022(NonSpare):
    name = "Y"
    title = "ATV (Y-Component)"
    rule = RuleVariation_218

class Item_1168(Item):
    non_spare = NonSpare_2022

class Variation_1255(Group):
    bit_size = 16
    items_list = [Item_1119, Item_1168]
    items_dict = {"X": NonSpare_1970, "Y": NonSpare_2022}

class RuleVariation_1189(RuleVariationContextFree):
    variation = Variation_1255

class NonSpare_598(NonSpare):
    name = "ATV"
    title = "Estimated Accuracy Of Track Velocity (Cartesian)"
    rule = RuleVariation_1189

class Content_729(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "m/s²"

class RuleContent_728(RuleContentContextFree):
    variation = Content_729

class Variation_225(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_728

class RuleVariation_219(RuleVariationContextFree):
    variation = Variation_225

class NonSpare_1968(NonSpare):
    name = "X"
    title = "AA (X-Component)"
    rule = RuleVariation_219

class Item_1117(Item):
    non_spare = NonSpare_1968

class NonSpare_2020(NonSpare):
    name = "Y"
    title = "AA (Y-Component)"
    rule = RuleVariation_219

class Item_1166(Item):
    non_spare = NonSpare_2020

class Variation_1253(Group):
    bit_size = 16
    items_list = [Item_1117, Item_1166]
    items_dict = {"X": NonSpare_1968, "Y": NonSpare_2020}

class RuleVariation_1187(RuleVariationContextFree):
    variation = Variation_1253

class NonSpare_511(NonSpare):
    name = "AA"
    title = "Estimated Accuracy Of Acceleration (Cartesian)"
    rule = RuleVariation_1187

class Content_756(ContentQuantity):
    signedness = Unsigned
    lsb = 6.25
    unit = "ft/min"

class RuleContent_755(RuleContentContextFree):
    variation = Content_756

class Variation_235(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_755

class RuleVariation_229(RuleVariationContextFree):
    variation = Variation_235

class NonSpare_585(NonSpare):
    name = "ARC"
    title = "Estimated Accuracy Of Rate Of Climb/Descent"
    rule = RuleVariation_229

class Variation_1393(Compound):
    items_list = [NonSpare_570, NonSpare_752, NonSpare_575, NonSpare_542, NonSpare_515, NonSpare_598, NonSpare_511, NonSpare_585]
    items_dict = {"APC": NonSpare_570, "COV": NonSpare_752, "APW": NonSpare_575, "AGA": NonSpare_542, "ABA": NonSpare_515, "ATV": NonSpare_598, "AA": NonSpare_511, "ARC": NonSpare_585}

class RuleVariation_1323(RuleVariationContextFree):
    variation = Variation_1393

class NonSpare_477(NonSpare):
    name = "500"
    title = "Estimated Accuracies"
    rule = RuleVariation_1323

class NonSpare_1621(NonSpare):
    name = "SID"
    title = "Sensor Identification"
    rule = RuleVariation_1104

class NonSpare_1504(NonSpare):
    name = "RHO"
    title = "Measured Distance"
    rule = RuleVariation_322

class Item_775(Item):
    non_spare = NonSpare_1504

class NonSpare_1762(NonSpare):
    name = "THETA"
    title = "Measured Azimuth"
    rule = RuleVariation_329

class Item_948(Item):
    non_spare = NonSpare_1762

class Variation_1142(Group):
    bit_size = 32
    items_list = [Item_775, Item_948]
    items_dict = {"RHO": NonSpare_1504, "THETA": NonSpare_1762}

class RuleVariation_1095(RuleVariationContextFree):
    variation = Variation_1142

class NonSpare_1362(NonSpare):
    name = "POS"
    title = "Measured Position"
    rule = RuleVariation_1095

class NonSpare_996(NonSpare):
    name = "HEIGHT"
    title = "Measured 3-D Height"
    rule = RuleVariation_295

class Content_627(ContentQuantity):
    signedness = Signed
    lsb = 0.25
    unit = "FL"

class RuleContent_627(RuleContentContextFree):
    variation = Content_627

class Variation_586(Element):
    bit_offset8 = 2
    bit_size = 14
    rule = RuleContent_627

class RuleVariation_576(RuleVariationContextFree):
    variation = Variation_586

class NonSpare_1096(NonSpare):
    name = "LMC"
    title = "Last Measured Mode C Code"
    rule = RuleVariation_576

class Item_451(Item):
    non_spare = NonSpare_1096

class Variation_1231(Group):
    bit_size = 16
    items_list = [Item_1050, Item_335, Item_451]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "LMC": NonSpare_1096}

class RuleVariation_1165(RuleVariationContextFree):
    variation = Variation_1231

class NonSpare_1178(NonSpare):
    name = "MDC"
    title = ""
    rule = RuleVariation_1165

class Content_267(ContentTable):
    values = {0: "Mode 3/A code as derived from the reply of the transponder", 1: "Mode 3/A code as provided by a sensor local tracker"}

class RuleContent_267(RuleContentContextFree):
    variation = Content_267

class Variation_524(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_267

class RuleVariation_514(RuleVariationContextFree):
    variation = Variation_524

class NonSpare_1060(NonSpare):
    name = "L"
    title = ""
    rule = RuleVariation_514

class Item_417(Item):
    non_spare = NonSpare_1060

class Variation_1226(Group):
    bit_size = 16
    items_list = [Item_1050, Item_335, Item_417, Item_16, Item_543]
    items_dict = {"V": NonSpare_1891, "G": NonSpare_945, "L": NonSpare_1060, "MODE3A": NonSpare_1232}

class RuleVariation_1160(RuleVariationContextFree):
    variation = Variation_1226

class NonSpare_1174(NonSpare):
    name = "MDA"
    title = ""
    rule = RuleVariation_1160

class Content_331(ContentTable):
    values = {0: "No detection", 1: "Single PSR detection", 2: "Single SSR detection", 3: "SSR + PSR detection", 4: "Single ModeS All-Call", 5: "Single ModeS Roll-Call", 6: "ModeS All-Call + PSR", 7: "ModeS Roll-Call + PSR"}

class RuleContent_331(RuleContentContextFree):
    variation = Content_331

class Variation_121(Element):
    bit_offset8 = 0
    bit_size = 3
    rule = RuleContent_331

class RuleVariation_121(RuleVariationContextFree):
    variation = Variation_121

class NonSpare_1867(NonSpare):
    name = "TYP"
    title = "Report Type"
    rule = RuleVariation_121

class Item_1027(Item):
    non_spare = NonSpare_1867

class Content_420(ContentTable):
    values = {0: "Report from target transponder", 1: "Report from field monitor (item transponder)"}

class RuleContent_420(RuleContentContextFree):
    variation = Content_420

class Variation_722(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_420

class RuleVariation_712(RuleVariationContextFree):
    variation = Variation_722

class NonSpare_1453(NonSpare):
    name = "RAB"
    title = ""
    rule = RuleVariation_712

class Item_729(Item):
    non_spare = NonSpare_1453

class Variation_839(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_414

class RuleVariation_810(RuleVariationContextFree):
    variation = Variation_839

class NonSpare_1837(NonSpare):
    name = "TST"
    title = ""
    rule = RuleVariation_810

class Item_1001(Item):
    non_spare = NonSpare_1837

class Variation_1197(Group):
    bit_size = 8
    items_list = [Item_1027, Item_857, Item_729, Item_1001, Item_27]
    items_dict = {"TYP": NonSpare_1867, "SIM": NonSpare_1635, "RAB": NonSpare_1453, "TST": NonSpare_1837}

class RuleVariation_1138(RuleVariationContextFree):
    variation = Variation_1197

class NonSpare_1864(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_1138

class Variation_1422(Compound):
    items_list = [NonSpare_1621, NonSpare_1362, NonSpare_996, NonSpare_1178, NonSpare_1174, NonSpare_1864]
    items_dict = {"SID": NonSpare_1621, "POS": NonSpare_1362, "HEIGHT": NonSpare_996, "MDC": NonSpare_1178, "MDA": NonSpare_1174, "TYP": NonSpare_1864}

class RuleVariation_1352(RuleVariationContextFree):
    variation = Variation_1422

class NonSpare_444(NonSpare):
    name = "340"
    title = "Measured Information"
    rule = RuleVariation_1352

class Record_40(Record):
    items_list = [NonSpare_45, UapItemSpare, NonSpare_57, NonSpare_202, NonSpare_262, NonSpare_250, NonSpare_356, NonSpare_378, NonSpare_189, NonSpare_404, NonSpare_447, NonSpare_141, NonSpare_223, NonSpare_432, NonSpare_362, NonSpare_435, NonSpare_297, NonSpare_284, NonSpare_296, NonSpare_389, NonSpare_455, NonSpare_424, NonSpare_439, NonSpare_270, NonSpare_283, NonSpare_482, NonSpare_477, NonSpare_444, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_45, "015": NonSpare_57, "070": NonSpare_202, "105": NonSpare_262, "100": NonSpare_250, "185": NonSpare_356, "210": NonSpare_378, "060": NonSpare_189, "245": NonSpare_404, "380": NonSpare_447, "040": NonSpare_141, "080": NonSpare_223, "290": NonSpare_432, "200": NonSpare_362, "295": NonSpare_435, "136": NonSpare_297, "130": NonSpare_284, "135": NonSpare_296, "220": NonSpare_389, "390": NonSpare_455, "270": NonSpare_424, "300": NonSpare_439, "110": NonSpare_270, "120": NonSpare_283, "510": NonSpare_482, "500": NonSpare_477, "340": NonSpare_444, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_36(UapSingle):
    record = Record_40

class Asterix_46(AstCat):
    category = 62
    edition = (1, 17)
    uap = Uap_36

class Content_309(ContentTable):
    values = {0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 6: "Not defined", 7: "Unknown or not yet extracted"}

class RuleContent_309(RuleContentContextFree):
    variation = Content_309

class Variation_660(Element):
    bit_offset8 = 3
    bit_size = 3
    rule = RuleContent_309

class RuleVariation_650(RuleVariationContextFree):
    variation = Variation_660

class NonSpare_1685(NonSpare):
    name = "STAT"
    title = "Flight Status"
    rule = RuleVariation_650

class Item_893(Item):
    non_spare = NonSpare_1685

class Variation_1041(Group):
    bit_size = 16
    items_list = [Item_182, Item_893, Item_27, Item_881, Item_77, Item_54, Item_104, Item_107]
    items_dict = {"COM": NonSpare_735, "STAT": NonSpare_1685, "SSC": NonSpare_1664, "ARC": NonSpare_581, "AIC": NonSpare_549, "B1A": NonSpare_621, "B1B": NonSpare_624}

class RuleVariation_1002(RuleVariationContextFree):
    variation = Variation_1041

class NonSpare_738(NonSpare):
    name = "COM"
    title = "Communications/ACAS Capability and Flight Status"
    rule = RuleVariation_1002

class Variation_1387(Compound):
    items_list = [NonSpare_533, NonSpare_1041, NonSpare_1203, NonSpare_1034, NonSpare_1737, NonSpare_1566, NonSpare_936, NonSpare_1778, NonSpare_1773, NonSpare_738, NonSpare_1560, NonSpare_525, NonSpare_648, NonSpare_981, NonSpare_1463, NonSpare_1732, NonSpare_1728, NonSpare_970, NonSpare_1929, NonSpare_1195, NonSpare_873, NonSpare_1367, NonSpare_953, NonSpare_1400, NonSpare_1155, NonSpare_1030, NonSpare_1145, NonSpare_646]
    items_dict = {"ADR": NonSpare_533, "ID": NonSpare_1041, "MHG": NonSpare_1203, "IAS": NonSpare_1034, "TAS": NonSpare_1737, "SAL": NonSpare_1566, "FSS": NonSpare_936, "TIS": NonSpare_1778, "TID": NonSpare_1773, "COM": NonSpare_738, "SAB": NonSpare_1560, "ACS": NonSpare_525, "BVR": NonSpare_648, "GVR": NonSpare_981, "RAN": NonSpare_1463, "TAR": NonSpare_1732, "TAN": NonSpare_1728, "GS": NonSpare_970, "VUN": NonSpare_1929, "MET": NonSpare_1195, "EMC": NonSpare_873, "POS": NonSpare_1367, "GAL": NonSpare_953, "PUN": NonSpare_1400, "MB": NonSpare_1155, "IAR": NonSpare_1030, "MAC": NonSpare_1145, "BPS": NonSpare_646}

class RuleVariation_1317(RuleVariationContextFree):
    variation = Variation_1387

class NonSpare_450(NonSpare):
    name = "380"
    title = "Aircraft Derived Data"
    rule = RuleVariation_1317

class Content_187(ContentTable):
    values = {0: "Default value", 1: "Surface target"}

class RuleContent_187(RuleContentContextFree):
    variation = Content_187

class Variation_612(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_187

class RuleVariation_602(RuleVariationContextFree):
    variation = Variation_612

class NonSpare_1611(NonSpare):
    name = "SFC"
    title = ""
    rule = RuleVariation_602

class Item_837(Item):
    non_spare = NonSpare_1611

class Content_348(ContentTable):
    values = {0: "No indication", 1: "Duplicate Flight-ID"}

class RuleContent_348(RuleContentContextFree):
    variation = Content_348

class Variation_713(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_348

class RuleVariation_703(RuleVariationContextFree):
    variation = Variation_713

class NonSpare_1042(NonSpare):
    name = "IDD"
    title = ""
    rule = RuleVariation_703

class Item_404(Item):
    non_spare = NonSpare_1042

class Content_180(ContentTable):
    values = {0: "Default value", 1: "Inconsistent Emergency Code"}

class RuleContent_180(RuleContentContextFree):
    variation = Content_180

class Variation_814(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_180

class RuleVariation_785(RuleVariationContextFree):
    variation = Variation_814

class NonSpare_1045(NonSpare):
    name = "IEC"
    title = ""
    rule = RuleVariation_785

class Item_407(Item):
    non_spare = NonSpare_1045

class Variation_1301(Extended):
    items = [Item_547, Item_867, Item_550, Item_878, Item_161, None, Item_855, Item_994, Item_992, Item_316, Item_49, Item_911, Item_414, None, Item_67, Item_505, Item_512, Item_519, Item_506, None, Item_213, Item_674, Item_883, Item_507, Item_45, Item_918, Item_34, None, Item_830, Item_282, Item_646, Item_317, None, Item_272, Item_270, Item_271, Item_837, Item_404, Item_407, Item_26, None]

class RuleVariation_1231(RuleVariationContextFree):
    variation = Variation_1301

class NonSpare_224(NonSpare):
    name = "080"
    title = "Track Status"
    rule = RuleVariation_1231

class Record_43(Record):
    items_list = [NonSpare_45, UapItemSpare, NonSpare_57, NonSpare_202, NonSpare_262, NonSpare_250, NonSpare_356, NonSpare_378, NonSpare_189, NonSpare_404, NonSpare_450, NonSpare_141, NonSpare_224, NonSpare_432, NonSpare_362, NonSpare_435, NonSpare_297, NonSpare_284, NonSpare_296, NonSpare_389, NonSpare_455, NonSpare_424, NonSpare_439, NonSpare_270, NonSpare_283, NonSpare_482, NonSpare_477, NonSpare_444, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_45, "015": NonSpare_57, "070": NonSpare_202, "105": NonSpare_262, "100": NonSpare_250, "185": NonSpare_356, "210": NonSpare_378, "060": NonSpare_189, "245": NonSpare_404, "380": NonSpare_450, "040": NonSpare_141, "080": NonSpare_224, "290": NonSpare_432, "200": NonSpare_362, "295": NonSpare_435, "136": NonSpare_297, "130": NonSpare_284, "135": NonSpare_296, "220": NonSpare_389, "390": NonSpare_455, "270": NonSpare_424, "300": NonSpare_439, "110": NonSpare_270, "120": NonSpare_283, "510": NonSpare_482, "500": NonSpare_477, "340": NonSpare_444, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_39(UapSingle):
    record = Record_43

class Asterix_47(AstCat):
    category = 62
    edition = (1, 18)
    uap = Uap_39

class NonSpare_644(NonSpare):
    name = "BPS"
    title = "Barometric Pressure Setting"
    rule = RuleVariation_955

class Variation_1386(Compound):
    items_list = [NonSpare_533, NonSpare_1041, NonSpare_1203, NonSpare_1034, NonSpare_1737, NonSpare_1566, NonSpare_936, NonSpare_1778, NonSpare_1773, NonSpare_738, NonSpare_1560, NonSpare_525, NonSpare_648, NonSpare_981, NonSpare_1463, NonSpare_1732, NonSpare_1728, NonSpare_970, NonSpare_1929, NonSpare_1195, NonSpare_873, NonSpare_1367, NonSpare_953, NonSpare_1400, NonSpare_1155, NonSpare_1030, NonSpare_1145, NonSpare_644]
    items_dict = {"ADR": NonSpare_533, "ID": NonSpare_1041, "MHG": NonSpare_1203, "IAS": NonSpare_1034, "TAS": NonSpare_1737, "SAL": NonSpare_1566, "FSS": NonSpare_936, "TIS": NonSpare_1778, "TID": NonSpare_1773, "COM": NonSpare_738, "SAB": NonSpare_1560, "ACS": NonSpare_525, "BVR": NonSpare_648, "GVR": NonSpare_981, "RAN": NonSpare_1463, "TAR": NonSpare_1732, "TAN": NonSpare_1728, "GS": NonSpare_970, "VUN": NonSpare_1929, "MET": NonSpare_1195, "EMC": NonSpare_873, "POS": NonSpare_1367, "GAL": NonSpare_953, "PUN": NonSpare_1400, "MB": NonSpare_1155, "IAR": NonSpare_1030, "MAC": NonSpare_1145, "BPS": NonSpare_644}

class RuleVariation_1316(RuleVariationContextFree):
    variation = Variation_1386

class NonSpare_449(NonSpare):
    name = "380"
    title = "Aircraft Derived Data"
    rule = RuleVariation_1316

class Content_725(ContentQuantity):
    signedness = Unsigned
    lsb = 0.25
    unit = "FL"

class RuleContent_724(RuleContentContextFree):
    variation = Content_725

class Variation_314(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_724

class RuleVariation_308(RuleVariationContextFree):
    variation = Variation_314

class NonSpare_678(NonSpare):
    name = "CFL"
    title = "Current Cleared Flight Level"
    rule = RuleVariation_308

class Variation_1429(Compound):
    items_list = [NonSpare_1727, NonSpare_780, NonSpare_1047, NonSpare_905, NonSpare_1726, NonSpare_1953, NonSpare_827, NonSpare_851, NonSpare_1478, NonSpare_678, NonSpare_797, NonSpare_1789, NonSpare_591, NonSpare_1707, NonSpare_1696, NonSpare_1679, NonSpare_1346, NonSpare_1345]
    items_dict = {"TAG": NonSpare_1727, "CS": NonSpare_780, "IFI": NonSpare_1047, "FCT": NonSpare_905, "TAC": NonSpare_1726, "WTC": NonSpare_1953, "DEP": NonSpare_827, "DST": NonSpare_851, "RDS": NonSpare_1478, "CFL": NonSpare_678, "CTL": NonSpare_797, "TOD": NonSpare_1789, "AST": NonSpare_591, "STS": NonSpare_1707, "STD": NonSpare_1696, "STA": NonSpare_1679, "PEM": NonSpare_1346, "PEC": NonSpare_1345}

class RuleVariation_1359(RuleVariationContextFree):
    variation = Variation_1429

class NonSpare_456(NonSpare):
    name = "390"
    title = "Flight Plan Related Data"
    rule = RuleVariation_1359

class NonSpare_995(NonSpare):
    name = "HEIGHT"
    title = "Measured 3-D Height"
    rule = RuleVariation_247

class Variation_1421(Compound):
    items_list = [NonSpare_1621, NonSpare_1362, NonSpare_995, NonSpare_1178, NonSpare_1174, NonSpare_1864]
    items_dict = {"SID": NonSpare_1621, "POS": NonSpare_1362, "HEIGHT": NonSpare_995, "MDC": NonSpare_1178, "MDA": NonSpare_1174, "TYP": NonSpare_1864}

class RuleVariation_1351(RuleVariationContextFree):
    variation = Variation_1421

class NonSpare_443(NonSpare):
    name = "340"
    title = "Measured Information"
    rule = RuleVariation_1351

class Record_42(Record):
    items_list = [NonSpare_45, UapItemSpare, NonSpare_57, NonSpare_202, NonSpare_262, NonSpare_250, NonSpare_356, NonSpare_378, NonSpare_189, NonSpare_404, NonSpare_449, NonSpare_141, NonSpare_224, NonSpare_432, NonSpare_362, NonSpare_435, NonSpare_297, NonSpare_284, NonSpare_296, NonSpare_389, NonSpare_456, NonSpare_424, NonSpare_439, NonSpare_270, NonSpare_283, NonSpare_482, NonSpare_477, NonSpare_443, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_45, "015": NonSpare_57, "070": NonSpare_202, "105": NonSpare_262, "100": NonSpare_250, "185": NonSpare_356, "210": NonSpare_378, "060": NonSpare_189, "245": NonSpare_404, "380": NonSpare_449, "040": NonSpare_141, "080": NonSpare_224, "290": NonSpare_432, "200": NonSpare_362, "295": NonSpare_435, "136": NonSpare_297, "130": NonSpare_284, "135": NonSpare_296, "220": NonSpare_389, "390": NonSpare_456, "270": NonSpare_424, "300": NonSpare_439, "110": NonSpare_270, "120": NonSpare_283, "510": NonSpare_482, "500": NonSpare_477, "340": NonSpare_443, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_38(UapSingle):
    record = Record_42

class Asterix_48(AstCat):
    category = 62
    edition = (1, 19)
    uap = Uap_38

class NonSpare_631(NonSpare):
    name = "BDSDATA"
    title = "BDS Register DATA"
    rule = RuleVariation_1262

class Variation_1385(Compound):
    items_list = [NonSpare_533, NonSpare_1041, NonSpare_1203, NonSpare_1034, NonSpare_1737, NonSpare_1566, NonSpare_936, NonSpare_1778, NonSpare_1773, NonSpare_738, NonSpare_1560, NonSpare_525, NonSpare_648, NonSpare_981, NonSpare_1463, NonSpare_1732, NonSpare_1728, NonSpare_970, NonSpare_1929, NonSpare_1195, NonSpare_873, NonSpare_1367, NonSpare_953, NonSpare_1400, NonSpare_631, NonSpare_1030, NonSpare_1145, NonSpare_644]
    items_dict = {"ADR": NonSpare_533, "ID": NonSpare_1041, "MHG": NonSpare_1203, "IAS": NonSpare_1034, "TAS": NonSpare_1737, "SAL": NonSpare_1566, "FSS": NonSpare_936, "TIS": NonSpare_1778, "TID": NonSpare_1773, "COM": NonSpare_738, "SAB": NonSpare_1560, "ACS": NonSpare_525, "BVR": NonSpare_648, "GVR": NonSpare_981, "RAN": NonSpare_1463, "TAR": NonSpare_1732, "TAN": NonSpare_1728, "GS": NonSpare_970, "VUN": NonSpare_1929, "MET": NonSpare_1195, "EMC": NonSpare_873, "POS": NonSpare_1367, "GAL": NonSpare_953, "PUN": NonSpare_1400, "BDSDATA": NonSpare_631, "IAR": NonSpare_1030, "MAC": NonSpare_1145, "BPS": NonSpare_644}

class RuleVariation_1315(RuleVariationContextFree):
    variation = Variation_1385

class NonSpare_448(NonSpare):
    name = "380"
    title = "Aircraft Derived Data"
    rule = RuleVariation_1315

class Content_169(ContentTable):
    values = {0: "Default value", 1: "Age of the last received MLAT track updateis higher than system dependent threshold"}

class RuleContent_169(RuleContentContextFree):
    variation = Content_169

class Variation_890(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_169

class RuleVariation_861(RuleVariationContextFree):
    variation = Variation_890

class NonSpare_1215(NonSpare):
    name = "MLAT"
    title = ""
    rule = RuleVariation_861

class Item_527(Item):
    non_spare = NonSpare_1215

class Variation_1302(Extended):
    items = [Item_547, Item_867, Item_550, Item_878, Item_161, None, Item_855, Item_994, Item_992, Item_316, Item_49, Item_911, Item_414, None, Item_67, Item_505, Item_512, Item_519, Item_506, None, Item_213, Item_674, Item_883, Item_507, Item_45, Item_918, Item_34, None, Item_830, Item_282, Item_646, Item_317, None, Item_272, Item_270, Item_271, Item_837, Item_404, Item_407, Item_527, None]

class RuleVariation_1232(RuleVariationContextFree):
    variation = Variation_1302

class NonSpare_225(NonSpare):
    name = "080"
    title = "Track Status"
    rule = RuleVariation_1232

class Record_41(Record):
    items_list = [NonSpare_45, UapItemSpare, NonSpare_57, NonSpare_202, NonSpare_262, NonSpare_250, NonSpare_356, NonSpare_378, NonSpare_189, NonSpare_404, NonSpare_448, NonSpare_141, NonSpare_225, NonSpare_432, NonSpare_362, NonSpare_435, NonSpare_297, NonSpare_284, NonSpare_296, NonSpare_389, NonSpare_456, NonSpare_424, NonSpare_439, NonSpare_270, NonSpare_283, NonSpare_482, NonSpare_477, NonSpare_443, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_45, "015": NonSpare_57, "070": NonSpare_202, "105": NonSpare_262, "100": NonSpare_250, "185": NonSpare_356, "210": NonSpare_378, "060": NonSpare_189, "245": NonSpare_404, "380": NonSpare_448, "040": NonSpare_141, "080": NonSpare_225, "290": NonSpare_432, "200": NonSpare_362, "295": NonSpare_435, "136": NonSpare_297, "130": NonSpare_284, "135": NonSpare_296, "220": NonSpare_389, "390": NonSpare_456, "270": NonSpare_424, "300": NonSpare_439, "110": NonSpare_270, "120": NonSpare_283, "510": NonSpare_482, "500": NonSpare_477, "340": NonSpare_443, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_37(UapSingle):
    record = Record_41

class Asterix_49(AstCat):
    category = 62
    edition = (1, 20)
    uap = Uap_37

class NonSpare_63(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class NonSpare_107(NonSpare):
    name = "030"
    title = "Time of Message"
    rule = RuleVariation_356

class NonSpare_166(NonSpare):
    name = "050"
    title = "Sensor Identifier"
    rule = RuleVariation_1104

class Content_392(ContentTable):
    values = {0: "Operational", 1: "Degraded", 2: "Initialization", 3: "Not currently connected"}

class RuleContent_392(RuleContentContextFree):
    variation = Content_392

class Variation_102(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_392

class RuleVariation_102(RuleVariationContextFree):
    variation = Variation_102

class NonSpare_742(NonSpare):
    name = "CON"
    title = ""
    rule = RuleVariation_102

class Item_185(Item):
    non_spare = NonSpare_742

class Content_401(ContentTable):
    values = {0: "PSR GO", 1: "PSR NOGO"}

class RuleContent_401(RuleContentContextFree):
    variation = Content_401

class Variation_549(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_401

class RuleVariation_539(RuleVariationContextFree):
    variation = Variation_549

class NonSpare_1388(NonSpare):
    name = "PSR"
    title = ""
    rule = RuleVariation_539

class Item_675(Item):
    non_spare = NonSpare_1388

class Content_427(ContentTable):
    values = {0: "SSR GO", 1: "SSR NOGO"}

class RuleContent_427(RuleContentContextFree):
    variation = Content_427

class Variation_635(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_427

class RuleVariation_625(RuleVariationContextFree):
    variation = Variation_635

class NonSpare_1667(NonSpare):
    name = "SSR"
    title = ""
    rule = RuleVariation_625

class Item_884(Item):
    non_spare = NonSpare_1667

class Content_249(ContentTable):
    values = {0: "MDS GO", 1: "MDS NOGO"}

class RuleContent_249(RuleContentContextFree):
    variation = Content_249

class Variation_703(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_249

class RuleVariation_693(RuleVariationContextFree):
    variation = Variation_703

class NonSpare_1180(NonSpare):
    name = "MDS"
    title = ""
    rule = RuleVariation_693

class Item_508(Item):
    non_spare = NonSpare_1180

class Content_8(ContentTable):
    values = {0: "ADS GO", 1: "ADS NOGO"}

class RuleContent_8(RuleContentContextFree):
    variation = Content_8

class Variation_789(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_8

class RuleVariation_760(RuleVariationContextFree):
    variation = Variation_789

class NonSpare_536(NonSpare):
    name = "ADS"
    title = ""
    rule = RuleVariation_760

class Item_46(Item):
    non_spare = NonSpare_536

class Content_250(ContentTable):
    values = {0: "MLT GO", 1: "MLT NOGO"}

class RuleContent_250(RuleContentContextFree):
    variation = Content_250

class Variation_904(Element):
    bit_offset8 = 6
    bit_size = 1
    rule = RuleContent_250

class RuleVariation_875(RuleVariationContextFree):
    variation = Variation_904

class NonSpare_1216(NonSpare):
    name = "MLT"
    title = ""
    rule = RuleVariation_875

class Item_528(Item):
    non_spare = NonSpare_1216

class Content_438(ContentTable):
    values = {0: "System is released for operational use", 1: "Operational use of System is inhibited"}

class RuleContent_438(RuleContentContextFree):
    variation = Content_438

class Variation_72(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_438

class RuleVariation_72(RuleVariationContextFree):
    variation = Variation_72

class NonSpare_1321(NonSpare):
    name = "OPS"
    title = "Operational Release Status of the System"
    rule = RuleVariation_72

class Item_625(Item):
    non_spare = NonSpare_1321

class NonSpare_1337(NonSpare):
    name = "OXT"
    title = "Transmission Subsystem Overload Status"
    rule = RuleVariation_504

class Item_640(Item):
    non_spare = NonSpare_1337

class Variation_622(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_281

class RuleVariation_612(RuleVariationContextFree):
    variation = Variation_622

class NonSpare_1247(NonSpare):
    name = "MSC"
    title = "Monitoring System Connected Status"
    rule = RuleVariation_612

class Item_556(Item):
    non_spare = NonSpare_1247

class Content_163(ContentTable):
    values = {0: "Default (no meaning)", 1: "No plots being received"}

class RuleContent_163(RuleContentContextFree):
    variation = Content_163

class Variation_813(Element):
    bit_offset8 = 5
    bit_size = 1
    rule = RuleContent_163

class RuleVariation_784(RuleVariationContextFree):
    variation = Variation_813

class NonSpare_1302(NonSpare):
    name = "NPW"
    title = "No Plot Warning"
    rule = RuleVariation_784

class Item_606(Item):
    non_spare = NonSpare_1302

class Variation_1288(Extended):
    items = [Item_185, Item_675, Item_884, Item_508, Item_46, Item_528, None, Item_625, Item_619, Item_640, Item_556, Item_1006, Item_606, Item_26, None]

class RuleVariation_1218(RuleVariationContextFree):
    variation = Variation_1288

class NonSpare_184(NonSpare):
    name = "060"
    title = "Sensor Configuration and Status"
    rule = RuleVariation_1218

class Content_602(ContentQuantity):
    signedness = Signed
    lsb = 1.0
    unit = "ms"

class RuleContent_602(RuleContentContextFree):
    variation = Content_602

class Variation_251(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_602

class RuleVariation_245(RuleVariationContextFree):
    variation = Variation_251

class NonSpare_203(NonSpare):
    name = "070"
    title = "Time Stamping Bias"
    rule = RuleVariation_245

class Content_623(ContentQuantity):
    signedness = Signed
    lsb = 1.0e-5
    unit = ""

class RuleContent_623(RuleContentContextFree):
    variation = Content_623

class Variation_262(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_623

class RuleVariation_256(RuleVariationContextFree):
    variation = Variation_262

class NonSpare_1660(NonSpare):
    name = "SRG"
    title = "Mode S Range Gain"
    rule = RuleVariation_256

class Item_879(Item):
    non_spare = NonSpare_1660

class Variation_277(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_648

class RuleVariation_271(RuleVariationContextFree):
    variation = Variation_277

class NonSpare_1654(NonSpare):
    name = "SRB"
    title = "Mode S Range Bias"
    rule = RuleVariation_271

class Item_874(Item):
    non_spare = NonSpare_1654

class Variation_1173(Group):
    bit_size = 32
    items_list = [Item_879, Item_874]
    items_dict = {"SRG": NonSpare_1660, "SRB": NonSpare_1654}

class RuleVariation_1120(RuleVariationContextFree):
    variation = Variation_1173

class NonSpare_219(NonSpare):
    name = "080"
    title = "SSR / Mode S Range Gain and Bias"
    rule = RuleVariation_1120

class Content_680(ContentQuantity):
    signedness = Signed
    lsb = 5.4931640625e-3
    unit = "°"

class RuleContent_680(RuleContentContextFree):
    variation = Content_680

class Variation_290(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_680

class RuleVariation_284(RuleVariationContextFree):
    variation = Variation_290

class NonSpare_227(NonSpare):
    name = "081"
    title = "SSR Mode S Azimuth Bias"
    rule = RuleVariation_284

class NonSpare_1380(NonSpare):
    name = "PRG"
    title = "PSR Range Gain"
    rule = RuleVariation_256

class Item_668(Item):
    non_spare = NonSpare_1380

class NonSpare_1373(NonSpare):
    name = "PRB"
    title = "PSR Range Bias"
    rule = RuleVariation_271

class Item_661(Item):
    non_spare = NonSpare_1373

class Variation_1125(Group):
    bit_size = 32
    items_list = [Item_668, Item_661]
    items_dict = {"PRG": NonSpare_1380, "PRB": NonSpare_1373}

class RuleVariation_1079(RuleVariationContextFree):
    variation = Variation_1125

class NonSpare_236(NonSpare):
    name = "090"
    title = "PSR Range Gain and Bias"
    rule = RuleVariation_1079

class NonSpare_243(NonSpare):
    name = "091"
    title = "PSR Azimuth Bias"
    rule = RuleVariation_284

class NonSpare_245(NonSpare):
    name = "092"
    title = "PSR Elevation Bias"
    rule = RuleVariation_284

class Record_21(Record):
    items_list = [NonSpare_36, NonSpare_63, NonSpare_107, NonSpare_166, NonSpare_184, NonSpare_203, NonSpare_219, NonSpare_227, NonSpare_236, NonSpare_243, NonSpare_245, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_36, "015": NonSpare_63, "030": NonSpare_107, "050": NonSpare_166, "060": NonSpare_184, "070": NonSpare_203, "080": NonSpare_219, "081": NonSpare_227, "090": NonSpare_236, "091": NonSpare_243, "092": NonSpare_245, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_19(UapSingle):
    record = Record_21

class Asterix_50(AstCat):
    category = 63
    edition = (1, 6)
    uap = Uap_19

class Content_565(ContentTable):
    values = {1: "SDPS Status", 2: "End of Batch", 3: "Service Status Report"}

class RuleContent_565(RuleContentContextFree):
    variation = Content_565

class Variation_184(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_565

class RuleVariation_178(RuleVariationContextFree):
    variation = Variation_184

class NonSpare_8(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_178

class NonSpare_59(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class NonSpare_106(NonSpare):
    name = "030"
    title = "Time of Message"
    rule = RuleVariation_356

class NonSpare_75(NonSpare):
    name = "020"
    title = "Batch Number"
    rule = RuleVariation_189

class Content_395(ContentTable):
    values = {0: "Operational", 1: "Degraded", 2: "Not currently connected", 3: "Unknown"}

class RuleContent_395(RuleContentContextFree):
    variation = Content_395

class Variation_105(Element):
    bit_offset8 = 0
    bit_size = 2
    rule = RuleContent_395

class RuleVariation_105(RuleVariationContextFree):
    variation = Variation_105

class NonSpare_1287(NonSpare):
    name = "NOGO"
    title = ""
    rule = RuleVariation_105

class Item_592(Item):
    non_spare = NonSpare_1287

class Content_131(ContentTable):
    values = {0: "Default", 1: "Overload"}

class RuleContent_131(RuleContentContextFree):
    variation = Content_131

class Variation_504(Element):
    bit_offset8 = 2
    bit_size = 1
    rule = RuleContent_131

class RuleVariation_494(RuleVariationContextFree):
    variation = Variation_504

class NonSpare_1328(NonSpare):
    name = "OVL"
    title = ""
    rule = RuleVariation_494

class Item_631(Item):
    non_spare = NonSpare_1328

class Content_105(ContentTable):
    values = {0: "Default", 1: "Invalid Time Source"}

class RuleContent_105(RuleContentContextFree):
    variation = Content_105

class Variation_599(Element):
    bit_offset8 = 3
    bit_size = 1
    rule = RuleContent_105

class RuleVariation_589(RuleVariationContextFree):
    variation = Variation_599

class NonSpare_1840(NonSpare):
    name = "TSV"
    title = ""
    rule = RuleVariation_589

class Item_1004(Item):
    non_spare = NonSpare_1840

class Content_373(ContentTable):
    values = {0: "Not applicable", 1: "SDPS-1 selected", 2: "SDPS-2 selected", 3: "SDPS-3 selected"}

class RuleContent_373(RuleContentContextFree):
    variation = Content_373

class Variation_738(Element):
    bit_offset8 = 4
    bit_size = 2
    rule = RuleContent_373

class RuleVariation_728(RuleVariationContextFree):
    variation = Variation_738

class NonSpare_1395(NonSpare):
    name = "PSS"
    title = "Processing System Status"
    rule = RuleVariation_728

class Item_677(Item):
    non_spare = NonSpare_1395

class NonSpare_1710(NonSpare):
    name = "STTN"
    title = "Track Re-numbering Indication"
    rule = RuleVariation_838

class Item_914(Item):
    non_spare = NonSpare_1710

class Variation_1114(Group):
    bit_size = 8
    items_list = [Item_592, Item_631, Item_1004, Item_677, Item_914, Item_29]
    items_dict = {"NOGO": NonSpare_1287, "OVL": NonSpare_1328, "TSV": NonSpare_1840, "PSS": NonSpare_1395, "STTN": NonSpare_1710}

class RuleVariation_1069(RuleVariationContextFree):
    variation = Variation_1114

class NonSpare_135(NonSpare):
    name = "040"
    title = "SDPS Configuration and Status"
    rule = RuleVariation_1069

class Content_566(ContentTable):
    values = {1: "Service degradation", 2: "Service degradation ended", 3: "Main radar out of service", 4: "Service interrupted by the operator", 5: "Service interrupted due to contingency", 6: "Ready for service restart after contingency", 7: "Service ended by the operator", 8: "Failure of user main radar", 9: "Service restarted by the operator", 10: "Main radar becoming operational", 11: "Main radar becoming degraded", 12: "Service continuity interrupted due to disconnection with adjacent unit", 13: "Service continuity restarted", 14: "Service synchronised on backup radar", 15: "Service synchronised on main radar", 16: "Main and backup radar, if any, failed"}

class RuleContent_566(RuleContentContextFree):
    variation = Content_566

class Variation_185(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_566

class RuleVariation_179(RuleVariationContextFree):
    variation = Variation_185

class NonSpare_168(NonSpare):
    name = "050"
    title = "Service Status Report"
    rule = RuleVariation_179

class Record_39(Record):
    items_list = [NonSpare_45, NonSpare_8, NonSpare_59, NonSpare_106, NonSpare_75, NonSpare_135, NonSpare_168, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, UapItemSpare, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_45, "000": NonSpare_8, "015": NonSpare_59, "030": NonSpare_106, "020": NonSpare_75, "040": NonSpare_135, "050": NonSpare_168, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_35(UapSingle):
    record = Record_39

class Asterix_51(AstCat):
    category = 65
    edition = (1, 4)
    uap = Uap_35

class Asterix_52(AstCat):
    category = 65
    edition = (1, 5)
    uap = Uap_35

class Asterix_53(AstCat):
    category = 65
    edition = (1, 6)
    uap = Uap_35

class NonSpare_32(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class NonSpare_61(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class Content_570(ContentTable):
    values = {1: "System Position Report", 2: "System Bearing Report", 3: "System Position Report of conflicting transmission", 4: "System Detection End Report", 5: "Sensor Data Report"}

class RuleContent_570(RuleContentContextFree):
    variation = Content_570

class Variation_188(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_570

class RuleVariation_182(RuleVariationContextFree):
    variation = Variation_188

class NonSpare_11(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_182

class NonSpare_105(NonSpare):
    name = "030"
    title = "Time of Day"
    rule = RuleVariation_356

class NonSpare_134(NonSpare):
    name = "040"
    title = "Report Number"
    rule = RuleVariation_154

class NonSpare_240(NonSpare):
    name = "090"
    title = "Radio Channel Name"
    rule = RuleVariation_378

class NonSpare_1083(NonSpare):
    name = "LAT"
    title = "Latitude in WGS-84"
    rule = RuleVariation_364

class Item_440(Item):
    non_spare = NonSpare_1083

class NonSpare_1111(NonSpare):
    name = "LON"
    title = "Longitude in WGS-84"
    rule = RuleVariation_363

class Item_466(Item):
    non_spare = NonSpare_1111

class Variation_1097(Group):
    bit_size = 64
    items_list = [Item_440, Item_466]
    items_dict = {"LAT": NonSpare_1083, "LON": NonSpare_1111}

class RuleVariation_1056(RuleVariationContextFree):
    variation = Variation_1097

class NonSpare_165(NonSpare):
    name = "050"
    title = "Position in WGS-84 Coordinates"
    rule = RuleVariation_1056

class NonSpare_182(NonSpare):
    name = "060"
    title = "Position in Cartesian Coordinates"
    rule = RuleVariation_1202

class Content_719(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-2
    unit = "°"

class RuleContent_719(RuleContentContextFree):
    variation = Content_719

class Variation_309(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_719

class RuleVariation_303(RuleVariationContextFree):
    variation = Variation_309

class NonSpare_192(NonSpare):
    name = "070"
    title = "Local Bearing"
    rule = RuleVariation_303

class NonSpare_220(NonSpare):
    name = "080"
    title = "System Bearing"
    rule = RuleVariation_303

class NonSpare_258(NonSpare):
    name = "100"
    title = "Quality of Measurement"
    rule = RuleVariation_154

class Content_701(ContentQuantity):
    signedness = Unsigned
    lsb = 100.0
    unit = "m"

class RuleContent_701(RuleContentContextFree):
    variation = Content_701

class Variation_216(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_701

class RuleVariation_210(RuleVariationContextFree):
    variation = Variation_216

class NonSpare_266(NonSpare):
    name = "110"
    title = "Estimated Uncertainty"
    rule = RuleVariation_210

class Variation_1324(Repetitive):
    rep_bytes = 1
    variation = Variation_159

class RuleVariation_1254(RuleVariationContextFree):
    variation = Variation_1324

class NonSpare_277(NonSpare):
    name = "120"
    title = "Contributing Sensors"
    rule = RuleVariation_1254

class NonSpare_285(NonSpare):
    name = "130"
    title = "Conflicting Transmitter Position in WGS-84 Coordinates"
    rule = RuleVariation_1056

class NonSpare_298(NonSpare):
    name = "140"
    title = "Conflicting Transmitter Position in Cartesian Coordinates"
    rule = RuleVariation_1202

class NonSpare_321(NonSpare):
    name = "150"
    title = "Conflicting Transmitter Estimated Uncertainty"
    rule = RuleVariation_210

class NonSpare_333(NonSpare):
    name = "160"
    title = "Track Number"
    rule = RuleVariation_240

class NonSpare_344(NonSpare):
    name = "170"
    title = "Sensor Identification"
    rule = RuleVariation_154

class Content_616(ContentQuantity):
    signedness = Signed
    lsb = 1.0e-2
    unit = "dBµV"

class RuleContent_616(RuleContentContextFree):
    variation = Content_616

class Variation_258(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_616

class RuleVariation_252(RuleVariationContextFree):
    variation = Variation_258

class NonSpare_355(NonSpare):
    name = "180"
    title = "Signal Level"
    rule = RuleVariation_252

class NonSpare_357(NonSpare):
    name = "190"
    title = "Signal Quality"
    rule = RuleVariation_154

class Content_622(ContentQuantity):
    signedness = Signed
    lsb = 1.0e-2
    unit = "°"

class RuleContent_622(RuleContentContextFree):
    variation = Content_622

class Variation_261(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_622

class RuleVariation_255(RuleVariationContextFree):
    variation = Variation_261

class NonSpare_364(NonSpare):
    name = "200"
    title = "Signal Elevation"
    rule = RuleVariation_255

class Record_12(Record):
    items_list = [NonSpare_32, NonSpare_61, NonSpare_11, NonSpare_105, NonSpare_134, NonSpare_240, NonSpare_165, NonSpare_182, NonSpare_192, NonSpare_220, NonSpare_258, NonSpare_266, NonSpare_277, NonSpare_285, NonSpare_298, NonSpare_321, NonSpare_333, NonSpare_344, NonSpare_355, NonSpare_357, NonSpare_364, NonSpare_1641]
    items_dict = {"010": NonSpare_32, "015": NonSpare_61, "000": NonSpare_11, "030": NonSpare_105, "040": NonSpare_134, "090": NonSpare_240, "050": NonSpare_165, "060": NonSpare_182, "070": NonSpare_192, "080": NonSpare_220, "100": NonSpare_258, "110": NonSpare_266, "120": NonSpare_277, "130": NonSpare_285, "140": NonSpare_298, "150": NonSpare_321, "160": NonSpare_333, "170": NonSpare_344, "180": NonSpare_355, "190": NonSpare_357, "200": NonSpare_364, "SP": NonSpare_1641}

class Uap_12(UapSingle):
    record = Record_12

class Asterix_54(AstCat):
    category = 205
    edition = (1, 0)
    uap = Uap_12

class NonSpare_43(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_1104

class Content_575(ContentTable):
    values = {1: "Video Summary message", 2: "Video message"}

class RuleContent_575(RuleContentContextFree):
    variation = Content_575

class Variation_191(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_575

class RuleVariation_185(RuleVariationContextFree):
    variation = Variation_191

class NonSpare_14(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_185

class NonSpare_92(NonSpare):
    name = "020"
    title = "Video Record Header"
    rule = RuleVariation_361

class Variation_1326(Repetitive):
    rep_bytes = 1
    variation = Variation_194

class RuleVariation_1256(RuleVariationContextFree):
    variation = Variation_1326

class NonSpare_109(NonSpare):
    name = "030"
    title = "Video Summary"
    rule = RuleVariation_1256

class Content_767(ContentQuantity):
    signedness = Unsigned
    lsb = 5.4931640625e-3
    unit = "°"

class RuleContent_766(RuleContentContextFree):
    variation = Content_767

class Variation_336(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_766

class RuleVariation_330(RuleVariationContextFree):
    variation = Variation_336

class NonSpare_1681(NonSpare):
    name = "STARTAZ"
    title = "Start Azimuth of the Cells Group"
    rule = RuleVariation_330

class Item_889(Item):
    non_spare = NonSpare_1681

class NonSpare_878(NonSpare):
    name = "ENDAZ"
    title = "End Azimuth of the Cells Group"
    rule = RuleVariation_330

class Item_283(Item):
    non_spare = NonSpare_878

class NonSpare_1682(NonSpare):
    name = "STARTRG"
    title = "Starting Range of the Cells Group, Expressed in Number of Cells"
    rule = RuleVariation_361

class Item_890(Item):
    non_spare = NonSpare_1682

class Variation_379(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_694

class RuleVariation_373(RuleVariationContextFree):
    variation = Variation_379

class NonSpare_671(NonSpare):
    name = "CELLDUR"
    title = "Video Cell Duration in Nano-seconds"
    rule = RuleVariation_373

class Item_141(Item):
    non_spare = NonSpare_671

class Variation_1175(Group):
    bit_size = 96
    items_list = [Item_889, Item_283, Item_890, Item_141]
    items_dict = {"STARTAZ": NonSpare_1681, "ENDAZ": NonSpare_878, "STARTRG": NonSpare_1682, "CELLDUR": NonSpare_671}

class RuleVariation_1122(RuleVariationContextFree):
    variation = Variation_1175

class NonSpare_143(NonSpare):
    name = "040"
    title = "Video Header Nano"
    rule = RuleVariation_1122

class Content_685(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "fs"

class RuleContent_685(RuleContentContextFree):
    variation = Content_685

class Variation_378(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_685

class RuleVariation_372(RuleVariationContextFree):
    variation = Variation_378

class NonSpare_670(NonSpare):
    name = "CELLDUR"
    title = "Video Cell Duration in Femto-seconds"
    rule = RuleVariation_372

class Item_140(Item):
    non_spare = NonSpare_670

class Variation_1174(Group):
    bit_size = 96
    items_list = [Item_889, Item_283, Item_890, Item_140]
    items_dict = {"STARTAZ": NonSpare_1681, "ENDAZ": NonSpare_878, "STARTRG": NonSpare_1682, "CELLDUR": NonSpare_670}

class RuleVariation_1121(RuleVariationContextFree):
    variation = Variation_1174

class NonSpare_149(NonSpare):
    name = "041"
    title = "Video Header Femto"
    rule = RuleVariation_1121

class Content_327(ContentTable):
    values = {0: "No compression applied", 1: "Compression applied"}

class RuleContent_327(RuleContentContextFree):
    variation = Content_327

class Variation_57(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_327

class RuleVariation_57(RuleVariationContextFree):
    variation = Variation_57

class NonSpare_652(NonSpare):
    name = "C"
    title = "Data Compression Indicator"
    rule = RuleVariation_57

class Item_127(Item):
    non_spare = NonSpare_652

class Content_559(ContentTable):
    values = {1: "Monobit Resolution (1 bit)", 2: "Low Resolution (2 bits)", 3: "Medium Resolution (4 bits)", 4: "High Resolution (8 bits)", 5: "Very High Resolution (16 bits)", 6: "Ultra High Resolution (32 bits)"}

class RuleContent_559(RuleContentContextFree):
    variation = Content_559

class Variation_178(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_559

class RuleVariation_172(RuleVariationContextFree):
    variation = Variation_178

class NonSpare_1497(NonSpare):
    name = "RES"
    title = "Bit Resolution"
    rule = RuleVariation_172

class Item_768(Item):
    non_spare = NonSpare_1497

class Variation_1026(Group):
    bit_size = 16
    items_list = [Item_127, Item_10, Item_768]
    items_dict = {"C": NonSpare_652, "RES": NonSpare_1497}

class RuleVariation_991(RuleVariationContextFree):
    variation = Variation_1026

class NonSpare_157(NonSpare):
    name = "048"
    title = "Video Cells Resolution & Data Compression Indicator"
    rule = RuleVariation_991

class NonSpare_1278(NonSpare):
    name = "NBVB"
    title = "Number of 'valid' Octets"
    rule = RuleVariation_241

class Item_583(Item):
    non_spare = NonSpare_1278

class NonSpare_1273(NonSpare):
    name = "NBCELLS"
    title = "Number of 'valid' Cells"
    rule = RuleVariation_337

class Item_578(Item):
    non_spare = NonSpare_1273

class Variation_1113(Group):
    bit_size = 40
    items_list = [Item_583, Item_578]
    items_dict = {"NBVB": NonSpare_1278, "NBCELLS": NonSpare_1273}

class RuleVariation_1068(RuleVariationContextFree):
    variation = Variation_1113

class NonSpare_158(NonSpare):
    name = "049"
    title = "Video Octets & Video Cells Counters"
    rule = RuleVariation_1068

class Variation_1329(Repetitive):
    rep_bytes = 1
    variation = Variation_365

class RuleVariation_1259(RuleVariationContextFree):
    variation = Variation_1329

class NonSpare_172(NonSpare):
    name = "050"
    title = "Video Block Low Data Volume"
    rule = RuleVariation_1259

class Variation_389(Element):
    bit_offset8 = 0
    bit_size = 512
    rule = RuleContent_0

class Variation_1333(Repetitive):
    rep_bytes = 1
    variation = Variation_389

class RuleVariation_1263(RuleVariationContextFree):
    variation = Variation_1333

class NonSpare_173(NonSpare):
    name = "051"
    title = "Video Block Medium Data Volume"
    rule = RuleVariation_1263

class Variation_390(Element):
    bit_offset8 = 0
    bit_size = 2048
    rule = RuleContent_0

class Variation_1334(Repetitive):
    rep_bytes = 1
    variation = Variation_390

class RuleVariation_1264(RuleVariationContextFree):
    variation = Variation_1334

class NonSpare_174(NonSpare):
    name = "052"
    title = "Video Block High Data Volume"
    rule = RuleVariation_1264

class NonSpare_306(NonSpare):
    name = "140"
    title = "Time of Day"
    rule = RuleVariation_356

class Record_37(Record):
    items_list = [NonSpare_43, NonSpare_14, NonSpare_92, NonSpare_109, NonSpare_143, NonSpare_149, NonSpare_157, NonSpare_158, NonSpare_172, NonSpare_173, NonSpare_174, NonSpare_306, NonSpare_1481, NonSpare_1641]
    items_dict = {"010": NonSpare_43, "000": NonSpare_14, "020": NonSpare_92, "030": NonSpare_109, "040": NonSpare_143, "041": NonSpare_149, "048": NonSpare_157, "049": NonSpare_158, "050": NonSpare_172, "051": NonSpare_173, "052": NonSpare_174, "140": NonSpare_306, "RE": NonSpare_1481, "SP": NonSpare_1641}

class Uap_33(UapSingle):
    record = Record_37

class Asterix_55(AstCat):
    category = 240
    edition = (1, 3)
    uap = Uap_33

class NonSpare_64(NonSpare):
    name = "015"
    title = "Service Identification"
    rule = RuleVariation_154

class NonSpare_308(NonSpare):
    name = "140"
    title = "Time of Day"
    rule = RuleVariation_356

class NonSpare_658(NonSpare):
    name = "CAT"
    title = "Category"
    rule = RuleVariation_189

class Item_131(Item):
    non_spare = NonSpare_658

class NonSpare_1150(NonSpare):
    name = "MAIN"
    title = "Main Version Number"
    rule = RuleVariation_189

class Item_495(Item):
    non_spare = NonSpare_1150

class NonSpare_1713(NonSpare):
    name = "SUB"
    title = "Sub Version Number"
    rule = RuleVariation_189

class Item_917(Item):
    non_spare = NonSpare_1713

class Variation_1028(Group):
    bit_size = 24
    items_list = [Item_131, Item_495, Item_917]
    items_dict = {"CAT": NonSpare_658, "MAIN": NonSpare_1150, "SUB": NonSpare_1713}

class Variation_1341(Repetitive):
    rep_bytes = 1
    variation = Variation_1028

class RuleVariation_1271(RuleVariationContextFree):
    variation = Variation_1341

class NonSpare_483(NonSpare):
    name = "550"
    title = "Category Version Number Report"
    rule = RuleVariation_1271

class Record_22(Record):
    items_list = [NonSpare_36, NonSpare_64, NonSpare_308, NonSpare_483, UapItemSpare, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_36, "015": NonSpare_64, "140": NonSpare_308, "550": NonSpare_483, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_20(UapSingle):
    record = Record_22

class Asterix_56(AstCat):
    category = 247
    edition = (1, 2)
    uap = Uap_20

class NonSpare_484(NonSpare):
    name = "550"
    title = "Category Version Number Report"
    rule = RuleVariation_1271

class Record_23(Record):
    items_list = [NonSpare_36, NonSpare_64, NonSpare_308, NonSpare_484, UapItemSpare, NonSpare_1641, NonSpare_1481]
    items_dict = {"010": NonSpare_36, "015": NonSpare_64, "140": NonSpare_308, "550": NonSpare_484, "SP": NonSpare_1641, "RE": NonSpare_1481}

class Uap_21(UapSingle):
    record = Record_23

class Asterix_57(AstCat):
    category = 247
    edition = (1, 3)
    uap = Uap_21

# Aliases

Cat_001_1_2 : TypeAlias = Asterix_0
Cat_001_1_3 : TypeAlias = Asterix_1
Cat_001_1_4 : TypeAlias = Asterix_2
Cat_002_1_0 : TypeAlias = Asterix_3
Cat_002_1_1 : TypeAlias = Asterix_4
Cat_004_1_12 : TypeAlias = Asterix_5
Cat_008_1_2 : TypeAlias = Asterix_6
Cat_008_1_3 : TypeAlias = Asterix_7
Cat_009_2_1 : TypeAlias = Asterix_8
Cat_010_1_1 : TypeAlias = Asterix_9
Cat_011_1_2 : TypeAlias = Asterix_10
Cat_011_1_3 : TypeAlias = Asterix_11
Cat_015_1_0 : TypeAlias = Asterix_12
Cat_015_1_1 : TypeAlias = Asterix_13
Cat_016_1_0 : TypeAlias = Asterix_14
Cat_017_1_3 : TypeAlias = Asterix_15
Cat_018_1_7 : TypeAlias = Asterix_16
Cat_019_1_3 : TypeAlias = Asterix_17
Cat_020_1_9 : TypeAlias = Asterix_18
Cat_020_1_10 : TypeAlias = Asterix_19
Cat_021_0_23 : TypeAlias = Asterix_20
Cat_021_0_24 : TypeAlias = Asterix_21
Cat_021_0_25 : TypeAlias = Asterix_22
Cat_021_0_26 : TypeAlias = Asterix_23
Ref_021_1_4 : TypeAlias = Asterix_24
Ref_021_1_5 : TypeAlias = Asterix_25
Cat_021_2_1 : TypeAlias = Asterix_26
Cat_021_2_2 : TypeAlias = Asterix_27
Cat_021_2_3 : TypeAlias = Asterix_28
Cat_021_2_4 : TypeAlias = Asterix_29
Cat_021_2_5 : TypeAlias = Asterix_30
Cat_021_2_6 : TypeAlias = Asterix_31
Cat_023_1_2 : TypeAlias = Asterix_32
Cat_023_1_3 : TypeAlias = Asterix_33
Cat_025_1_5 : TypeAlias = Asterix_34
Cat_032_1_1 : TypeAlias = Asterix_35
Cat_034_1_27 : TypeAlias = Asterix_36
Cat_034_1_28 : TypeAlias = Asterix_37
Cat_034_1_29 : TypeAlias = Asterix_38
Ref_048_1_11 : TypeAlias = Asterix_39
Cat_048_1_27 : TypeAlias = Asterix_40
Cat_048_1_28 : TypeAlias = Asterix_41
Cat_048_1_29 : TypeAlias = Asterix_42
Cat_048_1_30 : TypeAlias = Asterix_43
Cat_048_1_31 : TypeAlias = Asterix_44
Ref_062_1_2 : TypeAlias = Asterix_45
Cat_062_1_17 : TypeAlias = Asterix_46
Cat_062_1_18 : TypeAlias = Asterix_47
Cat_062_1_19 : TypeAlias = Asterix_48
Cat_062_1_20 : TypeAlias = Asterix_49
Cat_063_1_6 : TypeAlias = Asterix_50
Cat_065_1_4 : TypeAlias = Asterix_51
Cat_065_1_5 : TypeAlias = Asterix_52
Cat_065_1_6 : TypeAlias = Asterix_53
Cat_205_1_0 : TypeAlias = Asterix_54
Cat_240_1_3 : TypeAlias = Asterix_55
Cat_247_1_2 : TypeAlias = Asterix_56
Cat_247_1_3 : TypeAlias = Asterix_57

# Manifest

manifest = {
    'CATS': {
        1: {
            '1.2': Cat_001_1_2,
            '1.3': Cat_001_1_3,
            '1.4': Cat_001_1_4,
        },
        2: {
            '1.0': Cat_002_1_0,
            '1.1': Cat_002_1_1,
        },
        4: {
            '1.12': Cat_004_1_12,
        },
        8: {
            '1.2': Cat_008_1_2,
            '1.3': Cat_008_1_3,
        },
        9: {
            '2.1': Cat_009_2_1,
        },
        10: {
            '1.1': Cat_010_1_1,
        },
        11: {
            '1.2': Cat_011_1_2,
            '1.3': Cat_011_1_3,
        },
        15: {
            '1.0': Cat_015_1_0,
            '1.1': Cat_015_1_1,
        },
        16: {
            '1.0': Cat_016_1_0,
        },
        17: {
            '1.3': Cat_017_1_3,
        },
        18: {
            '1.7': Cat_018_1_7,
        },
        19: {
            '1.3': Cat_019_1_3,
        },
        20: {
            '1.9': Cat_020_1_9,
            '1.10': Cat_020_1_10,
        },
        21: {
            '0.23': Cat_021_0_23,
            '0.24': Cat_021_0_24,
            '0.25': Cat_021_0_25,
            '0.26': Cat_021_0_26,
            '2.1': Cat_021_2_1,
            '2.2': Cat_021_2_2,
            '2.3': Cat_021_2_3,
            '2.4': Cat_021_2_4,
            '2.5': Cat_021_2_5,
            '2.6': Cat_021_2_6,
        },
        23: {
            '1.2': Cat_023_1_2,
            '1.3': Cat_023_1_3,
        },
        25: {
            '1.5': Cat_025_1_5,
        },
        32: {
            '1.1': Cat_032_1_1,
        },
        34: {
            '1.27': Cat_034_1_27,
            '1.28': Cat_034_1_28,
            '1.29': Cat_034_1_29,
        },
        48: {
            '1.27': Cat_048_1_27,
            '1.28': Cat_048_1_28,
            '1.29': Cat_048_1_29,
            '1.30': Cat_048_1_30,
            '1.31': Cat_048_1_31,
        },
        62: {
            '1.17': Cat_062_1_17,
            '1.18': Cat_062_1_18,
            '1.19': Cat_062_1_19,
            '1.20': Cat_062_1_20,
        },
        63: {
            '1.6': Cat_063_1_6,
        },
        65: {
            '1.4': Cat_065_1_4,
            '1.5': Cat_065_1_5,
            '1.6': Cat_065_1_6,
        },
        205: {
            '1.0': Cat_205_1_0,
        },
        240: {
            '1.3': Cat_240_1_3,
        },
        247: {
            '1.2': Cat_247_1_2,
            '1.3': Cat_247_1_3,
        },
    },
    'REFS': {
        21: {
            '1.4': Ref_021_1_4,
            '1.5': Ref_021_1_5,
        },
        48: {
            '1.11': Ref_048_1_11,
        },
        62: {
            '1.2': Ref_062_1_2,
        },
    },
}
