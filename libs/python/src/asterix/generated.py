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
    values = { 0: "0 X-pulse set to zero or no Mode 2 reply", 1: "X-pulse set to one (present)" }
class Content_2(ContentTable):
    values = { 0: "0°", 1: "22.5°", 2: "45°", 3: "67.5°", 4: "90°", 5: "112.5°", 6: "135°", 7: "157.5°" }
class Content_3(ContentTable):
    values = { 0: "100 ft resolution", 1: "25 ft resolution" }
class Content_4(ContentTable):
    values = { 0: "24-Bit ICAO address", 1: "Duplicate address", 2: "Surface vehicle address", 3: "Anonymous address", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use" }
class Content_5(ContentTable):
    values = { 0: "25 ft", 1: "100 ft", 2: "Unknown", 3: "Invalid" }
class Content_6(ContentTable):
    values = { 0: ">= 70 Watts", 1: "< 70 Watts" }
class Content_7(ContentTable):
    values = { 0: "ADS GO", 1: "ADS NOGO" }
class Content_8(ContentTable):
    values = { 0: "ADSB not populated", 1: "ADSB populated" }
class Content_9(ContentTable):
    values = { 0: "Absence of SPI", 1: "Special Position Identification" }
class Content_10(ContentTable):
    values = { 0: "Accepted, the request is accepted and is under processing", 1: "Rejected, the request has not been accepted", 2: "Cancelled, the request has been cancelled", 3: "Finished, the request has been accepted and successfully processed", 4: "Delayed, the request processing is temporarily delayed but the request is still valid", 5: "In Progress, the request is being successfully processed", 6: "In Progress, the request is being successfully processed" }
class Content_11(ContentTable):
    values = { 0: "Active CAS (TCAS II) or no CAS", 1: "Active CAS (not TCAS II)", 2: "Active CAS (not TCAS II) with OCM transmit capability", 3: "Active CAS of Junior Status", 4: "Passive CAS with 1030TCAS Resolution Message receive capability", 5: "Passive CAS with only OCM receive capability", 6: "Reserved for future use", 7: "Reserved for future use" }
class Content_12(ContentTable):
    values = { 0: "Actual Target Report", 1: "Reference Target", 2: "Synthetic Target", 3: "Simulated / Replayed Target" }
class Content_13(ContentTable):
    values = { 0: "Actual Track", 1: "Simulated track" }
class Content_14(ContentTable):
    values = { 0: "Actual plot or track", 1: "Simulated plot or track" }
class Content_15(ContentTable):
    values = { 0: "Actual target report", 1: "Simulated target report" }
class Content_16(ContentTable):
    values = { 0: "Actual track", 1: "Simulated track" }
class Content_17(ContentTable):
    values = { 0: "Air Speed = IAS, LSB (Bit-1) = 2 -14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001" }
class Content_18(ContentTable):
    values = { 0: "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001" }
class Content_19(ContentTable):
    values = { 0: "Aircraft are not diverging at starting time of conflict", 1: "Aircraft are diverging at starting time of conflict" }
class Content_20(ContentTable):
    values = { 0: "Aircraft are not fast diverging laterally at current time", 1: "Aircraft are fast diverging laterally at current time" }
class Content_21(ContentTable):
    values = { 0: "Aircraft are not fast diverging vertically at current time", 1: "Aircraft are fast diverging vertically at current time" }
class Content_22(ContentTable):
    values = { 0: "Aircraft has not stopped", 1: "Aircraft has stopped" }
class Content_23(ContentTable):
    values = { 0: "Aircraft have not crossed at starting time of conflict", 1: "Aircraft have crossed at starting time of conflict" }
class Content_24(ContentTable):
    values = { 0: "Aircraft is airborne", 1: "Aircraft is on the ground" }
class Content_25(ContentTable):
    values = { 0: "Aircraft not receiving ATC-services", 1: "Aircraft receiving ATC services" }
class Content_26(ContentTable):
    values = { 0: "Alert acknowledged", 1: "Alert not acknowledged" }
class Content_27(ContentTable):
    values = { 0: "Altitude Hold not engaged", 1: "Altitude Hold engaged" }
class Content_28(ContentTable):
    values = { 0: "Antenna 1", 1: "Antenna 2" }
class Content_29(ContentTable):
    values = { 0: "Antenna Diversity", 1: "Single Antenna only" }
class Content_30(ContentTable):
    values = { 0: "Approach Mode not active", 1: "Approach Mode active" }
class Content_31(ContentTable):
    values = { 0: "Associate_req", 1: "Associate_resp", 2: "Release_req", 3: "Release_resp", 4: "Abort_req", 5: "Keep_alive", 16: "Aircraft_report", 17: "Aircraft_command", 18: "II_code_change", 32: "Uplink_packet", 33: "Cancel_uplink_packet", 34: "Uplink_packet_ack", 35: "Downlink_packet", 38: "Data_XON", 39: "Data_XOFF", 48: "Uplink_broadcast", 49: "Cancel_uplink_broadcast", 50: "Uplink_broadcast_ack", 52: "Downlink_broadcast", 64: "GICB_extraction", 65: "Cancel_GICB_extraction", 66: "GICB_extraction_ack", 67: "GICB_response" }
class Content_32(ContentTable):
    values = { 0: "Associated Plot does not contain a PSR component", 1: "Associated Plot contains at least a PSR component" }
class Content_33(ContentTable):
    values = { 0: "Associated Plot does not contain a Roll Call component", 1: "Associated Plot contains at least a Roll Call component" }
class Content_34(ContentTable):
    values = { 0: "Associated Plot does not contain an All Call component", 1: "Associated Plot contains at least an All Call component" }
class Content_35(ContentTable):
    values = { 0: "Associated Plot does not contain an SSR component", 1: "Associated Plot contains at least an SSR component" }
class Content_36(ContentTable):
    values = { 0: "Autonomous", 1: "Not autonomous" }
class Content_37(ContentTable):
    values = { 0: "Autopilot not engaged", 1: "Autopilot engaged" }
class Content_38(ContentTable):
    values = { 0: "Available", 1: "Not available", 2: "Unknown" }
class Content_39(ContentTable):
    values = { 0: "Available", 1: "Not available", 2: "Unknown", 3: "Invalid" }
class Content_40(ContentTable):
    values = { 0: "Barometric altitude (Mode C) more reliable", 1: "Geometric altitude more reliable" }
class Content_41(ContentTable):
    values = { 0: "CDTI not operational", 1: "CDTI operational" }
class Content_42(ContentTable):
    values = { 0: "CPR Validation correct", 1: "CPR Validation failed" }
class Content_43(ContentTable):
    values = { 0: "Callsign or registration downlinked from target", 1: "Callsign not downlinked from target", 2: "Registration not downlinked from target", 3: "Invalid" }
class Content_44(ContentTable):
    values = { 0: "Callsign or registration downlinked from transponder", 1: "Callsign not downlinked from transponder", 2: "Registration not downlinked from transponder" }
class Content_45(ContentTable):
    values = { 0: "Callsign or registration not downlinked from transponder", 1: "Registration downlinked from transponder", 2: "Callsign downlinked from transponder", 3: "Not defined" }
class Content_46(ContentTable):
    values = { 0: "Chain 1", 1: "Chain 2" }
class Content_47(ContentTable):
    values = { 0: "Channel A in use", 1: "Channel B in use" }
class Content_48(ContentTable):
    values = { 0: "Code not validated", 1: "Code validated" }
class Content_49(ContentTable):
    values = { 0: "Code validated", 1: "Code not validated" }
class Content_50(ContentTable):
    values = { 0: "Combined", 1: "Co-operative only", 2: "Non-Cooperative only", 3: "Not defined" }
class Content_51(ContentTable):
    values = { 0: "Combined Track", 1: "PSR Track", 2: "SSR/Mode S Track", 3: "Invalid" }
class Content_52(ContentTable):
    values = { 0: "Complementary service used", 1: "Background service used" }
class Content_53(ContentTable):
    values = { 0: "Confirmed Track", 1: "Tentative Track" }
class Content_54(ContentTable):
    values = { 0: "Confirmed Track", 1: "Track in initialisation phase" }
class Content_55(ContentTable):
    values = { 0: "Confirmed track", 1: "Tentative track" }
class Content_56(ContentTable):
    values = { 0: "Confirmed track", 1: "Track in initialisation phase" }
class Content_57(ContentTable):
    values = { 0: "Confirmed track", 1: "Track in initiation phase" }
class Content_58(ContentTable):
    values = { 0: "Conflict not predicted to occur in civil airspace", 1: "Conflict predicted to occur in civil airspace" }
class Content_59(ContentTable):
    values = { 0: "Conflict not predicted to occur in military airspace", 1: "Conflict predicted to occur in military airspace" }
class Content_60(ContentTable):
    values = { 0: "Constant course", 1: "Right turn", 2: "Left turn", 3: "Undetermined" }
class Content_61(ContentTable):
    values = { 0: "Constant groundspeed", 1: "Increasing groundspeed", 2: "Decreasing groundspeed", 3: "Undetermined" }
class Content_62(ContentTable):
    values = { 0: "Counter for antenna 1", 1: "Counter for antenna 2" }
class Content_63(ContentTable):
    values = { 0: "DC shall be ignored", 1: "DC shall be taken into account" }
class Content_64(ContentTable):
    values = { 0: "DME/TACAN multilateration", 1: "No DME/TACAN multilateration" }
class Content_65(ContentTable):
    values = { 0: "DRN not available", 1: "DRN available" }
class Content_66(ContentTable):
    values = { 0: "Data is either unavailable or invalid", 1: "Data is available and valid" }
class Content_67(ContentTable):
    values = { 0: "Data is released for operational use", 1: "Data must not be used operationally" }
class Content_68(ContentTable):
    values = { 0: "Data used by Tracker", 1: "Data not used by Tracker", 2: "2-127: Reserved for future use" }
class Content_69(ContentTable):
    values = { 0: "Default", 1: "ACAS RA function" }
class Content_70(ContentTable):
    values = { 0: "Default", 1: "AIW function" }
class Content_71(ContentTable):
    values = { 0: "Default", 1: "APM function" }
class Content_72(ContentTable):
    values = { 0: "Default", 1: "APW function" }
class Content_73(ContentTable):
    values = { 0: "Default", 1: "ASM sub-function" }
class Content_74(ContentTable):
    values = { 0: "Default", 1: "Aircraft manoeuvring" }
class Content_75(ContentTable):
    values = { 0: "Default", 1: "CATC function" }
class Content_76(ContentTable):
    values = { 0: "Default", 1: "CHAM function" }
class Content_77(ContentTable):
    values = { 0: "Default", 1: "CLAM function" }
class Content_78(ContentTable):
    values = { 0: "Default", 1: "CRA function" }
class Content_79(ContentTable):
    values = { 0: "Default", 1: "CUW function" }
class Content_80(ContentTable):
    values = { 0: "Default", 1: "DBPSM ARR sub-function" }
class Content_81(ContentTable):
    values = { 0: "Default", 1: "DBPSM DEP sub-function" }
class Content_82(ContentTable):
    values = { 0: "Default", 1: "DBPSM TL sub-function" }
class Content_83(ContentTable):
    values = { 0: "Default", 1: "DSAM function" }
class Content_84(ContentTable):
    values = { 0: "Default", 1: "Doubtful plot to track association" }
class Content_85(ContentTable):
    values = { 0: "Default", 1: "Error condition encountered" }
class Content_86(ContentTable):
    values = { 0: "Default", 1: "FTD Function" }
class Content_87(ContentTable):
    values = { 0: "Default", 1: "Garbled code" }
class Content_88(ContentTable):
    values = { 0: "Default", 1: "Garbled code / Error correction applied" }
class Content_89(ContentTable):
    values = { 0: "Default", 1: "Ghost track" }
class Content_90(ContentTable):
    values = { 0: "Default", 1: "Ground Vehicle" }
class Content_91(ContentTable):
    values = { 0: "Default", 1: "HAM HD sub-function" }
class Content_92(ContentTable):
    values = { 0: "Default", 1: "HAM RD sub-function" }
class Content_93(ContentTable):
    values = { 0: "Default", 1: "HAM VD sub-function" }
class Content_94(ContentTable):
    values = { 0: "Default", 1: "HVI function" }
class Content_95(ContentTable):
    values = { 0: "Default", 1: "Horizontal manoeuvre" }
class Content_96(ContentTable):
    values = { 0: "Default", 1: "IAVM sub-function" }
class Content_97(ContentTable):
    values = { 0: "Default", 1: "IIA function" }
class Content_98(ContentTable):
    values = { 0: "Default", 1: "ITD function" }
class Content_99(ContentTable):
    values = { 0: "Default", 1: "In Trouble" }
class Content_100(ContentTable):
    values = { 0: "Default", 1: "Invalid Time Source" }
class Content_101(ContentTable):
    values = { 0: "Default", 1: "LTW function" }
class Content_102(ContentTable):
    values = { 0: "Default", 1: "Last report for a track" }
class Content_103(ContentTable):
    values = { 0: "Default", 1: "List Lookup failed (see note)" }
class Content_104(ContentTable):
    values = { 0: "Default", 1: "MRVA function" }
class Content_105(ContentTable):
    values = { 0: "Default", 1: "MSAW function" }
class Content_106(ContentTable):
    values = { 0: "Default", 1: "Military emergency" }
class Content_107(ContentTable):
    values = { 0: "Default", 1: "Military identification" }
class Content_108(ContentTable):
    values = { 0: "Default", 1: "Msg Type 20 (DBPSM) indicates ARR" }
class Content_109(ContentTable):
    values = { 0: "Default", 1: "Msg Type 20 (DBPSM) indicates DEP" }
class Content_110(ContentTable):
    values = { 0: "Default", 1: "Msg Type 20 (DBPSM) indicates above TL" }
class Content_111(ContentTable):
    values = { 0: "Default", 1: "Msg Type 25 (VRAM) indicates CRM" }
class Content_112(ContentTable):
    values = { 0: "Default", 1: "Msg Type 25 (VRAM) indicates VRM" }
class Content_113(ContentTable):
    values = { 0: "Default", 1: "Msg Type 25 (VRAM) indicates VTM" }
class Content_114(ContentTable):
    values = { 0: "Default", 1: "Msg Type 29 (HAM) indicates HD" }
class Content_115(ContentTable):
    values = { 0: "Default", 1: "Msg Type 29 (HAM) indicates RD" }
class Content_116(ContentTable):
    values = { 0: "Default", 1: "Msg Type 29 (HAM) indicates VD" }
class Content_117(ContentTable):
    values = { 0: "Default", 1: "Msg Type 4 (MSAW) indicates MRVA" }
class Content_118(ContentTable):
    values = { 0: "Default", 1: "Msg Type 99 (AIW) indicates pAIW Alert" }
class Content_119(ContentTable):
    values = { 0: "Default", 1: "NOCLR sub-function" }
class Content_120(ContentTable):
    values = { 0: "Default", 1: "NOH function" }
class Content_121(ContentTable):
    values = { 0: "Default", 1: "NOMOV Function" }
class Content_122(ContentTable):
    values = { 0: "Default", 1: "NTCA function" }
class Content_123(ContentTable):
    values = { 0: "Default", 1: "OCAT function" }
class Content_124(ContentTable):
    values = { 0: "Default", 1: "ONGOING function" }
class Content_125(ContentTable):
    values = { 0: "Default", 1: "Overflow error" }
class Content_126(ContentTable):
    values = { 0: "Default", 1: "Overload" }
class Content_127(ContentTable):
    values = { 0: "Default", 1: "Overload error" }
class Content_128(ContentTable):
    values = { 0: "Default", 1: "PAIW function" }
class Content_129(ContentTable):
    values = { 0: "Default", 1: "Plot or track from a fixed transponder" }
class Content_130(ContentTable):
    values = { 0: "Default", 1: "RAMHD function" }
class Content_131(ContentTable):
    values = { 0: "Default", 1: "RAMLD function" }
class Content_132(ContentTable):
    values = { 0: "Default", 1: "RIMCA function" }
class Content_133(ContentTable):
    values = { 0: "Default", 1: "Range Check failed" }
class Content_134(ContentTable):
    values = { 0: "Default", 1: "Range Check passed, CPR Validation pending" }
class Content_135(ContentTable):
    values = { 0: "Default", 1: "Runway/Runway Crossing" }
class Content_136(ContentTable):
    values = { 0: "Default", 1: "Runway/Taxiway Crossing" }
class Content_137(ContentTable):
    values = { 0: "Default", 1: "SAM function" }
class Content_138(ContentTable):
    values = { 0: "Default", 1: "SQW function" }
class Content_139(ContentTable):
    values = { 0: "Default", 1: "STCA function" }
class Content_140(ContentTable):
    values = { 0: "Default", 1: "STOCC function" }
class Content_141(ContentTable):
    values = { 0: "Default", 1: "Special Position Identification" }
class Content_142(ContentTable):
    values = { 0: "Default", 1: "System degraded" }
class Content_143(ContentTable):
    values = { 0: "Default", 1: "TTA function" }
class Content_144(ContentTable):
    values = { 0: "Default", 1: "Test Target" }
class Content_145(ContentTable):
    values = { 0: "Default", 1: "Test target indicator" }
class Content_146(ContentTable):
    values = { 0: "Default", 1: "Test vector" }
class Content_147(ContentTable):
    values = { 0: "Default", 1: "Track numbering has restarted" }
class Content_148(ContentTable):
    values = { 0: "Default", 1: "Unlawful interference (code 7500)", 2: "Radio-communication failure (code 7600)", 3: "Emergency (code 7700)" }
class Content_149(ContentTable):
    values = { 0: "Default", 1: "VCD function" }
class Content_150(ContentTable):
    values = { 0: "Default", 1: "VPM function" }
class Content_151(ContentTable):
    values = { 0: "Default", 1: "VRAM CRM sub-function" }
class Content_152(ContentTable):
    values = { 0: "Default", 1: "VRAM VRM sub-function" }
class Content_153(ContentTable):
    values = { 0: "Default", 1: "VRAM VTM sub-function" }
class Content_154(ContentTable):
    values = { 0: "Default", 1: "WRTY function" }
class Content_155(ContentTable):
    values = { 0: "Default", 1: "X-pulse received in Mode-2 reply" }
class Content_156(ContentTable):
    values = { 0: "Default", 1: "X-pulse received in Mode-3/A reply" }
class Content_157(ContentTable):
    values = { 0: "Default", 1: "X-pulse received in Mode-C reply" }
class Content_158(ContentTable):
    values = { 0: "Default (no meaning)", 1: "No plots being received" }
class Content_159(ContentTable):
    values = { 0: "Default (see note)", 1: "Independent Position Check failed" }
class Content_160(ContentTable):
    values = { 0: "Default situation", 1: "Reset of RDPC" }
class Content_161(ContentTable):
    values = { 0: "Default value", 1: "ADS-B data inconsistent with other surveillance information" }
class Content_162(ContentTable):
    values = { 0: "Default value", 1: "Age of the last received ADS track update is higher than system dependent threshold" }
class Content_163(ContentTable):
    values = { 0: "Default value", 1: "Age of the last received ADS-B track update is higher than system dependent threshold" }
class Content_164(ContentTable):
    values = { 0: "Default value", 1: "Age of the last received MLAT track updateis higher than system dependent threshold" }
class Content_165(ContentTable):
    values = { 0: "Default value", 1: "Age of the last received Mode S track update is higher than system dependent threshold" }
class Content_166(ContentTable):
    values = { 0: "Default value", 1: "Age of the last received PSR track update is higher than system dependent threshold" }
class Content_167(ContentTable):
    values = { 0: "Default value", 1: "Age of the last received SSR track update is higher than system dependent threshold" }
class Content_168(ContentTable):
    values = { 0: "Default value", 1: "Age of the last received track update is higher than system dependent threshold (coasting)" }
class Content_169(ContentTable):
    values = { 0: "Default value", 1: "Assigned Mode A Code Conflict (same discrete Mode A Code assigned to another track)" }
class Content_170(ContentTable):
    values = { 0: "Default value", 1: "Assigned Mode A Code Conflict (same individual Mode A Code assigned to another track)" }
class Content_171(ContentTable):
    values = { 0: "Default value", 1: "Duplicate Flight Plan" }
class Content_172(ContentTable):
    values = { 0: "Default value", 1: "Duplicate Flight Plan due to manual correlation" }
class Content_173(ContentTable):
    values = { 0: "Default value", 1: "Duplicate Mode 3/A Code" }
class Content_174(ContentTable):
    values = { 0: "Default value", 1: "First message transmitted to the user for the track" }
class Content_175(ContentTable):
    values = { 0: "Default value", 1: "Inconsistent Emergency Code" }
class Content_176(ContentTable):
    values = { 0: "Default value", 1: "Last message transmitted to the user for the track" }
class Content_177(ContentTable):
    values = { 0: "Default value", 1: "Military Emergency present in the last report received from a sensor capable of decoding this data" }
class Content_178(ContentTable):
    values = { 0: "Default value", 1: "Military Identification present in the last report received from a sensor capable of decoding this data" }
class Content_179(ContentTable):
    values = { 0: "Default value", 1: "SPI present in the last report received from a sensor capable of decoding this data" }
class Content_180(ContentTable):
    values = { 0: "Default value", 1: "Slave Track Promotion" }
class Content_181(ContentTable):
    values = { 0: "Default value", 1: "Special Used Code (Mode A codes to be defined in the system to mark a track with special interest)" }
class Content_182(ContentTable):
    values = { 0: "Default value", 1: "Surface target" }
class Content_183(ContentTable):
    values = { 0: "Default value", 1: "Track created / updated with FPL data" }
class Content_184(ContentTable):
    values = { 0: "Default value", 1: "Track service begin (i.e. first message transmitted to the user for the track)" }
class Content_185(ContentTable):
    values = { 0: "Default value", 1: "Track service end (i.e. last message transmitted to the user for the track)" }
class Content_186(ContentTable):
    values = { 0: "Default, no overload", 1: "Overload in DP" }
class Content_187(ContentTable):
    values = { 0: "Default, no overload", 1: "Overload in RDP" }
class Content_188(ContentTable):
    values = { 0: "Default, no overload", 1: "Overload in transmission subsystem" }
class Content_189(ContentTable):
    values = { 0: "Doppler speed is valid", 1: "Doppler speed is doubtful" }
class Content_190(ContentTable):
    values = { 0: "ED102/DO-260 [Ref. 7]", 1: "DO-260A [Ref. 8]", 2: "ED102A/DO-260B [Ref. 10]", 3: "ED-102B/DO-260C [Ref. 11]" }
class Content_191(ContentTable):
    values = { 0: "ED102/DO-260 [Ref. 8]", 1: "DO-260A [Ref. 9]", 2: "ED102A/DO-260B [Ref. 11]" }
class Content_192(ContentTable):
    values = { 0: "Element Not Populated", 1: "Element Populated" }
class Content_193(ContentTable):
    values = { 0: "Element not populated", 1: "Element populated" }
class Content_194(ContentTable):
    values = { 0: "Empty", 1: "Occupied", 2: "Unknown" }
class Content_195(ContentTable):
    values = { 0: "Empty", 1: "Occupied", 2: "Unknown", 3: "Invalid" }
class Content_196(ContentTable):
    values = { 0: "End fo alert", 1: "Pre-alarm", 2: "Severe alert" }
class Content_197(ContentTable):
    values = { 0: "End of Data Item", 1: "Military Identification present in the last report received from a sensor capable of decoding this data" }
class Content_198(ContentTable):
    values = { 0: "Equipment capable to provide Selected Altitude", 1: "Equipment not capable to provide Selected Altitude" }
class Content_199(ContentTable):
    values = { 0: "Faulted", 1: "Good" }
class Content_200(ContentTable):
    values = { 0: "Flight plan data from active FDPS", 1: "Flight plan data retained from no longer active FDPS" }
class Content_201(ContentTable):
    values = { 0: "Flightlevel not present or not from Mode 5 reply/report", 1: "Flightlevel from Mode 5 reply/report" }
class Content_202(ContentTable):
    values = { 0: "Flyco (follow me)", 1: "ATC equipment maintenance", 2: "Airport maintenance", 3: "Fire", 4: "Bird scarer", 5: "Snow plough", 6: "Runway sweeper", 7: "Emergency", 8: "Police", 9: "Bus", 10: "Tug (push/tow)", 11: "Grass cutter", 12: "Fuel", 13: "Baggage", 14: "Catering", 15: "Aircraft maintenance", 16: "Unknown" }
class Content_203(ContentTable):
    values = { 0: "From UTC midnight", 1: "From the previous report" }
class Content_204(ContentTable):
    values = { 0: "From midnight", 1: "From the last report" }
class Content_205(ContentTable):
    values = { 0: "From previous scan", 1: "New in current scan", 2: "Requested in the beam by transponder", 3: "Invalid ASTERIX value" }
class Content_206(ContentTable):
    values = { 0: "GA reported in 100 ft increments", 1: "GA reported in 25 ft increments" }
class Content_207(ContentTable):
    values = { 0: "GICB extractions should be sent only when required by the periodicity", 1: "If a GICB extraction is done due to external conditions, an update will also be sent, even if it does not match the expected periodicity" }
class Content_208(ContentTable):
    values = { 0: "Ground Bit not set", 1: "Ground Bit set" }
class Content_209(ContentTable):
    values = { 0: "HF multilateration", 1: "No HF multilateration" }
class Content_210(ContentTable):
    values = { 0: "Heading data provided", 1: "Ground Track provided" }
class Content_211(ContentTable):
    values = { 0: "Heading/Ground Track data is not valid", 1: "Heading/Ground Track data is valid" }
class Content_212(ContentTable):
    values = { 0: "High quality pulse A1", 1: "Low quality pulse A1" }
class Content_213(ContentTable):
    values = { 0: "High quality pulse A2", 1: "Low quality pulse A2" }
class Content_214(ContentTable):
    values = { 0: "High quality pulse A4", 1: "Low quality pulse A4" }
class Content_215(ContentTable):
    values = { 0: "High quality pulse B1", 1: "Low quality pulse B1" }
class Content_216(ContentTable):
    values = { 0: "High quality pulse B2", 1: "Low quality pulse B2" }
class Content_217(ContentTable):
    values = { 0: "High quality pulse B4", 1: "Low quality pulse B4" }
class Content_218(ContentTable):
    values = { 0: "High quality pulse C1", 1: "Low quality pulse C1" }
class Content_219(ContentTable):
    values = { 0: "High quality pulse C2", 1: "Low quality pulse C2" }
class Content_220(ContentTable):
    values = { 0: "High quality pulse C4", 1: "Low quality pulse C4" }
class Content_221(ContentTable):
    values = { 0: "High quality pulse D1", 1: "Low quality pulse D1" }
class Content_222(ContentTable):
    values = { 0: "High quality pulse D2", 1: "Low quality pulse D2" }
class Content_223(ContentTable):
    values = { 0: "High quality pulse D4", 1: "Low quality pulse D4" }
class Content_224(ContentTable):
    values = { 0: "IDENT switch not active", 1: "IDENT switch active" }
class Content_225(ContentTable):
    values = { 0: "Identity not requested", 1: "Identity requested" }
class Content_226(ContentTable):
    values = { 0: "In progress", 1: "Completed", 2: "Cancelled", 3: "Invalid ASTERIX value" }
class Content_227(ContentTable):
    values = { 0: "Indicator on", 1: "Indicator off" }
class Content_228(ContentTable):
    values = { 0: "Instrument Flight Rules", 1: "Visual Flight Rules", 2: "Not applicable", 3: "Controlled Visual Flight Rules" }
class Content_229(ContentTable):
    values = { 0: "Instrument Flight Rules", 1: "Visual Flight rules", 2: "Not applicable", 3: "Controlled Visual Flight Rules" }
class Content_230(ContentTable):
    values = { 0: "Intermediate record of a contour", 1: "Last record of a contour of at least two records", 2: "First record of a contour of at least two records", 3: "First and only record, fully defining a contour" }
class Content_231(ContentTable):
    values = { 0: "Invalid value", 1: "Filter for Weather data", 2: "Filter for Jamming Strobe", 3: "Filter for PSR data", 4: "Filter for SSR/Mode S data", 5: "Filter for SSR/Mode S + PSR data", 6: "Enhanced Surveillance data", 7: "Filter for PSR+Enhanced Surveillance data", 8: "Filter for PSR+Enhanced Surveillance + SSR/Mode S data not in Area of Prime Interest", 9: "Filter for PSR+Enhanced Surveillance + all SSR/Mode S data" }
class Content_232(ContentTable):
    values = { 0: "LDPJ not detected", 1: "LDPJ detected" }
class Content_233(ContentTable):
    values = { 0: "LNAV Mode engaged", 1: "LNAV Mode not engaged" }
class Content_234(ContentTable):
    values = { 0: "LOW", 1: "HIGH" }
class Content_235(ContentTable):
    values = { 0: "Last Measured Flight Level", 1: "Predicted Flight Level" }
class Content_236(ContentTable):
    values = { 0: "Level", 1: "Climb", 2: "Descent", 3: "Undetermined" }
class Content_237(ContentTable):
    values = { 0: "Linear polarization", 1: "Circular polarization" }
class Content_238(ContentTable):
    values = { 0: "Local Coordinates", 1: "System Coordinates" }
class Content_239(ContentTable):
    values = { 0: "MCP/FCU Mode Bits not populated", 1: "MCP/FCU Mode Bits populated" }
class Content_240(ContentTable):
    values = { 0: "MDS GO", 1: "MDS NOGO" }
class Content_241(ContentTable):
    values = { 0: "MLT GO", 1: "MLT NOGO" }
class Content_242(ContentTable):
    values = { 0: "Maintaining", 1: "Climbing", 2: "Descending", 3: "Invalid" }
class Content_243(ContentTable):
    values = { 0: "Maintaining", 1: "Climbing", 2: "Descending", 3: "Unknown" }
class Content_244(ContentTable):
    values = { 0: "Manned Operation", 1: "Unmanned Operation" }
class Content_245(ContentTable):
    values = { 0: "Measured per flight-hour", 1: "Measured per sample" }
class Content_246(ContentTable):
    values = { 0: "Measured position", 1: "No measured position (coasted)" }
class Content_247(ContentTable):
    values = { 0: "Measured position", 1: "Smoothed position" }
class Content_248(ContentTable):
    values = { 0: "Merge or split indication undetermined", 1: "Track merged by association to plot", 2: "Track merged by non-association to plot", 3: "Split track" }
class Content_249(ContentTable):
    values = { 0: "Minor separation infringement", 1: "Major separation infringement" }
class Content_250(ContentTable):
    values = { 0: "Mode 1 Code derived from the reply of the transponder", 1: "Mode 1 Code not extracted during the last scan" }
class Content_251(ContentTable):
    values = { 0: "Mode 1 code as derived from the report of the transponder", 1: "Smoothed Mode 1 code as provided by a local tracker" }
class Content_252(ContentTable):
    values = { 0: "Mode 1 code not present or not from Mode 5 reply", 1: "Mode 1 code from Mode 5 reply" }
class Content_253(ContentTable):
    values = { 0: "Mode 1 code not present or not from Mode 5 reply/report", 1: "Mode 1 code from Mode 5 reply/report" }
class Content_254(ContentTable):
    values = { 0: "Mode 2 code not present or not from Mode 5 reply", 1: "Mode 2 code from Mode 5 reply" }
class Content_255(ContentTable):
    values = { 0: "Mode 2 code not present or not from Mode 5 reply/report", 1: "Mode 2 code from Mode 5 reply/report" }
class Content_256(ContentTable):
    values = { 0: "Mode 3 code not present or not from Mode 5 reply", 1: "Mode 3 code from Mode 5 reply" }
class Content_257(ContentTable):
    values = { 0: "Mode 3 code not present or not from Mode 5 reply/report", 1: "Mode 3 code from Mode 5 reply/report" }
class Content_258(ContentTable):
    values = { 0: "Mode 3/A code as derived from the reply of the transponder", 1: "Mode 3/A code as provided by a sensor local tracker" }
class Content_259(ContentTable):
    values = { 0: "Mode C altitude code not present or not from Mode 5 reply", 1: "Mode C altitude from Mode 5 reply" }
class Content_260(ContentTable):
    values = { 0: "Mode C altitude not present or not from Mode 5 reply/report", 1: "Mode C altitude from Mode 5 reply/report" }
class Content_261(ContentTable):
    values = { 0: "Mode S available", 1: "Mode S not available" }
class Content_262(ContentTable):
    values = { 0: "Mode-1 code as derived from the reply of the transponder", 1: "Smoothed Mode-1 code as provided by a local tracker" }
class Content_263(ContentTable):
    values = { 0: "Mode-1 code derived from the reply of the transponder", 1: "Smoothed Mode-1 code as provided by a local tracker" }
class Content_264(ContentTable):
    values = { 0: "Mode-2 code as derived from the reply of the transponder", 1: "Smoothed Mode-2 code as provided by a local tracker" }
class Content_265(ContentTable):
    values = { 0: "Mode-2 code derived from the reply of the transponder", 1: "Smoothed Mode-2 code as provided by a local tracker n" }
class Content_266(ContentTable):
    values = { 0: "Mode-3/A code derived during last update", 1: "Mode-3/A code not extracted during the last update" }
class Content_267(ContentTable):
    values = { 0: "Mode-3/A code derived from the reply of the transponder", 1: "Mode-3/A code not extracted during the last scan" }
class Content_268(ContentTable):
    values = { 0: "Mode-3/A code derived from the reply of the transponder", 1: "Mode-3/A code not extracted during the last update period" }
class Content_269(ContentTable):
    values = { 0: "Mode-3/A code derived from the reply of the transponder", 1: "Smoothed Mode-3/A code as provided by a local tracker" }
class Content_270(ContentTable):
    values = { 0: "Mode-3/A code derived from the reply of the transponder", 1: "Smoothed Mode-3/A code not extracted during the last scan" }
class Content_271(ContentTable):
    values = { 0: "Mode-S 1090 MHz multilateration", 1: "No Mode-S 1090 MHz multilateration" }
class Content_272(ContentTable):
    values = { 0: "Monitoring system connected", 1: "Monitoring system disconnected" }
class Content_273(ContentTable):
    values = { 0: "Monitoring system not connected or unknown", 1: "Monitoring system connected" }
class Content_274(ContentTable):
    values = { 0: "Mono-Static Sensor", 1: "Multi-Static Sensor", 2: "Other", 3: "Unknown" }
class Content_275(ContentTable):
    values = { 0: "Multisensor Track", 1: "Monosensor Track" }
class Content_276(ContentTable):
    values = { 0: "Multisensor track", 1: "Monosensor track" }
class Content_277(ContentTable):
    values = { 0: "N/A", 1: "Turn right", 2: "Turn left", 3: "No turn" }
class Content_278(ContentTable):
    values = { 0: "NOGO-bit not set", 1: "NOGO-bit set" }
class Content_279(ContentTable):
    values = { 0: "National Origin is valid", 1: "National Origin is invalid" }
class Content_280(ContentTable):
    values = { 0: "Network information", 10: "Track data", 20: "Track data request", 21: "Track data stop", 22: "Cancel track data request", 23: "Track data stop acknowledgement", 30: "New Node / Change-over Initial or intermediate message segment", 31: "New Node / Change-over Final or only message segment", 32: "New Node / Change-over Initial or intermediate message segment reply", 33: "New Node / Change-over Final or only message segment reply", 110: "Move node to new cluster state;", 111: "Move node to new cluster state acknowledgement" }
class Content_281(ContentTable):
    values = { 0: "No", 1: "Yes" }
class Content_282(ContentTable):
    values = { 0: "No ADS-B Emitter Category Information", 1: "Light aircraft <= 15500 lbs", 2: "15500 lbs < small aircraft <75000 lbs", 3: "75000 lbs < medium a/c < 300000 lbs", 4: "High Vortex Large", 5: "300000 lbs <= heavy aircraft", 6: "Highly manoeuvrable (5g acceleration capability) and high speed (>400 knots cruise)", 7: "Reserved", 8: "Reserved", 9: "Reserved", 10: "Rotocraft", 11: "Glider / sailplane", 12: "Lighter-than-air", 13: "Unmanned aerial vehicle", 14: "Space / transatmospheric vehicle", 15: "Ultralight / handglider / paraglider", 16: "Parachutist / skydiver", 17: "Reserved", 18: "Reserved", 19: "Reserved", 20: "Surface emergency vehicle", 21: "Surface service vehicle", 22: "Fixed ground or tethered obstruction", 23: "Cluster obstacle", 24: "Line obstacle" }
class Content_283(ContentTable):
    values = { 0: "No Corrupted reply in multilateration", 1: "Corrupted replies in multilateration" }
class Content_284(ContentTable):
    values = { 0: "No Error Detected", 1: "Error Code Undefined", 2: "Reserved for allocation by the AMG", 3: "Reserved for allocation by the AMG", 4: "Reserved for allocation by the AMG", 5: "Reserved for allocation by the AMG", 6: "Reserved for allocation by the AMG", 7: "Reserved for allocation by the AMG", 8: "Reserved for allocation by the AMG", 9: "Reserved for allocation by the AMG", 10: "Reserved for allocation by the AMG", 11: "Reserved for allocation by the AMG", 12: "Reserved for allocation by the AMG", 13: "Reserved for allocation by the AMG", 14: "Reserved for allocation by the AMG", 15: "Reserved for allocation by the AMG", 16: "Reserved for allocation by system manufacturers", 17: "Reserved for allocation by system manufacturers", 18: "Reserved for allocation by system manufacturers", 19: "Reserved for allocation by system manufacturers", 20: "Reserved for allocation by system manufacturers", 21: "Reserved for allocation by system manufacturers", 22: "Reserved for allocation by system manufacturers", 23: "Reserved for allocation by system manufacturers", 24: "Reserved for allocation by system manufacturers", 25: "Reserved for allocation by system manufacturers", 26: "Reserved for allocation by system manufacturers", 27: "Reserved for allocation by system manufacturers", 28: "Reserved for allocation by system manufacturers", 29: "Reserved for allocation by system manufacturers", 30: "Reserved for allocation by system manufacturers", 31: "Reserved for allocation by system manufacturers", 32: "Reserved for allocation by system manufacturers", 33: "Reserved for allocation by system manufacturers", 34: "Reserved for allocation by system manufacturers", 35: "Reserved for allocation by system manufacturers", 36: "Reserved for allocation by system manufacturers", 37: "Reserved for allocation by system manufacturers", 38: "Reserved for allocation by system manufacturers", 39: "Reserved for allocation by system manufacturers", 40: "Reserved for allocation by system manufacturers", 41: "Reserved for allocation by system manufacturers", 42: "Reserved for allocation by system manufacturers", 43: "Reserved for allocation by system manufacturers", 44: "Reserved for allocation by system manufacturers", 45: "Reserved for allocation by system manufacturers", 46: "Reserved for allocation by system manufacturers", 47: "Reserved for allocation by system manufacturers", 48: "Reserved for allocation by system manufacturers", 49: "Reserved for allocation by system manufacturers", 50: "Reserved for allocation by system manufacturers", 51: "Reserved for allocation by system manufacturers", 52: "Reserved for allocation by system manufacturers", 53: "Reserved for allocation by system manufacturers", 54: "Reserved for allocation by system manufacturers", 55: "Reserved for allocation by system manufacturers", 56: "Reserved for allocation by system manufacturers", 57: "Reserved for allocation by system manufacturers", 58: "Reserved for allocation by system manufacturers", 59: "Reserved for allocation by system manufacturers", 60: "Reserved for allocation by system manufacturers", 61: "Reserved for allocation by system manufacturers", 62: "Reserved for allocation by system manufacturers", 63: "Reserved for allocation by system manufacturers" }
class Content_285(ContentTable):
    values = { 0: "No Extended Range", 1: "Extended Range present" }
class Content_286(ContentTable):
    values = { 0: "No Mode 4 interrogation", 1: "Friendly target", 2: "Unknown target", 3: "No reply" }
class Content_287(ContentTable):
    values = { 0: "No Mode 4 interrogation", 1: "Possibly friendly target", 2: "Probably friendly target", 3: "Friendly target" }
class Content_288(ContentTable):
    values = { 0: "No Mode 4 interrogationt", 1: "Friendly target", 2: "Unknown target", 3: "No reply" }
class Content_289(ContentTable):
    values = { 0: "No Mode 5 interrogation", 1: "Friendly target", 2: "Unknown target", 3: "No reply" }
class Content_290(ContentTable):
    values = { 0: "No Mode 5 interrogation", 1: "Mode 5 interrogation" }
class Content_291(ContentTable):
    values = { 0: "No QNH Correction Applied", 1: "QNH Correction Applied" }
class Content_292(ContentTable):
    values = { 0: "No QNH correction applied", 1: "QNH correction applied" }
class Content_293(ContentTable):
    values = { 0: "No RWC Capability", 1: "RWC/RA/OCM Capability", 2: "RWC/OCM Capability", 3: "Invalid ASTERIX Value" }
class Content_294(ContentTable):
    values = { 0: "No X-Pulse present", 1: "X-Pulse present" }
class Content_295(ContentTable):
    values = { 0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground" }
class Content_296(ContentTable):
    values = { 0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 6: "General Emergency", 7: "Lifeguard / medical", 8: "Minimum fuel", 9: "No communications", 10: "Unlawful" }
class Content_297(ContentTable):
    values = { 0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 6: "General Emergency", 7: "Lifeguard / medical", 8: "Minimum fuel", 9: "No communications", 10: "Unlawful interference" }
class Content_298(ContentTable):
    values = { 0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 6: "Not assigned", 7: "Information not yet extracted" }
class Content_299(ContentTable):
    values = { 0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 6: "Not defined", 7: "Unknown or not yet extracted" }
class Content_300(ContentTable):
    values = { 0: "No alert, no SPI, aircraft airborne", 1: "No alert, no SPI, aircraft on ground", 2: "Alert, no SPI, aircraft airborne", 3: "Alert, no SPI, aircraft on ground", 4: "Alert, SPI, aircraft airborne or on ground", 5: "No alert, SPI, aircraft airborne or on ground", 7: "Unknown" }
class Content_301(ContentTable):
    values = { 0: "No altitude discrepancy", 1: "Altitude discrepancy" }
class Content_302(ContentTable):
    values = { 0: "No authenticated Mode 5 Data reply or Report", 1: "Authenticated Mode 5 Data reply or Report (i.e any valid Mode 5 reply type other than ID)" }
class Content_303(ContentTable):
    values = { 0: "No authenticated Mode 5 Data reply/report", 1: "Authenticated Mode 5 Data reply/report (i.e any valid Mode 5 reply type other than ID)" }
class Content_304(ContentTable):
    values = { 0: "No authenticated Mode 5 ID reply", 1: "Authenticated Mode 5 ID reply" }
class Content_305(ContentTable):
    values = { 0: "No authenticated Mode 5 ID reply/report", 1: "Authenticated Mode 5 ID reply/report" }
class Content_306(ContentTable):
    values = { 0: "No capability for Trajectory Change Reports", 1: "Support for TC+0 reports only", 2: "Support for multiple TC reports", 3: "Reserved" }
class Content_307(ContentTable):
    values = { 0: "No capability to generate ARV-reports", 1: "Capable of generate ARV-reports" }
class Content_308(ContentTable):
    values = { 0: "No capability to support Target State Reports", 1: "Capable of supporting target State Reports" }
class Content_309(ContentTable):
    values = { 0: "No change", 1: "Mode 3/A has changed" }
class Content_310(ContentTable):
    values = { 0: "No channel selected", 1: "Channel A only selected", 2: "Channel B only selected", 3: "Diversity mode ; Channel A and B selected" }
class Content_311(ContentTable):
    values = { 0: "No channel selected", 1: "Channel A only selected", 2: "Channel B only selected", 3: "Illegal combination" }
class Content_312(ContentTable):
    values = { 0: "No channel selected", 1: "Channel A only selected", 2: "Channel B only selected", 3: "Invalid combination" }
class Content_313(ContentTable):
    values = { 0: "No communications capability (surveillance only)", 1: "Comm. A and Comm. B capability", 2: "Comm. A, Comm. B and Uplink ELM", 3: "Comm. A, Comm. B and Uplink ELM and Downlink ELM", 4: "Level 5 Transponder capability" }
class Content_314(ContentTable):
    values = { 0: "No communications capability (surveillance only)", 1: "Comm. A and Comm. B capability", 2: "Comm. A, Comm. B and Uplink ELM", 3: "Comm. A, Comm. B, Uplink ELM and Downlink ELM", 4: "Level 5 Transponder capability" }
class Content_315(ContentTable):
    values = { 0: "No communications capability (surveillance only)", 1: "Comm. A and Comm. B capability", 2: "Comm. A, Comm. B and Uplink ELM", 3: "Comm. A, Comm. B, Uplink ELM and Downlink ELM", 4: "Level 5 Transponder capability", 5: "Not assigned", 6: "Not assigned", 7: "Not assigned" }
class Content_316(ContentTable):
    values = { 0: "No communications capability (surveillance only), no ability to set CA code 7 either airborne or on the ground", 1: "Reserved", 2: "Reserved", 3: "Reserved", 4: "At Least Comm. A and Comm. B capability and the ability to set CA code 7 and on the ground", 5: "At Least Comm. A and Comm. B capability and the ability to set CA code 7 and airborne", 6: "At Least Comm. A and Comm. B capability and the ability to set CA code 7 and either airborne or on the ground", 7: "Signifies the DR field is not equal to 0 or the FS field equals 2, 3, 4 or 5 and either airborne or on the ground SI/II-capabilities of the Transponder" }
class Content_317(ContentTable):
    values = { 0: "No compression applied", 1: "Compression applied" }
class Content_318(ContentTable):
    values = { 0: "No condition reported", 1: "Permanent Alert (Emergency condition)", 2: "Temporary Alert (change in Mode 3/A Code other than emergency)", 3: "SPI set" }
class Content_319(ContentTable):
    values = { 0: "No data", 1: "0 ≤ TAO ≤ 1", 2: "1 < TAO ≤ 2", 3: "2 < TAO ≤ 4", 4: "4 < TAO ≤ 6", 5: "6 < TAO ≤ 8", 6: "8 < TAO ≤ 10", 7: "10 < TAO ≤ 12", 8: "12 < TAO ≤ 14", 9: "14 < TAO ≤ 16", 10: "16 < TAO ≤ 18", 11: "18 < TAO ≤ 20", 12: "20 < TAO ≤ 22", 13: "22 < TAO ≤ 24", 14: "24 < TAO ≤ 26", 15: "26 < TAO ≤ 28", 16: "28 < TAO ≤ 30", 17: "30 < TAO ≤ 32", 18: "32 < TAO ≤ 34", 19: "34 < TAO ≤ 36", 20: "36 < TAO ≤ 38", 21: "38 < TAO ≤ 40", 22: "40 < TAO ≤ 42", 23: "42 < TAO ≤ 44", 24: "44 < TAO ≤ 46", 25: "46 < TAO ≤ 48", 26: "48 < TAO ≤ 50", 27: "50 < TAO ≤ 52", 28: "52 < TAO ≤ 54", 29: "54 < TAO ≤ 56", 30: "56 < TAO ≤ 58", 31: "TAO > 58" }
class Content_320(ContentTable):
    values = { 0: "No detection", 1: "Single PSR detection", 2: "Single SSR detection", 3: "SSR + PSR detection", 4: "Single ModeS All-Call", 5: "Single ModeS Roll-Call", 6: "ModeS All-Call + PSR", 7: "ModeS Roll-Call + PSR" }
class Content_321(ContentTable):
    values = { 0: "No detection", 1: "Single PSR detection", 2: "Single SSR detection", 3: "SSR + PSR detection", 4: "Single ModeS All-Call", 5: "Single ModeS Roll-Call", 6: "ModeS All-Call + PSR", 7: "ModeS Roll-Call +PSR" }
class Content_322(ContentTable):
    values = { 0: "No detection", 1: "Single PSR detection", 2: "Single SSR detection", 3: "SSR+PSR detection", 4: "Single Mode S All-Call", 5: "Single Mode S Roll-Call", 6: "Mode S All-Call + PSR", 7: "Mode S Roll-Call + PSR", 8: "ADS-B", 9: "WAM" }
class Content_323(ContentTable):
    values = { 0: "No detection", 1: "Sole primary detection", 2: "Sole secondary detection", 3: "Combined primary and secondary detection" }
class Content_324(ContentTable):
    values = { 0: "No detection (number of misses)", 1: "Single PSR target reports", 2: "Single SSR target reports (Non-Mode S)", 3: "SSR+PSR target reports (Non-Mode S)", 4: "Single All-Call target reports (Mode S)", 5: "Single Roll-Call target reports (Mode S)", 6: "All-Call + PSR (Mode S) target reports", 7: "Roll-Call + PSR (Mode S) target reports", 8: "Filter for Weather data", 9: "Filter for Jamming Strobe", 10: "Filter for PSR data", 11: "Filter for SSR/Mode S data", 12: "Filter for SSR/Mode S+PSR data", 13: "Filter for Enhanced Surveillance data", 14: "Filter for PSR+Enhanced Surveillance", 15: "Filter for PSR+Enhanced Surveillance + SSR/Mode S data not in Area of Prime Interest", 16: "Filter for PSR+Enhanced Surveillance + all SSR/Mode S data" }
class Content_325(ContentTable):
    values = { 0: "No detection (number of misses)", 1: "Single PSR target reports", 2: "Single SSR target reports (Non-Mode S)", 3: "SSR+PSR target reports (Non-Mode S)", 4: "Single All-Call target reports (Mode S)", 5: "Single Roll-Call target reports (Mode S)", 6: "All-Call + PSR (Mode S) target reports", 7: "Roll-Call + PSR (Mode S) target reports", 8: "Filter for Weather data", 9: "Filter for Jamming Strobe", 10: "Filter for PSR data", 11: "Filter for SSR/Mode S data", 12: "Filter for SSR/Mode S+PSR data", 13: "Filter for Enhanced Surveillance data", 14: "Filter for PSR+Enhanced Surveillance", 15: "Filter for PSR+Enhanced Surveillance + SSR/Mode S data not in Area of Prime Interest", 16: "Filter for PSR+Enhanced Surveillance + all SSR/Mode S data", 17: "Re-Interrogations (per sector)", 18: "BDS Swap and wrong DF replies(per sector)", 19: "Mode A/C FRUIT (per sector)", 20: "Mode S FRUIT (per sector)" }
class Content_326(ContentTable):
    values = { 0: "No diagnostic available", 1: "Aircraft Exit", 2: "Incorrect aircraft address", 3: "Impossibility to process the message", 4: "Insufficient or change in data link capability", 5: "Invalid LV field", 6: "Duplicate request number", 7: "Unknown request number", 8: "Timer T3 expiry", 9: "Expiry of I/R delivery timer", 10: "Uplink flow disabled by UC" }
class Content_327(ContentTable):
    values = { 0: "No differential correction (ADS-B)", 1: "Differential correction (ADS-B)" }
class Content_328(ContentTable):
    values = { 0: "No doubt", 1: "Doubtful correlation (undetermined reason)", 2: "Doubtful correlation in clutter", 3: "Loss of accuracy", 4: "Loss of accuracy in clutter", 5: "Unstable track", 6: "Previously coasted" }
class Content_329(ContentTable):
    values = { 0: "No emergency", 1: "General emergency", 2: "Lifeguard / medical", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference", 6: "Downed Aircraft", 7: "Undefined" }
class Content_330(ContentTable):
    values = { 0: "No emergency", 1: "General emergency", 2: "Lifeguard / medical", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference", 6: "Downed Aircraft", 7: "Unknown" }
class Content_331(ContentTable):
    values = { 0: "No emergency / not reported", 1: "General emergency", 2: "Lifeguard / medical", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference" }
class Content_332(ContentTable):
    values = { 0: "No emergency / not reported", 1: "General emergency", 2: "Lifeguard / medical emergency", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference", 6: "DOWNED Aircraft" }
class Content_333(ContentTable):
    values = { 0: "No emergency / not reported", 1: "General emergency", 2: "UAS/RPAS - Lost link", 3: "Minimum fuel", 4: "No communications", 5: "Unlawful interference", 6: "Aircraft in Distress", 7: "Aircraft in Distress Manual Activation" }
class Content_334(ContentTable):
    values = { 0: "No error detected (shall not be sent)", 1: "Error Code Undefined", 2: "Time Source Invalid", 3: "Time Source Coasting", 4: "Track ID numbering has restarted", 5: "Data Processor Overload", 6: "Ground Interface Data Communications Overload", 7: "System stopped by operator", 8: "CBIT failed", 9: "Test Target Failure", 10: "Reserved for allocation by the AMG", 11: "Reserved for allocation by the AMG", 12: "Reserved for allocation by the AMG", 13: "Reserved for allocation by the AMG", 14: "Reserved for allocation by the AMG", 15: "Reserved for allocation by the AMG", 16: "Reserved for allocation by the AMG", 17: "Reserved for allocation by the AMG", 18: "Reserved for allocation by the AMG", 19: "Reserved for allocation by the AMG", 20: "Reserved for allocation by the AMG", 21: "Reserved for allocation by the AMG", 22: "Reserved for allocation by the AMG", 23: "Reserved for allocation by the AMG", 24: "Reserved for allocation by the AMG", 25: "Reserved for allocation by the AMG", 26: "Reserved for allocation by the AMG", 27: "Reserved for allocation by the AMG", 28: "Reserved for allocation by the AMG", 29: "Reserved for allocation by the AMG", 30: "Reserved for allocation by the AMG", 31: "Reserved for allocation by the AMG", 32: "Reserved for allocation by system manufacturers", 33: "Reserved for allocation by system manufacturers", 34: "Reserved for allocation by system manufacturers", 35: "Reserved for allocation by system manufacturers", 36: "Reserved for allocation by system manufacturers", 37: "Reserved for allocation by system manufacturers", 38: "Reserved for allocation by system manufacturers", 39: "Reserved for allocation by system manufacturers", 40: "Reserved for allocation by system manufacturers", 41: "Reserved for allocation by system manufacturers", 42: "Reserved for allocation by system manufacturers", 43: "Reserved for allocation by system manufacturers", 44: "Reserved for allocation by system manufacturers", 45: "Reserved for allocation by system manufacturers", 46: "Reserved for allocation by system manufacturers", 47: "Reserved for allocation by system manufacturers", 48: "Reserved for allocation by system manufacturers", 49: "Reserved for allocation by system manufacturers", 50: "Reserved for allocation by system manufacturers", 51: "Reserved for allocation by system manufacturers", 52: "Reserved for allocation by system manufacturers", 53: "Reserved for allocation by system manufacturers", 54: "Reserved for allocation by system manufacturers", 55: "Reserved for allocation by system manufacturers", 56: "Reserved for allocation by system manufacturers", 57: "Reserved for allocation by system manufacturers", 58: "Reserved for allocation by system manufacturers", 59: "Reserved for allocation by system manufacturers", 60: "Reserved for allocation by system manufacturers", 61: "Reserved for allocation by system manufacturers", 62: "Reserved for allocation by system manufacturers", 63: "Reserved for allocation by system manufacturers", 64: "Reserved for allocation by system manufacturers", 65: "Reserved for allocation by system manufacturers", 66: "Reserved for allocation by system manufacturers", 67: "Reserved for allocation by system manufacturers", 68: "Reserved for allocation by system manufacturers", 69: "Reserved for allocation by system manufacturers", 70: "Reserved for allocation by system manufacturers", 71: "Reserved for allocation by system manufacturers", 72: "Reserved for allocation by system manufacturers", 73: "Reserved for allocation by system manufacturers", 74: "Reserved for allocation by system manufacturers", 75: "Reserved for allocation by system manufacturers", 76: "Reserved for allocation by system manufacturers", 77: "Reserved for allocation by system manufacturers", 78: "Reserved for allocation by system manufacturers", 79: "Reserved for allocation by system manufacturers", 80: "Reserved for allocation by system manufacturers", 81: "Reserved for allocation by system manufacturers", 82: "Reserved for allocation by system manufacturers", 83: "Reserved for allocation by system manufacturers", 84: "Reserved for allocation by system manufacturers", 85: "Reserved for allocation by system manufacturers", 86: "Reserved for allocation by system manufacturers", 87: "Reserved for allocation by system manufacturers", 88: "Reserved for allocation by system manufacturers", 89: "Reserved for allocation by system manufacturers", 90: "Reserved for allocation by system manufacturers", 91: "Reserved for allocation by system manufacturers", 92: "Reserved for allocation by system manufacturers", 93: "Reserved for allocation by system manufacturers", 94: "Reserved for allocation by system manufacturers", 95: "Reserved for allocation by system manufacturers", 96: "Reserved for allocation by system manufacturers", 97: "Reserved for allocation by system manufacturers", 98: "Reserved for allocation by system manufacturers", 99: "Reserved for allocation by system manufacturers", 100: "Reserved for allocation by system manufacturers", 101: "Reserved for allocation by system manufacturers", 102: "Reserved for allocation by system manufacturers", 103: "Reserved for allocation by system manufacturers", 104: "Reserved for allocation by system manufacturers", 105: "Reserved for allocation by system manufacturers", 106: "Reserved for allocation by system manufacturers", 107: "Reserved for allocation by system manufacturers", 108: "Reserved for allocation by system manufacturers", 109: "Reserved for allocation by system manufacturers", 110: "Reserved for allocation by system manufacturers", 111: "Reserved for allocation by system manufacturers", 112: "Reserved for allocation by system manufacturers", 113: "Reserved for allocation by system manufacturers", 114: "Reserved for allocation by system manufacturers", 115: "Reserved for allocation by system manufacturers", 116: "Reserved for allocation by system manufacturers", 117: "Reserved for allocation by system manufacturers", 118: "Reserved for allocation by system manufacturers", 119: "Reserved for allocation by system manufacturers", 120: "Reserved for allocation by system manufacturers", 121: "Reserved for allocation by system manufacturers", 122: "Reserved for allocation by system manufacturers", 123: "Reserved for allocation by system manufacturers", 124: "Reserved for allocation by system manufacturers", 125: "Reserved for allocation by system manufacturers", 126: "Reserved for allocation by system manufacturers", 127: "Reserved for allocation by system manufacturers", 128: "Reserved for allocation by system manufacturers", 129: "Reserved for allocation by system manufacturers", 130: "Reserved for allocation by system manufacturers", 131: "Reserved for allocation by system manufacturers", 132: "Reserved for allocation by system manufacturers", 133: "Reserved for allocation by system manufacturers", 134: "Reserved for allocation by system manufacturers", 135: "Reserved for allocation by system manufacturers", 136: "Reserved for allocation by system manufacturers", 137: "Reserved for allocation by system manufacturers", 138: "Reserved for allocation by system manufacturers", 139: "Reserved for allocation by system manufacturers", 140: "Reserved for allocation by system manufacturers", 141: "Reserved for allocation by system manufacturers", 142: "Reserved for allocation by system manufacturers", 143: "Reserved for allocation by system manufacturers", 144: "Reserved for allocation by system manufacturers", 145: "Reserved for allocation by system manufacturers", 146: "Reserved for allocation by system manufacturers", 147: "Reserved for allocation by system manufacturers", 148: "Reserved for allocation by system manufacturers", 149: "Reserved for allocation by system manufacturers", 150: "Reserved for allocation by system manufacturers", 151: "Reserved for allocation by system manufacturers", 152: "Reserved for allocation by system manufacturers", 153: "Reserved for allocation by system manufacturers", 154: "Reserved for allocation by system manufacturers", 155: "Reserved for allocation by system manufacturers", 156: "Reserved for allocation by system manufacturers", 157: "Reserved for allocation by system manufacturers", 158: "Reserved for allocation by system manufacturers", 159: "Reserved for allocation by system manufacturers", 160: "Reserved for allocation by system manufacturers", 161: "Reserved for allocation by system manufacturers", 162: "Reserved for allocation by system manufacturers", 163: "Reserved for allocation by system manufacturers", 164: "Reserved for allocation by system manufacturers", 165: "Reserved for allocation by system manufacturers", 166: "Reserved for allocation by system manufacturers", 167: "Reserved for allocation by system manufacturers", 168: "Reserved for allocation by system manufacturers", 169: "Reserved for allocation by system manufacturers", 170: "Reserved for allocation by system manufacturers", 171: "Reserved for allocation by system manufacturers", 172: "Reserved for allocation by system manufacturers", 173: "Reserved for allocation by system manufacturers", 174: "Reserved for allocation by system manufacturers", 175: "Reserved for allocation by system manufacturers", 176: "Reserved for allocation by system manufacturers", 177: "Reserved for allocation by system manufacturers", 178: "Reserved for allocation by system manufacturers", 179: "Reserved for allocation by system manufacturers", 180: "Reserved for allocation by system manufacturers", 181: "Reserved for allocation by system manufacturers", 182: "Reserved for allocation by system manufacturers", 183: "Reserved for allocation by system manufacturers", 184: "Reserved for allocation by system manufacturers", 185: "Reserved for allocation by system manufacturers", 186: "Reserved for allocation by system manufacturers", 187: "Reserved for allocation by system manufacturers", 188: "Reserved for allocation by system manufacturers", 189: "Reserved for allocation by system manufacturers", 190: "Reserved for allocation by system manufacturers", 191: "Reserved for allocation by system manufacturers", 192: "Reserved for allocation by system manufacturers", 193: "Reserved for allocation by system manufacturers", 194: "Reserved for allocation by system manufacturers", 195: "Reserved for allocation by system manufacturers", 196: "Reserved for allocation by system manufacturers", 197: "Reserved for allocation by system manufacturers", 198: "Reserved for allocation by system manufacturers", 199: "Reserved for allocation by system manufacturers", 200: "Reserved for allocation by system manufacturers", 201: "Reserved for allocation by system manufacturers", 202: "Reserved for allocation by system manufacturers", 203: "Reserved for allocation by system manufacturers", 204: "Reserved for allocation by system manufacturers", 205: "Reserved for allocation by system manufacturers", 206: "Reserved for allocation by system manufacturers", 207: "Reserved for allocation by system manufacturers", 208: "Reserved for allocation by system manufacturers", 209: "Reserved for allocation by system manufacturers", 210: "Reserved for allocation by system manufacturers", 211: "Reserved for allocation by system manufacturers", 212: "Reserved for allocation by system manufacturers", 213: "Reserved for allocation by system manufacturers", 214: "Reserved for allocation by system manufacturers", 215: "Reserved for allocation by system manufacturers", 216: "Reserved for allocation by system manufacturers", 217: "Reserved for allocation by system manufacturers", 218: "Reserved for allocation by system manufacturers", 219: "Reserved for allocation by system manufacturers", 220: "Reserved for allocation by system manufacturers", 221: "Reserved for allocation by system manufacturers", 222: "Reserved for allocation by system manufacturers", 223: "Reserved for allocation by system manufacturers", 224: "Reserved for allocation by system manufacturers", 225: "Reserved for allocation by system manufacturers", 226: "Reserved for allocation by system manufacturers", 227: "Reserved for allocation by system manufacturers", 228: "Reserved for allocation by system manufacturers", 229: "Reserved for allocation by system manufacturers", 230: "Reserved for allocation by system manufacturers", 231: "Reserved for allocation by system manufacturers", 232: "Reserved for allocation by system manufacturers", 233: "Reserved for allocation by system manufacturers", 234: "Reserved for allocation by system manufacturers", 235: "Reserved for allocation by system manufacturers", 236: "Reserved for allocation by system manufacturers", 237: "Reserved for allocation by system manufacturers", 238: "Reserved for allocation by system manufacturers", 239: "Reserved for allocation by system manufacturers", 240: "Reserved for allocation by system manufacturers", 241: "Reserved for allocation by system manufacturers", 242: "Reserved for allocation by system manufacturers", 243: "Reserved for allocation by system manufacturers", 244: "Reserved for allocation by system manufacturers", 245: "Reserved for allocation by system manufacturers", 246: "Reserved for allocation by system manufacturers", 247: "Reserved for allocation by system manufacturers", 248: "Reserved for allocation by system manufacturers", 249: "Reserved for allocation by system manufacturers", 250: "Reserved for allocation by system manufacturers", 251: "Reserved for allocation by system manufacturers", 252: "Reserved for allocation by system manufacturers", 253: "Reserved for allocation by system manufacturers", 254: "Reserved for allocation by system manufacturers", 255: "Reserved for allocation by system manufacturers" }
class Content_335(ContentTable):
    values = { 0: "No extrapolation", 1: "Predictable extrapolation due to sensor refresh period (see NOTE)", 2: "Predictable extrapolation in masked area", 3: "Extrapolation due to unpredictable absence of detection" }
class Content_336(ContentTable):
    values = { 0: "No horizontal man.sensed", 1: "Horizontal man. sensed" }
class Content_337(ContentTable):
    values = { 0: "No indication", 1: "Duplicate Flight-ID" }
class Content_338(ContentTable):
    values = { 0: "No indication", 1: "Potential False Track Indication" }
class Content_339(ContentTable):
    values = { 0: "No information", 1: "NRA class", 2: "Reserved for future use", 3: "Reserved for future use", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use" }
class Content_340(ContentTable):
    values = { 0: "No intent change active", 1: "Intent change flag raised" }
class Content_341(ContentTable):
    values = { 0: "No military emergency", 1: "Military emergency" }
class Content_342(ContentTable):
    values = { 0: "No military identification", 1: "Military identification" }
class Content_343(ContentTable):
    values = { 0: "No overload", 1: "Overload" }
class Content_344(ContentTable):
    values = { 0: "No reduction active", 1: "Reduction step 1 active", 2: "Reduction step 2 active", 3: "Reduction step 3 active", 4: "Reduction step 4 active", 5: "Reduction step 5 active", 6: "Reduction step 6 active", 7: "Reduction step 7 active" }
class Content_345(ContentTable):
    values = { 0: "No source", 1: "GNSS", 2: "3D radar", 3: "Triangulation", 4: "Height from coverage", 5: "Speed look-up table", 6: "Default height", 7: "Multilateration" }
class Content_346(ContentTable):
    values = { 0: "No source", 1: "GPS", 2: "3d radar", 3: "Triangulation", 4: "Height from coverage", 5: "Speed look-up table", 6: "Default height", 7: "Multilateration" }
class Content_347(ContentTable):
    values = { 0: "No source information provided", 1: "Source Information provided" }
class Content_348(ContentTable):
    values = { 0: "No source information provided", 1: "Source information provided" }
class Content_349(ContentTable):
    values = { 0: "No spoofing detected", 1: "Potential spoofing attack" }
class Content_350(ContentTable):
    values = { 0: "No valid Mode 3/A available", 1: "Valid Mode 3/A available" }
class Content_351(ContentTable):
    values = { 0: "No warning nor error condition", 1: "Garbled reply", 2: "Reflection", 3: "Sidelobe reply", 4: "Split plot", 5: "Second time around reply", 6: "Angels", 7: "Terrestrial vehicles", 64: "Possible wrong code in Mode-3/A", 65: "Possible wrong altitude information, transmitted when the Code C credibility check fails together with the Mode-C code in binary notation", 66: "Possible phantom MSSR plot", 80: "Fixed PSR plot", 81: "Slow PSR plot", 82: "Low quality PSR plot" }
class Content_352(ContentTable):
    values = { 0: "Non primary target", 1: "Primary target" }
class Content_353(ContentTable):
    values = { 0: "Non unique address", 1: "24-Bit ICAO address", 2: "Surface vehicle address", 3: "Anonymous address", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use" }
class Content_354(ContentTable):
    values = { 0: "Non-Mode S 1090MHz multilateration", 1: "No Non-Mode S 1090MHz multilat" }
class Content_355(ContentTable):
    values = { 0: "Normal Operation", 1: "Diversity degraded" }
class Content_356(ContentTable):
    values = { 0: "Normal Priority Flight", 1: "High Priority Flight" }
class Content_357(ContentTable):
    values = { 0: "Normal confidence", 1: "Low confidence in plot to track association" }
class Content_358(ContentTable):
    values = { 0: "Not Coasted", 1: "Coasted" }
class Content_359(ContentTable):
    values = { 0: "Not RCE", 1: "TABS (see Note 2)", 2: "Reserved for future use", 3: "Other RCE" }
class Content_360(ContentTable):
    values = { 0: "Not active", 1: "Active" }
class Content_361(ContentTable):
    values = { 0: "Not active or unknown", 1: "Active" }
class Content_362(ContentTable):
    values = { 0: "Not applicable", 1: "SDPS-1 selected", 2: "SDPS-2 selected", 3: "SDPS-3 selected" }
class Content_363(ContentTable):
    values = { 0: "Not available", 1: "Available" }
class Content_364(ContentTable):
    values = { 0: "Not available", 1: "Left", 2: "Right", 3: "Straight" }
class Content_365(ContentTable):
    values = { 0: "Not defined; never used", 1: "Multipath Reply (Reflection)", 2: "Reply due to sidelobe interrogation/reception", 3: "Split plot", 4: "Second time around reply", 5: "Angel", 6: "Slow moving target correlated with road infrastructure (terrestrial vehicle)", 7: "Fixed PSR plot", 8: "Slow PSR target", 9: "Low quality PSR plot", 10: "Phantom SSR plot", 11: "Non-Matching Mode-3/A Code", 12: "Mode C code / Mode S altitude code abnormal value compared to the track", 13: "Target in Clutter Area", 14: "Maximum Doppler Response in Zero Filter", 15: "Transponder anomaly detected", 16: "Duplicated or Illegal Mode S Aircraft Address", 17: "Mode S error correction applied", 18: "Undecodable Mode C code / Mode S altitude code", 19: "Birds", 20: "Flock of Birds", 21: "Mode-1 was present in original reply", 22: "Mode-2 was present in original reply", 23: "Plot potentially caused by Wind Turbine", 24: "Helicopter", 25: "Maximum number of re-interrogations reached (surveillance information)", 26: "Maximum number of re-interrogations reached (BDS Extractions)", 27: "BDS Overlay Incoherence", 28: "Potential BDS Swap Detected", 29: "Track Update in the Zenithal Gap", 30: "Mode S Track re-acquired", 31: "Duplicated Mode 5 Pair NO/PIN detected" }
class Content_366(ContentTable):
    values = { 0: "Not defined; never used", 1: "Multipath Reply (Reflection)", 2: "Reply due to sidelobe interrogation/reception", 3: "Split plot", 4: "Second time around reply", 5: "Angel", 6: "Slow moving target correlated with road infrastructure (terrestrial vehicle)", 7: "Fixed PSR plot", 8: "Slow PSR target", 9: "Low quality PSR plot", 10: "Phantom SSR plot", 11: "Non-Matching Mode-3/A Code", 12: "Mode C code / Mode S altitude code abnormal value compared to the track", 13: "Target in Clutter Area", 14: "Maximum Doppler Response in Zero Filter", 15: "Transponder anomaly detected", 16: "Duplicated or Illegal Mode S Aircraft Address", 17: "Mode S error correction applied", 18: "Undecodable Mode C code / Mode S altitude code", 19: "Birds", 20: "Flock of Birds", 21: "Mode-1 was present in original reply", 22: "Mode-2 was present in original reply", 23: "Plot potentially caused by Wind Turbine", 24: "Helicopter", 25: "Maximum number of re-interrogations reached (surveillance information)", 26: "Maximum number of re-interrogations reached (BDS Extractions)", 27: "BDS Overlay Incoherence", 28: "Potential BDS Swap Detected", 29: "Track Update in the Zenithal Gap", 30: "Mode S Track re-acquired", 31: "Duplicated Mode 5 Pair NO/PIN detected", 32: "Wrong DF reply format detected", 33: "Transponder anomaly (MS XPD replies with Mode A/C to Mode A/C-only all-call)", 34: "Transponder anomaly (SI capability report wrong)" }
class Content_367(ContentTable):
    values = { 0: "Not defined; never used", 1: "Multipath Reply (Reflection)", 2: "Reply due to sidelobe interrogation/reception", 3: "Split plot", 4: "Second time around reply", 5: "Angel", 6: "Slow moving target correlated with road infrastructure (terrestrial vehicle)", 7: "Fixed PSR plot", 8: "Slow PSR target", 9: "Low quality PSR plot", 10: "Phantom SSR plot", 11: "Non-Matching Mode-3/A Code", 12: "Mode C code / Mode S altitude code abnormal value compared to the track", 13: "Target in Clutter Area", 14: "Maximum Doppler Response in Zero Filter", 15: "Transponder anomaly detected", 16: "Duplicated or Illegal Mode S Aircraft Address", 17: "Mode S error correction applied", 18: "Undecodable Mode C code / Mode S altitude code", 19: "Birds", 20: "Flock of Birds", 21: "Mode-1 was present in original reply", 22: "Mode-2 was present in original reply", 23: "Plot potentially caused by Wind Turbine", 24: "Helicopter", 25: "Maximum number of re-interrogations reached (surveillance information)", 26: "Maximum number of re-interrogations reached (BDS Extractions)", 27: "BDS Overlay Incoherence", 28: "Potential BDS Swap Detected", 29: "Track Update in the Zenithal Gap", 30: "Mode S Track re-acquired", 31: "Duplicated Mode 5 Pair NO/PIN detected", 32: "Wrong DF reply format detected", 33: "Transponder anomaly (MS XPD replies with Mode A/C to Mode A/C-only all-call)", 34: "Transponder anomaly (SI capability report wrong)", 35: "Potential IC Conflict", 36: "IC Conflict detection possible-no conflict currently detected" }
class Content_368(ContentTable):
    values = { 0: "Not defined; never used", 1: "Multipath Reply (Reflection)", 3: "Split plot", 10: "Phantom SSR plot", 11: "Non-Matching Mode-3/A Code", 12: "Mode C code / Mode S altitude code abnormal value compared to the track", 15: "Transponder anomaly detected", 16: "Duplicated or Illegal Mode S Aircraft Address", 17: "Mode S error correction applied", 18: "Undecodable Mode C code / Mode S altitude code" }
class Content_369(ContentTable):
    values = { 0: "Not extrapolated", 1: "Extrapolated" }
class Content_370(ContentTable):
    values = { 0: "Not flight-plan correlated", 1: "Flight plan correlated" }
class Content_371(ContentTable):
    values = { 0: "Not present", 1: "Present" }
class Content_372(ContentTable):
    values = { 0: "Not used", 1: "Used" }
class Content_373(ContentTable):
    values = { 0: "Not valid Temperature", 1: "Valid Temperature" }
class Content_374(ContentTable):
    values = { 0: "Not valid Turbulence", 1: "Valid Turbulence" }
class Content_375(ContentTable):
    values = { 0: "Not valid Wind Direction", 1: "Valid Wind Direction" }
class Content_376(ContentTable):
    values = { 0: "Not valid Wind Speed", 1: "Valid Wind Speed" }
class Content_377(ContentTable):
    values = { 0: "Number of unknown messages received", 1: "Number of too old messages received", 2: "Number of failed message conversions", 3: "Total Number of messages received", 4: "Total Number of messages transmitted", 20: "Number of TIS-B management messages received", 21: "Number of Basic messages received", 22: "Number of High Dynamic messages received", 23: "Number of Full Position messages received", 24: "Number of Basic Ground  messages received", 25: "Number of TCP messages received", 26: "Number of UTC time  messages received", 27: "Number of Data messages received", 28: "Number of High Resolution messages received", 29: "Number of Aircraft Target Airborne messages received", 30: "Number of Aircraft Target Ground messages received", 31: "Number of Ground Vehicle Target messages received", 32: "Number of 2 slots TCP messages received" }
class Content_378(ContentTable):
    values = { 0: "Number of unknown messages received", 1: "Number of too old messages received", 2: "Number of failed message conversions", 3: "Total Number of messages received", 4: "Total number of messages transmitted", 5: "Reserved for AMG", 6: "Reserved for AMG", 7: "Reserved for AMG", 8: "Reserved for AMG", 9: "Reserved for AMG", 10: "Reserved for AMG", 11: "Reserved for AMG", 12: "Reserved for AMG", 13: "Reserved for AMG", 14: "Reserved for AMG", 15: "Reserved for AMG", 16: "Reserved for AMG", 17: "Reserved for AMG", 18: "Reserved for AMG", 19: "Reserved for AMG", 20: "Implementation specific", 21: "Implementation specific", 22: "Implementation specific", 23: "Implementation specific", 24: "Implementation specific", 25: "Implementation specific", 26: "Implementation specific", 27: "Implementation specific", 28: "Implementation specific", 29: "Implementation specific", 30: "Implementation specific", 31: "Implementation specific", 32: "Implementation specific", 33: "Implementation specific", 34: "Implementation specific", 35: "Implementation specific", 36: "Implementation specific", 37: "Implementation specific", 38: "Implementation specific", 39: "Implementation specific", 40: "Implementation specific", 41: "Implementation specific", 42: "Implementation specific", 43: "Implementation specific", 44: "Implementation specific", 45: "Implementation specific", 46: "Implementation specific", 47: "Implementation specific", 48: "Implementation specific", 49: "Implementation specific", 50: "Implementation specific", 51: "Implementation specific", 52: "Implementation specific", 53: "Implementation specific", 54: "Implementation specific", 55: "Implementation specific", 56: "Implementation specific", 57: "Implementation specific", 58: "Implementation specific", 59: "Implementation specific", 60: "Implementation specific", 61: "Implementation specific", 62: "Implementation specific", 63: "Implementation specific", 64: "Implementation specific", 65: "Implementation specific", 66: "Implementation specific", 67: "Implementation specific", 68: "Implementation specific", 69: "Implementation specific", 70: "Implementation specific", 71: "Implementation specific", 72: "Implementation specific", 73: "Implementation specific", 74: "Implementation specific", 75: "Implementation specific", 76: "Implementation specific", 77: "Implementation specific", 78: "Implementation specific", 79: "Implementation specific", 80: "Implementation specific", 81: "Implementation specific", 82: "Implementation specific", 83: "Implementation specific", 84: "Implementation specific", 85: "Implementation specific", 86: "Implementation specific", 87: "Implementation specific", 88: "Implementation specific", 89: "Implementation specific", 90: "Implementation specific", 91: "Implementation specific", 92: "Implementation specific", 93: "Implementation specific", 94: "Implementation specific", 95: "Implementation specific", 96: "Implementation specific", 97: "Implementation specific", 98: "Implementation specific", 99: "Implementation specific", 100: "Implementation specific", 101: "Implementation specific", 102: "Implementation specific", 103: "Implementation specific", 104: "Implementation specific", 105: "Implementation specific", 106: "Implementation specific", 107: "Implementation specific", 108: "Implementation specific", 109: "Implementation specific", 110: "Implementation specific", 111: "Implementation specific", 112: "Implementation specific", 113: "Implementation specific", 114: "Implementation specific", 115: "Implementation specific", 116: "Implementation specific", 117: "Implementation specific", 118: "Implementation specific", 119: "Implementation specific", 120: "Implementation specific", 121: "Implementation specific", 122: "Implementation specific", 123: "Implementation specific", 124: "Implementation specific", 125: "Implementation specific", 126: "Implementation specific", 127: "Implementation specific", 128: "Implementation specific", 129: "Implementation specific", 130: "Implementation specific", 131: "Implementation specific", 132: "Implementation specific", 133: "Implementation specific", 134: "Implementation specific", 135: "Implementation specific", 136: "Implementation specific", 137: "Implementation specific", 138: "Implementation specific", 139: "Implementation specific", 140: "Implementation specific", 141: "Implementation specific", 142: "Implementation specific", 143: "Implementation specific", 144: "Implementation specific", 145: "Implementation specific", 146: "Implementation specific", 147: "Implementation specific", 148: "Implementation specific", 149: "Implementation specific", 150: "Implementation specific", 151: "Implementation specific", 152: "Implementation specific", 153: "Implementation specific", 154: "Implementation specific", 155: "Implementation specific", 156: "Implementation specific", 157: "Implementation specific", 158: "Implementation specific", 159: "Implementation specific", 160: "Implementation specific", 161: "Implementation specific", 162: "Implementation specific", 163: "Implementation specific", 164: "Implementation specific", 165: "Implementation specific", 166: "Implementation specific", 167: "Implementation specific", 168: "Implementation specific", 169: "Implementation specific", 170: "Implementation specific", 171: "Implementation specific", 172: "Implementation specific", 173: "Implementation specific", 174: "Implementation specific", 175: "Implementation specific", 176: "Implementation specific", 177: "Implementation specific", 178: "Implementation specific", 179: "Implementation specific", 180: "Implementation specific", 181: "Implementation specific", 182: "Implementation specific", 183: "Implementation specific", 184: "Implementation specific", 185: "Implementation specific", 186: "Implementation specific", 187: "Implementation specific", 188: "Implementation specific", 189: "Implementation specific", 190: "Implementation specific", 191: "Implementation specific", 192: "Implementation specific", 193: "Implementation specific", 194: "Implementation specific", 195: "Implementation specific", 196: "Implementation specific", 197: "Implementation specific", 198: "Implementation specific", 199: "Implementation specific", 200: "Implementation specific", 201: "Implementation specific", 202: "Implementation specific", 203: "Implementation specific", 204: "Implementation specific", 205: "Implementation specific", 206: "Implementation specific", 207: "Implementation specific", 208: "Implementation specific", 209: "Implementation specific", 210: "Implementation specific", 211: "Implementation specific", 212: "Implementation specific", 213: "Implementation specific", 214: "Implementation specific", 215: "Implementation specific", 216: "Implementation specific", 217: "Implementation specific", 218: "Implementation specific", 219: "Implementation specific", 220: "Implementation specific", 221: "Implementation specific", 222: "Implementation specific", 223: "Implementation specific", 224: "Implementation specific", 225: "Implementation specific", 226: "Implementation specific", 227: "Implementation specific", 228: "Implementation specific", 229: "Implementation specific", 230: "Implementation specific", 231: "Implementation specific", 232: "Implementation specific", 233: "Implementation specific", 234: "Implementation specific", 235: "Implementation specific", 236: "Implementation specific", 237: "Implementation specific", 238: "Implementation specific", 239: "Implementation specific", 240: "Implementation specific", 241: "Implementation specific", 242: "Implementation specific", 243: "Implementation specific", 244: "Implementation specific", 245: "Implementation specific", 246: "Implementation specific", 247: "Implementation specific", 248: "Implementation specific", 249: "Implementation specific", 250: "Implementation specific", 251: "Implementation specific", 252: "Implementation specific", 253: "Implementation specific", 254: "Implementation specific", 255: "Implementation specific" }
class Content_379(ContentTable):
    values = { 0: "OK", 1: "Failed", 2: "Degraded", 3: "Undefined", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use" }
class Content_380(ContentTable):
    values = { 0: "Offline", 1: "Online" }
class Content_381(ContentTable):
    values = { 0: "Operational", 1: "Degraded", 2: "Initialization", 3: "Not currently connected" }
class Content_382(ContentTable):
    values = { 0: "Operational", 1: "Degraded", 2: "NOGO" }
class Content_383(ContentTable):
    values = { 0: "Operational", 1: "Degraded", 2: "NOGO", 3: "Undefined" }
class Content_384(ContentTable):
    values = { 0: "Operational", 1: "Degraded", 2: "Not currently connected", 3: "Unknown" }
class Content_385(ContentTable):
    values = { 0: "Operational", 1: "Operational but in Standby", 2: "Maintenance", 3: "Reserved for future use" }
class Content_386(ContentTable):
    values = { 0: "Other", 1: "UAT", 2: "1090 ES", 3: "VDL 4", 4: "Not assigned", 5: "Not assigned", 6: "Not assigned", 7: "Not assigned" }
class Content_387(ContentTable):
    values = { 0: "Other Technology Multilateration", 1: "No Other Technology Multilateration" }
class Content_388(ContentTable):
    values = { 0: "PAI not populated", 1: "PAI populated" }
class Content_389(ContentTable):
    values = { 0: "PSR Echo", 1: "SSR Reply", 2: "All Call Reply", 3: "Roll Call Reply" }
class Content_390(ContentTable):
    values = { 0: "PSR GO", 1: "PSR NOGO" }
class Content_391(ContentTable):
    values = { 0: "Periodic Report", 1: "Event Driven Report" }
class Content_392(ContentTable):
    values = { 0: "Plan Number", 1: "Unit 1 internal flight number", 2: "Unit 2 internal flight number", 3: "Unit 3 internal flight number" }
class Content_393(ContentTable):
    values = { 0: "Plan number", 1: "Unit 1 internal flight number", 2: "Unit 2 internal flight number", 3: "Unit 3 internal flight number" }
class Content_394(ContentTable):
    values = { 0: "Plot", 1: "Track" }
class Content_395(ContentTable):
    values = { 0: "Position not from Mode 5 report (ADS-B report)", 1: "Position from Mode 5 report" }
class Content_396(ContentTable):
    values = { 0: "Position transmitted is not ADS-B position reference point", 1: "Position transmitted is the ADS-B position reference point" }
class Content_397(ContentTable):
    values = { 0: "Primary track", 1: "SSR/Combined track" }
class Content_398(ContentTable):
    values = { 0: "RDP Chain 1", 1: "RDP Chain 2" }
class Content_399(ContentTable):
    values = { 0: "RDPC-1 selected", 1: "RDPC-2 selected" }
class Content_400(ContentTable):
    values = { 0: "RWC Corrective Alert not active", 1: "RWC Corrective Alert active" }
class Content_401(ContentTable):
    values = { 0: "Radar tracker calculation", 1: "Integrated ADS-B", 2: "External ADS-B", 3: "SCN" }
class Content_402(ContentTable):
    values = { 0: "Real target report", 1: "Test target report" }
class Content_403(ContentTable):
    values = { 0: "Reply Rate Limiting is not active", 1: "Reply Rate Limiting is active" }
class Content_404(ContentTable):
    values = { 0: "Report from RDP Chain 1", 1: "Report from RDP Chain 2" }
class Content_405(ContentTable):
    values = { 0: "Report from aircraft transponder", 1: "Report from field monitor (fixed transponder)" }
class Content_406(ContentTable):
    values = { 0: "Report from target transponder", 1: "Report from field monitor (element transponder)" }
class Content_407(ContentTable):
    values = { 0: "Report from target transponder", 1: "Report from field monitor (fixed transponder)" }
class Content_408(ContentTable):
    values = { 0: "Report from target transponder", 1: "Report from field monitor (item transponder)" }
class Content_409(ContentTable):
    values = { 0: "Report valid", 1: "Report suspect", 2: "No information", 3: "Reserved for future use" }
class Content_410(ContentTable):
    values = { 0: "Running", 1: "Failed", 2: "Degraded", 3: "Undefined", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use", 8: "Reserved for future use", 9: "Reserved for future use", 10: "Reserved for future use", 11: "Reserved for future use", 12: "Reserved for future use", 13: "Reserved for future use", 14: "Reserved for future use", 15: "Reserved for future use" }
class Content_411(ContentTable):
    values = { 0: "Running", 1: "Failed", 2: "Maintenance", 3: "Reserved" }
class Content_412(ContentTable):
    values = { 0: "Running / OK", 1: "Failed", 2: "Degraded", 3: "Undefined", 4: "Reserved for future use", 5: "Reserved for future use", 6: "Reserved for future use", 7: "Reserved for future use" }
class Content_413(ContentTable):
    values = { 0: "SCN not populated", 1: "SCN populated" }
class Content_414(ContentTable):
    values = { 0: "SI-Code Capable", 1: "II-Code Capable" }
class Content_415(ContentTable):
    values = { 0: "SSR GO", 1: "SSR NOGO" }
class Content_416(ContentTable):
    values = { 0: "SSR multilateration", 1: "Mode S multilateration", 2: "ADS-B", 3: "PSR", 4: "Magnetic Loop System", 5: "HF multilateration", 6: "Not defined", 7: "Other types" }
class Content_417(ContentTable):
    values = { 0: "STC Map-1", 1: "STC Map-2", 2: "STC Map-3", 3: "STC Map-4" }
class Content_418(ContentTable):
    values = { 0: "SVC packets", 1: "MSP packets", 2: "Route packets" }
class Content_419(ContentTable):
    values = { 0: "Scheduled Off-Block Time", 1: "Estimated Off-Block Time", 2: "Estimated Take-Off Time", 3: "Actual Off-Block Time", 4: "Predicted Time at Runway Hold", 5: "Actual Time at Runway Hold", 6: "Actual Line-Up Time", 7: "Actual Take-Off Time", 8: "Estimated Time of Arrival", 9: "Predicted Landing Time", 10: "Actual Landing Time", 11: "Actual Time off Runway", 12: "Predicted Time to Gate", 13: "Actual On-Block Time" }
class Content_420(ContentTable):
    values = { 0: "Scheduled off-block time", 1: "Estimated off-block time", 2: "Estimated take-off time", 3: "Actual off-block time", 4: "Predicted time at runway hold", 5: "Actual time at runway hold", 6: "Actual line-up time", 7: "Actual take-off time", 8: "Estimated time of arrival", 9: "Predicted landing time", 10: "Actual landing time", 11: "Actual time off runway", 12: "Predicted time to gate", 13: "Actual on-block time" }
class Content_421(ContentTable):
    values = { 0: "Seconds available", 1: "Seconds not available" }
class Content_422(ContentTable):
    values = { 0: "Standby", 1: "Exec" }
class Content_423(ContentTable):
    values = { 0: "Surveillance Mode A (alert bit or periodic)", 1: "Comm-A", 2: "Ground Initiated Comm-B", 3: "Air Initiated Comm-B", 4: "Broadcast Comm-B", 5: "Comm-C", 6: "Comm-D", 7: "Reserved for future use", 8: "Reserved for future use", 9: "Reserved for future use", 10: "Reserved for future use", 11: "Reserved for future use", 12: "Reserved for future use", 13: "Reserved for future use", 14: "Reserved for future use", 15: "Reserved for future use" }
class Content_424(ContentTable):
    values = { 0: "System is released for operational use", 1: "Operational use of System is inhibited" }
class Content_425(ContentTable):
    values = { 0: "System is released for operational use", 1: "Operational use of System is inhibited, i.e. the data shall be discarded by an operational SDPS" }
class Content_426(ContentTable):
    values = { 0: "TCAS II or ACAS RA not active", 1: "TCAS RA active" }
class Content_427(ContentTable):
    values = { 0: "TCAS operational", 1: "TCAS not operational" }
class Content_428(ContentTable):
    values = { 0: "TCP compliance", 1: "TCP non-compliance" }
class Content_429(ContentTable):
    values = { 0: "TCP number available", 1: "TCP number not available" }
class Content_430(ContentTable):
    values = { 0: "TOV available", 1: "TOV not available" }
class Content_431(ContentTable):
    values = { 0: "TTR not available", 1: "TTR available" }
class Content_432(ContentTable):
    values = { 0: "TU1/RU1 has NOT contributed to the target detection", 1: "TU1/RU1 has contributed to the target detection" }
class Content_433(ContentTable):
    values = { 0: "TU2/RU2 has NOT contributed to the target detection", 1: "TU2/RU2 has contributed to the target detection" }
class Content_434(ContentTable):
    values = { 0: "TU3/RU3 has NOT contributed to the target detection", 1: "TU3/RU3 has contributed to the target detection" }
class Content_435(ContentTable):
    values = { 0: "TU4/RU4 has NOT contributed to the target detection", 1: "TU4/RU4 has contributed to the target detection" }
class Content_436(ContentTable):
    values = { 0: "TU5/RU5 has NOT contributed to the target detection", 1: "TU5/RU5 has contributed to the target detection" }
class Content_437(ContentTable):
    values = { 0: "TU6/RU6 has NOT contributed to the target detection", 1: "TU6/RU6 has contributed to the target detection" }
class Content_438(ContentTable):
    values = { 0: "TU7/RU7 has NOT contributed to the target detection", 1: "TU7/RU7 has contributed to the target detection" }
class Content_439(ContentTable):
    values = { 0: "TU8/RU8 has NOT contributed to the target detection", 1: "TU8/RU8 has contributed to the target detection" }
class Content_440(ContentTable):
    values = { 0: "Target is not 1090 ES IN capable", 1: "Target is 1090 ES IN capable" }
class Content_441(ContentTable):
    values = { 0: "Target is not UAT IN capable", 1: "Target is UAT IN capable" }
class Content_442(ContentTable):
    values = { 0: "Target not in Blanked Zone", 1: "Target in Blanked Zone" }
class Content_443(ContentTable):
    values = { 0: "Target not in Blind Zone", 1: "Target in Blind Zone" }
class Content_444(ContentTable):
    values = { 0: "Target not locked out by this radar", 1: "Target locked out by this radar" }
class Content_445(ContentTable):
    values = { 0: "Target report from antenna 1", 1: "Target report from antenna 2" }
class Content_446(ContentTable):
    values = { 0: "Tentative Track with One Plot", 1: "Tentative Track with at least Two Plots", 2: "Pre-Confirmed Track", 3: "Confirmed Track" }
class Content_447(ContentTable):
    values = { 0: "Test Target Operative", 1: "Test Target Failure" }
class Content_448(ContentTable):
    values = { 0: "The CQF calculation method is not supported", 1: "The CQF is minimum", 126: "The CQF is maximum", 127: "The CQF is undefined according to the calculation method" }
class Content_449(ContentTable):
    values = { 0: "The GICB extraction is attempted according to the periodicity", 1: "There will no GICB attempts" }
class Content_450(ContentTable):
    values = { 0: "The MOPS Version is supported by the GS", 1: "The MOPS Version is not supported by the GS" }
class Content_451(ContentTable):
    values = { 0: "The aircraft is in the Datalink coverage map of the interrogator", 1: "The aircraft is not in the Datalink coverage map of the interrogator" }
class Content_452(ContentTable):
    values = { 0: "The extracted GICB must be sent only on the Data Link line", 1: "The extracted GICB must be sent only on the Surveillance line", 2: "The extracted GICB must be sent both on the Data Link and on the Surveillance lines" }
class Content_453(ContentTable):
    values = { 0: "The interrogator is enabled to extract frames", 1: "The interrogator is disabled to extract frames" }
class Content_454(ContentTable):
    values = { 0: "The interrogator is enabled to uplink frames", 1: "The interrogator is disabled to uplink frames" }
class Content_455(ContentTable):
    values = { 0: "The interrogators current ability to uplink/downlink frames (UCS/DCS) and the content of the Aircraft_report could be changed using D_Data_link_command", 1: "The interrogators current ability to uplink/downlink frames (UCS/DCS) and the content of the Aircraft_report cannot be changed using D_Data_link_command" }
class Content_456(ContentTable):
    values = { 0: "The next Aircraft_report may not include Aircraft_ID", 1: "The next Aircraft_report shall include Aircraft_ID" }
class Content_457(ContentTable):
    values = { 0: "The next Aircraft_report may not include D_COM", 1: "The next Aircraft_report shall include D_COM" }
class Content_458(ContentTable):
    values = { 0: "The next Aircraft_report may not include D_CQF", 1: "The next Aircraft_report shall include D_CQF" }
class Content_459(ContentTable):
    values = { 0: "The next Aircraft_report may not include D_CQF_method", 1: "The next Aircraft_report shall include D_CQF_method" }
class Content_460(ContentTable):
    values = { 0: "The next Aircraft_report may not include D_Cartesian_position", 1: "The next Aircraft_report shall include D_Cartesian_position" }
class Content_461(ContentTable):
    values = { 0: "The next Aircraft_report may not include D_Data_link_status", 1: "The next Aircraft_report shall include D_Data_link_status" }
class Content_462(ContentTable):
    values = { 0: "The next Aircraft_report may not include D_ECA", 1: "The next Aircraft_report shall include D_ECA" }
class Content_463(ContentTable):
    values = { 0: "The next Aircraft_report may not include D_Polar_position", 1: "The next Aircraft_report shall include D_Polar_position" }
class Content_464(ContentTable):
    values = { 0: "The next Aircraft_report may not include Heading", 1: "The next Aircraft_report shall include Heading" }
class Content_465(ContentTable):
    values = { 0: "The next Aircraft_report may not include Height", 1: "The next Aircraft_report shall include Height" }
class Content_466(ContentTable):
    values = { 0: "The next Aircraft_report may not include Mode_A", 1: "The next Aircraft_report shall include Mode_A" }
class Content_467(ContentTable):
    values = { 0: "The next Aircraft_report may not include Speed", 1: "The next Aircraft_report shall include Speed" }
class Content_468(ContentTable):
    values = { 0: "The periodicity may not be strictly respected", 1: "The periodicity shall be strictly respected" }
class Content_469(ContentTable):
    values = { 0: "Today", 1: "Yesterday", 2: "Tomorrow" }
class Content_470(ContentTable):
    values = { 0: "Today", 1: "Yesterday", 2: "Tomorrow", 3: "Invalid" }
class Content_471(ContentTable):
    values = { 0: "Track Alive", 1: "Track Terminated by User Request" }
class Content_472(ContentTable):
    values = { 0: "Track is not associated with an SCN Plot", 1: "Track is associated with an SCN Plot" }
class Content_473(ContentTable):
    values = { 0: "Track not resulting from amalgamation process", 1: "Track resulting from amalgamation process" }
class Content_474(ContentTable):
    values = { 0: "Track still alive", 1: "End of track lifetime(last report for this track)" }
class Content_475(ContentTable):
    values = { 0: "Tracking performed in 'Sensor Plane', i.e. neither slant range correction nor projection was applied", 1: "Slant range correction and a suitable projection technique are used to track in a 2D.reference plane, tangential to the earth model at the Sensor Site co-ordinates" }
class Content_476(ContentTable):
    values = { 0: "Tracking performed in so-called 'Radar Plane', i.e. neither slant range correction nor stereographical projection was applied", 1: "Slant range correction and a suitable projection technique are used to track in a 2D.reference plane, tangential to the earth model at the Radar Site co-ordinates" }
class Content_477(ContentTable):
    values = { 0: "Trajectory Intent Data is available for this aircraft", 1: "Trajectory Intent Data is not available for this aircraft" }
class Content_478(ContentTable):
    values = { 0: "Trajectory Intent Data is valid", 1: "Trajectory Intent Data is not valid" }
class Content_479(ContentTable):
    values = { 0: "Trajectory intent data is available for this aircraft", 1: "Trajectory intent data is not available for this aircraft" }
class Content_480(ContentTable):
    values = { 0: "Trajectory intent data is valid", 1: "Trajectory intent data is not valid" }
class Content_481(ContentTable):
    values = { 0: "Transponder Ground bit not set", 1: "Transponder Ground bit set" }
class Content_482(ContentTable):
    values = { 0: "Transponder Ground bit not set or unknown", 1: "Transponder Ground bit set" }
class Content_483(ContentTable):
    values = { 0: "Transponder SI capable", 1: "Transponder not SI capable" }
class Content_484(ContentTable):
    values = { 0: "Transponder ground bit not set or unknown", 1: "Transponder Ground Bit set" }
class Content_485(ContentTable):
    values = { 0: "True North", 1: "Magnetic North" }
class Content_486(ContentTable):
    values = { 0: "True target track", 1: "Ghost target track" }
class Content_487(ContentTable):
    values = { 0: "UAT available", 1: "UAT not available" }
class Content_488(ContentTable):
    values = { 0: "UAT multilateration", 1: "No UAT multilateration" }
class Content_489(ContentTable):
    values = { 0: "UC shall be ignored", 1: "UC shall be taken into account" }
class Content_490(ContentTable):
    values = { 0: "Unavailable, Unknown, or less than 70 W", 1: "70 W", 2: "125 W", 3: "200 W" }
class Content_491(ContentTable):
    values = { 0: "Undetermined", 1: "Aircraft", 2: "Ground vehicle", 3: "Helicopter" }
class Content_492(ContentTable):
    values = { 0: "Undetermined", 1: "Loop start", 2: "Loop finish" }
class Content_493(ContentTable):
    values = { 0: "Unknown", 1: "25 ft", 2: "100 ft" }
class Content_494(ContentTable):
    values = { 0: "Unknown", 1: "ACAS not operational", 2: "ACAS operartional", 3: "Invalid" }
class Content_495(ContentTable):
    values = { 0: "Unknown", 1: "ACAS not operational", 2: "ACAS operational", 3: "Invalid" }
class Content_496(ContentTable):
    values = { 0: "Unknown", 1: "ATC equipment maintenance", 2: "Airport maintenance", 3: "Fire", 4: "Bird scarer", 5: "Snow plough", 6: "Runway sweeper", 7: "Emergency", 8: "Police", 9: "Bus", 10: "Tug (push/tow)", 11: "Grass cutter", 12: "Fuel", 13: "Baggage", 14: "Catering", 15: "Aircraft maintenance", 16: "Flyco (follow me)" }
class Content_497(ContentTable):
    values = { 0: "Unknown", 1: "Aircraft Altitude (Holding Altitude)", 2: "MCP/FCU Selected Altitude", 3: "FMS Selected Altitude" }
class Content_498(ContentTable):
    values = { 0: "Unknown", 1: "Aircraft altitude", 2: "FCU/MCP selected altitude", 3: "FMS selected altitude" }
class Content_499(ContentTable):
    values = { 0: "Unknown", 1: "Aircraft equiped with CDTI" }
class Content_500(ContentTable):
    values = { 0: "Unknown", 1: "Approved", 2: "Exempt", 3: "Not Approved" }
class Content_501(ContentTable):
    values = { 0: "Unknown", 1: "Approved", 2: "Exempt", 3: "Not approved" }
class Content_502(ContentTable):
    values = { 0: "Unknown", 1: "Differencial Correction", 2: "NO Differencial Correction", 3: "Invalid" }
class Content_503(ContentTable):
    values = { 0: "Unknown", 1: "Differential correction", 2: "No differential correction", 3: "Invalid" }
class Content_504(ContentTable):
    values = { 0: "Unknown", 1: "Failed", 2: "Disabled", 3: "Degraded", 4: "Normal", 5: "Initialisation" }
class Content_505(ContentTable):
    values = { 0: "Unknown", 1: "Fly by waypoint (LT)", 2: "Fly over waypoint (LT)", 3: "Hold pattern (LT)", 4: "Procedure hold (LT)", 5: "Procedure turn (LT)", 6: "RF leg (LT)", 7: "Top of climb (VT)", 8: "Top of descent (VT)", 9: "Start of level (VT)", 10: "Cross-over altitude (VT)", 11: "Transition altitude (VT)" }
class Content_506(ContentTable):
    values = { 0: "Unknown", 1: "Forward", 2: "Backward", 3: "Static" }
class Content_507(ContentTable):
    values = { 0: "Unknown", 1: "General Air Traffic", 2: "Operational Air Traffic", 3: "Not applicable" }
class Content_508(ContentTable):
    values = { 0: "Unknown", 1: "Multiple Navigation not operational", 2: "Multiple Navigation operartional", 3: "Invalid" }
class Content_509(ContentTable):
    values = { 0: "Unknown", 1: "Multiple navigational aids not operating", 2: "Multiple navigational aids operating", 3: "Invalid" }
class Content_510(ContentTable):
    values = { 0: "Unknown", 1: "On stand", 2: "Taxiing for departure", 3: "Taxiing for arrival", 4: "Runway for departure", 5: "Runway for arrival", 6: "Hold for departure", 7: "Hold for arrival", 8: "Push back", 9: "On finals" }
class Content_511(ContentTable):
    values = { 0: "Unknown", 1: "Transponder #1 (left/pilot side or single)", 2: "Transponder #2 (right/co-pilot side)", 3: "Transponder #3 (auxiliary or Backup)" }
class Content_512(ContentTable):
    values = { 0: "Unknown type of movement", 1: "Taking-off", 2: "Landing", 3: "Other types of movement" }
class Content_513(ContentTable):
    values = { 0: "VDL Mode 4 available", 1: "VDL Mode 4 not available" }
class Content_514(ContentTable):
    values = { 0: "VDL Mode 4 multilateration", 1: "No VDL Mode 4 multilateration" }
class Content_515(ContentTable):
    values = { 0: "Valid", 1: "Invalid" }
class Content_516(ContentTable):
    values = { 0: "Value in defined range", 1: "Value exceeds defined range" }
class Content_517(ContentTable):
    values = { 0: "Vertical Navigation not active", 1: "Vertical Navigation active" }
class Content_518(ContentTable):
    values = { 0: "Vertical Only", 1: "Horizontal Only", 2: "Blended", 3: "Vertical Only or Horizontal Only per intruder" }
class Content_519(ContentTable):
    values = { 0: "X-Pulse not present", 1: "X-pulse present" }
class Content_520(ContentTable):
    values = { 0: "X-pulse set to zero or no Mode 1 reply", 1: "X-pulse set to one (present)" }
class Content_521(ContentTable):
    values = { 0: "X-pulse set to zero or no Mode 2 reply", 1: "X-pulse set to one (present)" }
class Content_522(ContentTable):
    values = { 0: "X-pulse set to zero or no Mode 3/A reply", 1: "X-pulse set to one (present)" }
class Content_523(ContentTable):
    values = { 0: "X-pulse set to zero or no Mode C reply", 1: "X-pulse set to one (present)" }
class Content_524(ContentTable):
    values = { 0: "X-pulse set to zero or no authenticated Data reply or Report received", 1: "X-pulse set to one" }
class Content_525(ContentTable):
    values = { 0: "X-pulse set to zero or no authenticated Data reply or Report received", 1: "X-pulse set to one (present)" }
class Content_526(ContentTable):
    values = { 0: "Yes", 1: "No" }
class Content_527(ContentTable):
    values = { 0: "the downlink flow shall be enabled", 1: "the downlink flow shall be stopped" }
class Content_528(ContentTable):
    values = { 0: "the uplink flow shall be enabled", 1: "the uplink flow shall be stopped" }
class Content_529(ContentTable):
    values = { 1: "ADS-B VDL4", 2: "ADS-B Ext Squitter", 3: "ADS-B UAT", 4: "TIS-B VDL4", 5: "TIS-B Ext Squitter", 6: "TIS-B UAT", 7: "FIS-B VDL4", 8: "GRAS VDL4", 9: "MLT" }
class Content_530(ContentTable):
    values = { 1: "Alive Message (AM)", 2: "Route Adherence Monitor Longitudinal Deviation (RAMLD)", 3: "Route Adherence Monitor Heading Deviation (RAMHD)", 4: "Minimum Safe Altitude Warning (MSAW)", 5: "Area Proximity Warning (APW)", 6: "Clearance Level Adherence Monitor (CLAM)", 7: "Short Term Conflict Alert (STCA)", 8: "Approach Path Monitor (APM)", 9: "RIMCAS Arrival / Landing Monitor (ALM)", 10: "RIMCAS Arrival / Departure Wrong Runway Alert (WRA)", 11: "RIMCAS Arrival / Departure Opposite Traffic Alert (OTA)", 12: "RIMCAS Departure Monitor (RDM)", 13: "RIMCAS Runway / Taxiway Crossing Monitor (RCM)", 14: "RIMCAS Taxiway Separation Monitor (TSM)", 15: "RIMCAS Unauthorized Taxiway Movement Monitor(UTMM)", 16: "RIMCAS Stop Bar Overrun Alert (SBOA)", 17: "End Of Conflict (EOC)", 18: "ACAS Resolution Advisory (ACASRA)", 19: "Near Term Conflict Alert (NTCA)", 20: "Downlinked Barometric Pressure Setting Monitor (DBPSM)", 21: "Speed Adherence Monitor (SAM)", 22: "Outside Controlled Airspace Tool (OCAT)", 23: "Vertical Conflict Detection (VCD)", 24: "Vertical Rate Adherence Monitor (VRAM)", 25: "Cleared Heading Adherence Monitor (CHAM)", 26: "Downlinked Selected Altitude Monitor (DSAM)", 27: "Holding Adherence Monitor (HAM)", 28: "Vertical Path Monitor (VPM)", 29: "RIMCAS Taxiway Traffic Alert (TTA)", 30: "RIMCAS Arrival/Departure Close Runway Alert (CRA)", 31: "RIMCAS Arrival/Departure Aircraft Separation Monitor (ASM)", 32: "RIMCAS ILS Area Violation Monitor (IAVM)", 33: "Final Target Distance Indicator (FTD)", 34: "Initial Target Distance Indicator (ITD)", 35: "Wake Vortex Indicator Infringement Alert (IIA)", 36: "Sequence Warning (SQW)", 37: "Catch Up Warning (CUW)", 38: "Conflicting ATC Clearances (CATC)", 39: "No ATC Clearance (NOCLR)", 40: "Aircraft Not Moving despite ATC Clearance (NOMOV)", 41: "Aircraft leaving/entering the aerodrome area without proper handover (NOH)", 42: "Wrong Runway or Taxiway Type (WRTY)", 43: "Stand Occupied (STOCC)", 44: "Ongoing Alert (ONGOING)", 97: "Lost Track Warning (LTW)", 98: "Holding Volume Infringement (HVI)", 99: "Airspace Infringement Warning (AIW)" }
class Content_531(ContentTable):
    values = { 1: "Flight Plan to track initial correlation", 2: "Miniplan update", 3: "End of correlation", 4: "Miniplan Cancellation", 5: "Retained Miniplan" }
class Content_532(ContentTable):
    values = { 1: "Ground station status report", 2: "Service status report", 3: "Service statistics report" }
class Content_533(ContentTable):
    values = { 1: "Information sent by an FPPS" }
class Content_534(ContentTable):
    values = { 1: "Light aircraft <= 7000 kg", 2: "Reserved", 3: "7000 kg &lt; medium aircraft &lt; 136000 kg", 4: "Reserved", 5: "136000 kg <= heavy aircraft", 6: "Highly manoeuvrable (5g acceleration capability) and high speed (&gt;400 knots cruise)", 7: "Reserved", 8: "Reserved", 9: "Reserved", 10: "Rotocraft", 11: "Glider / sailplane", 12: "Lighter-than-air", 13: "Unmanned aerial vehicle", 14: "Space / transatmospheric vehicle", 15: "Ultralight / handglider / paraglider", 16: "Parachutist / skydiver", 17: "Reserved", 18: "Reserved", 19: "Reserved", 20: "Surface emergency vehicle", 21: "Surface service vehicle", 22: "Fixed ground or tethered obstruction", 23: "Reserved", 24: "Reserved" }
class Content_535(ContentTable):
    values = { 1: "Light aircraft <= 7000 kg", 2: "Reserved", 3: "7000 kg < Medium aircraft < 136000 kg", 4: "Reserved", 5: "136000 kg <= Heavy aircraft", 6: "Highly manoeuvrable (5g acceleration capability) and high speed (>400 knots cruise)", 7: "Reserved", 8: "Reserved", 9: "Reserved", 10: "Rotocraft", 11: "Glider / sailplane", 12: "Lighter-than-air", 13: "Unmanned aerial vehicle", 14: "Space / transatmospheric vehicle", 15: "Ultralight / handglider / paraglider", 16: "Parachutist / skydiver", 17: "Reserved", 18: "Reserved", 19: "Reserved", 20: "Surface emergency vehicle", 21: "Surface service vehicle", 22: "Fixed ground or tethered obstruction", 23: "Reserved", 24: "Reserved" }
class Content_536(ContentTable):
    values = { 1: "Light aircraft =< 7000 kg", 2: "Reserved", 3: "7000 kg < medium aircraft < 136000 kg", 4: "Reserved", 5: "136000 kg <= heavy aircraft", 6: "Highly manoeuvrable (5g acceleration capability) and high speed (>400 knots cruise)", 7: "Reserved", 8: "Reserved", 9: "Reserved", 10: "Rotocraft", 11: "Glider / sailplane", 12: "Lighter-than-air", 13: "Unmanned aerial vehicle", 14: "Space / transatmospheric vehicle", 15: "Ultralight / handglider / paraglider", 16: "Parachutist / skydiver", 17: "Reserved", 18: "Reserved", 19: "Reserved", 20: "Surface emergency vehicle", 21: "Surface service vehicle", 22: "Fixed ground or tethered obstruction", 23: "Reserved", 24: "Reserved" }
class Content_537(ContentTable):
    values = { 1: "Measurement Plot", 2: "Measurement Track", 3: "Sensor Centric Plot", 4: "Sensor Centric Track", 5: "Track End Message" }
class Content_538(ContentTable):
    values = { 1: "Monobit Resolution (1 bit)", 2: "Low Resolution (2 bits)", 3: "Medium Resolution (4 bits)", 4: "High Resolution (8 bits)", 5: "Very High Resolution (16 bits)", 6: "Ultra High Resolution (32 bits)" }
class Content_539(ContentTable):
    values = { 1: "North marker message", 2: "Sector crossing message", 3: "Geographical filtering message", 4: "Jamming strobe message" }
class Content_540(ContentTable):
    values = { 1: "North marker message", 2: "Sector crossing message", 3: "Geographical filtering message", 4: "Jamming strobe message", 5: "Solar Storm Message" }
class Content_541(ContentTable):
    values = { 1: "North marker message", 2: "Sector crossing message", 3: "Geographical filtering message", 4: "Jamming strobe message", 5: "Solar Storm Message", 6: "SSR Jamming Strobe Message", 7: "Mode S Jamming Strobe Message" }
class Content_542(ContentTable):
    values = { 1: "North marker message", 2: "Sector crossing message", 3: "South marker message", 8: "Activation of blind zone filtering", 9: "Stop of blind zone filtering" }
class Content_543(ContentTable):
    values = { 1: "Polar vector", 2: "Cartesian vector of start point/length", 3: "Contour record", 4: "Cartesian start point and end point vector", 254: "SOP message", 255: "EOP message" }
class Content_544(ContentTable):
    values = { 1: "SDPS Status", 2: "End of Batch", 3: "Service Status Report" }
class Content_545(ContentTable):
    values = { 1: "Service degradation", 2: "Service degradation ended", 3: "Main radar out of service", 4: "Service interrupted by the operator", 5: "Service interrupted due to contingency", 6: "Ready for service restart after contingency", 7: "Service ended by the operator", 8: "Failure of user main radar", 9: "Service restarted by the operator", 10: "Main radar becoming operational", 11: "Main radar becoming degraded", 12: "Service continuity interrupted due to disconnection with adjacent unit", 13: "Service continuity restarted", 14: "Service synchronised on backup radar", 15: "Service synchronised on main radar", 16: "Main and backup radar, if any, failed" }
class Content_546(ContentTable):
    values = { 1: "Sole primary plots", 2: "Sole SSR plots", 3: "Combined plots" }
class Content_547(ContentTable):
    values = { 1: "Start of Update Cycle", 2: "Periodic Status Message", 3: "Event-triggered Status Message" }
class Content_548(ContentTable):
    values = { 1: "System Configuration", 2: "Transmitter / Receiver Configuration" }
class Content_549(ContentTable):
    values = { 1: "System Position Report", 2: "System Bearing Report", 3: "System Position Report of conflicting transmission", 4: "System Detection End Report", 5: "Sensor Data Report" }
class Content_550(ContentTable):
    values = { 1: "Target Report", 2: "Start of Update Cycle", 3: "Periodic Status Message", 4: "Event-triggered Status Message" }
class Content_551(ContentTable):
    values = { 1: "Target reports, flight plan data and basic alerts", 2: "Manual attachment of flight plan to track", 3: "Manual detachment of flight plan to track", 4: "Insertion of flight plan data", 5: "Suppression of flight plan data", 6: "Modification of flight plan data", 7: "Holdbar status" }
class Content_552(ContentTable):
    values = { 1: "Towing aircraft", 2: "FOLLOW-ME operation", 3: "Runway check", 4: "Emergency operation (fire, medical...)", 5: "Work in progress (maintenance, birds scarer, sweepers...)" }
class Content_553(ContentTable):
    values = { 1: "Towing aircraft", 2: "“Follow me” operation", 3: "Runway check", 4: "Emergency operation (fire, medical...)", 5: "Work in progress (maintenance, birds scarer, sweepers...)" }
class Content_554(ContentTable):
    values = { 1: "Video Summary message", 2: "Video message" }
class Content_555(ContentTable):
    values = { 1: "Warning", 2: "Faulted", 3: "Good" }
class Content_556(ContentTable):
    values = { 2: "Cartesian vector", 253: "Intermediate-update-step message", 254: "Start-of-picture message", 255: "End-of-picture message" }
class Content_557(ContentTable):
    values = { 3: "Reserved", 2: "TOMRp whole seconds = (I021/073) Whole seconds - 1", 1: "TOMRp whole seconds = (I021/073) Whole seconds + 1", 0: "TOMRp whole seconds = (I021/073) Whole seconds" }
class Content_558(ContentTable):
    values = { 3: "Reserved", 2: "TOMRp whole seconds = (I021/075) Whole seconds - 1", 1: "TOMRp whole seconds = (I021/075) Whole seconds + 1", 0: "TOMRp whole seconds = (I021/075) Whole seconds" }
class Content_559(ContentTable):
    values = { 76: "Light", 77: "Medium", 72: "Heavy", 74: "Super" }
class Content_560(ContentString):
    t = StringAscii
class Content_561(ContentString):
    t = StringICAO
class Content_562(ContentString):
    t = StringOctal
class Content_563(ContentInteger):
    sig = Unsigned
class Content_564(ContentQuantity):
    sig = Signed
    lsb = 1.0
    unit = ""
class Content_565(ContentQuantity):
    sig = Signed
    lsb = 1.0
    unit = "NM"
class Content_566(ContentQuantity):
    sig = Signed
    lsb = 1.0
    unit = "dB"
class Content_567(ContentQuantity):
    sig = Signed
    lsb = 1.0
    unit = "dBm"
class Content_568(ContentQuantity):
    sig = Signed
    lsb = 1.0
    unit = "m"
class Content_569(ContentQuantity):
    sig = Signed
    lsb = 1.0
    unit = "m/s"
class Content_570(ContentQuantity):
    sig = Signed
    lsb = 1.0
    unit = "ms"
class Content_571(ContentQuantity):
    sig = Signed
    lsb = 2.0
    unit = "ns"
class Content_572(ContentQuantity):
    sig = Signed
    lsb = 10.0
    unit = "ft"
class Content_573(ContentQuantity):
    sig = Signed
    lsb = 25.0
    unit = "ft"
class Content_574(ContentQuantity):
    sig = Signed
    lsb = 32.0
    unit = "m"
class Content_575(ContentQuantity):
    sig = Signed
    lsb = 0.5
    unit = "m"
class Content_576(ContentQuantity):
    sig = Signed
    lsb = 0.1
    unit = "m"
class Content_577(ContentQuantity):
    sig = Signed
    lsb = 0.1
    unit = "m/s"
class Content_578(ContentQuantity):
    sig = Signed
    lsb = 1.0e-2
    unit = "dBµV"
class Content_579(ContentQuantity):
    sig = Signed
    lsb = 1.0e-2
    unit = "m"
class Content_580(ContentQuantity):
    sig = Signed
    lsb = 1.0e-2
    unit = "m/s"
class Content_581(ContentQuantity):
    sig = Signed
    lsb = 1.0e-2
    unit = "m/s2"
class Content_582(ContentQuantity):
    sig = Signed
    lsb = 1.0e-2
    unit = "°"
class Content_583(ContentQuantity):
    sig = Signed
    lsb = 1.0e-5
    unit = ""
class Content_584(ContentQuantity):
    sig = Signed
    lsb = 0.25
    unit = "FL"
class Content_585(ContentQuantity):
    sig = Signed
    lsb = 0.25
    unit = "m"
class Content_586(ContentQuantity):
    sig = Signed
    lsb = 0.25
    unit = "m/s"
class Content_587(ContentQuantity):
    sig = Signed
    lsb = 0.25
    unit = "m/s2"
class Content_588(ContentQuantity):
    sig = Signed
    lsb = 0.25
    unit = "°/s"
class Content_589(ContentQuantity):
    sig = Signed
    lsb = 0.25
    unit = "°C"
class Content_590(ContentQuantity):
    sig = Signed
    lsb = 6.25e-2
    unit = "m/s"
class Content_591(ContentQuantity):
    sig = Signed
    lsb = 6.25e-2
    unit = "m/s2"
class Content_592(ContentQuantity):
    sig = Signed
    lsb = 3.125e-2
    unit = "°/s"
class Content_593(ContentQuantity):
    sig = Signed
    lsb = 1.5625e-2
    unit = "NM"
class Content_594(ContentQuantity):
    sig = Signed
    lsb = 1.5625e-2
    unit = "m/s2"
class Content_595(ContentQuantity):
    sig = Signed
    lsb = 7.8125e-3
    unit = ""
class Content_596(ContentQuantity):
    sig = Signed
    lsb = 7.8125e-3
    unit = "NM"
class Content_597(ContentQuantity):
    sig = Signed
    lsb = 7.8125e-3
    unit = "s"
class Content_598(ContentQuantity):
    sig = Signed
    lsb = 3.90625e-3
    unit = "NM"
class Content_599(ContentQuantity):
    sig = Signed
    lsb = 3.90625e-3
    unit = "NM/s"
class Content_600(ContentQuantity):
    sig = Signed
    lsb = 6.103515625e-5
    unit = "NM/s"
class Content_601(ContentQuantity):
    sig = Signed
    lsb = 0.15
    unit = "°"
class Content_602(ContentQuantity):
    sig = Signed
    lsb = 6.25
    unit = "ft"
class Content_603(ContentQuantity):
    sig = Signed
    lsb = 6.25
    unit = "ft/min"
class Content_604(ContentQuantity):
    sig = Signed
    lsb = 2.74658203125e-3
    unit = "°"
class Content_605(ContentQuantity):
    sig = Signed
    lsb = 2.74658203125e-3
    unit = "°/s"
class Content_606(ContentQuantity):
    sig = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"
class Content_607(ContentQuantity):
    sig = Signed
    lsb = 5.364418029785156e-6
    unit = "°"
class Content_608(ContentQuantity):
    sig = Signed
    lsb = 1.6763806343078613e-7
    unit = "°"
class Content_609(ContentQuantity):
    sig = Signed
    lsb = 8.381903171539307e-8
    unit = "°"
class Content_610(ContentQuantity):
    sig = Signed
    lsb = 4.190951585769653e-8
    unit = "°"
class Content_611(ContentQuantity):
    sig = Signed
    lsb = 2.197265625e-2
    unit = "°"
class Content_612(ContentQuantity):
    sig = Signed
    lsb = 5.4931640625e-3
    unit = "°"
class Content_613(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = ""
class Content_614(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "%"
class Content_615(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "MHz"
class Content_616(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "NM"
class Content_617(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "fs"
class Content_618(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "kt"
class Content_619(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "m"
class Content_620(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "m/s"
class Content_621(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "ms"
class Content_622(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "ns"
class Content_623(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "s"
class Content_624(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "°"
class Content_625(ContentQuantity):
    sig = Unsigned
    lsb = 16.0
    unit = "m"
class Content_626(ContentQuantity):
    sig = Unsigned
    lsb = 25.0
    unit = "ft"
class Content_627(ContentQuantity):
    sig = Unsigned
    lsb = 100.0
    unit = "m"
class Content_628(ContentQuantity):
    sig = Unsigned
    lsb = 0.5
    unit = "%"
class Content_629(ContentQuantity):
    sig = Unsigned
    lsb = 0.5
    unit = "m"
class Content_630(ContentQuantity):
    sig = Unsigned
    lsb = 0.5
    unit = "s"
class Content_631(ContentQuantity):
    sig = Unsigned
    lsb = 0.1
    unit = "dB"
class Content_632(ContentQuantity):
    sig = Unsigned
    lsb = 0.1
    unit = "hPa"
class Content_633(ContentQuantity):
    sig = Unsigned
    lsb = 0.1
    unit = "m"
class Content_634(ContentQuantity):
    sig = Unsigned
    lsb = 0.1
    unit = "m/s"
class Content_635(ContentQuantity):
    sig = Unsigned
    lsb = 0.1
    unit = "mb"
class Content_636(ContentQuantity):
    sig = Unsigned
    lsb = 0.1
    unit = "s"
class Content_637(ContentQuantity):
    sig = Unsigned
    lsb = 1.0e-2
    unit = "NM"
class Content_638(ContentQuantity):
    sig = Unsigned
    lsb = 1.0e-2
    unit = "m"
class Content_639(ContentQuantity):
    sig = Unsigned
    lsb = 1.0e-2
    unit = "m/s"
class Content_640(ContentQuantity):
    sig = Unsigned
    lsb = 1.0e-2
    unit = "m/s2"
class Content_641(ContentQuantity):
    sig = Unsigned
    lsb = 1.0e-2
    unit = "°"
class Content_642(ContentQuantity):
    sig = Unsigned
    lsb = 8.0e-3
    unit = "Mach"
class Content_643(ContentQuantity):
    sig = Unsigned
    lsb = 0.25
    unit = ""
class Content_644(ContentQuantity):
    sig = Unsigned
    lsb = 0.25
    unit = "FL"
class Content_645(ContentQuantity):
    sig = Unsigned
    lsb = 0.25
    unit = "m"
class Content_646(ContentQuantity):
    sig = Unsigned
    lsb = 0.25
    unit = "m/s"
class Content_647(ContentQuantity):
    sig = Unsigned
    lsb = 0.25
    unit = "m/s2"
class Content_648(ContentQuantity):
    sig = Unsigned
    lsb = 0.25
    unit = "s"
class Content_649(ContentQuantity):
    sig = Unsigned
    lsb = 0.125
    unit = "kt"
class Content_650(ContentQuantity):
    sig = Unsigned
    lsb = 6.25e-2
    unit = "m/s2"
class Content_651(ContentQuantity):
    sig = Unsigned
    lsb = 1.5625e-2
    unit = "m/s"
class Content_652(ContentQuantity):
    sig = Unsigned
    lsb = 1.5625e-2
    unit = "m/s2"
class Content_653(ContentQuantity):
    sig = Unsigned
    lsb = 7.8125e-3
    unit = "NM"
class Content_654(ContentQuantity):
    sig = Unsigned
    lsb = 7.8125e-3
    unit = "m/s2"
class Content_655(ContentQuantity):
    sig = Unsigned
    lsb = 7.8125e-3
    unit = "s"
class Content_656(ContentQuantity):
    sig = Unsigned
    lsb = 3.90625e-3
    unit = "NM"
class Content_657(ContentQuantity):
    sig = Unsigned
    lsb = 3.90625e-3
    unit = "s"
class Content_658(ContentQuantity):
    sig = Unsigned
    lsb = 6.103515625e-5
    unit = "NM/s"
class Content_659(ContentQuantity):
    sig = Unsigned
    lsb = 9.313225746154785e-10
    unit = "s"
class Content_660(ContentQuantity):
    sig = Unsigned
    lsb = 6.25
    unit = "ft"
class Content_661(ContentQuantity):
    sig = Unsigned
    lsb = 6.25
    unit = "ft/min"
class Content_662(ContentQuantity):
    sig = Unsigned
    lsb = 2.8125
    unit = "°"
class Content_663(ContentQuantity):
    sig = Unsigned
    lsb = 0.703125
    unit = "°"
class Content_664(ContentQuantity):
    sig = Unsigned
    lsb = 6.866455078125e-4
    unit = "°"
class Content_665(ContentQuantity):
    sig = Unsigned
    lsb = 6.866455078125e-4
    unit = "°/s"
class Content_666(ContentQuantity):
    sig = Unsigned
    lsb = 5.364418029785156e-6
    unit = "°"
class Content_667(ContentQuantity):
    sig = Unsigned
    lsb = 2.8125
    unit = "°"
class Content_668(ContentQuantity):
    sig = Unsigned
    lsb = 1.40625
    unit = "°"
class Content_669(ContentQuantity):
    sig = Unsigned
    lsb = 8.7890625e-2
    unit = "°"
class Content_670(ContentQuantity):
    sig = Unsigned
    lsb = 4.39453125e-2
    unit = "°"
class Content_671(ContentQuantity):
    sig = Unsigned
    lsb = 5.4931640625e-3
    unit = "°"
class Content_672(ContentQuantity):
    sig = Unsigned
    lsb = 0.152587890625
    unit = "m"

# Variation and Item set
class Variation_0(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_0
class Variation_1(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_8
class Variation_2(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_9
class Variation_3(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_13
class Variation_4(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_15
class Variation_5(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_16
class Variation_6(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_17
class Variation_7(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_18
class Variation_8(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_22
class Variation_9(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_24
class Variation_10(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_26
class Variation_11(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_28
class Variation_12(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_37
class Variation_13(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_48
class Variation_14(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_49
class Variation_15(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_53
class Variation_16(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_54
class Variation_17(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_56
class Variation_18(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_57
class Variation_19(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_59
class Variation_20(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_62
class Variation_21(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_67
class Variation_22(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_70
class Variation_23(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_71
class Variation_24(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_73
class Variation_25(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_75
class Variation_26(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_80
class Variation_27(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_89
class Variation_28(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_92
class Variation_29(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_99
class Variation_30(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_102
class Variation_31(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_104
class Variation_32(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_115
class Variation_33(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_135
class Variation_34(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_145
class Variation_35(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_156
class Variation_36(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_168
class Variation_37(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_173
class Variation_38(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_189
class Variation_39(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_192
class Variation_40(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_193
class Variation_41(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_200
class Variation_42(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_203
class Variation_43(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_204
class Variation_44(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_216
class Variation_45(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_220
class Variation_46(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_225
class Variation_47(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_227
class Variation_48(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_237
class Variation_49(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_238
class Variation_50(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_246
class Variation_51(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_275
class Variation_52(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_276
class Variation_53(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_281
class Variation_54(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_290
class Variation_55(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_291
class Variation_56(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_292
class Variation_57(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_317
class Variation_58(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_327
class Variation_59(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_340
class Variation_60(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_343
class Variation_61(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_347
class Variation_62(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_348
class Variation_63(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_354
class Variation_64(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_360
class Variation_65(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_361
class Variation_66(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_376
class Variation_67(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_394
class Variation_68(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_402
class Variation_69(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_406
class Variation_70(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_421
class Variation_71(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_422
class Variation_72(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_424
class Variation_73(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_425
class Variation_74(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_426
class Variation_75(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_429
class Variation_76(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_432
class Variation_77(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_440
class Variation_78(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_443
class Variation_79(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_444
class Variation_80(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_454
class Variation_81(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_455
class Variation_82(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_456
class Variation_83(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_461
class Variation_84(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_468
class Variation_85(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_473
class Variation_86(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_474
class Variation_87(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_477
class Variation_88(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_479
class Variation_89(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_489
class Variation_90(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_513
class Variation_91(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_516
class Variation_92(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_43
class Variation_93(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_44
class Variation_94(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_45
class Variation_95(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_50
class Variation_96(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_60
class Variation_97(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_194
class Variation_98(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_195
class Variation_99(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_242
class Variation_100(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_274
class Variation_101(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_364
class Variation_102(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_381
class Variation_103(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_382
class Variation_104(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_383
class Variation_105(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_384
class Variation_106(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_392
class Variation_107(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_393
class Variation_108(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_446
class Variation_109(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_494
class Variation_110(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_495
class Variation_111(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_507
class Variation_112(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_512
class Variation_113(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_555
class Variation_114(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_557
class Variation_115(Element):
    bit_offset8 = 0
    bit_size = 2
    content = Content_558
class Variation_116(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_0
class Variation_117(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_4
class Variation_118(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_314
class Variation_119(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_315
class Variation_120(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_316
class Variation_121(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_320
class Variation_122(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_321
class Variation_123(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_339
class Variation_124(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_344
class Variation_125(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_353
class Variation_126(Element):
    bit_offset8 = 0
    bit_size = 3
    content = Content_416
class Variation_127(Element):
    bit_offset8 = 0
    bit_size = 4
    content = Content_0
class Variation_128(Element):
    bit_offset8 = 0
    bit_size = 4
    content = Content_10
class Variation_129(Element):
    bit_offset8 = 0
    bit_size = 4
    content = Content_423
class Variation_130(Element):
    bit_offset8 = 0
    bit_size = 4
    content = Content_505
class Variation_131(Element):
    bit_offset8 = 0
    bit_size = 4
    content = Content_533
class Variation_132(Element):
    bit_offset8 = 0
    bit_size = 4
    content = Content_563
class Variation_133(Element):
    bit_offset8 = 0
    bit_size = 5
    content = Content_0
class Variation_134(Element):
    bit_offset8 = 0
    bit_size = 5
    content = Content_324
class Variation_135(Element):
    bit_offset8 = 0
    bit_size = 5
    content = Content_325
class Variation_136(Element):
    bit_offset8 = 0
    bit_size = 5
    content = Content_419
class Variation_137(Element):
    bit_offset8 = 0
    bit_size = 5
    content = Content_420
class Variation_138(Element):
    bit_offset8 = 0
    bit_size = 5
    content = Content_563
class Variation_139(Element):
    bit_offset8 = 0
    bit_size = 5
    content = Content_564
class Variation_140(Element):
    bit_offset8 = 0
    bit_size = 6
    content = Content_284
class Variation_141(Element):
    bit_offset8 = 0
    bit_size = 6
    content = Content_563
class Variation_142(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_0
class Variation_143(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_68
class Variation_144(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_351
class Variation_145(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_365
class Variation_146(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_366
class Variation_147(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_367
class Variation_148(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_368
class Variation_149(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_537
class Variation_150(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_588
class Variation_151(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_619
class Variation_152(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_623
class Variation_153(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_662
class Variation_154(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_667
class Variation_155(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_0
class Variation_156(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_31
class Variation_157(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_202
class Variation_158(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_231
class Variation_159(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_280
class Variation_160(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_282
class Variation_161(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_331
class Variation_162(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_334
class Variation_163(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_377
class Variation_164(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_378
class Variation_165(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_389
class Variation_166(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_401
class Variation_167(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_496
class Variation_168(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_510
class Variation_169(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_530
class Variation_170(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_532
class Variation_171(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_534
class Variation_172(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_535
class Variation_173(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_536
class Variation_174(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_538
class Variation_175(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_539
class Variation_176(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_540
class Variation_177(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_541
class Variation_178(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_542
class Variation_179(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_543
class Variation_180(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_544
class Variation_181(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_545
class Variation_182(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_547
class Variation_183(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_548
class Variation_184(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_549
class Variation_185(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_550
class Variation_186(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_551
class Variation_187(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_554
class Variation_188(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_556
class Variation_189(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_559
class Variation_190(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_560
class Variation_191(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_563
class Variation_192(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_566
class Variation_193(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_567
class Variation_194(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_568
class Variation_195(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_587
class Variation_196(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_591
class Variation_197(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_595
class Variation_198(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_596
class Variation_199(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_597
class Variation_200(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_598
class Variation_201(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_599
class Variation_202(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_601
class Variation_203(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_611
class Variation_204(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_614
class Variation_205(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_616
class Variation_206(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_623
class Variation_207(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_627
class Variation_208(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_628
class Variation_209(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_630
class Variation_210(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_634
class Variation_211(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_636
class Variation_212(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_640
class Variation_213(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_644
class Variation_214(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_645
class Variation_215(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_646
class Variation_216(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_647
class Variation_217(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_648
class Variation_218(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_653
class Variation_219(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_655
class Variation_220(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_657
class Variation_221(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_658
class Variation_222(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_660
class Variation_223(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_661
class Variation_224(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_668
class Variation_225(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_669
class Variation_226(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_670
class Variation_227(Element):
    bit_offset8 = 0
    bit_size = 9
    content = Content_563
class Variation_228(Element):
    bit_offset8 = 0
    bit_size = 12
    content = Content_591
class Variation_229(Element):
    bit_offset8 = 0
    bit_size = 12
    content = Content_625
class Variation_230(Element):
    bit_offset8 = 0
    bit_size = 12
    content = Content_650
class Variation_231(Element):
    bit_offset8 = 0
    bit_size = 14
    content = Content_0
class Variation_232(Element):
    bit_offset8 = 0
    bit_size = 15
    content = Content_0
class Variation_233(Element):
    bit_offset8 = 0
    bit_size = 15
    content = Content_563
class Variation_234(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_0
class Variation_235(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_563
class Variation_236(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_565
class Variation_237(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_568
class Variation_238(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_570
class Variation_239(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_572
class Variation_240(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_573
class Variation_241(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_574
class Variation_242(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_575
class Variation_243(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_577
class Variation_244(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_578
class Variation_245(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_581
class Variation_246(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_582
class Variation_247(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_583
class Variation_248(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_584
class Variation_249(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_585
class Variation_250(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_586
class Variation_251(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_589
class Variation_252(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_590
class Variation_253(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_593
class Variation_254(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_594
class Variation_255(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_596
class Variation_256(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_600
class Variation_257(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_602
class Variation_258(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_603
class Variation_259(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_604
class Variation_260(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_605
class Variation_261(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_609
class Variation_262(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_612
class Variation_263(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_615
class Variation_264(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_618
class Variation_265(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_619
class Variation_266(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_620
class Variation_267(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_621
class Variation_268(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_623
class Variation_269(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_624
class Variation_270(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_626
class Variation_271(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_629
class Variation_272(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_631
class Variation_273(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_637
class Variation_274(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_638
class Variation_275(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_639
class Variation_276(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_640
class Variation_277(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_641
class Variation_278(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_642
class Variation_279(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_643
class Variation_280(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_644
class Variation_281(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_645
class Variation_282(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_648
class Variation_283(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_651
class Variation_284(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_652
class Variation_285(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_653
class Variation_286(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_654
class Variation_287(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_655
class Variation_288(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_656
class Variation_289(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_658
class Variation_290(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_664
class Variation_291(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_665
class Variation_292(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_666
class Variation_293(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_671
class Variation_294(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_672
class Variation_295(Element):
    bit_offset8 = 0
    bit_size = 20
    content = Content_580
class Variation_296(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_0
class Variation_297(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_560
class Variation_298(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_563
class Variation_299(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_575
class Variation_300(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_576
class Variation_301(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_577
class Variation_302(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_579
class Variation_303(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_580
class Variation_304(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_606
class Variation_305(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_607
class Variation_306(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_623
class Variation_307(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_629
class Variation_308(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_633
class Variation_309(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_634
class Variation_310(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_638
class Variation_311(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_655
class Variation_312(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_656
class Variation_313(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_0
class Variation_314(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_560
class Variation_315(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_563
class Variation_316(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_571
class Variation_317(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_607
class Variation_318(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_608
class Variation_319(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_609
class Variation_320(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_610
class Variation_321(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_617
class Variation_322(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_622
class Variation_323(Element):
    bit_offset8 = 0
    bit_size = 40
    content = Content_0
class Variation_324(Element):
    bit_offset8 = 0
    bit_size = 48
    content = Content_0
class Variation_325(Element):
    bit_offset8 = 0
    bit_size = 48
    content = Content_560
class Variation_326(Element):
    bit_offset8 = 0
    bit_size = 48
    content = Content_561
class Variation_327(Element):
    bit_offset8 = 0
    bit_size = 56
    content = Content_0
class Variation_328(Element):
    bit_offset8 = 0
    bit_size = 56
    content = Content_560
class Variation_329(Element):
    bit_offset8 = 0
    bit_size = 64
    content = Content_0
class Variation_330(Element):
    bit_offset8 = 0
    bit_size = 512
    content = Content_0
class Variation_331(Element):
    bit_offset8 = 0
    bit_size = 2048
    content = Content_0
class Variation_332(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_3
class Variation_333(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_9
class Variation_334(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_14
class Variation_335(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_58
class Variation_336(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_63
class Variation_337(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_81
class Variation_338(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_87
class Variation_339(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_88
class Variation_340(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_93
class Variation_341(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_96
class Variation_342(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_102
class Variation_343(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_103
class Variation_344(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_116
class Variation_345(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_119
class Variation_346(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_128
class Variation_347(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_131
class Variation_348(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_132
class Variation_349(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_136
class Variation_350(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_144
class Variation_351(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_166
class Variation_352(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_171
class Variation_353(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_176
class Variation_354(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_179
class Variation_355(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_185
class Variation_356(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_186
class Variation_357(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_199
class Variation_358(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_206
class Variation_359(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_207
class Variation_360(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_208
class Variation_361(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_211
class Variation_362(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_214
class Variation_363(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_215
class Variation_364(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_227
class Variation_365(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_233
class Variation_366(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_235
class Variation_367(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_261
class Variation_368(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_271
class Variation_369(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_281
class Variation_370(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_285
class Variation_371(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_304
class Variation_372(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_305
class Variation_373(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_360
class Variation_374(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_361
class Variation_375(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_363
class Variation_376(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_371
class Variation_377(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_375
class Variation_378(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_397
class Variation_379(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_399
class Variation_380(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_428
class Variation_381(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_433
class Variation_382(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_441
class Variation_383(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_442
class Variation_384(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_450
class Variation_385(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_453
class Variation_386(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_457
class Variation_387(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_466
class Variation_388(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_478
class Variation_389(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_480
class Variation_390(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_482
class Variation_391(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_486
class Variation_392(Element):
    bit_offset8 = 1
    bit_size = 1
    content = Content_517
class Variation_393(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_0
class Variation_394(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_51
class Variation_395(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_148
class Variation_396(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_196
class Variation_397(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_286
class Variation_398(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_293
class Variation_399(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_306
class Variation_400(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_310
class Variation_401(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_311
class Variation_402(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_312
class Variation_403(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_385
class Variation_404(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_497
class Variation_405(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_498
class Variation_406(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_511
class Variation_407(Element):
    bit_offset8 = 1
    bit_size = 2
    content = Content_518
class Variation_408(Element):
    bit_offset8 = 1
    bit_size = 3
    content = Content_0
class Variation_409(Element):
    bit_offset8 = 1
    bit_size = 3
    content = Content_333
class Variation_410(Element):
    bit_offset8 = 1
    bit_size = 3
    content = Content_344
class Variation_411(Element):
    bit_offset8 = 1
    bit_size = 3
    content = Content_412
class Variation_412(Element):
    bit_offset8 = 1
    bit_size = 3
    content = Content_563
class Variation_413(Element):
    bit_offset8 = 1
    bit_size = 5
    content = Content_319
class Variation_414(Element):
    bit_offset8 = 1
    bit_size = 5
    content = Content_546
class Variation_415(Element):
    bit_offset8 = 1
    bit_size = 5
    content = Content_563
class Variation_416(Element):
    bit_offset8 = 1
    bit_size = 6
    content = Content_563
class Variation_417(Element):
    bit_offset8 = 1
    bit_size = 7
    content = Content_448
class Variation_418(Element):
    bit_offset8 = 1
    bit_size = 7
    content = Content_552
class Variation_419(Element):
    bit_offset8 = 1
    bit_size = 7
    content = Content_553
class Variation_420(Element):
    bit_offset8 = 1
    bit_size = 7
    content = Content_563
class Variation_421(Element):
    bit_offset8 = 1
    bit_size = 7
    content = Content_623
class Variation_422(Element):
    bit_offset8 = 1
    bit_size = 15
    content = Content_0
class Variation_423(Element):
    bit_offset8 = 1
    bit_size = 15
    content = Content_584
class Variation_424(Element):
    bit_offset8 = 1
    bit_size = 15
    content = Content_603
class Variation_425(Element):
    bit_offset8 = 1
    bit_size = 15
    content = Content_618
class Variation_426(Element):
    bit_offset8 = 1
    bit_size = 15
    content = Content_621
class Variation_427(Element):
    bit_offset8 = 1
    bit_size = 15
    content = Content_658
class Variation_428(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_0
class Variation_429(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_15
class Variation_430(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_20
class Variation_431(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_27
class Variation_432(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_40
class Variation_433(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_46
class Variation_434(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_69
class Variation_435(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_74
class Variation_436(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_82
class Variation_437(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_86
class Variation_438(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_94
class Variation_439(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_108
class Variation_440(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_117
class Variation_441(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_121
class Variation_442(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_123
class Variation_443(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_126
class Variation_444(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_130
class Variation_445(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_157
class Variation_446(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_159
class Variation_447(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_160
class Variation_448(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_167
class Variation_449(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_168
class Variation_450(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_172
class Variation_451(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_174
class Variation_452(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_184
class Variation_453(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_188
class Variation_454(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_193
class Variation_455(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_209
class Variation_456(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_210
class Variation_457(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_215
class Variation_458(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_220
class Variation_459(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_227
class Variation_460(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_245
class Variation_461(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_250
class Variation_462(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_251
class Variation_463(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_258
class Variation_464(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_262
class Variation_465(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_263
class Variation_466(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_264
class Variation_467(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_265
class Variation_468(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_266
class Variation_469(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_267
class Variation_470(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_268
class Variation_471(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_269
class Variation_472(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_270
class Variation_473(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_279
class Variation_474(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_281
class Variation_475(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_294
class Variation_476(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_302
class Variation_477(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_303
class Variation_478(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_309
class Variation_479(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_341
class Variation_480(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_343
class Variation_481(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_352
class Variation_482(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_358
class Variation_483(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_360
class Variation_484(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_361
class Variation_485(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_369
class Variation_486(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_371
class Variation_487(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_373
class Variation_488(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_390
class Variation_489(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_396
class Variation_490(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_407
class Variation_491(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_413
class Variation_492(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_422
class Variation_493(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_434
class Variation_494(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_449
class Variation_495(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_454
class Variation_496(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_462
class Variation_497(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_467
class Variation_498(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_471
class Variation_499(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_487
class Variation_500(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_519
class Variation_501(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_526
class Variation_502(Element):
    bit_offset8 = 2
    bit_size = 1
    content = Content_528
class Variation_503(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_12
class Variation_504(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_38
class Variation_505(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_39
class Variation_506(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_61
class Variation_507(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_228
class Variation_508(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_229
class Variation_509(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_323
class Variation_510(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_335
class Variation_511(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_508
class Variation_512(Element):
    bit_offset8 = 2
    bit_size = 2
    content = Content_509
class Variation_513(Element):
    bit_offset8 = 2
    bit_size = 3
    content = Content_0
class Variation_514(Element):
    bit_offset8 = 2
    bit_size = 3
    content = Content_190
class Variation_515(Element):
    bit_offset8 = 2
    bit_size = 3
    content = Content_191
class Variation_516(Element):
    bit_offset8 = 2
    bit_size = 3
    content = Content_328
class Variation_517(Element):
    bit_offset8 = 2
    bit_size = 3
    content = Content_329
class Variation_518(Element):
    bit_offset8 = 2
    bit_size = 6
    content = Content_0
class Variation_519(Element):
    bit_offset8 = 2
    bit_size = 6
    content = Content_563
class Variation_520(Element):
    bit_offset8 = 2
    bit_size = 14
    content = Content_0
class Variation_521(Element):
    bit_offset8 = 2
    bit_size = 14
    content = Content_563
class Variation_522(Element):
    bit_offset8 = 2
    bit_size = 14
    content = Content_573
class Variation_523(Element):
    bit_offset8 = 2
    bit_size = 14
    content = Content_584
class Variation_524(Element):
    bit_offset8 = 2
    bit_size = 14
    content = Content_644
class Variation_525(Element):
    bit_offset8 = 2
    bit_size = 14
    content = Content_655
class Variation_526(Element):
    bit_offset8 = 2
    bit_size = 30
    content = Content_659
class Variation_527(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_0
class Variation_528(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_15
class Variation_529(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_21
class Variation_530(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_30
class Variation_531(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_36
class Variation_532(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_41
class Variation_533(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_84
class Variation_534(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_90
class Variation_535(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_98
class Variation_536(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_100
class Variation_537(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_101
class Variation_538(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_105
class Variation_539(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_106
class Variation_540(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_109
class Variation_541(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_111
class Variation_542(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_120
class Variation_543(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_122
class Variation_544(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_137
class Variation_545(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_144
class Variation_546(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_151
class Variation_547(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_165
class Variation_548(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_177
class Variation_549(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_182
class Variation_550(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_187
class Variation_551(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_193
class Variation_552(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_199
class Variation_553(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_214
class Variation_554(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_219
class Variation_555(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_221
class Variation_556(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_227
class Variation_557(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_252
class Variation_558(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_253
class Variation_559(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_272
class Variation_560(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_273
class Variation_561(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_278
class Variation_562(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_308
class Variation_563(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_327
class Variation_564(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_341
class Variation_565(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_343
class Variation_566(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_350
class Variation_567(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_357
class Variation_568(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_363
class Variation_569(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_370
class Variation_570(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_371
class Variation_571(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_374
class Variation_572(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_415
class Variation_573(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_435
class Variation_574(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_453
class Variation_575(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_458
class Variation_576(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_465
class Variation_577(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_472
class Variation_578(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_476
class Variation_579(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_481
class Variation_580(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_483
class Variation_581(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_485
class Variation_582(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_499
class Variation_583(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_514
class Variation_584(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_515
class Variation_585(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_525
class Variation_586(Element):
    bit_offset8 = 3
    bit_size = 1
    content = Content_527
class Variation_587(Element):
    bit_offset8 = 3
    bit_size = 2
    content = Content_0
class Variation_588(Element):
    bit_offset8 = 3
    bit_size = 2
    content = Content_5
class Variation_589(Element):
    bit_offset8 = 3
    bit_size = 2
    content = Content_242
class Variation_590(Element):
    bit_offset8 = 3
    bit_size = 2
    content = Content_288
class Variation_591(Element):
    bit_offset8 = 3
    bit_size = 2
    content = Content_359
class Variation_592(Element):
    bit_offset8 = 3
    bit_size = 2
    content = Content_452
class Variation_593(Element):
    bit_offset8 = 3
    bit_size = 2
    content = Content_492
class Variation_594(Element):
    bit_offset8 = 3
    bit_size = 2
    content = Content_493
class Variation_595(Element):
    bit_offset8 = 3
    bit_size = 3
    content = Content_295
class Variation_596(Element):
    bit_offset8 = 3
    bit_size = 3
    content = Content_298
class Variation_597(Element):
    bit_offset8 = 3
    bit_size = 3
    content = Content_299
class Variation_598(Element):
    bit_offset8 = 3
    bit_size = 3
    content = Content_300
class Variation_599(Element):
    bit_offset8 = 3
    bit_size = 3
    content = Content_332
class Variation_600(Element):
    bit_offset8 = 3
    bit_size = 3
    content = Content_345
class Variation_601(Element):
    bit_offset8 = 3
    bit_size = 3
    content = Content_346
class Variation_602(Element):
    bit_offset8 = 3
    bit_size = 4
    content = Content_0
class Variation_603(Element):
    bit_offset8 = 3
    bit_size = 4
    content = Content_296
class Variation_604(Element):
    bit_offset8 = 3
    bit_size = 4
    content = Content_297
class Variation_605(Element):
    bit_offset8 = 3
    bit_size = 4
    content = Content_410
class Variation_606(Element):
    bit_offset8 = 3
    bit_size = 5
    content = Content_0
class Variation_607(Element):
    bit_offset8 = 3
    bit_size = 5
    content = Content_563
class Variation_608(Element):
    bit_offset8 = 3
    bit_size = 13
    content = Content_573
class Variation_609(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_0
class Variation_610(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_6
class Variation_611(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_33
class Variation_612(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_42
class Variation_613(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_46
class Variation_614(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_72
class Variation_615(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_95
class Variation_616(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_97
class Variation_617(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_107
class Variation_618(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_110
class Variation_619(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_112
class Variation_620(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_142
class Variation_621(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_149
class Variation_622(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_150
class Variation_623(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_153
class Variation_624(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_154
class Variation_625(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_161
class Variation_626(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_162
class Variation_627(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_163
class Variation_628(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_178
class Variation_629(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_188
class Variation_630(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_193
class Variation_631(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_198
class Variation_632(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_199
class Variation_633(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_213
class Variation_634(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_214
class Variation_635(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_216
class Variation_636(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_218
class Variation_637(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_227
class Variation_638(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_240
class Variation_639(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_244
class Variation_640(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_249
class Variation_641(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_254
class Variation_642(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_255
class Variation_643(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_272
class Variation_644(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_279
class Variation_645(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_283
class Variation_646(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_307
class Variation_647(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_336
class Variation_648(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_337
class Variation_649(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_342
class Variation_650(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_355
class Variation_651(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_369
class Variation_652(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_372
class Variation_653(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_388
class Variation_654(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_398
class Variation_655(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_404
class Variation_656(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_407
class Variation_657(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_408
class Variation_658(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_422
class Variation_659(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_436
class Variation_660(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_445
class Variation_661(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_447
class Variation_662(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_459
class Variation_663(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_464
class Variation_664(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_485
class Variation_665(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_488
class Variation_666(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_515
class Variation_667(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_523
class Variation_668(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_0
class Variation_669(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_205
class Variation_670(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_236
class Variation_671(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_277
class Variation_672(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_362
class Variation_673(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_417
class Variation_674(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_500
class Variation_675(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_502
class Variation_676(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_503
class Variation_677(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_506
class Variation_678(Element):
    bit_offset8 = 4
    bit_size = 2
    content = Content_555
class Variation_679(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_0
class Variation_680(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_2
class Variation_681(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_11
class Variation_682(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_344
class Variation_683(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_379
class Variation_684(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_504
class Variation_685(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_0
class Variation_686(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_322
class Variation_687(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_326
class Variation_688(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_529
class Variation_689(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_531
class Variation_690(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_563
class Variation_691(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_564
class Variation_692(Element):
    bit_offset8 = 4
    bit_size = 11
    content = Content_649
class Variation_693(Element):
    bit_offset8 = 4
    bit_size = 12
    content = Content_0
class Variation_694(Element):
    bit_offset8 = 4
    bit_size = 12
    content = Content_562
class Variation_695(Element):
    bit_offset8 = 4
    bit_size = 12
    content = Content_591
class Variation_696(Element):
    bit_offset8 = 4
    bit_size = 12
    content = Content_625
class Variation_697(Element):
    bit_offset8 = 4
    bit_size = 12
    content = Content_632
class Variation_698(Element):
    bit_offset8 = 4
    bit_size = 12
    content = Content_635
class Variation_699(Element):
    bit_offset8 = 4
    bit_size = 12
    content = Content_650
class Variation_700(Element):
    bit_offset8 = 4
    bit_size = 20
    content = Content_580
class Variation_701(Element):
    bit_offset8 = 4
    bit_size = 20
    content = Content_622
class Variation_702(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_0
class Variation_703(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_7
class Variation_704(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_9
class Variation_705(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_15
class Variation_706(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_23
class Variation_707(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_25
class Variation_708(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_34
class Variation_709(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_41
class Variation_710(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_47
class Variation_711(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_64
class Variation_712(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_66
class Variation_713(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_76
class Variation_714(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_77
class Variation_715(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_95
class Variation_716(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_113
class Variation_717(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_118
class Variation_718(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_125
class Variation_719(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_134
class Variation_720(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_138
class Variation_721(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_140
class Variation_722(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_141
class Variation_723(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_143
class Variation_724(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_146
class Variation_725(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_152
class Variation_726(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_155
class Variation_727(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_158
class Variation_728(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_175
class Variation_729(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_177
class Variation_730(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_180
class Variation_731(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_181
class Variation_732(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_193
class Variation_733(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_198
class Variation_734(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_199
class Variation_735(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_212
class Variation_736(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_213
class Variation_737(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_216
class Variation_738(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_222
class Variation_739(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_223
class Variation_740(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_227
class Variation_741(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_232
class Variation_742(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_239
class Variation_743(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_256
class Variation_744(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_257
class Variation_745(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_272
class Variation_746(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_338
class Variation_747(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_349
class Variation_748(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_363
class Variation_749(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_369
class Variation_750(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_372
class Variation_751(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_380
class Variation_752(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_402
class Variation_753(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_437
class Variation_754(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_447
class Variation_755(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_463
class Variation_756(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_475
class Variation_757(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_481
class Variation_758(Element):
    bit_offset8 = 5
    bit_size = 1
    content = Content_522
class Variation_759(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_0
class Variation_760(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_243
class Variation_761(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_248
class Variation_762(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_286
class Variation_763(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_287
class Variation_764(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_289
class Variation_765(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_409
class Variation_766(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_469
class Variation_767(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_470
class Variation_768(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_490
class Variation_769(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_491
class Variation_770(Element):
    bit_offset8 = 5
    bit_size = 2
    content = Content_501
class Variation_771(Element):
    bit_offset8 = 5
    bit_size = 3
    content = Content_0
class Variation_772(Element):
    bit_offset8 = 5
    bit_size = 3
    content = Content_313
class Variation_773(Element):
    bit_offset8 = 5
    bit_size = 3
    content = Content_330
class Variation_774(Element):
    bit_offset8 = 5
    bit_size = 3
    content = Content_386
class Variation_775(Element):
    bit_offset8 = 5
    bit_size = 11
    content = Content_0
class Variation_776(Element):
    bit_offset8 = 5
    bit_size = 11
    content = Content_563
class Variation_777(Element):
    bit_offset8 = 5
    bit_size = 27
    content = Content_0
class Variation_778(Element):
    bit_offset8 = 5
    bit_size = 27
    content = Content_563
class Variation_779(Element):
    bit_offset8 = 5
    bit_size = 27
    content = Content_613
class Variation_780(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_0
class Variation_781(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_1
class Variation_782(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_9
class Variation_783(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_19
class Variation_784(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_35
class Variation_785(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_47
class Variation_786(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_52
class Variation_787(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_53
class Variation_788(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_55
class Variation_789(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_78
class Variation_790(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_79
class Variation_791(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_83
class Variation_792(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_85
class Variation_793(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_89
class Variation_794(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_91
class Variation_795(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_114
class Variation_796(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_124
class Variation_797(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_127
class Variation_798(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_129
class Variation_799(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_133
class Variation_800(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_139
class Variation_801(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_144
class Variation_802(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_147
class Variation_803(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_164
class Variation_804(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_169
class Variation_805(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_170
class Variation_806(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_183
class Variation_807(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_197
class Variation_808(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_201
class Variation_809(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_212
class Variation_810(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_216
class Variation_811(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_217
class Variation_812(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_219
class Variation_813(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_222
class Variation_814(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_224
class Variation_815(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_227
class Variation_816(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_241
class Variation_817(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_247
class Variation_818(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_259
class Variation_819(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_260
class Variation_820(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_283
class Variation_821(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_301
class Variation_822(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_356
class Variation_823(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_372
class Variation_824(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_387
class Variation_825(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_400
class Variation_826(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_403
class Variation_827(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_405
class Variation_828(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_407
class Variation_829(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_414
class Variation_830(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_422
class Variation_831(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_427
class Variation_832(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_431
class Variation_833(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_438
class Variation_834(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_451
class Variation_835(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_460
class Variation_836(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_484
class Variation_837(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_515
class Variation_838(Element):
    bit_offset8 = 6
    bit_size = 1
    content = Content_521
class Variation_839(Element):
    bit_offset8 = 6
    bit_size = 2
    content = Content_226
class Variation_840(Element):
    bit_offset8 = 6
    bit_size = 2
    content = Content_230
class Variation_841(Element):
    bit_offset8 = 6
    bit_size = 2
    content = Content_318
class Variation_842(Element):
    bit_offset8 = 6
    bit_size = 2
    content = Content_411
class Variation_843(Element):
    bit_offset8 = 6
    bit_size = 2
    content = Content_418
class Variation_844(Element):
    bit_offset8 = 6
    bit_size = 4
    content = Content_0
class Variation_845(Element):
    bit_offset8 = 6
    bit_size = 10
    content = Content_563
class Variation_846(Element):
    bit_offset8 = 6
    bit_size = 10
    content = Content_569
class Variation_847(Element):
    bit_offset8 = 6
    bit_size = 10
    content = Content_592
class Variation_848(Element):
    bit_offset8 = 6
    bit_size = 10
    content = Content_663
class Variation_849(Element):
    bit_offset8 = 6
    bit_size = 26
    content = Content_0
class Variation_850(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_29
class Variation_851(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_32
class Variation_852(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_65
class Variation_853(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_199
class Variation_854(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_213
class Variation_855(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_215
class Variation_856(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_217
class Variation_857(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_221
class Variation_858(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_223
class Variation_859(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_227
class Variation_860(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_234
class Variation_861(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_343
class Variation_862(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_356
class Variation_863(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_372
class Variation_864(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_391
class Variation_865(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_395
class Variation_866(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_430
class Variation_867(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_439
class Variation_868(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_520
class Variation_869(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_524
class Variation_870(Element):
    bit_offset8 = 7
    bit_size = 3
    content = Content_0
class Variation_871(Element):
    bit_offset8 = 7
    bit_size = 4
    content = Content_563
class Variation_872(Element):
    bit_offset8 = 7
    bit_size = 9
    content = Content_566
class Item_0(Spare):
    bit_offset8 = 0
    bit_size = 1
class Item_889(Item):
    name = "FTN"
    title = "Fusion Track Number"
    var = Variation_422
class Variation_873(Group):
    items = [Item_0, Item_889]
class Item_1313(Item):
    name = "PR"
    title = "Mode S Packet Internal Priority"
    var = Variation_415
class Item_1337(Item):
    name = "PT"
    title = "Packet Type"
    var = Variation_843
class Variation_874(Group):
    items = [Item_0, Item_1313, Item_1337]
class Item_1426(Item):
    name = "REDRDP"
    title = "Reduction Steps in Use for An Overload of the RDP"
    var = Variation_410
class Item_1427(Item):
    name = "REDXMT"
    title = "Reduction Steps in Use for An Overload of the Transmission Subsystem"
    var = Variation_682
class Item_29(Spare):
    bit_offset8 = 7
    bit_size = 1
class Variation_875(Group):
    items = [Item_0, Item_1426, Item_1427, Item_29]
class Item_1435(Item):
    name = "RES"
    title = ""
    var = Variation_358
class Item_896(Item):
    name = "GA"
    title = ""
    var = Variation_522
class Variation_876(Group):
    items = [Item_0, Item_1435, Item_896]
class Item_1437(Item):
    name = "RES"
    title = "Resolution with which the GNSS-derived Altitude (GA) is Reported"
    var = Variation_358
class Item_897(Item):
    name = "GA"
    title = "GNSS-derived Altitude of Target, Expressed as Height Above WGS 84 Ellipsoid"
    var = Variation_522
class Variation_877(Group):
    items = [Item_0, Item_1437, Item_897]
class Item_1858(Item):
    name = "VNS"
    title = "Version Not Supported"
    var = Variation_384
class Item_1855(Item):
    name = "VN"
    title = "Version Number"
    var = Variation_514
class Item_1068(Item):
    name = "LTT"
    title = "Link Technology Type"
    var = Variation_774
class Variation_878(Group):
    items = [Item_0, Item_1858, Item_1855, Item_1068]
class Item_1856(Item):
    name = "VN"
    title = "Version Number"
    var = Variation_515
class Variation_879(Group):
    items = [Item_0, Item_1858, Item_1856, Item_1068]
class Item_1(Spare):
    bit_offset8 = 0
    bit_size = 2
class Item_413(Item):
    name = "3DH"
    title = "3D Height, in Binary Notation. Negative Values Are Expressed in Two's Complement"
    var = Variation_522
class Variation_880(Group):
    items = [Item_1, Item_413]
class Item_1293(Item):
    name = "PIN"
    title = "PIN Code"
    var = Variation_520
class Item_1214(Item):
    name = "NAV"
    title = "Validity of NAT"
    var = Variation_473
class Item_1208(Item):
    name = "NAT"
    title = "National Origin"
    var = Variation_606
class Item_1157(Item):
    name = "MIS"
    title = "Mission Code"
    var = Variation_518
class Variation_881(Group):
    items = [Item_1, Item_1293, Item_1, Item_1214, Item_1208, Item_1, Item_1157]
class Item_2(Spare):
    bit_offset8 = 0
    bit_size = 3
class Variation_882(Group):
    items = [Item_1, Item_1293, Item_2, Item_1208, Item_1, Item_1157]
class Item_3(Spare):
    bit_offset8 = 0
    bit_size = 4
class Item_1242(Item):
    name = "NOV"
    title = "Validity of NO"
    var = Variation_644
class Item_1225(Item):
    name = "NO"
    title = "National Origin"
    var = Variation_775
class Variation_883(Group):
    items = [Item_1, Item_1293, Item_3, Item_1242, Item_1225]
class Item_4(Spare):
    bit_offset8 = 0
    bit_size = 5
class Item_1226(Item):
    name = "NO"
    title = "National Origin Code"
    var = Variation_775
class Variation_884(Group):
    items = [Item_1, Item_1293, Item_4, Item_1226]
class Item_1826(Item):
    name = "UPD"
    title = "Update Period"
    var = Variation_525
class Variation_885(Group):
    items = [Item_1, Item_1826]
class Item_1927(Item):
    name = "XP"
    title = "X-pulse from Mode 5 PIN Reply/Report"
    var = Variation_500
class Item_1920(Item):
    name = "X5"
    title = "X-pulse from Mode 5 Data Reply or Report"
    var = Variation_585
class Item_1923(Item):
    name = "XC"
    title = "X-pulse from Mode C Reply"
    var = Variation_667
class Item_1919(Item):
    name = "X3"
    title = "X-pulse from Mode 3/A Reply"
    var = Variation_758
class Item_1917(Item):
    name = "X2"
    title = "X-pulse from Mode 2 Reply"
    var = Variation_838
class Item_1913(Item):
    name = "X1"
    title = "X-pulse from Mode 1 Reply"
    var = Variation_868
class Variation_886(Group):
    items = [Item_1, Item_1927, Item_1920, Item_1923, Item_1919, Item_1917, Item_1913]
class Item_1928(Item):
    name = "XP"
    title = "X-pulse from Mode 5 PIN Reply/report"
    var = Variation_500
class Item_1916(Item):
    name = "X2"
    title = "X-pulse from Mode 2 Reply"
    var = Variation_781
class Variation_887(Group):
    items = [Item_1, Item_1928, Item_1920, Item_1923, Item_1919, Item_1916, Item_1913]
class Item_804(Item):
    name = "DTI"
    title = "Cockpit Display of Traffic Information"
    var = Variation_582
class Item_1129(Item):
    name = "MDS"
    title = "Mode-S Extended Squitter"
    var = Variation_652
class Item_1818(Item):
    name = "UAT"
    title = "UAT"
    var = Variation_750
class Item_1852(Item):
    name = "VDL"
    title = "VDL Mode 4"
    var = Variation_823
class Item_1269(Item):
    name = "OTR"
    title = "Other Technology"
    var = Variation_863
class Variation_888(Group):
    items = [Item_2, Item_804, Item_1129, Item_1818, Item_1852, Item_1269]
class Item_865(Item):
    name = "FOM"
    title = ""
    var = Variation_606
class Variation_889(Group):
    items = [Item_2, Item_865]
class Item_866(Item):
    name = "FOM"
    title = "Figure of Merit"
    var = Variation_606
class Variation_890(Group):
    items = [Item_2, Item_866]
class Item_1352(Item):
    name = "QA4"
    title = ""
    var = Variation_553
class Item_1347(Item):
    name = "QA2"
    title = ""
    var = Variation_633
class Item_1344(Item):
    name = "QA1"
    title = ""
    var = Variation_735
class Item_1361(Item):
    name = "QB2"
    title = ""
    var = Variation_810
class Item_1357(Item):
    name = "QB1"
    title = ""
    var = Variation_855
class Variation_891(Group):
    items = [Item_2, Item_1352, Item_1347, Item_1344, Item_1361, Item_1357]
class Item_1518(Item):
    name = "SCN"
    title = "Track / SCN Association"
    var = Variation_577
class Item_1409(Item):
    name = "RC"
    title = "Roll Call Component"
    var = Variation_611
class Item_474(Item):
    name = "AC"
    title = "All Call Component"
    var = Variation_708
class Item_1610(Item):
    name = "SSR"
    title = "SSR Component"
    var = Variation_784
class Item_1333(Item):
    name = "PSR"
    title = "PSR Component"
    var = Variation_851
class Item_1294(Item):
    name = "PLOTNR"
    title = ""
    var = Variation_234
class Variation_892(Group):
    items = [Item_2, Item_1518, Item_1409, Item_474, Item_1610, Item_1333, Item_1294]
class Item_1830(Item):
    name = "VA"
    title = ""
    var = Variation_566
class Item_1175(Item):
    name = "MODE3A"
    title = "Mode-3/A Reply in Octal Representation"
    var = Variation_694
class Variation_893(Group):
    items = [Item_2, Item_1830, Item_1175]
class Variation_894(Group):
    items = [Item_2, Item_1920, Item_1923, Item_1919, Item_1917, Item_1913]
class Item_593(Item):
    name = "BPS"
    title = ""
    var = Variation_698
class Variation_895(Group):
    items = [Item_3, Item_593]
class Item_594(Item):
    name = "BPS"
    title = "Barometric Pressure Setting"
    var = Variation_697
class Variation_896(Group):
    items = [Item_3, Item_594]
class Item_821(Item):
    name = "EM1"
    title = "Extended Mode 1 Reply in Octal Representation"
    var = Variation_694
class Variation_897(Group):
    items = [Item_3, Item_821]
class Item_890(Item):
    name = "FTN"
    title = "Fusion Track Number"
    var = Variation_693
class Variation_898(Group):
    items = [Item_3, Item_890]
class Item_940(Item):
    name = "HDR"
    title = "Horizontal Reference Direction"
    var = Variation_664
class Item_1629(Item):
    name = "STAT"
    title = "Selected Heading Status"
    var = Variation_712
class Item_1552(Item):
    name = "SH"
    title = "Selected Heading"
    var = Variation_848
class Variation_899(Group):
    items = [Item_3, Item_940, Item_1629, Item_1552]
class Item_1165(Item):
    name = "MOD3A"
    title = "Mode-3/A Reply in Octal Representation"
    var = Variation_694
class Variation_900(Group):
    items = [Item_3, Item_1165]
class Item_1169(Item):
    name = "MODE2"
    title = "Mode-2 Code in Octal Representation"
    var = Variation_694
class Variation_901(Group):
    items = [Item_3, Item_1169]
class Item_1171(Item):
    name = "MODE3A"
    title = "(Mode 3/A Code) 4 Digits, Octal Representation"
    var = Variation_694
class Variation_902(Group):
    items = [Item_3, Item_1171]
class Item_1173(Item):
    name = "MODE3A"
    title = "Mode-3/A Code (Converted Into Octal Representation) of Aircraft 1 Involved in the Conflict"
    var = Variation_694
class Variation_903(Group):
    items = [Item_3, Item_1173]
class Item_1174(Item):
    name = "MODE3A"
    title = "Mode-3/A Code (Converted Into Octal Representation) of Aircraft 2 Involved in the Conflict"
    var = Variation_694
class Variation_904(Group):
    items = [Item_3, Item_1174]
class Variation_905(Group):
    items = [Item_3, Item_1175]
class Item_1253(Item):
    name = "OCT1"
    title = "1st Octal Digit"
    var = Variation_679
class Item_1254(Item):
    name = "OCT2"
    title = "2nd Octal Digit"
    var = Variation_870
class Item_1255(Item):
    name = "OCT3"
    title = "3rd Octal Digit"
    var = Variation_513
class Item_1256(Item):
    name = "OCT4"
    title = "4th Octal Digit"
    var = Variation_771
class Variation_906(Group):
    items = [Item_3, Item_1253, Item_1254, Item_1255, Item_1256]
class Item_1340(Item):
    name = "PUN"
    title = "Position Uncertainty"
    var = Variation_685
class Variation_907(Group):
    items = [Item_3, Item_1340]
class Item_1353(Item):
    name = "QA4"
    title = ""
    var = Variation_634
class Item_1348(Item):
    name = "QA2"
    title = ""
    var = Variation_736
class Item_1345(Item):
    name = "QA1"
    title = ""
    var = Variation_809
class Item_1364(Item):
    name = "QB4"
    title = ""
    var = Variation_856
class Item_1359(Item):
    name = "QB2"
    title = ""
    var = Variation_44
class Item_1355(Item):
    name = "QB1"
    title = ""
    var = Variation_363
class Item_1372(Item):
    name = "QC4"
    title = ""
    var = Variation_458
class Item_1368(Item):
    name = "QC2"
    title = ""
    var = Variation_554
class Item_1366(Item):
    name = "QC1"
    title = ""
    var = Variation_636
class Item_1380(Item):
    name = "QD4"
    title = ""
    var = Variation_739
class Item_1378(Item):
    name = "QD2"
    title = ""
    var = Variation_813
class Item_1375(Item):
    name = "QD1"
    title = ""
    var = Variation_857
class Variation_908(Group):
    items = [Item_3, Item_1353, Item_1348, Item_1345, Item_1364, Item_1359, Item_1355, Item_1372, Item_1368, Item_1366, Item_1380, Item_1378, Item_1375]
class Item_1630(Item):
    name = "STAT"
    title = "Status of the Alert"
    var = Variation_679
class Variation_909(Group):
    items = [Item_3, Item_1630, Item_29]
class Item_1758(Item):
    name = "TRK"
    title = "Track Number"
    var = Variation_693
class Variation_910(Group):
    items = [Item_3, Item_1758]
class Item_1760(Item):
    name = "TRN"
    title = "Track Number"
    var = Variation_693
class Variation_911(Group):
    items = [Item_3, Item_1760]
class Item_1762(Item):
    name = "TRNUM"
    title = "Track Number"
    var = Variation_693
class Variation_912(Group):
    items = [Item_3, Item_1762]
class Item_686(Item):
    name = "COM"
    title = "Communications Capability of the Transponder"
    var = Variation_772
class Variation_913(Group):
    items = [Item_4, Item_686]
class Item_1217(Item):
    name = "NBR"
    title = ""
    var = Variation_779
class Variation_914(Group):
    items = [Item_4, Item_1217]
class Item_1319(Item):
    name = "PREFIX"
    title = "Prefix Field"
    var = Variation_777
class Variation_915(Group):
    items = [Item_4, Item_1319]
class Item_1487(Item):
    name = "RVSM"
    title = ""
    var = Variation_770
class Item_954(Item):
    name = "HPR"
    title = ""
    var = Variation_862
class Variation_916(Group):
    items = [Item_4, Item_1487, Item_954]
class Item_5(Spare):
    bit_offset8 = 0
    bit_size = 6
class Item_1669(Item):
    name = "TAR"
    title = "Track Angle Rate"
    var = Variation_847
class Variation_917(Group):
    items = [Item_5, Item_1669]
class Item_6(Spare):
    bit_offset8 = 0
    bit_size = 7
class Item_1459(Item):
    name = "RPP"
    title = ""
    var = Variation_872
class Variation_918(Group):
    items = [Item_6, Item_1459]
class Item_1689(Item):
    name = "TCOUNT1"
    title = ""
    var = Variation_871
class Item_1686(Item):
    name = "TCODE1"
    title = ""
    var = Variation_606
class Item_1690(Item):
    name = "TCOUNT2"
    title = ""
    var = Variation_132
class Item_1687(Item):
    name = "TCODE2"
    title = ""
    var = Variation_694
class Item_1691(Item):
    name = "TCOUNT3"
    title = ""
    var = Variation_132
class Item_1688(Item):
    name = "TCODE3"
    title = ""
    var = Variation_694
class Variation_919(Group):
    items = [Item_6, Item_1689, Item_1686, Item_1690, Item_1687, Item_1691, Item_1688]
class Item_465(Item):
    name = "A"
    title = "Aerial Identification"
    var = Variation_20
class Item_989(Item):
    name = "IDENT"
    title = ""
    var = Variation_414
class Item_701(Item):
    name = "COUNTER"
    title = ""
    var = Variation_845
class Variation_920(Group):
    items = [Item_465, Item_989, Item_701]
class Item_471(Item):
    name = "AC"
    title = "ACAS Capabilities"
    var = Variation_109
class Item_1161(Item):
    name = "MN"
    title = "Multiple Navigation Aids"
    var = Variation_511
class Item_768(Item):
    name = "DC"
    title = "Differencial Correction"
    var = Variation_675
class Item_28(Spare):
    bit_offset8 = 6
    bit_size = 6
class Item_1281(Item):
    name = "PA"
    title = "Position Accuracy"
    var = Variation_691
class Variation_921(Group):
    items = [Item_471, Item_1161, Item_768, Item_28, Item_1281]
class Item_473(Item):
    name = "AC"
    title = "ACAS Status"
    var = Variation_110
class Item_1163(Item):
    name = "MN"
    title = "Multiple Navigational Aids Status"
    var = Variation_512
class Item_770(Item):
    name = "DC"
    title = "Differential Correction Status"
    var = Variation_676
class Item_909(Item):
    name = "GBS"
    title = "Ground Bit Set"
    var = Variation_836
class Item_31(Spare):
    bit_offset8 = 7
    bit_size = 6
class Item_1628(Item):
    name = "STAT"
    title = "Flight Status"
    var = Variation_773
class Variation_922(Group):
    items = [Item_473, Item_1163, Item_770, Item_909, Item_31, Item_1628]
class Item_478(Item):
    name = "ACK"
    title = "Alert Acknowleged"
    var = Variation_10
class Item_1660(Item):
    name = "SVR"
    title = "Alert Severity"
    var = Variation_396
class Item_19(Spare):
    bit_offset8 = 3
    bit_size = 5
class Item_546(Item):
    name = "AT"
    title = "Alert Type"
    var = Variation_155
class Item_518(Item):
    name = "AN"
    title = "Alert Number"
    var = Variation_155
class Variation_923(Group):
    items = [Item_478, Item_1660, Item_19, Item_546, Item_518]
class Item_479(Item):
    name = "ACQI"
    title = ""
    var = Variation_108
class Item_1759(Item):
    name = "TRKUPDCTR"
    title = ""
    var = Variation_521
class Item_1017(Item):
    name = "LASTTRKUPD"
    title = ""
    var = Variation_267
class Variation_924(Group):
    items = [Item_479, Item_1759, Item_1017]
class Item_521(Item):
    name = "ANT"
    title = "Selected Antenna"
    var = Variation_11
class Item_630(Item):
    name = "CHAB"
    title = "Channel A/B Selection Status"
    var = Variation_400
class Item_1271(Item):
    name = "OVL"
    title = "Overload Condition"
    var = Variation_565
class Item_1192(Item):
    name = "MSC"
    title = "Monitoring System Connected Status"
    var = Variation_643
class Item_25(Spare):
    bit_offset8 = 5
    bit_size = 3
class Variation_925(Group):
    items = [Item_521, Item_630, Item_1271, Item_1192, Item_25]
class Item_631(Item):
    name = "CHAB"
    title = "Channel A/B Selection Status"
    var = Variation_401
class Item_1276(Item):
    name = "OVLSUR"
    title = "Overload Condition"
    var = Variation_565
class Item_1194(Item):
    name = "MSC"
    title = "Monitoring System Connected Status:"
    var = Variation_643
class Item_1516(Item):
    name = "SCF"
    title = "Channel A/B Selection Status for Surveillance Co-ordination Function"
    var = Variation_710
class Item_784(Item):
    name = "DLF"
    title = "Channel A/B Selection Status for Data Link Function"
    var = Variation_785
class Item_1275(Item):
    name = "OVLSCF"
    title = "Overload in Surveillance Co-ordination Function"
    var = Variation_861
class Item_1273(Item):
    name = "OVLDLF"
    title = "Overload in Data Link Function"
    var = Variation_60
class Item_10(Spare):
    bit_offset8 = 1
    bit_size = 7
class Variation_926(Group):
    items = [Item_521, Item_631, Item_1276, Item_1194, Item_1516, Item_784, Item_1275, Item_1273, Item_10]
class Item_632(Item):
    name = "CHAB"
    title = "Channel A/B Selection Status"
    var = Variation_402
class Variation_927(Group):
    items = [Item_521, Item_632, Item_1271, Item_1194, Item_25]
class Item_523(Item):
    name = "AP"
    title = "Autopilot"
    var = Variation_12
class Item_1857(Item):
    name = "VN"
    title = "Vertical Navigation"
    var = Variation_392
class Item_499(Item):
    name = "AH"
    title = "Altitude Hold"
    var = Variation_431
class Item_515(Item):
    name = "AM"
    title = "Approach Mode"
    var = Variation_530
class Item_22(Spare):
    bit_offset8 = 4
    bit_size = 4
class Variation_928(Group):
    items = [Item_523, Item_1857, Item_499, Item_515, Item_22]
class Item_834(Item):
    name = "EP"
    title = "Element Populated Bit"
    var = Variation_630
class Item_1845(Item):
    name = "VAL"
    title = "Value"
    var = Variation_742
class Variation_972(Group):
    items = [Item_834, Item_1845]
class Item_1144(Item):
    name = "MFM"
    title = "Status of MCP/FCU Mode Bits"
    var = Variation_972
class Item_27(Spare):
    bit_offset8 = 6
    bit_size = 2
class Variation_929(Group):
    items = [Item_523, Item_1857, Item_499, Item_515, Item_1144, Item_27]
class Item_561(Item):
    name = "AX"
    title = ""
    var = Variation_195
class Item_564(Item):
    name = "AY"
    title = ""
    var = Variation_195
class Variation_930(Group):
    items = [Item_561, Item_564]
class Item_562(Item):
    name = "AX"
    title = "Ax"
    var = Variation_195
class Item_565(Item):
    name = "AY"
    title = "Ay"
    var = Variation_195
class Variation_931(Group):
    items = [Item_562, Item_565]
class Item_563(Item):
    name = "AX"
    title = "X Acceleration"
    var = Variation_196
class Item_566(Item):
    name = "AY"
    title = "Y Acceleration"
    var = Variation_196
class Variation_932(Group):
    items = [Item_563, Item_566]
class Item_568(Item):
    name = "AZCON"
    title = ""
    var = Variation_293
class Item_815(Item):
    name = "ELCON"
    title = ""
    var = Variation_259
class Item_1440(Item):
    name = "RGCONSTOP"
    title = ""
    var = Variation_294
class Item_1439(Item):
    name = "RGCONSTART"
    title = ""
    var = Variation_294
class Variation_933(Group):
    items = [Item_568, Item_815, Item_1440, Item_1439]
class Item_583(Item):
    name = "BIT1"
    title = "TU1/RU1 Contribution"
    var = Variation_76
class Item_584(Item):
    name = "BIT2"
    title = "TU2/RU2 Contribution"
    var = Variation_381
class Item_585(Item):
    name = "BIT3"
    title = "TU3/RU3 Contribution"
    var = Variation_493
class Item_586(Item):
    name = "BIT4"
    title = "TU4/RU4 Contribution"
    var = Variation_573
class Item_587(Item):
    name = "BIT5"
    title = "TU5/RU5 Contribution"
    var = Variation_659
class Item_588(Item):
    name = "BIT6"
    title = "TU6/RU6 Contribution"
    var = Variation_753
class Item_589(Item):
    name = "BIT7"
    title = "TU7/RU7 Contribution"
    var = Variation_833
class Item_590(Item):
    name = "BIT8"
    title = "TU8/RU8 Contribution"
    var = Variation_867
class Variation_934(Group):
    items = [Item_583, Item_584, Item_585, Item_586, Item_587, Item_588, Item_589, Item_590]
class Item_592(Item):
    name = "BKN"
    title = "Bank Number"
    var = Variation_127
class Item_964(Item):
    name = "I1"
    title = "Indicator 1"
    var = Variation_637
class Item_968(Item):
    name = "I2"
    title = "Indicator 2"
    var = Variation_740
class Item_969(Item):
    name = "I3"
    title = "Indicator 3"
    var = Variation_815
class Item_970(Item):
    name = "I4"
    title = "Indicator 4"
    var = Variation_859
class Item_971(Item):
    name = "I5"
    title = "Indicator 5"
    var = Variation_47
class Item_972(Item):
    name = "I6"
    title = "Indicator 6"
    var = Variation_364
class Item_973(Item):
    name = "I7"
    title = "Indicator 7"
    var = Variation_459
class Item_974(Item):
    name = "I8"
    title = "Indicator 8"
    var = Variation_556
class Item_975(Item):
    name = "I9"
    title = "Indicator 9"
    var = Variation_637
class Item_965(Item):
    name = "I10"
    title = "Indicator 10"
    var = Variation_740
class Item_966(Item):
    name = "I11"
    title = "Indicator 11"
    var = Variation_815
class Item_967(Item):
    name = "I12"
    title = "Indicator 12"
    var = Variation_859
class Variation_935(Group):
    items = [Item_592, Item_964, Item_968, Item_969, Item_970, Item_971, Item_972, Item_973, Item_974, Item_975, Item_965, Item_966, Item_967]
class Item_603(Item):
    name = "C"
    title = "Data Compression Indicator"
    var = Variation_57
class Item_1436(Item):
    name = "RES"
    title = "Bit Resolution"
    var = Variation_174
class Variation_936(Group):
    items = [Item_603, Item_10, Item_1436]
class Item_604(Item):
    name = "CA"
    title = "Communications Capability of the Transponder"
    var = Variation_120
class Item_1555(Item):
    name = "SI"
    title = "SI/II-capabilities of the Transponder"
    var = Variation_580
class Variation_937(Group):
    items = [Item_604, Item_1555, Item_22]
class Item_609(Item):
    name = "CAT"
    title = "Category"
    var = Variation_191
class Item_1094(Item):
    name = "MAIN"
    title = "Main Version Number"
    var = Variation_191
class Item_1652(Item):
    name = "SUB"
    title = "Sub Version Number"
    var = Variation_191
class Variation_938(Group):
    items = [Item_609, Item_1094, Item_1652]
class Item_612(Item):
    name = "CAUSE"
    title = "Cause"
    var = Variation_128
class Item_781(Item):
    name = "DIAG"
    title = "Diagnostic"
    var = Variation_687
class Variation_939(Group):
    items = [Item_612, Item_781]
class Item_623(Item):
    name = "CEN"
    title = ""
    var = Variation_155
class Item_1303(Item):
    name = "POS"
    title = ""
    var = Variation_155
class Variation_940(Group):
    items = [Item_623, Item_1303]
class Item_624(Item):
    name = "CEN"
    title = "Centre"
    var = Variation_155
class Item_1307(Item):
    name = "POS"
    title = "Position"
    var = Variation_155
class Variation_941(Group):
    items = [Item_624, Item_1307]
class Item_625(Item):
    name = "CENTRE"
    title = "8-bit Group Identification Code"
    var = Variation_155
class Item_1310(Item):
    name = "POSITION"
    title = "8-bit Control Position Identification Code"
    var = Variation_155
class Variation_942(Group):
    items = [Item_625, Item_1310]
class Item_641(Item):
    name = "CID"
    title = "Component ID"
    var = Variation_234
class Item_844(Item):
    name = "ERRC"
    title = "Error Code"
    var = Variation_140
class Item_730(Item):
    name = "CS"
    title = "Component State/Mode"
    var = Variation_842
class Variation_943(Group):
    items = [Item_641, Item_844, Item_730]
class Item_644(Item):
    name = "CLS"
    title = "Classification"
    var = Variation_227
class Item_1315(Item):
    name = "PRB"
    title = "Probability"
    var = Variation_420
class Variation_944(Group):
    items = [Item_644, Item_1315]
class Item_667(Item):
    name = "COHAXHPX"
    title = ""
    var = Variation_197
class Item_668(Item):
    name = "COHAXHPY"
    title = ""
    var = Variation_197
class Item_672(Item):
    name = "COHAYHPX"
    title = ""
    var = Variation_197
class Item_652(Item):
    name = "COAYHPY"
    title = ""
    var = Variation_197
class Variation_945(Group):
    items = [Item_667, Item_668, Item_672, Item_652]
class Item_669(Item):
    name = "COHAXHVX"
    title = ""
    var = Variation_197
class Item_670(Item):
    name = "COHAXHVY"
    title = ""
    var = Variation_197
class Item_673(Item):
    name = "COHAYHVX"
    title = ""
    var = Variation_197
class Item_674(Item):
    name = "COHAYHVY"
    title = ""
    var = Variation_197
class Variation_946(Group):
    items = [Item_669, Item_670, Item_673, Item_674]
class Item_676(Item):
    name = "COHVXHPX"
    title = ""
    var = Variation_197
class Item_677(Item):
    name = "COHVXHPY"
    title = ""
    var = Variation_197
class Item_679(Item):
    name = "COHVYHPX"
    title = ""
    var = Variation_197
class Item_680(Item):
    name = "COHVYHPY"
    title = ""
    var = Variation_197
class Variation_947(Group):
    items = [Item_676, Item_677, Item_679, Item_680]
class Item_684(Item):
    name = "COM"
    title = "Communications Capability of the Transponder"
    var = Variation_118
class Item_1625(Item):
    name = "STAT"
    title = "Flight Status"
    var = Variation_598
class Item_1554(Item):
    name = "SI"
    title = "SI/II Transponder Capability"
    var = Variation_829
class Item_1198(Item):
    name = "MSSC"
    title = "Mode-S Specific Service Capability"
    var = Variation_53
class Item_535(Item):
    name = "ARC"
    title = "Altitude Reporting Capability"
    var = Variation_332
class Item_504(Item):
    name = "AIC"
    title = "Aircraft Identification Capability"
    var = Variation_474
class Item_572(Item):
    name = "B1A"
    title = "BDS 1,0 Bit 16"
    var = Variation_527
class Item_574(Item):
    name = "B1B"
    title = "BDS 1,0 Bits 37/40"
    var = Variation_685
class Variation_948(Group):
    items = [Item_684, Item_1625, Item_1554, Item_29, Item_1198, Item_535, Item_504, Item_572, Item_574]
class Item_685(Item):
    name = "COM"
    title = "Communications Capability of the Transponder"
    var = Variation_119
class Item_1622(Item):
    name = "STAT"
    title = "Flight Status"
    var = Variation_595
class Item_1603(Item):
    name = "SSC"
    title = "Specific Service Capability"
    var = Variation_53
class Item_575(Item):
    name = "B1B"
    title = "BDS BDS 1,0 Bits 37/40"
    var = Variation_685
class Variation_949(Group):
    items = [Item_685, Item_1622, Item_27, Item_1603, Item_535, Item_504, Item_572, Item_575]
class Item_1623(Item):
    name = "STAT"
    title = "Flight Status"
    var = Variation_596
class Variation_950(Group):
    items = [Item_685, Item_1623, Item_27, Item_1198, Item_535, Item_504, Item_572, Item_574]
class Item_1624(Item):
    name = "STAT"
    title = "Flight Status"
    var = Variation_597
class Variation_951(Group):
    items = [Item_685, Item_1624, Item_27, Item_1603, Item_535, Item_504, Item_572, Item_575]
class Item_1626(Item):
    name = "STAT"
    title = "Flight Status"
    var = Variation_603
class Item_573(Item):
    name = "B1B"
    title = "BDS 1,0 Bit 37/40"
    var = Variation_685
class Item_472(Item):
    name = "AC"
    title = "ACAS Operational"
    var = Variation_53
class Item_1162(Item):
    name = "MN"
    title = "Multiple Navigational Aids Operating"
    var = Variation_369
class Item_769(Item):
    name = "DC"
    title = "Differential Correction"
    var = Variation_501
class Variation_952(Group):
    items = [Item_685, Item_1626, Item_29, Item_1603, Item_535, Item_504, Item_572, Item_573, Item_472, Item_1162, Item_769, Item_19]
class Item_1627(Item):
    name = "STAT"
    title = "Flight Status"
    var = Variation_604
class Variation_953(Group):
    items = [Item_685, Item_1627, Item_29, Item_1603, Item_535, Item_504, Item_572, Item_573, Item_472, Item_1162, Item_769, Item_19]
class Item_741(Item):
    name = "CST"
    title = "Track Coasted"
    var = Variation_50
class Item_862(Item):
    name = "FLT"
    title = "Flight Level Tracking"
    var = Variation_366
class Item_15(Spare):
    bit_offset8 = 2
    bit_size = 6
class Variation_954(Group):
    items = [Item_741, Item_862, Item_15]
class Item_751(Item):
    name = "D"
    title = ""
    var = Variation_38
class Item_8(Spare):
    bit_offset8 = 1
    bit_size = 5
class Item_606(Item):
    name = "CAL"
    title = "Calculated Doppler Speed, Coded in Two's Complement"
    var = Variation_846
class Variation_955(Group):
    items = [Item_751, Item_8, Item_606]
class Item_773(Item):
    name = "DCR"
    title = "Differential Correction"
    var = Variation_58
class Item_910(Item):
    name = "GBS"
    title = "Ground Bit Setting"
    var = Variation_360
class Item_1576(Item):
    name = "SIM"
    title = "Simulated Target"
    var = Variation_429
class Item_1777(Item):
    name = "TST"
    title = "Test Target"
    var = Variation_545
class Item_1397(Item):
    name = "RAB"
    title = "Report Type"
    var = Variation_656
class Item_1498(Item):
    name = "SAA"
    title = "Selected Altitude Available"
    var = Variation_733
class Item_1589(Item):
    name = "SPI"
    title = "Special Position Identification"
    var = Variation_782
class Item_551(Item):
    name = "ATP"
    title = "Address Type"
    var = Variation_125
class Item_537(Item):
    name = "ARC"
    title = "Altitude Reporting Capability"
    var = Variation_594
class Variation_956(Group):
    items = [Item_773, Item_910, Item_1576, Item_1777, Item_1397, Item_1498, Item_1589, Item_29, Item_551, Item_537, Item_25]
class Item_777(Item):
    name = "DEVX"
    title = "Standard Deviation of X Component"
    var = Variation_214
class Item_778(Item):
    name = "DEVY"
    title = "Standard Deviation of Y Component"
    var = Variation_214
class Item_713(Item):
    name = "COVXY"
    title = "Covariance in Two’s Complement Form"
    var = Variation_249
class Variation_957(Group):
    items = [Item_777, Item_778, Item_713]
class Item_789(Item):
    name = "DOP"
    title = "Doppler Speed"
    var = Variation_266
class Item_517(Item):
    name = "AMB"
    title = "Ambiguity Range"
    var = Variation_266
class Item_880(Item):
    name = "FRQ"
    title = "Transmitter Frequency"
    var = Variation_263
class Variation_958(Group):
    items = [Item_789, Item_517, Item_880]
class Item_795(Item):
    name = "DRHO"
    title = ""
    var = Variation_194
class Item_803(Item):
    name = "DTHETA"
    title = ""
    var = Variation_202
class Variation_959(Group):
    items = [Item_795, Item_803]
class Item_824(Item):
    name = "EMP"
    title = ""
    var = Variation_98
class Item_555(Item):
    name = "AVL"
    title = ""
    var = Variation_505
class Variation_960(Group):
    items = [Item_824, Item_555, Item_22]
class Item_825(Item):
    name = "EMP"
    title = "Stand Empty"
    var = Variation_97
class Item_556(Item):
    name = "AVL"
    title = "Stand Available"
    var = Variation_504
class Variation_961(Group):
    items = [Item_825, Item_556, Item_22]
class Item_829(Item):
    name = "EP"
    title = "ADSB Element Populated Bit"
    var = Variation_1
class Item_1832(Item):
    name = "VAL"
    title = "On-Site ADS-B Information"
    var = Variation_375
class Variation_962(Group):
    items = [Item_829, Item_1832]
class Item_830(Item):
    name = "EP"
    title = "Element Populated Bit"
    var = Variation_39
class Item_1839(Item):
    name = "VAL"
    title = "Value"
    var = Variation_413
class Item_26(Spare):
    bit_offset8 = 6
    bit_size = 1
class Variation_963(Group):
    items = [Item_830, Item_1839, Item_26]
class Item_831(Item):
    name = "EP"
    title = "Element Populated Bit"
    var = Variation_40
class Item_1835(Item):
    name = "VAL"
    title = "Value"
    var = Variation_398
class Variation_964(Group):
    items = [Item_831, Item_1835]
class Item_1836(Item):
    name = "VAL"
    title = "Value"
    var = Variation_406
class Variation_965(Group):
    items = [Item_831, Item_1836]
class Item_1837(Item):
    name = "VAL"
    title = "Value"
    var = Variation_407
class Variation_966(Group):
    items = [Item_831, Item_1837]
class Item_1838(Item):
    name = "VAL"
    title = "Value"
    var = Variation_409
class Variation_967(Group):
    items = [Item_831, Item_1838]
class Item_1840(Item):
    name = "VAL"
    title = "Value"
    var = Variation_416
class Variation_968(Group):
    items = [Item_831, Item_1840]
class Item_832(Item):
    name = "EP"
    title = "Element Populated Bit"
    var = Variation_454
class Item_1841(Item):
    name = "VAL"
    title = "Value"
    var = Variation_591
class Variation_969(Group):
    items = [Item_832, Item_1841]
class Item_833(Item):
    name = "EP"
    title = "Element Populated Bit"
    var = Variation_551
class Item_1842(Item):
    name = "VAL"
    title = "Value"
    var = Variation_639
class Variation_970(Group):
    items = [Item_833, Item_1842]
class Item_1843(Item):
    name = "VAL"
    title = "Value"
    var = Variation_679
class Variation_971(Group):
    items = [Item_833, Item_1843]
class Item_1846(Item):
    name = "VAL"
    title = "Value"
    var = Variation_768
class Variation_973(Group):
    items = [Item_834, Item_1846]
class Item_835(Item):
    name = "EP"
    title = "Element Populated Bit"
    var = Variation_732
class Item_1847(Item):
    name = "VAL"
    title = "Value"
    var = Variation_825
class Variation_974(Group):
    items = [Item_835, Item_1847]
class Item_1848(Item):
    name = "VAL"
    title = "Value"
    var = Variation_826
class Variation_975(Group):
    items = [Item_835, Item_1848]
class Item_836(Item):
    name = "EP"
    title = "Element Population Bit"
    var = Variation_551
class Item_1844(Item):
    name = "VAL"
    title = "Value"
    var = Variation_681
class Variation_976(Group):
    items = [Item_836, Item_1844]
class Item_837(Item):
    name = "EP"
    title = "PAI Element Populated Bit"
    var = Variation_653
class Item_1833(Item):
    name = "VAL"
    title = "Passive Acquisition Interface Information"
    var = Variation_748
class Variation_977(Group):
    items = [Item_837, Item_1833]
class Item_838(Item):
    name = "EP"
    title = "SCN Element Populated Bit"
    var = Variation_491
class Item_1834(Item):
    name = "VAL"
    title = "Surveillance Cluster Network Information"
    var = Variation_568
class Variation_978(Group):
    items = [Item_838, Item_1834]
class Item_853(Item):
    name = "FAMILY"
    title = ""
    var = Variation_131
class Item_1209(Item):
    name = "NATURE"
    title = ""
    var = Variation_689
class Variation_979(Group):
    items = [Item_853, Item_1209]
class Item_881(Item):
    name = "FS"
    title = "Flight Status"
    var = Variation_9
class Item_723(Item):
    name = "CQF"
    title = "Aircraft CQF"
    var = Variation_417
class Variation_980(Group):
    items = [Item_881, Item_723]
class Item_883(Item):
    name = "FSI"
    title = "Full Second Indication"
    var = Variation_114
class Item_1729(Item):
    name = "TOMRP"
    title = "Fractional Part of the Time of Message Reception for Position in the Ground Station"
    var = Variation_526
class Variation_981(Group):
    items = [Item_883, Item_1729]
class Item_884(Item):
    name = "FSI"
    title = "Full Second Indication"
    var = Variation_115
class Variation_982(Group):
    items = [Item_884, Item_1729]
class Item_903(Item):
    name = "GATOAT"
    title = ""
    var = Variation_111
class Item_875(Item):
    name = "FR1FR2"
    title = ""
    var = Variation_507
class Item_1486(Item):
    name = "RVSM"
    title = ""
    var = Variation_674
class Item_953(Item):
    name = "HPR"
    title = ""
    var = Variation_822
class Variation_983(Group):
    items = [Item_903, Item_875, Item_1486, Item_953, Item_29]
class Item_876(Item):
    name = "FR1FR2"
    title = ""
    var = Variation_508
class Item_1583(Item):
    name = "SP3"
    title = ""
    var = Variation_609
class Item_1582(Item):
    name = "SP2"
    title = ""
    var = Variation_702
class Item_1581(Item):
    name = "SP1"
    title = ""
    var = Variation_780
class Variation_984(Group):
    items = [Item_903, Item_876, Item_1583, Item_1582, Item_1581, Item_29]
class Item_904(Item):
    name = "GATOAT"
    title = "Flight Type"
    var = Variation_111
class Item_877(Item):
    name = "FR1FR2"
    title = "Flight Rules"
    var = Variation_507
class Item_1488(Item):
    name = "RVSM"
    title = "RVSM"
    var = Variation_674
class Item_955(Item):
    name = "HPR"
    title = "Flight Priority"
    var = Variation_822
class Variation_985(Group):
    items = [Item_904, Item_877, Item_1488, Item_955, Item_29]
class Item_878(Item):
    name = "FR1FR2"
    title = "Flight Rules"
    var = Variation_508
class Variation_986(Group):
    items = [Item_904, Item_878, Item_1488, Item_955, Item_29]
class Item_918(Item):
    name = "GS"
    title = "Ground Speed in Two's Complement Form Referenced to WGS84"
    var = Variation_256
class Item_1662(Item):
    name = "TA"
    title = "Track Angle"
    var = Variation_293
class Variation_987(Group):
    items = [Item_918, Item_1662]
class Item_919(Item):
    name = "GSP"
    title = "Calculated Groundspeed"
    var = Variation_289
class Item_939(Item):
    name = "HDG"
    title = "Calculated Heading"
    var = Variation_293
class Variation_988(Group):
    items = [Item_919, Item_939]
class Item_920(Item):
    name = "GSP"
    title = "Ground Speed"
    var = Variation_289
class Item_1744(Item):
    name = "TRA"
    title = "Track Angle"
    var = Variation_293
class Variation_989(Group):
    items = [Item_920, Item_1744]
class Item_946(Item):
    name = "HGT"
    title = "Height of Data Source"
    var = Variation_237
class Item_1022(Item):
    name = "LAT"
    title = "Latitude"
    var = Variation_304
class Item_1049(Item):
    name = "LON"
    title = "Longitude"
    var = Variation_304
class Variation_990(Group):
    items = [Item_946, Item_1022, Item_1049]
class Item_983(Item):
    name = "ICF"
    title = "Intent Change Flag (see Note)"
    var = Variation_59
class Item_1043(Item):
    name = "LNAV"
    title = "LNAV Mode"
    var = Variation_365
class Item_11(Spare):
    bit_offset8 = 2
    bit_size = 1
class Item_1326(Item):
    name = "PS"
    title = "Priority Status"
    var = Variation_599
class Item_1602(Item):
    name = "SS"
    title = "Surveillance Status"
    var = Variation_841
class Variation_991(Group):
    items = [Item_983, Item_1043, Item_11, Item_1326, Item_1602]
class Item_1136(Item):
    name = "ME"
    title = "Military Emergency"
    var = Variation_479
class Variation_992(Group):
    items = [Item_983, Item_1043, Item_1136, Item_1326, Item_1602]
class Item_996(Item):
    name = "IM"
    title = ""
    var = Variation_6
class Item_541(Item):
    name = "AS"
    title = "Air Speed (IAS or Mach)"
    var = Variation_422
class Variation_993(Group):
    items = [Item_996, Item_541]
class Item_997(Item):
    name = "IM"
    title = ""
    var = Variation_7
class Item_978(Item):
    name = "IAS"
    title = ""
    var = Variation_422
class Variation_994(Group):
    items = [Item_997, Item_978]
class Item_999(Item):
    name = "IR"
    title = ""
    var = Variation_46
class Item_1083(Item):
    name = "M3A"
    title = "Age of Mode 3/A Code (I048/070)"
    var = Variation_421
class Variation_995(Group):
    items = [Item_999, Item_1083]
class Item_1018(Item):
    name = "LAT"
    title = "APW (Latitude Component)"
    var = Variation_292
class Item_1045(Item):
    name = "LON"
    title = "APW (Longitude Component)"
    var = Variation_292
class Variation_996(Group):
    items = [Item_1018, Item_1045]
class Item_1019(Item):
    name = "LAT"
    title = "APW Latitude Component Accuracy"
    var = Variation_261
class Item_1046(Item):
    name = "LON"
    title = "APW Longitude Component Accuracy"
    var = Variation_261
class Variation_997(Group):
    items = [Item_1019, Item_1046]
class Item_1020(Item):
    name = "LAT"
    title = "In WGS-84 in Two’s Complement"
    var = Variation_317
class Item_1047(Item):
    name = "LON"
    title = "In WGS-84 in Two’s Complement"
    var = Variation_317
class Item_512(Item):
    name = "ALT"
    title = "Altitude of Predicted Conflict"
    var = Variation_240
class Variation_998(Group):
    items = [Item_1020, Item_1047, Item_512]
class Variation_999(Group):
    items = [Item_1022, Item_1049]
class Item_1023(Item):
    name = "LAT"
    title = "Latitude"
    var = Variation_305
class Item_1050(Item):
    name = "LON"
    title = "Longitude"
    var = Variation_305
class Variation_1000(Group):
    items = [Item_1023, Item_1050]
class Item_1024(Item):
    name = "LAT"
    title = "Latitude"
    var = Variation_317
class Item_1051(Item):
    name = "LON"
    title = "Longitude"
    var = Variation_317
class Variation_1001(Group):
    items = [Item_1024, Item_1051]
class Item_1025(Item):
    name = "LAT"
    title = "Latitude"
    var = Variation_318
class Item_1052(Item):
    name = "LON"
    title = "Longitude"
    var = Variation_318
class Variation_1002(Group):
    items = [Item_1025, Item_1052]
class Item_1026(Item):
    name = "LAT"
    title = "Latitude"
    var = Variation_319
class Item_1053(Item):
    name = "LON"
    title = "Longitude"
    var = Variation_319
class Variation_1003(Group):
    items = [Item_1026, Item_1053]
class Item_1027(Item):
    name = "LAT"
    title = "Latitude"
    var = Variation_320
class Item_1054(Item):
    name = "LON"
    title = "Longitude"
    var = Variation_320
class Variation_1004(Group):
    items = [Item_1027, Item_1054]
class Item_1028(Item):
    name = "LAT"
    title = "Latitude in WGS 84"
    var = Variation_304
class Item_1055(Item):
    name = "LON"
    title = "Longitude in WGS 84"
    var = Variation_304
class Variation_1005(Group):
    items = [Item_1028, Item_1055]
class Item_1029(Item):
    name = "LAT"
    title = "Latitude in WGS-84"
    var = Variation_317
class Item_1056(Item):
    name = "LON"
    title = "Longitude in WGS-84"
    var = Variation_317
class Variation_1006(Group):
    items = [Item_1029, Item_1056]
class Item_1030(Item):
    name = "LAT"
    title = "Latitude in WGS-84 in Two's Complement"
    var = Variation_319
class Item_1057(Item):
    name = "LON"
    title = "Longitude in WGS-84 in Two's Complement"
    var = Variation_319
class Variation_1007(Group):
    items = [Item_1030, Item_1057]
class Item_1032(Item):
    name = "LAT"
    title = "Latitude in WGS.84 in Two's Complement Form"
    var = Variation_304
class Item_1059(Item):
    name = "LON"
    title = "Longitude in WGS.84 in Two's Complement Form"
    var = Variation_304
class Variation_1008(Group):
    items = [Item_1032, Item_1059]
class Item_1033(Item):
    name = "LATITUDE"
    title = ""
    var = Variation_319
class Item_1061(Item):
    name = "LONGITUDE"
    title = ""
    var = Variation_319
class Variation_1009(Group):
    items = [Item_1033, Item_1061]
class Item_1065(Item):
    name = "LS"
    title = "Lockout State"
    var = Variation_79
class Item_1044(Item):
    name = "LOCTIM"
    title = "Lockout Time"
    var = Variation_426
class Variation_1010(Group):
    items = [Item_1065, Item_1044]
class Item_1086(Item):
    name = "M5"
    title = ""
    var = Variation_54
class Item_985(Item):
    name = "ID"
    title = ""
    var = Variation_371
class Item_754(Item):
    name = "DA"
    title = ""
    var = Variation_476
class Item_1073(Item):
    name = "M1"
    title = ""
    var = Variation_557
class Item_1075(Item):
    name = "M2"
    title = ""
    var = Variation_641
class Item_1079(Item):
    name = "M3"
    title = ""
    var = Variation_743
class Item_1106(Item):
    name = "MC"
    title = ""
    var = Variation_818
class Item_1911(Item):
    name = "X"
    title = "X-pulse from Mode 5 Data Reply or Report"
    var = Variation_869
class Variation_1011(Group):
    items = [Item_1086, Item_985, Item_754, Item_1073, Item_1075, Item_1079, Item_1106, Item_1911]
class Item_986(Item):
    name = "ID"
    title = ""
    var = Variation_372
class Item_1074(Item):
    name = "M1"
    title = ""
    var = Variation_558
class Item_1076(Item):
    name = "M2"
    title = ""
    var = Variation_642
class Item_1080(Item):
    name = "M3"
    title = ""
    var = Variation_744
class Item_1105(Item):
    name = "MC"
    title = ""
    var = Variation_808
class Item_1300(Item):
    name = "PO"
    title = ""
    var = Variation_865
class Variation_1012(Group):
    items = [Item_1086, Item_986, Item_754, Item_1074, Item_1076, Item_1080, Item_1105, Item_1300]
class Item_755(Item):
    name = "DA"
    title = ""
    var = Variation_477
class Item_1107(Item):
    name = "MC"
    title = ""
    var = Variation_819
class Variation_1013(Group):
    items = [Item_1086, Item_986, Item_755, Item_1074, Item_1076, Item_1080, Item_1107, Item_29]
class Item_1102(Item):
    name = "MBDATA"
    title = ""
    var = Variation_327
class Item_578(Item):
    name = "BDS1"
    title = ""
    var = Variation_127
class Item_580(Item):
    name = "BDS2"
    title = ""
    var = Variation_685
class Variation_1014(Group):
    items = [Item_1102, Item_578, Item_580]
class Item_1103(Item):
    name = "MBDATA"
    title = "56-bit Message Conveying Mode S Comm B Message Data"
    var = Variation_327
class Item_579(Item):
    name = "BDS1"
    title = "Comm B Data Buffer Store 1 Address"
    var = Variation_127
class Item_581(Item):
    name = "BDS2"
    title = "Comm B Data Buffer Store 2 Address"
    var = Variation_685
class Variation_1015(Group):
    items = [Item_1103, Item_579, Item_581]
class Item_1104(Item):
    name = "MBDATA"
    title = "Mode S Comm B Message Data"
    var = Variation_327
class Variation_1016(Group):
    items = [Item_1104, Item_579, Item_581]
class Item_1199(Item):
    name = "MT"
    title = "Message Type"
    var = Variation_149
class Item_1438(Item):
    name = "RG"
    title = "Report Generation"
    var = Variation_864
class Variation_1017(Group):
    items = [Item_1199, Item_1438]
class Item_1204(Item):
    name = "MV"
    title = "Manage Vertical Mode"
    var = Variation_64
class Item_498(Item):
    name = "AH"
    title = "Altitude Hold"
    var = Variation_373
class Item_513(Item):
    name = "AM"
    title = "Approach Mode"
    var = Variation_483
class Item_511(Item):
    name = "ALT"
    title = "Altitude in Two's Complement Form"
    var = Variation_608
class Variation_1018(Group):
    items = [Item_1204, Item_498, Item_513, Item_511]
class Item_500(Item):
    name = "AH"
    title = "Altitude Hold Mode"
    var = Variation_373
class Item_509(Item):
    name = "ALT"
    title = "Altitude"
    var = Variation_608
class Variation_1019(Group):
    items = [Item_1204, Item_500, Item_513, Item_509]
class Item_1205(Item):
    name = "MV"
    title = "Manage Vertical Mode"
    var = Variation_65
class Item_501(Item):
    name = "AH"
    title = "Altitude Hold Mode"
    var = Variation_374
class Item_514(Item):
    name = "AM"
    title = "Approach Mode"
    var = Variation_484
class Variation_1020(Group):
    items = [Item_1205, Item_501, Item_514, Item_509]
class Item_1220(Item):
    name = "NBVB"
    title = "Number of 'valid' Octets"
    var = Variation_235
class Item_1215(Item):
    name = "NBCELLS"
    title = "Number of 'valid' Cells"
    var = Variation_298
class Variation_1021(Group):
    items = [Item_1220, Item_1215]
class Item_1229(Item):
    name = "NOGO"
    title = ""
    var = Variation_105
class Item_1270(Item):
    name = "OVL"
    title = ""
    var = Variation_443
class Item_1778(Item):
    name = "TSV"
    title = ""
    var = Variation_536
class Item_1336(Item):
    name = "PSS"
    title = "Processing System Status"
    var = Variation_672
class Item_1649(Item):
    name = "STTN"
    title = "Track Re-numbering Indication"
    var = Variation_780
class Variation_1022(Group):
    items = [Item_1229, Item_1270, Item_1778, Item_1336, Item_1649, Item_29]
class Item_1232(Item):
    name = "NOGO"
    title = "Operational Release Status of the System"
    var = Variation_73
class Item_1415(Item):
    name = "RDPC"
    title = "Radar Data Processor Chain Selection Status"
    var = Variation_379
class Item_1416(Item):
    name = "RDPR"
    title = "Event to Signal a Reset/restart of the Selected Radar Data Processor Chain, I.e. Expect a New Assignment of Track Numbers"
    var = Variation_447
class Item_1274(Item):
    name = "OVLRDP"
    title = "Radar Data Processor Overload Indicator"
    var = Variation_550
class Item_1277(Item):
    name = "OVLXMT"
    title = "Transmission Subsystem Overload Status"
    var = Variation_629
class Item_1193(Item):
    name = "MSC"
    title = "Monitoring System Connected Status"
    var = Variation_745
class Item_1781(Item):
    name = "TSV"
    title = "Time Source Validity"
    var = Variation_837
class Variation_1023(Group):
    items = [Item_1232, Item_1415, Item_1416, Item_1274, Item_1277, Item_1193, Item_1781, Item_29]
class Item_1233(Item):
    name = "NOGO"
    title = "Operational Release Status of the System"
    var = Variation_103
class Item_1272(Item):
    name = "OVL"
    title = "Overload Indicator"
    var = Variation_480
class Item_1779(Item):
    name = "TSV"
    title = "Time Source Validity"
    var = Variation_584
class Item_782(Item):
    name = "DIV"
    title = ""
    var = Variation_650
class Item_1786(Item):
    name = "TTF"
    title = ""
    var = Variation_754
class Variation_1024(Group):
    items = [Item_1233, Item_1272, Item_1779, Item_782, Item_1786, Item_27]
class Item_1234(Item):
    name = "NOGO"
    title = "Operational Release Status of the System"
    var = Variation_104
class Item_1787(Item):
    name = "TTF"
    title = "Test Target"
    var = Variation_661
class Variation_1025(Group):
    items = [Item_1234, Item_1272, Item_1779, Item_1787, Item_25]
class Item_1246(Item):
    name = "NU1"
    title = "First Number"
    var = Variation_190
class Item_1247(Item):
    name = "NU2"
    title = "Second Number"
    var = Variation_190
class Item_1067(Item):
    name = "LTR"
    title = "Letter"
    var = Variation_190
class Variation_1026(Group):
    items = [Item_1246, Item_1247, Item_1067]
class Item_1264(Item):
    name = "ORG"
    title = ""
    var = Variation_49
class Item_962(Item):
    name = "I"
    title = "Intensity Level"
    var = Variation_408
class Item_20(Spare):
    bit_offset8 = 4
    bit_size = 2
class Item_887(Item):
    name = "FSTLST"
    title = ""
    var = Variation_840
class Item_733(Item):
    name = "CSN"
    title = "Contour Serial Number"
    var = Variation_155
class Variation_1027(Group):
    items = [Item_1264, Item_962, Item_20, Item_887, Item_733]
class Item_1291(Item):
    name = "PID"
    title = "Pair Identification"
    var = Variation_234
class Item_1713(Item):
    name = "TID"
    title = "Transmitter Identification"
    var = Variation_234
class Item_1448(Item):
    name = "RID"
    title = "Receiver Identification"
    var = Variation_234
class Variation_1028(Group):
    items = [Item_1291, Item_1713, Item_1448]
class Item_1292(Item):
    name = "PID"
    title = "Pair Identifier"
    var = Variation_235
class Item_1260(Item):
    name = "ON"
    title = "Observation Number"
    var = Variation_298
class Variation_1029(Group):
    items = [Item_1292, Item_1260]
class Item_1302(Item):
    name = "POL"
    title = "Polarization in Use by PSR"
    var = Variation_48
class Item_1424(Item):
    name = "REDRAD"
    title = "Reduction Steps in Use as Result of An Overload Within the PSR Subsystem"
    var = Variation_410
class Item_1633(Item):
    name = "STC"
    title = "Sensitivity Time Control Map in Use"
    var = Variation_673
class Variation_1030(Group):
    items = [Item_1302, Item_1424, Item_1633, Item_27]
class Item_1316(Item):
    name = "PREDRHO"
    title = "Predicted Range"
    var = Variation_285
class Item_1317(Item):
    name = "PREDTHETA"
    title = "Predicted Azimuth"
    var = Variation_293
class Item_849(Item):
    name = "EVOLRHOSTART"
    title = "Predicted Closest Range"
    var = Variation_285
class Item_848(Item):
    name = "EVOLRHOEND"
    title = "Predicted Largest Range"
    var = Variation_285
class Item_851(Item):
    name = "EVOLTHETASTART"
    title = "Predicted Smallest Azimuth"
    var = Variation_293
class Item_850(Item):
    name = "EVOLTHETAEND"
    title = "Predicted Largest Azimuth"
    var = Variation_293
class Item_1237(Item):
    name = "NOISERHOSTART"
    title = "Predicted Closest Range"
    var = Variation_285
class Item_1236(Item):
    name = "NOISERHOEND"
    title = "Predicted Largest Range"
    var = Variation_285
class Item_1239(Item):
    name = "NOISETHETASTART"
    title = "Predicted Smallest Azimuth"
    var = Variation_293
class Item_1238(Item):
    name = "NOISETHETAEND"
    title = "Predicted Largest Azimuth"
    var = Variation_293
class Item_1318(Item):
    name = "PREDTIME"
    title = "Predicted Detection Time"
    var = Variation_287
class Variation_1031(Group):
    items = [Item_1316, Item_1317, Item_849, Item_848, Item_851, Item_850, Item_1237, Item_1236, Item_1239, Item_1238, Item_1318]
class Item_1320(Item):
    name = "PREVIOUSII"
    title = "Former II Code"
    var = Variation_127
class Item_747(Item):
    name = "CURRENTII"
    title = "Current II Code"
    var = Variation_685
class Variation_1032(Group):
    items = [Item_1320, Item_747]
class Item_1321(Item):
    name = "PRG"
    title = "PSR Range Gain"
    var = Variation_247
class Item_1314(Item):
    name = "PRB"
    title = "PSR Range Bias"
    var = Variation_255
class Variation_1033(Group):
    items = [Item_1321, Item_1314]
class Item_1323(Item):
    name = "PRIORITY"
    title = "GICB Priority"
    var = Variation_138
class Item_1285(Item):
    name = "PC"
    title = "Periodicity Constraint"
    var = Variation_84
class Item_553(Item):
    name = "AU"
    title = "Asynchronous Update"
    var = Variation_359
class Item_1223(Item):
    name = "NE"
    title = "Non Extraction"
    var = Variation_494
class Item_1412(Item):
    name = "RD"
    title = "Reply Destination"
    var = Variation_592
class Variation_1034(Group):
    items = [Item_1323, Item_25, Item_1285, Item_553, Item_1223, Item_1412, Item_25]
class Item_1324(Item):
    name = "PRIORITY"
    title = "Priority"
    var = Variation_132
class Item_1311(Item):
    name = "POWER"
    title = "Power"
    var = Variation_690
class Item_808(Item):
    name = "DURATION"
    title = "Duration"
    var = Variation_206
class Item_708(Item):
    name = "COVERAGE"
    title = "Coverage"
    var = Variation_313
class Variation_1035(Group):
    items = [Item_1324, Item_1311, Item_808, Item_708]
class Item_1384(Item):
    name = "QNH"
    title = ""
    var = Variation_56
class Item_744(Item):
    name = "CTB"
    title = "Calculated Track Barometric Altitude"
    var = Variation_423
class Variation_1036(Group):
    items = [Item_1384, Item_744]
class Item_1385(Item):
    name = "QNH"
    title = "QNH Correction Applied"
    var = Variation_55
class Item_745(Item):
    name = "CTBA"
    title = "Calculated Track Barometric Altitude"
    var = Variation_423
class Variation_1037(Group):
    items = [Item_1385, Item_745]
class Item_1386(Item):
    name = "QNH"
    title = "QNH Correction Applied"
    var = Variation_56
class Variation_1038(Group):
    items = [Item_1386, Item_745]
class Item_1391(Item):
    name = "RA"
    title = "TCAS Resolution Advisory Active"
    var = Variation_74
class Item_1678(Item):
    name = "TC"
    title = "Target Trajectory Change Report Capability"
    var = Variation_399
class Item_1763(Item):
    name = "TS"
    title = "Target State Report Capability"
    var = Variation_562
class Item_540(Item):
    name = "ARV"
    title = "Air-Referenced Velocity Report Capability"
    var = Variation_646
class Item_619(Item):
    name = "CDTIA"
    title = "Cockpit Display of Traffic Information Airborne"
    var = Variation_709
class Item_1241(Item):
    name = "NOTTCAS"
    title = "TCAS System Status"
    var = Variation_831
class Item_1496(Item):
    name = "SA"
    title = "Single Antenna"
    var = Variation_850
class Variation_1039(Group):
    items = [Item_1391, Item_1678, Item_1763, Item_540, Item_619, Item_1241, Item_1496]
class Item_1419(Item):
    name = "RE"
    title = "Range Error"
    var = Variation_198
class Item_495(Item):
    name = "AE"
    title = "Azimuth Error"
    var = Variation_203
class Variation_1040(Group):
    items = [Item_1419, Item_495]
class Item_1420(Item):
    name = "RE"
    title = "Range Exceeded Indicator"
    var = Variation_91
class Item_600(Item):
    name = "BVR"
    title = "Barometric Vertical Rate"
    var = Variation_424
class Variation_1041(Group):
    items = [Item_1420, Item_600]
class Item_917(Item):
    name = "GS"
    title = "Ground Speed Referenced to WGS-84"
    var = Variation_427
class Item_1663(Item):
    name = "TA"
    title = "Track Angle Clockwise Reference to True North"
    var = Variation_293
class Variation_1042(Group):
    items = [Item_1420, Item_917, Item_1663]
class Item_928(Item):
    name = "GVR"
    title = "Geometric Vertical Rate"
    var = Variation_424
class Variation_1043(Group):
    items = [Item_1420, Item_928]
class Item_1673(Item):
    name = "TAS"
    title = "True Air Speed"
    var = Variation_425
class Variation_1044(Group):
    items = [Item_1420, Item_1673]
class Item_1423(Item):
    name = "REDRAD"
    title = "Reduction Steps in Use as Result of An Overload Within the Mode S Subsystem"
    var = Variation_124
class Item_645(Item):
    name = "CLU"
    title = "Cluster State"
    var = Variation_531
class Variation_1045(Group):
    items = [Item_1423, Item_645, Item_22]
class Item_1425(Item):
    name = "REDRAD"
    title = "Reduction Steps in Use as Result of An Overload Within the SSR Subsystem"
    var = Variation_124
class Variation_1046(Group):
    items = [Item_1425, Item_19]
class Item_1441(Item):
    name = "RHO"
    title = ""
    var = Variation_285
class Item_1699(Item):
    name = "THETA"
    title = ""
    var = Variation_293
class Variation_1047(Group):
    items = [Item_1441, Item_1699]
class Item_1442(Item):
    name = "RHO"
    title = ""
    var = Variation_288
class Variation_1048(Group):
    items = [Item_1442, Item_1699]
class Item_1443(Item):
    name = "RHO"
    title = "Measured Distance"
    var = Variation_288
class Item_1700(Item):
    name = "THETA"
    title = "Measured Azimuth"
    var = Variation_293
class Variation_1049(Group):
    items = [Item_1443, Item_1700]
class Item_1444(Item):
    name = "RHO"
    title = "RHO"
    var = Variation_265
class Item_1698(Item):
    name = "TH"
    title = "Theta"
    var = Variation_293
class Variation_1050(Group):
    items = [Item_1444, Item_1698]
class Item_1446(Item):
    name = "RHOST"
    title = "Rho Start"
    var = Variation_288
class Item_1445(Item):
    name = "RHOEND"
    title = "Rho End"
    var = Variation_288
class Item_1702(Item):
    name = "THETAST"
    title = "Theta Start"
    var = Variation_293
class Item_1701(Item):
    name = "THETAEND"
    title = "Theta End"
    var = Variation_293
class Variation_1051(Group):
    items = [Item_1446, Item_1445, Item_1702, Item_1701]
class Item_1447(Item):
    name = "RID"
    title = "Receiver Component ID"
    var = Variation_234
class Item_507(Item):
    name = "ALT"
    title = "Altitude"
    var = Variation_249
class Variation_1052(Group):
    items = [Item_1447, Item_1026, Item_1053, Item_507]
class Item_1451(Item):
    name = "RNG"
    title = "Range Error"
    var = Variation_198
class Item_570(Item):
    name = "AZM"
    title = "Azimuth Error"
    var = Variation_203
class Variation_1053(Group):
    items = [Item_1451, Item_570]
class Item_1465(Item):
    name = "RS"
    title = "Rho Start"
    var = Variation_285
class Item_1422(Item):
    name = "RE"
    title = "Rho End"
    var = Variation_285
class Item_1765(Item):
    name = "TS"
    title = "Theta Start"
    var = Variation_293
class Item_1696(Item):
    name = "TE"
    title = "Theta End"
    var = Variation_293
class Variation_1054(Group):
    items = [Item_1465, Item_1422, Item_1765, Item_1696]
class Item_1470(Item):
    name = "RSHPX"
    title = ""
    var = Variation_271
class Item_1471(Item):
    name = "RSHPY"
    title = ""
    var = Variation_271
class Item_696(Item):
    name = "CORSHPXY"
    title = ""
    var = Variation_197
class Variation_1055(Group):
    items = [Item_1470, Item_1471, Item_696]
class Item_1473(Item):
    name = "RSI"
    title = "8-bit Identification Number of RS"
    var = Variation_155
class Item_1466(Item):
    name = "RS1090"
    title = "Receiver 1090 MHz"
    var = Variation_376
class Item_1793(Item):
    name = "TX1030"
    title = "Transmitter 1030 MHz"
    var = Variation_486
class Item_1794(Item):
    name = "TX1090"
    title = "Transmitter 1090 MHz"
    var = Variation_570
class Item_1477(Item):
    name = "RSS"
    title = "RS Status"
    var = Variation_632
class Item_1474(Item):
    name = "RSO"
    title = "RS Operational"
    var = Variation_751
class Variation_1056(Group):
    items = [Item_1473, Item_0, Item_1466, Item_1793, Item_1794, Item_1477, Item_1474, Item_27]
class Item_1484(Item):
    name = "RTYP"
    title = "Report Type"
    var = Variation_142
class Variation_1057(Group):
    items = [Item_1484, Item_1438]
class Item_1492(Item):
    name = "S"
    title = ""
    var = Variation_259
class Item_810(Item):
    name = "E"
    title = ""
    var = Variation_259
class Variation_1058(Group):
    items = [Item_1492, Item_810]
class Item_1493(Item):
    name = "S"
    title = ""
    var = Variation_293
class Item_811(Item):
    name = "E"
    title = ""
    var = Variation_293
class Variation_1059(Group):
    items = [Item_1493, Item_811]
class Item_1501(Item):
    name = "SAC"
    title = "SAC of Radar Concerned"
    var = Variation_155
class Item_1556(Item):
    name = "SIC"
    title = "SIC of Radar Concerned"
    var = Variation_155
class Item_714(Item):
    name = "CP"
    title = "Circular Polarisation"
    var = Variation_527
class Item_1882(Item):
    name = "WO"
    title = "Weather Channel Overload"
    var = Variation_609
class Item_1389(Item):
    name = "R"
    title = "Reduction Step in Use By Radar  Concerned"
    var = Variation_771
class Variation_1060(Group):
    items = [Item_1501, Item_1556, Item_2, Item_714, Item_1882, Item_1389]
class Item_1502(Item):
    name = "SAC"
    title = "System Area Code"
    var = Variation_155
class Item_1557(Item):
    name = "SIC"
    title = "System Identification Code"
    var = Variation_155
class Variation_1061(Group):
    items = [Item_1502, Item_1557]
class Item_1801(Item):
    name = "TYP"
    title = ""
    var = Variation_686
class Variation_1062(Group):
    items = [Item_1502, Item_1557, Item_3, Item_1801]
class Item_1066(Item):
    name = "LTN"
    title = "Local Track Number"
    var = Variation_234
class Variation_1063(Group):
    items = [Item_1502, Item_1557, Item_3, Item_1801, Item_1066]
class Item_1558(Item):
    name = "SIC"
    title = "System Identity Code"
    var = Variation_155
class Variation_1064(Group):
    items = [Item_1502, Item_1558]
class Item_1503(Item):
    name = "SAC"
    title = "System Area Code Fixed to Zero"
    var = Variation_155
class Variation_1065(Group):
    items = [Item_1503, Item_1557]
class Item_1504(Item):
    name = "SACADJS"
    title = "SAC of the Adjacent Sensor"
    var = Variation_155
class Item_1559(Item):
    name = "SICADJS"
    title = "SIC of the Adjacent Sensor"
    var = Variation_155
class Item_1714(Item):
    name = "TIMEOFDAYSCN"
    title = "Absolute Timestamp in UTC Provided by the SCN"
    var = Variation_287
class Item_758(Item):
    name = "DATAUSE"
    title = "Use of Adjacent Sensor Data"
    var = Variation_143
class Item_797(Item):
    name = "DRNA"
    title = "DRN Availability"
    var = Variation_852
class Item_796(Item):
    name = "DRN"
    title = ""
    var = Variation_234
class Variation_1066(Group):
    items = [Item_1504, Item_1559, Item_1714, Item_758, Item_797, Item_796]
class Item_1510(Item):
    name = "SAS"
    title = ""
    var = Variation_62
class Item_1594(Item):
    name = "SRC"
    title = ""
    var = Variation_405
class Variation_1067(Group):
    items = [Item_1510, Item_1594, Item_511]
class Item_1511(Item):
    name = "SAS"
    title = "Source Availability"
    var = Variation_61
class Item_1495(Item):
    name = "S"
    title = "Source"
    var = Variation_404
class Variation_1068(Group):
    items = [Item_1511, Item_1495, Item_509]
class Item_1597(Item):
    name = "SRC"
    title = "Source"
    var = Variation_404
class Variation_1069(Group):
    items = [Item_1511, Item_1597, Item_509]
class Item_1522(Item):
    name = "SDAZR"
    title = ""
    var = Variation_290
class Item_653(Item):
    name = "COAZRAZ"
    title = ""
    var = Variation_197
class Variation_1070(Group):
    items = [Item_1522, Item_653]
class Item_1524(Item):
    name = "SDDA"
    title = ""
    var = Variation_284
class Item_654(Item):
    name = "CODADV"
    title = ""
    var = Variation_197
class Variation_1071(Group):
    items = [Item_1524, Item_654]
class Item_1528(Item):
    name = "SDELR"
    title = ""
    var = Variation_291
class Item_661(Item):
    name = "COELREL"
    title = ""
    var = Variation_197
class Variation_1072(Group):
    items = [Item_1528, Item_661]
class Item_1533(Item):
    name = "SDHPX"
    title = ""
    var = Variation_281
class Item_1534(Item):
    name = "SDHPY"
    title = ""
    var = Variation_281
class Item_698(Item):
    name = "COSDHPXY"
    title = ""
    var = Variation_197
class Variation_1073(Group):
    items = [Item_1533, Item_1534, Item_698]
class Item_1538(Item):
    name = "SDRA"
    title = ""
    var = Variation_286
class Item_693(Item):
    name = "CORAR"
    title = ""
    var = Variation_197
class Item_694(Item):
    name = "CORARR"
    title = ""
    var = Variation_197
class Variation_1074(Group):
    items = [Item_1538, Item_693, Item_694]
class Item_1540(Item):
    name = "SDRR"
    title = ""
    var = Variation_309
class Item_695(Item):
    name = "CORRR"
    title = ""
    var = Variation_197
class Variation_1075(Group):
    items = [Item_1540, Item_695]
class Item_1543(Item):
    name = "SDVA"
    title = ""
    var = Variation_276
class Item_703(Item):
    name = "COVAGH"
    title = ""
    var = Variation_197
class Item_707(Item):
    name = "COVAVV"
    title = ""
    var = Variation_197
class Variation_1076(Group):
    items = [Item_1543, Item_703, Item_707]
class Item_1544(Item):
    name = "SDVV"
    title = ""
    var = Variation_275
class Item_709(Item):
    name = "COVVGH"
    title = ""
    var = Variation_197
class Variation_1077(Group):
    items = [Item_1544, Item_709]
class Item_1561(Item):
    name = "SID"
    title = "Service Identification"
    var = Variation_127
class Item_1651(Item):
    name = "STYP"
    title = "Type of Service"
    var = Variation_688
class Variation_1078(Group):
    items = [Item_1561, Item_1651]
class Item_1566(Item):
    name = "SIGX"
    title = "Sigma (X)) Standard Deviation on the Horizontal Axis of the Local Grid System"
    var = Variation_218
class Item_1567(Item):
    name = "SIGY"
    title = "Sigma (Y)) Standard Deviation on the Vertical Axis of the Local Grid System"
    var = Variation_218
class Item_1565(Item):
    name = "SIGV"
    title = "Sigma (V)) Standard Deviation on the Groundspeed Within the Local Grid System"
    var = Variation_221
class Item_1564(Item):
    name = "SIGH"
    title = "Sigma (H)) Standard Deviation on the Heading Within the Local Grid System"
    var = Variation_225
class Variation_1079(Group):
    items = [Item_1566, Item_1567, Item_1565, Item_1564]
class Item_1599(Item):
    name = "SRG"
    title = "Mode S Range Gain"
    var = Variation_247
class Item_1593(Item):
    name = "SRB"
    title = "Mode S Range Bias"
    var = Variation_255
class Variation_1080(Group):
    items = [Item_1599, Item_1593]
class Item_1620(Item):
    name = "STARTAZ"
    title = "Start Azimuth of the Cells Group"
    var = Variation_293
class Item_827(Item):
    name = "ENDAZ"
    title = "End Azimuth of the Cells Group"
    var = Variation_293
class Item_1621(Item):
    name = "STARTRG"
    title = "Starting Range of the Cells Group, Expressed in Number of Cells"
    var = Variation_315
class Item_621(Item):
    name = "CELLDUR"
    title = "Video Cell Duration in Femto-seconds"
    var = Variation_321
class Variation_1081(Group):
    items = [Item_1620, Item_827, Item_1621, Item_621]
class Item_622(Item):
    name = "CELLDUR"
    title = "Video Cell Duration in Nano-seconds"
    var = Variation_322
class Variation_1082(Group):
    items = [Item_1620, Item_827, Item_1621, Item_622]
class Item_1637(Item):
    name = "STI"
    title = ""
    var = Variation_92
class Item_636(Item):
    name = "CHR"
    title = "Characters 1-8 (Coded on 6 Bits Each) Defining Target Identification"
    var = Variation_326
class Variation_1083(Group):
    items = [Item_1637, Item_15, Item_636]
class Item_1638(Item):
    name = "STI"
    title = ""
    var = Variation_93
class Variation_1084(Group):
    items = [Item_1638, Item_15, Item_636]
class Item_1707(Item):
    name = "TID"
    title = "Target Identification"
    var = Variation_326
class Variation_1085(Group):
    items = [Item_1638, Item_15, Item_1707]
class Item_1639(Item):
    name = "STI"
    title = ""
    var = Variation_94
class Item_637(Item):
    name = "CHR"
    title = "Characters 1-8 (coded on 6 Bits Each) Defining Target Identification"
    var = Variation_326
class Variation_1086(Group):
    items = [Item_1639, Item_15, Item_637]
class Item_1644(Item):
    name = "STR"
    title = "Start Range"
    var = Variation_155
class Item_828(Item):
    name = "ENDR"
    title = "End Range"
    var = Variation_155
class Item_567(Item):
    name = "AZ"
    title = "Azimuth"
    var = Variation_293
class Variation_1087(Group):
    items = [Item_1644, Item_828, Item_567]
class Item_1681(Item):
    name = "TCA"
    title = ""
    var = Variation_75
class Item_1221(Item):
    name = "NC"
    title = ""
    var = Variation_380
class Item_1692(Item):
    name = "TCPN"
    title = ""
    var = Variation_518
class Item_510(Item):
    name = "ALT"
    title = "Altitude in Two's Complement Form"
    var = Variation_239
class Item_1021(Item):
    name = "LAT"
    title = "In WGS.84 in Two's Complement"
    var = Variation_304
class Item_1048(Item):
    name = "LON"
    title = "In WGS.84 in Two's Complement"
    var = Variation_304
class Item_1338(Item):
    name = "PT"
    title = "Point Type"
    var = Variation_130
class Item_1694(Item):
    name = "TD"
    title = ""
    var = Variation_671
class Item_1743(Item):
    name = "TRA"
    title = ""
    var = Variation_832
class Item_1722(Item):
    name = "TOA"
    title = ""
    var = Variation_866
class Item_1733(Item):
    name = "TOV"
    title = "Time Over Point"
    var = Variation_306
class Item_1790(Item):
    name = "TTR"
    title = "TCP Turn Radius"
    var = Variation_273
class Variation_1088(Group):
    items = [Item_1681, Item_1221, Item_1692, Item_510, Item_1021, Item_1048, Item_1338, Item_1694, Item_1743, Item_1722, Item_1733, Item_1790]
class Item_1682(Item):
    name = "TCA"
    title = "TCP Number Availability"
    var = Variation_75
class Item_1222(Item):
    name = "NC"
    title = "TCP Compliance"
    var = Variation_380
class Item_1693(Item):
    name = "TCPN"
    title = "Trajectory Change Point Number"
    var = Variation_518
class Item_1031(Item):
    name = "LAT"
    title = "Latitude in WGS.84 in Two's Complement"
    var = Variation_304
class Item_1058(Item):
    name = "LON"
    title = "Longitude in WGS.84 in Two's Complement"
    var = Variation_304
class Item_1695(Item):
    name = "TD"
    title = "Turn Direction"
    var = Variation_671
class Item_1745(Item):
    name = "TRA"
    title = "Turn Radius Availability"
    var = Variation_832
class Item_1723(Item):
    name = "TOA"
    title = "TOV Available"
    var = Variation_866
class Variation_1089(Group):
    items = [Item_1682, Item_1222, Item_1693, Item_510, Item_1031, Item_1058, Item_1338, Item_1695, Item_1745, Item_1723, Item_1733, Item_1790]
class Item_1703(Item):
    name = "TI"
    title = "Turn Indicator"
    var = Variation_101
class Item_1454(Item):
    name = "ROT"
    title = "Rate of Turn in Two's Complement Form"
    var = Variation_150
class Variation_1090(Group):
    items = [Item_1703, Item_15, Item_1454, Item_29]
class Item_1706(Item):
    name = "TID"
    title = "Identification of Conflict Categories Definition Table"
    var = Variation_127
class Item_716(Item):
    name = "CP"
    title = "Conflict Properties Class"
    var = Variation_679
class Item_731(Item):
    name = "CS"
    title = "Conflict Severity"
    var = Variation_860
class Variation_1091(Group):
    items = [Item_1706, Item_716, Item_731]
class Item_1712(Item):
    name = "TID"
    title = "Transmitter ID"
    var = Variation_234
class Item_1789(Item):
    name = "TTO"
    title = "Transmission Time Offset"
    var = Variation_316
class Item_549(Item):
    name = "ATO"
    title = "Accuracy of Transmission Time Offset"
    var = Variation_701
class Item_1286(Item):
    name = "PCI"
    title = "Parallel Transmitter Index"
    var = Variation_235
class Variation_1092(Group):
    items = [Item_1712, Item_1026, Item_1053, Item_507, Item_1789, Item_3, Item_549, Item_1286]
class Item_1734(Item):
    name = "TP1A"
    title = ""
    var = Variation_71
class Item_1735(Item):
    name = "TP1B"
    title = ""
    var = Variation_357
class Item_1736(Item):
    name = "TP2A"
    title = ""
    var = Variation_492
class Item_1737(Item):
    name = "TP2B"
    title = ""
    var = Variation_552
class Item_1738(Item):
    name = "TP3A"
    title = ""
    var = Variation_658
class Item_1739(Item):
    name = "TP3B"
    title = ""
    var = Variation_734
class Item_1740(Item):
    name = "TP4A"
    title = ""
    var = Variation_830
class Item_1741(Item):
    name = "TP4B"
    title = ""
    var = Variation_853
class Variation_1093(Group):
    items = [Item_1734, Item_1735, Item_1736, Item_1737, Item_1738, Item_1739, Item_1740, Item_1741]
class Item_1746(Item):
    name = "TRANS"
    title = "Transversal Acceleration"
    var = Variation_96
class Item_1060(Item):
    name = "LONG"
    title = "Longitudinal Acceleration"
    var = Variation_506
class Item_1854(Item):
    name = "VERT"
    title = "Transversal Acceleration"
    var = Variation_670
class Item_486(Item):
    name = "ADF"
    title = "Altitude Discrepancy Flag"
    var = Variation_821
class Variation_1094(Group):
    items = [Item_1746, Item_1060, Item_1854, Item_486, Item_29]
class Item_1747(Item):
    name = "TRB"
    title = ""
    var = Variation_29
class Item_1195(Item):
    name = "MSG"
    title = ""
    var = Variation_418
class Variation_1095(Group):
    items = [Item_1747, Item_1195]
class Item_1196(Item):
    name = "MSG"
    title = ""
    var = Variation_419
class Variation_1096(Group):
    items = [Item_1747, Item_1196]
class Item_1748(Item):
    name = "TRB"
    title = "In Trouble"
    var = Variation_29
class Item_1197(Item):
    name = "MSG"
    title = "Message"
    var = Variation_418
class Variation_1097(Group):
    items = [Item_1748, Item_1197]
class Item_1796(Item):
    name = "TYP"
    title = ""
    var = Variation_106
class Item_13(Spare):
    bit_offset8 = 2
    bit_size = 3
class Item_1216(Item):
    name = "NBR"
    title = ""
    var = Variation_778
class Variation_1098(Group):
    items = [Item_1796, Item_13, Item_1216]
class Item_1219(Item):
    name = "NBR"
    title = "Number from 0 to 99 999 999"
    var = Variation_778
class Variation_1099(Group):
    items = [Item_1796, Item_13, Item_1219]
class Item_1799(Item):
    name = "TYP"
    title = ""
    var = Variation_136
class Item_760(Item):
    name = "DAY"
    title = ""
    var = Variation_767
class Item_30(Spare):
    bit_offset8 = 7
    bit_size = 4
class Item_949(Item):
    name = "HOR"
    title = ""
    var = Variation_607
class Item_1154(Item):
    name = "MIN"
    title = ""
    var = Variation_519
class Item_557(Item):
    name = "AVS"
    title = ""
    var = Variation_70
class Item_7(Spare):
    bit_offset8 = 1
    bit_size = 1
class Item_1546(Item):
    name = "SEC"
    title = ""
    var = Variation_519
class Variation_1100(Group):
    items = [Item_1799, Item_760, Item_30, Item_949, Item_1, Item_1154, Item_557, Item_7, Item_1546]
class Item_1800(Item):
    name = "TYP"
    title = ""
    var = Variation_137
class Item_950(Item):
    name = "HOR"
    title = "Hours"
    var = Variation_607
class Item_1155(Item):
    name = "MIN"
    title = "Minutes"
    var = Variation_519
class Item_559(Item):
    name = "AVS"
    title = "Seconds Available Flag"
    var = Variation_70
class Item_1547(Item):
    name = "SEC"
    title = "Seconds"
    var = Variation_519
class Variation_1101(Group):
    items = [Item_1800, Item_760, Item_30, Item_950, Item_1, Item_1155, Item_559, Item_7, Item_1547]
class Item_1803(Item):
    name = "TYP"
    title = "IFPS Flight ID Type"
    var = Variation_107
class Item_1218(Item):
    name = "NBR"
    title = "IFPS Flight ID Number"
    var = Variation_777
class Variation_1102(Group):
    items = [Item_1803, Item_13, Item_1218]
class Item_1804(Item):
    name = "TYP"
    title = "Message Type (= 28 for 1090 ES, Version 2)"
    var = Variation_133
class Item_1650(Item):
    name = "STYP"
    title = "Message Sub-type (= 2 for 1090 ES, Version 2)"
    var = Variation_771
class Item_534(Item):
    name = "ARA"
    title = "Active Resolution Advisories"
    var = Variation_231
class Item_1399(Item):
    name = "RAC"
    title = "RAC (RA Complement) Record"
    var = Variation_844
class Item_1407(Item):
    name = "RAT"
    title = "RA Terminated"
    var = Variation_428
class Item_1200(Item):
    name = "MTE"
    title = "Multiple Threat Encounter"
    var = Variation_527
class Item_1788(Item):
    name = "TTI"
    title = "Threat Type Indicator"
    var = Variation_668
class Item_1708(Item):
    name = "TID"
    title = "Threat Identity Data"
    var = Variation_849
class Variation_1103(Group):
    items = [Item_1804, Item_1650, Item_534, Item_1399, Item_1407, Item_1200, Item_1788, Item_1708]
class Item_1805(Item):
    name = "TYP"
    title = "Report Type"
    var = Variation_121
class Item_1574(Item):
    name = "SIM"
    title = ""
    var = Variation_528
class Item_1394(Item):
    name = "RAB"
    title = ""
    var = Variation_657
class Item_1775(Item):
    name = "TST"
    title = ""
    var = Variation_752
class Variation_1104(Group):
    items = [Item_1805, Item_1574, Item_1394, Item_1775, Item_27]
class Item_1806(Item):
    name = "TYP"
    title = "Time Type"
    var = Variation_137
class Item_761(Item):
    name = "DAY"
    title = "Day"
    var = Variation_766
class Item_951(Item):
    name = "HOR"
    title = "Hours, from 0 to 23"
    var = Variation_607
class Item_1156(Item):
    name = "MIN"
    title = "Minutes, from 0 to 59"
    var = Variation_519
class Item_558(Item):
    name = "AVS"
    title = "Seconds Available"
    var = Variation_70
class Item_1548(Item):
    name = "SEC"
    title = "Seconds, from 0 to 59"
    var = Variation_519
class Variation_1105(Group):
    items = [Item_1806, Item_761, Item_30, Item_951, Item_1, Item_1156, Item_558, Item_7, Item_1548]
class Item_1807(Item):
    name = "TYP"
    title = "Type of Message Counter"
    var = Variation_134
class Item_699(Item):
    name = "COUNT"
    title = "COUNTER"
    var = Variation_776
class Variation_1106(Group):
    items = [Item_1807, Item_699]
class Item_1808(Item):
    name = "TYP"
    title = "Type of Message Counter"
    var = Variation_135
class Variation_1107(Group):
    items = [Item_1808, Item_699]
class Item_1809(Item):
    name = "TYPE"
    title = ""
    var = Variation_129
class Item_1266(Item):
    name = "ORIGIN"
    title = ""
    var = Variation_669
class Item_1632(Item):
    name = "STATE"
    title = ""
    var = Variation_839
class Variation_1108(Group):
    items = [Item_1809, Item_1266, Item_1632]
class Item_1810(Item):
    name = "TYPE"
    title = "Reply Type"
    var = Variation_165
class Item_1434(Item):
    name = "REPLYNBR"
    title = ""
    var = Variation_234
class Variation_1109(Group):
    items = [Item_1810, Item_1434]
class Item_1811(Item):
    name = "TYPE"
    title = "Type of Report Counter"
    var = Variation_163
class Item_1429(Item):
    name = "REF"
    title = "Reference from which the Messages Are Countered"
    var = Variation_43
class Item_749(Item):
    name = "CV"
    title = "32-bit Counter Value"
    var = Variation_313
class Variation_1110(Group):
    items = [Item_1811, Item_1429, Item_10, Item_749]
class Item_1812(Item):
    name = "TYPE"
    title = "Type of Report Counter"
    var = Variation_164
class Item_1428(Item):
    name = "REF"
    title = "Reference from which the Messages Are Counted"
    var = Variation_42
class Item_700(Item):
    name = "COUNT"
    title = "Counter Value"
    var = Variation_315
class Variation_1111(Group):
    items = [Item_1812, Item_1428, Item_10, Item_700]
class Item_1821(Item):
    name = "UCI6"
    title = ""
    var = Variation_229
class Item_1034(Item):
    name = "LCI6"
    title = ""
    var = Variation_696
class Variation_1112(Group):
    items = [Item_1821, Item_1034]
class Item_1822(Item):
    name = "UCI9"
    title = ""
    var = Variation_229
class Item_1035(Item):
    name = "LCI9"
    title = ""
    var = Variation_696
class Variation_1113(Group):
    items = [Item_1822, Item_1035]
class Item_1825(Item):
    name = "UM"
    title = "Uplink Mask"
    var = Variation_89
class Item_786(Item):
    name = "DM"
    title = "Downlink Mask"
    var = Variation_336
class Item_1820(Item):
    name = "UC"
    title = "Uplink Command"
    var = Variation_502
class Item_771(Item):
    name = "DC"
    title = "Downlink Command"
    var = Variation_586
class Variation_1114(Group):
    items = [Item_1825, Item_786, Item_1820, Item_771, Item_22]
class Item_1827(Item):
    name = "V"
    title = ""
    var = Variation_13
class Item_892(Item):
    name = "G"
    title = ""
    var = Variation_338
class Item_1004(Item):
    name = "L"
    title = ""
    var = Variation_461
class Item_16(Spare):
    bit_offset8 = 3
    bit_size = 1
class Item_817(Item):
    name = "EM1"
    title = "Extended Mode 1 Code in Octal Representation"
    var = Variation_694
class Variation_1115(Group):
    items = [Item_1827, Item_892, Item_1004, Item_16, Item_817]
class Item_1828(Item):
    name = "V"
    title = ""
    var = Variation_14
class Item_1005(Item):
    name = "L"
    title = ""
    var = Variation_462
class Variation_1116(Group):
    items = [Item_1828, Item_7, Item_1005, Item_16, Item_817]
class Item_1009(Item):
    name = "L"
    title = ""
    var = Variation_466
class Item_1168(Item):
    name = "MODE2"
    title = "Mode 2 Code in Octal Representation"
    var = Variation_694
class Variation_1117(Group):
    items = [Item_1828, Item_7, Item_1009, Item_16, Item_1168]
class Item_12(Spare):
    bit_offset8 = 2
    bit_size = 2
class Item_1176(Item):
    name = "MODEC"
    title = "Mode-C Reply in Gray Notation"
    var = Variation_693
class Item_1369(Item):
    name = "QC2"
    title = ""
    var = Variation_812
class Item_1349(Item):
    name = "QA2"
    title = ""
    var = Variation_854
class Item_1371(Item):
    name = "QC4"
    title = ""
    var = Variation_45
class Item_1351(Item):
    name = "QA4"
    title = ""
    var = Variation_362
class Item_1356(Item):
    name = "QB1"
    title = ""
    var = Variation_457
class Item_1374(Item):
    name = "QD1"
    title = ""
    var = Variation_555
class Item_1360(Item):
    name = "QB2"
    title = ""
    var = Variation_635
class Item_1377(Item):
    name = "QD2"
    title = ""
    var = Variation_738
class Item_1363(Item):
    name = "QB4"
    title = ""
    var = Variation_811
class Item_1381(Item):
    name = "QD4"
    title = ""
    var = Variation_858
class Variation_1118(Group):
    items = [Item_1828, Item_892, Item_12, Item_1176, Item_3, Item_1366, Item_1344, Item_1369, Item_1349, Item_1371, Item_1351, Item_1356, Item_1374, Item_1360, Item_1377, Item_1363, Item_1381]
class Item_856(Item):
    name = "FL"
    title = ""
    var = Variation_524
class Variation_1119(Group):
    items = [Item_1828, Item_892, Item_856]
class Item_857(Item):
    name = "FL"
    title = "Flight Level"
    var = Variation_523
class Variation_1120(Group):
    items = [Item_1828, Item_892, Item_857]
class Item_947(Item):
    name = "HGT"
    title = "Mode-C HEIGHT"
    var = Variation_523
class Variation_1121(Group):
    items = [Item_1828, Item_892, Item_947]
class Item_1007(Item):
    name = "L"
    title = ""
    var = Variation_464
class Item_1166(Item):
    name = "MODE1"
    title = "Mode-1 Code"
    var = Variation_606
class Variation_1122(Group):
    items = [Item_1828, Item_892, Item_1007, Item_1166]
class Variation_1123(Group):
    items = [Item_1828, Item_892, Item_1009, Item_16, Item_1169]
class Item_1011(Item):
    name = "L"
    title = ""
    var = Variation_468
class Variation_1124(Group):
    items = [Item_1828, Item_892, Item_1011, Item_16, Item_1175]
class Item_1012(Item):
    name = "L"
    title = ""
    var = Variation_469
class Item_1164(Item):
    name = "MOD3A"
    title = ""
    var = Variation_694
class Variation_1125(Group):
    items = [Item_1828, Item_892, Item_1012, Item_16, Item_1164]
class Variation_1126(Group):
    items = [Item_1828, Item_892, Item_1012, Item_16, Item_1175]
class Item_1014(Item):
    name = "L"
    title = ""
    var = Variation_471
class Variation_1127(Group):
    items = [Item_1828, Item_892, Item_1014, Item_16, Item_1175]
class Item_1015(Item):
    name = "L"
    title = ""
    var = Variation_472
class Item_1172(Item):
    name = "MODE3A"
    title = "Mode 3/A Reply in Octal Representation"
    var = Variation_694
class Variation_1128(Group):
    items = [Item_1828, Item_892, Item_1015, Item_16, Item_1172]
class Item_893(Item):
    name = "G"
    title = ""
    var = Variation_339
class Item_508(Item):
    name = "ALT"
    title = "Altitude"
    var = Variation_524
class Variation_1129(Group):
    items = [Item_1828, Item_893, Item_508]
class Item_1829(Item):
    name = "V"
    title = "Validated"
    var = Variation_14
class Item_894(Item):
    name = "G"
    title = "Garbled"
    var = Variation_338
class Item_1367(Item):
    name = "QC1"
    title = "Quality Pulse C1"
    var = Variation_636
class Item_1346(Item):
    name = "QA1"
    title = "Quality Pulse A1"
    var = Variation_735
class Item_1370(Item):
    name = "QC2"
    title = "Quality Pulse C2"
    var = Variation_812
class Item_1350(Item):
    name = "QA2"
    title = "Quality Pulse A2"
    var = Variation_854
class Item_1373(Item):
    name = "QC4"
    title = "Quality Pulse C4"
    var = Variation_45
class Item_1354(Item):
    name = "QA4"
    title = "Quality Pulse A4"
    var = Variation_362
class Item_1358(Item):
    name = "QB1"
    title = "Quality Pulse B1"
    var = Variation_457
class Item_1376(Item):
    name = "QD1"
    title = "Quality Pulse D1"
    var = Variation_555
class Item_1362(Item):
    name = "QB2"
    title = "Quality Pulse B2"
    var = Variation_635
class Item_1379(Item):
    name = "QD2"
    title = "Quality Pulse D2"
    var = Variation_737
class Item_1365(Item):
    name = "QB4"
    title = "Quality Pulse B4"
    var = Variation_811
class Item_1382(Item):
    name = "QD4"
    title = "Quality Pulse D4"
    var = Variation_858
class Variation_1130(Group):
    items = [Item_1829, Item_894, Item_12, Item_1176, Item_3, Item_1367, Item_1346, Item_1370, Item_1350, Item_1373, Item_1354, Item_1358, Item_1376, Item_1362, Item_1379, Item_1365, Item_1382]
class Item_629(Item):
    name = "CH"
    title = "Change in Mode 3/A"
    var = Variation_478
class Variation_1131(Group):
    items = [Item_1829, Item_894, Item_629, Item_16, Item_1175]
class Variation_1132(Group):
    items = [Item_1829, Item_894, Item_857]
class Item_1006(Item):
    name = "L"
    title = ""
    var = Variation_463
class Variation_1133(Group):
    items = [Item_1829, Item_894, Item_1006, Item_16, Item_1175]
class Item_1008(Item):
    name = "L"
    title = ""
    var = Variation_465
class Item_1167(Item):
    name = "MODE1"
    title = "Mode-1 Code in Octal Representation"
    var = Variation_606
class Variation_1134(Group):
    items = [Item_1829, Item_894, Item_1008, Item_1167]
class Item_1010(Item):
    name = "L"
    title = ""
    var = Variation_467
class Item_1170(Item):
    name = "MODE2"
    title = "Mode-2 Reply in Octal Representation"
    var = Variation_694
class Variation_1135(Group):
    items = [Item_1829, Item_894, Item_1010, Item_16, Item_1170]
class Variation_1136(Group):
    items = [Item_1829, Item_894, Item_1012, Item_16, Item_1175]
class Item_1013(Item):
    name = "L"
    title = ""
    var = Variation_470
class Variation_1137(Group):
    items = [Item_1829, Item_894, Item_1013, Item_16, Item_1175]
class Item_1042(Item):
    name = "LMC"
    title = "Last Measured Mode C Code"
    var = Variation_523
class Variation_1138(Group):
    items = [Item_1829, Item_894, Item_1042]
class Item_1851(Item):
    name = "VDL"
    title = "VDL Mode 4"
    var = Variation_90
class Item_1127(Item):
    name = "MDS"
    title = "Mode S"
    var = Variation_367
class Item_1817(Item):
    name = "UAT"
    title = "UAT"
    var = Variation_499
class Variation_1139(Group):
    items = [Item_1851, Item_1127, Item_1817, Item_19]
class Item_1869(Item):
    name = "VX"
    title = ""
    var = Variation_250
class Item_1873(Item):
    name = "VY"
    title = ""
    var = Variation_250
class Variation_1140(Group):
    items = [Item_1869, Item_1873]
class Item_1870(Item):
    name = "VX"
    title = "Velocity (X-component)"
    var = Variation_250
class Item_1874(Item):
    name = "VY"
    title = "Velocity (Y-component)"
    var = Variation_250
class Variation_1141(Group):
    items = [Item_1870, Item_1874]
class Item_1871(Item):
    name = "VX"
    title = "Vx"
    var = Variation_250
class Item_1875(Item):
    name = "VY"
    title = "Vy"
    var = Variation_250
class Variation_1142(Group):
    items = [Item_1871, Item_1875]
class Item_1872(Item):
    name = "VX"
    title = "X Velocity"
    var = Variation_252
class Item_1876(Item):
    name = "VY"
    title = "Y Velocity"
    var = Variation_252
class Variation_1143(Group):
    items = [Item_1872, Item_1876]
class Item_1885(Item):
    name = "WS"
    title = "Wind Speed Valid Flag"
    var = Variation_66
class Item_1878(Item):
    name = "WD"
    title = "Wind Direction Valid Flag"
    var = Variation_377
class Item_1719(Item):
    name = "TMP"
    title = "Temperature Valid Flag"
    var = Variation_487
class Item_1750(Item):
    name = "TRB"
    title = "Turbulence Valid Flag"
    var = Variation_571
class Item_1886(Item):
    name = "WSD"
    title = "Wind Speed"
    var = Variation_264
class Item_1879(Item):
    name = "WDD"
    title = "Wind Direction"
    var = Variation_269
class Item_1720(Item):
    name = "TMPD"
    title = "Temperature in Degrees Celsius"
    var = Variation_251
class Item_1751(Item):
    name = "TRBD"
    title = "Turbulence"
    var = Variation_191
class Variation_1144(Group):
    items = [Item_1885, Item_1878, Item_1719, Item_1750, Item_22, Item_1886, Item_1879, Item_1720, Item_1751]
class Item_1889(Item):
    name = "X"
    title = ""
    var = Variation_197
class Item_1932(Item):
    name = "Y"
    title = ""
    var = Variation_197
class Variation_1145(Group):
    items = [Item_1889, Item_1932]
class Item_1890(Item):
    name = "X"
    title = ""
    var = Variation_228
class Item_1934(Item):
    name = "Y"
    title = ""
    var = Variation_695
class Variation_1146(Group):
    items = [Item_1890, Item_1934]
class Item_1891(Item):
    name = "X"
    title = ""
    var = Variation_230
class Item_1935(Item):
    name = "Y"
    title = ""
    var = Variation_699
class Item_671(Item):
    name = "COHAXY"
    title = ""
    var = Variation_197
class Variation_1147(Group):
    items = [Item_1891, Item_1935, Item_671]
class Item_1892(Item):
    name = "X"
    title = ""
    var = Variation_275
class Item_1933(Item):
    name = "Y"
    title = ""
    var = Variation_275
class Item_678(Item):
    name = "COHVXY"
    title = ""
    var = Variation_197
class Variation_1148(Group):
    items = [Item_1892, Item_1933, Item_678]
class Item_697(Item):
    name = "CORSHVXY"
    title = ""
    var = Variation_197
class Variation_1149(Group):
    items = [Item_1892, Item_1933, Item_697]
class Item_1893(Item):
    name = "X"
    title = ""
    var = Variation_295
class Item_1936(Item):
    name = "Y"
    title = ""
    var = Variation_700
class Variation_1150(Group):
    items = [Item_1893, Item_1936]
class Item_1894(Item):
    name = "X"
    title = "AA (X-Component)"
    var = Variation_216
class Item_1937(Item):
    name = "Y"
    title = "AA (Y-Component)"
    var = Variation_216
class Variation_1151(Group):
    items = [Item_1894, Item_1937]
class Item_1895(Item):
    name = "X"
    title = "APC (X-Component)"
    var = Variation_271
class Item_1938(Item):
    name = "Y"
    title = "APC (Y-Component)"
    var = Variation_271
class Variation_1152(Group):
    items = [Item_1895, Item_1938]
class Item_1896(Item):
    name = "X"
    title = "ATV (X-Component)"
    var = Variation_215
class Item_1939(Item):
    name = "Y"
    title = "ATV (Y-Component)"
    var = Variation_215
class Variation_1153(Group):
    items = [Item_1896, Item_1939]
class Item_1897(Item):
    name = "X"
    title = "DOP (X-Component)"
    var = Variation_279
class Item_1940(Item):
    name = "Y"
    title = "DOP (Y-Component)"
    var = Variation_279
class Item_1930(Item):
    name = "XY"
    title = "DOP (Correlation XY)"
    var = Variation_279
class Variation_1154(Group):
    items = [Item_1897, Item_1940, Item_1930]
class Item_1898(Item):
    name = "X"
    title = "Estimated Accuracy Of Acceleration of X Component"
    var = Variation_212
class Item_1941(Item):
    name = "Y"
    title = "Estimated Accuracy Of Acceleration of Y Component"
    var = Variation_212
class Variation_1155(Group):
    items = [Item_1898, Item_1941]
class Item_1899(Item):
    name = "X"
    title = "Estimated Accuracy of the Calculated Position of X Component"
    var = Variation_214
class Item_1942(Item):
    name = "Y"
    title = "Estimated Accuracy of the Calculated Position of Y Component"
    var = Variation_214
class Variation_1156(Group):
    items = [Item_1899, Item_1942]
class Item_1900(Item):
    name = "X"
    title = "Estimated Accuracy of the Calculated Velocity of X Component"
    var = Variation_210
class Item_1943(Item):
    name = "Y"
    title = "Estimated Accuracy of the Calculated Velocity of Y Component"
    var = Variation_210
class Variation_1157(Group):
    items = [Item_1900, Item_1943]
class Item_1901(Item):
    name = "X"
    title = "SDP (X-Component)"
    var = Variation_281
class Item_1944(Item):
    name = "Y"
    title = "SDP (Y-Component)"
    var = Variation_281
class Item_1931(Item):
    name = "XY"
    title = "SDP (Correlation XY)"
    var = Variation_279
class Variation_1158(Group):
    items = [Item_1901, Item_1944, Item_1931]
class Item_1902(Item):
    name = "X"
    title = "Starting X-position of the Conflict"
    var = Variation_299
class Item_1945(Item):
    name = "Y"
    title = "Starting Y-position of the Conflict"
    var = Variation_299
class Item_1962(Item):
    name = "Z"
    title = "Starting Z-position of the Conflict"
    var = Variation_240
class Variation_1159(Group):
    items = [Item_1902, Item_1945, Item_1962]
class Item_1903(Item):
    name = "X"
    title = "X Coordinate"
    var = Variation_237
class Item_1947(Item):
    name = "Y"
    title = "Y Coordinate"
    var = Variation_237
class Variation_1160(Group):
    items = [Item_1903, Item_1947]
class Item_1904(Item):
    name = "X"
    title = "X Coordinate"
    var = Variation_299
class Item_1948(Item):
    name = "Y"
    title = "Y Coordinate"
    var = Variation_299
class Variation_1161(Group):
    items = [Item_1904, Item_1948]
class Item_1905(Item):
    name = "X"
    title = "X-Component"
    var = Variation_155
class Item_1949(Item):
    name = "Y"
    title = "Y-Component"
    var = Variation_155
class Item_1040(Item):
    name = "LENGTH"
    title = "Length"
    var = Variation_155
class Variation_1162(Group):
    items = [Item_1905, Item_1949, Item_1040]
class Item_1906(Item):
    name = "X"
    title = "X-Component"
    var = Variation_237
class Item_1950(Item):
    name = "Y"
    title = "Y-Component"
    var = Variation_237
class Variation_1163(Group):
    items = [Item_1906, Item_1950]
class Item_1907(Item):
    name = "X"
    title = "X-Component"
    var = Variation_253
class Item_1951(Item):
    name = "Y"
    title = "Y-Component"
    var = Variation_253
class Variation_1164(Group):
    items = [Item_1907, Item_1951]
class Item_1908(Item):
    name = "X"
    title = "X-Component"
    var = Variation_255
class Item_1946(Item):
    name = "Y"
    title = "X-Component"
    var = Variation_255
class Variation_1165(Group):
    items = [Item_1908, Item_1946]
class Item_1952(Item):
    name = "Y"
    title = "Y-Component"
    var = Variation_255
class Variation_1166(Group):
    items = [Item_1908, Item_1952]
class Item_1909(Item):
    name = "X"
    title = "X-coordinate"
    var = Variation_236
class Item_1953(Item):
    name = "Y"
    title = "Y-coordinate"
    var = Variation_236
class Item_1016(Item):
    name = "L"
    title = "Vector Length"
    var = Variation_235
class Variation_1167(Group):
    items = [Item_1909, Item_1953, Item_1016]
class Item_1910(Item):
    name = "X"
    title = "X-coordinate"
    var = Variation_299
class Item_1954(Item):
    name = "Y"
    title = "Y-coordinate"
    var = Variation_299
class Variation_1168(Group):
    items = [Item_1910, Item_1954]
class Item_1912(Item):
    name = "X1"
    title = ""
    var = Variation_155
class Item_1956(Item):
    name = "Y1"
    title = ""
    var = Variation_155
class Variation_1169(Group):
    items = [Item_1912, Item_1956]
class Item_1914(Item):
    name = "X1"
    title = "X1-Component"
    var = Variation_155
class Item_1957(Item):
    name = "Y1"
    title = "Y1-Component"
    var = Variation_155
class Item_1918(Item):
    name = "X2"
    title = "X2-Component"
    var = Variation_155
class Item_1959(Item):
    name = "Y2"
    title = "Y2-Component"
    var = Variation_155
class Variation_1170(Group):
    items = [Item_1914, Item_1957, Item_1918, Item_1959]
class Item_1921(Item):
    name = "XA"
    title = ""
    var = Variation_35
class Item_1922(Item):
    name = "XC"
    title = ""
    var = Variation_445
class Item_17(Spare):
    bit_offset8 = 3
    bit_size = 2
class Item_1915(Item):
    name = "X2"
    title = ""
    var = Variation_726
class Variation_1171(Group):
    items = [Item_1921, Item_7, Item_1922, Item_17, Item_1915, Item_27]
class Item_1955(Item):
    name = "Y1"
    title = ""
    var = Variation_132
class Item_1958(Item):
    name = "Y2"
    title = ""
    var = Variation_690
class Item_1960(Item):
    name = "Y3"
    title = ""
    var = Variation_132
class Item_1961(Item):
    name = "Y4"
    title = ""
    var = Variation_690
class Item_1072(Item):
    name = "M1"
    title = ""
    var = Variation_132
class Item_1077(Item):
    name = "M2"
    title = ""
    var = Variation_690
class Item_752(Item):
    name = "D1"
    title = ""
    var = Variation_132
class Item_753(Item):
    name = "D2"
    title = ""
    var = Variation_690
class Variation_1172(Group):
    items = [Item_1955, Item_1958, Item_1960, Item_1961, Item_1072, Item_1077, Item_752, Item_753]
class Item_1301(Item):
    name = "POA"
    title = "Position Offset Applied"
    var = Variation_489
class Item_620(Item):
    name = "CDTIS"
    title = "Cockpit Display of Traffic Information Surface"
    var = Variation_532
class Item_576(Item):
    name = "B2LOW"
    title = "Class B2 Transmit Power Less Than 70 Watts"
    var = Variation_610
class Item_1406(Item):
    name = "RAS"
    title = "Receiving ATC Services"
    var = Variation_707
class Item_990(Item):
    name = "IDENT"
    title = "Setting of IDENT Switch"
    var = Variation_814
class Item_1071(Item):
    name = "LW"
    title = "Length and Width of the Aircraft"
    var = Variation_685
class Variation_1173(Extended):
    items = [Item_1, Item_1301, Item_620, Item_576, Item_1406, Item_990, None, Item_3, Item_1071]
class Item_1070(Item):
    name = "LW"
    title = "Length and Width of the Aircraft"
    var = Variation_127
class Item_21(Spare):
    bit_offset8 = 4
    bit_size = 3
class Variation_1174(Extended):
    items = [Item_1, Item_1301, Item_620, Item_576, Item_1406, Item_990, None, Item_1070, Item_21, None]
class Item_1631(Item):
    name = "STAT"
    title = "Status of the Service"
    var = Variation_684
class Variation_1175(Extended):
    items = [Item_3, Item_1631, None]
class Item_864(Item):
    name = "FOEFRI"
    title = "Indication Foe/Friend (Mode4)"
    var = Variation_763
class Variation_1176(Extended):
    items = [Item_4, Item_864, None]
class Item_550(Item):
    name = "ATP"
    title = "Address Type"
    var = Variation_117
class Item_536(Item):
    name = "ARC"
    title = "Altitude Reporting Capability"
    var = Variation_588
class Item_1408(Item):
    name = "RC"
    title = "Range Check"
    var = Variation_719
class Item_1398(Item):
    name = "RAB"
    title = "Report Type"
    var = Variation_828
class Item_1497(Item):
    name = "SAA"
    title = "Selected Altitude Available"
    var = Variation_631
class Item_642(Item):
    name = "CL"
    title = "Confidence Level"
    var = Variation_765
class Item_1041(Item):
    name = "LLC"
    title = "List Lookup Check"
    var = Variation_343
class Item_998(Item):
    name = "IPC"
    title = "Independent Position Check"
    var = Variation_446
class Item_1230(Item):
    name = "NOGO"
    title = "No-go Bit Status"
    var = Variation_561
class Item_720(Item):
    name = "CPR"
    title = "Compact Position Reporting"
    var = Variation_612
class Item_1037(Item):
    name = "LDPJ"
    title = "Local Decoding Position Jump"
    var = Variation_741
class Item_1411(Item):
    name = "RCF"
    title = "Range Check"
    var = Variation_799
class Variation_1177(Extended):
    items = [Item_550, Item_536, Item_1408, Item_1398, None, Item_773, Item_910, Item_1576, Item_1777, Item_1497, Item_642, None, Item_0, Item_1041, Item_998, Item_1230, Item_720, Item_1037, Item_1411, None]
class Item_1677(Item):
    name = "TBC"
    title = "Total Bits Corrected"
    var = Variation_968
class Item_1101(Item):
    name = "MBC"
    title = "Maximum Bits Corrected"
    var = Variation_968
class Variation_1178(Extended):
    items = [Item_550, Item_536, Item_1408, Item_1398, None, Item_773, Item_910, Item_1576, Item_1777, Item_1497, Item_642, None, Item_0, Item_1041, Item_998, Item_1230, Item_720, Item_1037, Item_1411, None, Item_1677, None, Item_1101, None]
class Variation_1179(Extended):
    items = [Item_550, Item_536, Item_1408, Item_1398, None, Item_773, Item_910, Item_1576, Item_1777, Item_1497, Item_642, None, Item_1, Item_998, Item_1230, Item_720, Item_1037, Item_1411, None]
class Item_591(Item):
    name = "BIZ"
    title = ""
    var = Variation_78
class Item_577(Item):
    name = "BAZ"
    title = ""
    var = Variation_383
class Item_1791(Item):
    name = "TUR"
    title = ""
    var = Variation_498
class Item_743(Item):
    name = "CSTP"
    title = "Coasted - Position"
    var = Variation_651
class Item_742(Item):
    name = "CSTH"
    title = "Coasted – Height"
    var = Variation_749
class Item_651(Item):
    name = "CNF"
    title = "Confirmed vs. Tentative Track"
    var = Variation_787
class Variation_1180(Extended):
    items = [Item_591, Item_577, Item_1791, Item_16, Item_743, Item_742, Item_651, None]
class Item_647(Item):
    name = "CNF"
    title = ""
    var = Variation_17
class Item_1754(Item):
    name = "TRE"
    title = ""
    var = Variation_342
class Item_739(Item):
    name = "CST"
    title = ""
    var = Variation_510
class Item_1091(Item):
    name = "MAH"
    title = ""
    var = Variation_615
class Item_1684(Item):
    name = "TCC"
    title = ""
    var = Variation_756
class Item_1636(Item):
    name = "STH"
    title = ""
    var = Variation_817
class Item_1728(Item):
    name = "TOM"
    title = ""
    var = Variation_112
class Item_790(Item):
    name = "DOU"
    title = ""
    var = Variation_516
class Item_1183(Item):
    name = "MRS"
    title = ""
    var = Variation_761
class Item_913(Item):
    name = "GHO"
    title = ""
    var = Variation_27
class Item_9(Spare):
    bit_offset8 = 1
    bit_size = 6
class Variation_1181(Extended):
    items = [Item_647, Item_1754, Item_739, Item_1091, Item_1684, Item_1636, None, Item_1728, Item_790, Item_1183, None, Item_913, Item_9, None]
class Item_648(Item):
    name = "CNF"
    title = ""
    var = Variation_18
class Item_737(Item):
    name = "CST"
    title = ""
    var = Variation_482
class Item_616(Item):
    name = "CDM"
    title = ""
    var = Variation_589
class Item_1092(Item):
    name = "MAH"
    title = ""
    var = Variation_715
class Variation_1182(Extended):
    items = [Item_648, Item_1754, Item_737, Item_616, Item_1092, Item_1636, None, Item_913, Item_9, None]
class Item_738(Item):
    name = "CST"
    title = ""
    var = Variation_485
class Variation_1183(Extended):
    items = [Item_648, Item_1754, Item_738, Item_616, Item_1092, Item_1636, None, Item_913, Item_9, None]
class Item_650(Item):
    name = "CNF"
    title = "Confirmed Vs. Tentative Track"
    var = Variation_15
class Item_1401(Item):
    name = "RAD"
    title = "Type of Sensor(s) Maintaining Track"
    var = Variation_394
class Item_792(Item):
    name = "DOU"
    title = "Signals Level of Confidence in Plot to Track Association Process"
    var = Variation_567
class Item_1093(Item):
    name = "MAH"
    title = "Manoeuvre Detection in Horizontal Sense"
    var = Variation_647
class Item_617(Item):
    name = "CDM"
    title = "Climbing / Descending Mode"
    var = Variation_760
class Item_1755(Item):
    name = "TRE"
    title = "Signal for End_of_Track"
    var = Variation_86
class Item_915(Item):
    name = "GHO"
    title = "Ghost Vs. True Target"
    var = Variation_391
class Item_1658(Item):
    name = "SUP"
    title = "Track Maintained with Track Information from Neighbouring Node B on the Cluster, or Network"
    var = Variation_474
class Item_1685(Item):
    name = "TCC"
    title = "Type of Plot Coordinate Transformation Mechanism:"
    var = Variation_578
class Variation_1184(Extended):
    items = [Item_650, Item_1401, Item_792, Item_1093, Item_617, None, Item_1755, Item_915, Item_1658, Item_1685, Item_21, None]
class Item_691(Item):
    name = "CON"
    title = ""
    var = Variation_16
class Item_1400(Item):
    name = "RAD"
    title = ""
    var = Variation_378
class Item_1096(Item):
    name = "MAN"
    title = ""
    var = Variation_435
class Item_791(Item):
    name = "DOU"
    title = ""
    var = Variation_533
class Item_1414(Item):
    name = "RDPC"
    title = "Radar Data Processing Chain"
    var = Variation_654
class Item_23(Spare):
    bit_offset8 = 5
    bit_size = 1
class Item_914(Item):
    name = "GHO"
    title = ""
    var = Variation_793
class Item_1753(Item):
    name = "TRE"
    title = ""
    var = Variation_30
class Variation_1185(Extended):
    items = [Item_691, Item_1400, Item_1096, Item_791, Item_1414, Item_23, Item_914, None, Item_1753, Item_9, None]
class Item_692(Item):
    name = "CON"
    title = ""
    var = Variation_102
class Item_1329(Item):
    name = "PSR"
    title = ""
    var = Variation_488
class Item_1606(Item):
    name = "SSR"
    title = ""
    var = Variation_572
class Item_1124(Item):
    name = "MDS"
    title = ""
    var = Variation_638
class Item_491(Item):
    name = "ADS"
    title = ""
    var = Variation_703
class Item_1159(Item):
    name = "MLT"
    title = ""
    var = Variation_816
class Item_1263(Item):
    name = "OPS"
    title = "Operational Release Status of the System"
    var = Variation_72
class Item_1257(Item):
    name = "ODP"
    title = "Data Processor Overload Indicator"
    var = Variation_356
class Item_1279(Item):
    name = "OXT"
    title = "Transmission Subsystem Overload Status"
    var = Variation_453
class Item_1190(Item):
    name = "MSC"
    title = "Monitoring System Connected Status"
    var = Variation_559
class Item_1780(Item):
    name = "TSV"
    title = "Time Source Validity"
    var = Variation_666
class Item_1244(Item):
    name = "NPW"
    title = "No Plot Warning"
    var = Variation_727
class Variation_1186(Extended):
    items = [Item_692, Item_1329, Item_1606, Item_1124, Item_491, Item_1159, None, Item_1263, Item_1257, Item_1279, Item_1190, Item_1780, Item_1244, Item_26, None]
class Item_845(Item):
    name = "ES"
    title = ""
    var = Variation_77
class Item_1814(Item):
    name = "UAT"
    title = ""
    var = Variation_382
class Item_14(Spare):
    bit_offset8 = 2
    bit_size = 5
class Variation_1187(Extended):
    items = [Item_845, Item_1814, Item_14, None]
class Item_847(Item):
    name = "ES"
    title = "ES IN Capability"
    var = Variation_77
class Item_1819(Item):
    name = "UAT"
    title = "UAT IN Capability"
    var = Variation_382
class Item_1410(Item):
    name = "RCE"
    title = "Reduced Capability Equipment"
    var = Variation_969
class Item_1464(Item):
    name = "RRL"
    title = "Reply Rate Limiting"
    var = Variation_975
class Item_1327(Item):
    name = "PS3"
    title = "Priority Status for Version 3 ADS-B Systems"
    var = Variation_967
class Item_1742(Item):
    name = "TPW"
    title = "Transmit Power"
    var = Variation_973
class Item_1770(Item):
    name = "TSI"
    title = "Transponder Side Indication"
    var = Variation_965
class Item_1203(Item):
    name = "MUO"
    title = "Manned / Unmanned Operation"
    var = Variation_970
class Item_1490(Item):
    name = "RWC"
    title = "Remain Well Clear Corrective Alert"
    var = Variation_974
class Item_757(Item):
    name = "DAA"
    title = "Detectand Avoid Capabilities"
    var = Variation_964
class Item_779(Item):
    name = "DF17CA"
    title = "Transponder Capability"
    var = Variation_971
class Item_1659(Item):
    name = "SVH"
    title = "Sense Vertical & Horizontal"
    var = Variation_966
class Item_611(Item):
    name = "CATC"
    title = "CAS Type & Capability"
    var = Variation_976
class Item_1668(Item):
    name = "TAO"
    title = "Transponder Antenna Offset"
    var = Variation_963
class Variation_1188(Extended):
    items = [Item_847, Item_1819, Item_1410, Item_1464, None, Item_1327, Item_1742, None, Item_1770, Item_1203, Item_1490, None, Item_757, Item_779, None, Item_1659, Item_611, None, Item_1668, None]
class Item_852(Item):
    name = "F"
    title = "Scaling Factor"
    var = Variation_139
class Item_1387(Item):
    name = "R"
    title = "Current Reduction Stage in Use"
    var = Variation_771
class Item_1343(Item):
    name = "Q"
    title = "Processing Parameters"
    var = Variation_232
class Variation_1189(Extended):
    items = [Item_852, Item_1387, Item_1343, None]
class Item_855(Item):
    name = "FDR"
    title = "Flight Data Retained"
    var = Variation_41
class Variation_1190(Extended):
    items = [Item_855, Item_9, None]
class Item_905(Item):
    name = "GATOAT"
    title = "Identification of Conflict Categories Definition Table"
    var = Variation_111
class Item_618(Item):
    name = "CDM"
    title = "Climbing/Descending Mode"
    var = Variation_99
class Item_1322(Item):
    name = "PRI"
    title = ""
    var = Variation_481
class Item_924(Item):
    name = "GV"
    title = ""
    var = Variation_534
class Variation_1191(Extended):
    items = [Item_905, Item_878, Item_1486, Item_953, None, Item_618, Item_1322, Item_924, Item_21, None]
class Item_1039(Item):
    name = "LENGTH"
    title = "Length"
    var = Variation_151
class Item_1265(Item):
    name = "ORIENTATION"
    title = "Orientation"
    var = Variation_154
class Item_1881(Item):
    name = "WIDTH"
    title = "Width"
    var = Variation_151
class Variation_1192(Extended):
    items = [Item_1039, None, Item_1265, None, Item_1881, None]
class Item_1097(Item):
    name = "MAS"
    title = "Conflict Location in Military Airspace"
    var = Variation_19
class Item_608(Item):
    name = "CAS"
    title = "Conflict Location in Civil Airspace"
    var = Variation_335
class Item_859(Item):
    name = "FLD"
    title = "Fast Lateral Divergence"
    var = Variation_430
class Item_891(Item):
    name = "FVD"
    title = "Fast Vertical Divergence"
    var = Variation_529
class Item_1813(Item):
    name = "TYPE"
    title = "Type of Separation Infringement"
    var = Variation_640
class Item_726(Item):
    name = "CROSS"
    title = "Crossing Test"
    var = Variation_706
class Item_783(Item):
    name = "DIV"
    title = "Divergence Test"
    var = Variation_783
class Item_1463(Item):
    name = "RRC"
    title = "Runway/Runway Crossing in RIMCAS"
    var = Variation_33
class Item_1483(Item):
    name = "RTC"
    title = "Runway/Taxiway Crossing in RIMCAS"
    var = Variation_349
class Item_1185(Item):
    name = "MRVA"
    title = ""
    var = Variation_440
class Item_1860(Item):
    name = "VRAMCRM"
    title = ""
    var = Variation_541
class Item_1862(Item):
    name = "VRAMVRM"
    title = ""
    var = Variation_619
class Item_1865(Item):
    name = "VRAMVTM"
    title = ""
    var = Variation_716
class Item_933(Item):
    name = "HAMHD"
    title = ""
    var = Variation_795
class Item_935(Item):
    name = "HAMRD"
    title = ""
    var = Variation_32
class Item_937(Item):
    name = "HAMVD"
    title = ""
    var = Variation_344
class Item_763(Item):
    name = "DBPSMARR"
    title = ""
    var = Variation_439
class Item_765(Item):
    name = "DBPSMDEP"
    title = ""
    var = Variation_540
class Item_767(Item):
    name = "DBPSMTL"
    title = ""
    var = Variation_618
class Item_506(Item):
    name = "AIW"
    title = ""
    var = Variation_717
class Variation_1193(Extended):
    items = [Item_1097, Item_608, Item_859, Item_891, Item_1813, Item_726, Item_783, None, Item_1463, Item_1483, Item_1185, Item_1860, Item_1862, Item_1865, Item_933, None, Item_935, Item_937, Item_763, Item_765, Item_767, Item_506, Item_26, None]
class Item_1153(Item):
    name = "MIDENT"
    title = "Master System Unit Identification"
    var = Variation_155
class Item_1201(Item):
    name = "MTRACK"
    title = "Master System Track Number"
    var = Variation_232
class Item_1563(Item):
    name = "SIDENT"
    title = "Slave System Unit Identification"
    var = Variation_155
class Item_1645(Item):
    name = "STRACK"
    title = "Slave System Track Number"
    var = Variation_232
class Variation_1194(Extended):
    items = [Item_1153, Item_1201, None, Item_1563, Item_1645, None]
class Item_1177(Item):
    name = "MOMU"
    title = "Mono-Static Target Report or Multi-Static Target Report"
    var = Variation_100
class Item_1785(Item):
    name = "TTAX"
    title = "Target Taxonomy"
    var = Variation_503
class Item_1515(Item):
    name = "SCD"
    title = "Scanning Direction"
    var = Variation_677
class Variation_1195(Extended):
    items = [Item_1177, Item_1785, Item_1515, Item_26, None]
class Item_1178(Item):
    name = "MON"
    title = ""
    var = Variation_51
class Item_906(Item):
    name = "GBS"
    title = ""
    var = Variation_390
class Item_1181(Item):
    name = "MRH"
    title = ""
    var = Variation_432
class Item_1595(Item):
    name = "SRC"
    title = ""
    var = Variation_601
class Item_649(Item):
    name = "CNF"
    title = ""
    var = Variation_788
class Item_1570(Item):
    name = "SIM"
    title = ""
    var = Variation_3
class Item_1769(Item):
    name = "TSE"
    title = ""
    var = Variation_355
class Item_1767(Item):
    name = "TSB"
    title = ""
    var = Variation_452
class Item_879(Item):
    name = "FRIFOE"
    title = ""
    var = Variation_590
class Item_1135(Item):
    name = "ME"
    title = ""
    var = Variation_729
class Item_1152(Item):
    name = "MI"
    title = ""
    var = Variation_807
class Item_516(Item):
    name = "AMA"
    title = ""
    var = Variation_85
class Item_1586(Item):
    name = "SPI"
    title = ""
    var = Variation_354
class Item_736(Item):
    name = "CST"
    title = ""
    var = Variation_449
class Item_871(Item):
    name = "FPC"
    title = ""
    var = Variation_569
class Item_496(Item):
    name = "AFF"
    title = ""
    var = Variation_625
class Item_24(Spare):
    bit_offset8 = 5
    bit_size = 2
class Variation_1196(Extended):
    items = [Item_1178, Item_906, Item_1181, Item_1595, Item_649, None, Item_1570, Item_1769, Item_1767, Item_879, Item_1135, Item_1152, None, Item_516, Item_1586, Item_736, Item_871, Item_496, Item_24, None]
class Item_1328(Item):
    name = "PSR"
    title = ""
    var = Variation_351
class Item_1605(Item):
    name = "SSR"
    title = ""
    var = Variation_448
class Item_1123(Item):
    name = "MDS"
    title = ""
    var = Variation_547
class Item_489(Item):
    name = "ADS"
    title = ""
    var = Variation_626
class Item_1653(Item):
    name = "SUC"
    title = ""
    var = Variation_731
class Item_468(Item):
    name = "AAC"
    title = ""
    var = Variation_805
class Variation_1197(Extended):
    items = [Item_1178, Item_906, Item_1181, Item_1595, Item_649, None, Item_1570, Item_1769, Item_1767, Item_879, Item_1135, Item_1152, None, Item_516, Item_1586, Item_736, Item_871, Item_496, Item_24, None, Item_0, Item_1328, Item_1605, Item_1123, Item_489, Item_1653, Item_468, None]
class Item_1179(Item):
    name = "MON"
    title = ""
    var = Variation_52
class Item_1182(Item):
    name = "MRH"
    title = "Most Reliable Height"
    var = Variation_432
class Item_1598(Item):
    name = "SRC"
    title = "Source of Calculated Track Altitude for I062/130"
    var = Variation_600
class Item_1572(Item):
    name = "SIM"
    title = ""
    var = Variation_5
class Item_1768(Item):
    name = "TSE"
    title = ""
    var = Variation_353
class Item_1766(Item):
    name = "TSB"
    title = ""
    var = Variation_451
class Item_1643(Item):
    name = "STP"
    title = ""
    var = Variation_730
class Item_1003(Item):
    name = "KOS"
    title = ""
    var = Variation_786
class Item_1113(Item):
    name = "MD4"
    title = ""
    var = Variation_397
class Item_1133(Item):
    name = "ME"
    title = ""
    var = Variation_548
class Item_1150(Item):
    name = "MI"
    title = ""
    var = Variation_628
class Item_1115(Item):
    name = "MD5"
    title = ""
    var = Variation_764
class Item_735(Item):
    name = "CST"
    title = ""
    var = Variation_36
class Item_490(Item):
    name = "ADS"
    title = ""
    var = Variation_627
class Item_467(Item):
    name = "AAC"
    title = ""
    var = Variation_804
class Item_1542(Item):
    name = "SDS"
    title = ""
    var = Variation_95
class Item_826(Item):
    name = "EMS"
    title = ""
    var = Variation_517
class Item_1289(Item):
    name = "PFT"
    title = ""
    var = Variation_746
class Item_872(Item):
    name = "FPLT"
    title = ""
    var = Variation_806
class Item_807(Item):
    name = "DUPT"
    title = ""
    var = Variation_37
class Item_805(Item):
    name = "DUPF"
    title = ""
    var = Variation_352
class Item_806(Item):
    name = "DUPM"
    title = ""
    var = Variation_450
class Item_18(Spare):
    bit_offset8 = 3
    bit_size = 4
class Variation_1198(Extended):
    items = [Item_1179, Item_1586, Item_1182, Item_1598, Item_649, None, Item_1572, Item_1768, Item_1766, Item_871, Item_496, Item_1643, Item_1003, None, Item_516, Item_1113, Item_1133, Item_1150, Item_1115, None, Item_735, Item_1328, Item_1605, Item_1123, Item_490, Item_1653, Item_467, None, Item_1542, Item_826, Item_1289, Item_872, None, Item_807, Item_805, Item_806, Item_18, None]
class Item_1550(Item):
    name = "SFC"
    title = ""
    var = Variation_549
class Item_988(Item):
    name = "IDD"
    title = ""
    var = Variation_648
class Item_991(Item):
    name = "IEC"
    title = ""
    var = Variation_728
class Variation_1199(Extended):
    items = [Item_1179, Item_1586, Item_1182, Item_1598, Item_649, None, Item_1572, Item_1768, Item_1766, Item_871, Item_496, Item_1643, Item_1003, None, Item_516, Item_1113, Item_1133, Item_1150, Item_1115, None, Item_735, Item_1328, Item_1605, Item_1123, Item_490, Item_1653, Item_467, None, Item_1542, Item_826, Item_1289, Item_872, None, Item_807, Item_805, Item_806, Item_1550, Item_988, Item_991, Item_26, None]
class Item_1158(Item):
    name = "MLAT"
    title = ""
    var = Variation_803
class Variation_1200(Extended):
    items = [Item_1179, Item_1586, Item_1182, Item_1598, Item_649, None, Item_1572, Item_1768, Item_1766, Item_871, Item_496, Item_1643, Item_1003, None, Item_516, Item_1113, Item_1133, Item_1150, Item_1115, None, Item_735, Item_1328, Item_1605, Item_1123, Item_490, Item_1653, Item_467, None, Item_1542, Item_826, Item_1289, Item_872, None, Item_807, Item_805, Item_806, Item_1550, Item_988, Item_991, Item_1158, None]
class Item_1184(Item):
    name = "MRVA"
    title = ""
    var = Variation_31
class Item_1403(Item):
    name = "RAMLD"
    title = ""
    var = Variation_347
class Item_1402(Item):
    name = "RAMHD"
    title = ""
    var = Variation_444
class Item_1189(Item):
    name = "MSAW"
    title = ""
    var = Variation_538
class Item_528(Item):
    name = "APW"
    title = ""
    var = Variation_614
class Item_643(Item):
    name = "CLAM"
    title = ""
    var = Variation_714
class Item_1634(Item):
    name = "STCA"
    title = ""
    var = Variation_800
class Item_527(Item):
    name = "APM"
    title = ""
    var = Variation_23
class Item_1449(Item):
    name = "RIMCA"
    title = ""
    var = Variation_348
class Item_477(Item):
    name = "ACASRA"
    title = ""
    var = Variation_434
class Item_1245(Item):
    name = "NTCA"
    title = ""
    var = Variation_543
class Item_780(Item):
    name = "DG"
    title = ""
    var = Variation_620
class Item_1258(Item):
    name = "OF"
    title = ""
    var = Variation_718
class Item_1259(Item):
    name = "OL"
    title = ""
    var = Variation_797
class Item_505(Item):
    name = "AIW"
    title = ""
    var = Variation_22
class Item_1283(Item):
    name = "PAIW"
    title = ""
    var = Variation_346
class Item_1252(Item):
    name = "OCAT"
    title = ""
    var = Variation_442
class Item_1508(Item):
    name = "SAM"
    title = ""
    var = Variation_544
class Item_1849(Item):
    name = "VCD"
    title = ""
    var = Variation_621
class Item_633(Item):
    name = "CHAM"
    title = ""
    var = Variation_713
class Item_799(Item):
    name = "DSAM"
    title = ""
    var = Variation_791
class Item_762(Item):
    name = "DBPSMARR"
    title = ""
    var = Variation_26
class Item_764(Item):
    name = "DBPSMDEP"
    title = ""
    var = Variation_337
class Item_766(Item):
    name = "DBPSMTL"
    title = ""
    var = Variation_436
class Item_1861(Item):
    name = "VRAMCRM"
    title = ""
    var = Variation_546
class Item_1864(Item):
    name = "VRAMVTM"
    title = ""
    var = Variation_623
class Item_1863(Item):
    name = "VRAMVRM"
    title = ""
    var = Variation_725
class Item_932(Item):
    name = "HAMHD"
    title = ""
    var = Variation_794
class Item_934(Item):
    name = "HAMRD"
    title = ""
    var = Variation_28
class Item_936(Item):
    name = "HAMVD"
    title = ""
    var = Variation_340
class Item_961(Item):
    name = "HVI"
    title = ""
    var = Variation_438
class Item_1069(Item):
    name = "LTW"
    title = ""
    var = Variation_537
class Item_1859(Item):
    name = "VPM"
    title = ""
    var = Variation_622
class Item_1784(Item):
    name = "TTA"
    title = ""
    var = Variation_723
class Item_725(Item):
    name = "CRA"
    title = ""
    var = Variation_789
class Item_544(Item):
    name = "ASM"
    title = ""
    var = Variation_24
class Item_981(Item):
    name = "IAVM"
    title = ""
    var = Variation_341
class Item_888(Item):
    name = "FTD"
    title = ""
    var = Variation_437
class Item_1002(Item):
    name = "ITD"
    title = ""
    var = Variation_535
class Item_995(Item):
    name = "IIA"
    title = ""
    var = Variation_616
class Item_1591(Item):
    name = "SQW"
    title = ""
    var = Variation_720
class Item_748(Item):
    name = "CUW"
    title = ""
    var = Variation_790
class Item_610(Item):
    name = "CATC"
    title = ""
    var = Variation_25
class Item_1227(Item):
    name = "NOCLR"
    title = ""
    var = Variation_345
class Item_1240(Item):
    name = "NOMOV"
    title = ""
    var = Variation_441
class Item_1235(Item):
    name = "NOH"
    title = ""
    var = Variation_542
class Item_1883(Item):
    name = "WRTY"
    title = ""
    var = Variation_624
class Item_1641(Item):
    name = "STOCC"
    title = ""
    var = Variation_721
class Item_1261(Item):
    name = "ONGOING"
    title = ""
    var = Variation_796
class Variation_1201(Extended):
    items = [Item_1184, Item_1403, Item_1402, Item_1189, Item_528, Item_643, Item_1634, None, Item_527, Item_1449, Item_477, Item_1245, Item_780, Item_1258, Item_1259, None, Item_505, Item_1283, Item_1252, Item_1508, Item_1849, Item_633, Item_799, None, Item_762, Item_764, Item_766, Item_1861, Item_1864, Item_1863, Item_932, None, Item_934, Item_936, Item_961, Item_1069, Item_1859, Item_1784, Item_725, None, Item_544, Item_981, Item_888, Item_1002, Item_995, Item_1591, Item_748, None, Item_610, Item_1227, Item_1240, Item_1235, Item_1883, Item_1641, Item_1261, None]
class Item_1210(Item):
    name = "NAV"
    title = ""
    var = Variation_87
class Item_1250(Item):
    name = "NVB"
    title = ""
    var = Variation_388
class Variation_1202(Extended):
    items = [Item_1210, Item_1250, Item_14, None]
class Item_1213(Item):
    name = "NAV"
    title = "TID Available"
    var = Variation_88
class Item_1251(Item):
    name = "NVB"
    title = "TID Valid"
    var = Variation_389
class Variation_1203(Extended):
    items = [Item_1213, Item_1251, Item_14, None]
class Item_1228(Item):
    name = "NOGO"
    title = ""
    var = Variation_21
class Item_1262(Item):
    name = "OPS"
    title = ""
    var = Variation_403
class Item_1615(Item):
    name = "SSTAT"
    title = ""
    var = Variation_605
class Item_1661(Item):
    name = "SYSTAT"
    title = ""
    var = Variation_411
class Item_1549(Item):
    name = "SESTAT"
    title = ""
    var = Variation_683
class Variation_1204(Extended):
    items = [Item_1228, Item_1262, Item_1615, None, Item_0, Item_1661, Item_1549, None]
class Item_1231(Item):
    name = "NOGO"
    title = "Operational Release Status of the Data"
    var = Variation_21
class Item_1278(Item):
    name = "OXT"
    title = "Ground Interface Data Communications Overload"
    var = Variation_453
class Item_1191(Item):
    name = "MSC"
    title = "Monitoring System Connected Status"
    var = Variation_560
class Item_1590(Item):
    name = "SPO"
    title = "Indication of Spoofing Attack"
    var = Variation_747
class Item_1450(Item):
    name = "RN"
    title = "Renumbering Indication for Track ID"
    var = Variation_802
class Item_923(Item):
    name = "GSSP"
    title = "Ground Station Status Reporting Period"
    var = Variation_152
class Variation_1205(Extended):
    items = [Item_1231, Item_1257, Item_1278, Item_1191, Item_1780, Item_1590, Item_1450, None, Item_923, None]
class Item_1249(Item):
    name = "NUCRNACV"
    title = "Navigation Uncertainty Category for Velocity NUCr or the Navigation Accuracy Category for Velocity NACv"
    var = Variation_116
class Item_1248(Item):
    name = "NUCPNIC"
    title = "Navigation Uncertainty Category for Position NUCp or Navigation Integrity Category NIC"
    var = Variation_602
class Item_1224(Item):
    name = "NICBARO"
    title = "Navigation Integrity Category for Barometric Altitude"
    var = Variation_0
class Item_1568(Item):
    name = "SIL"
    title = "Surveillance (version 1) or Source (version 2) Integrity Level"
    var = Variation_393
class Item_1207(Item):
    name = "NACP"
    title = "Navigation Accuracy Category for Position"
    var = Variation_602
class Item_1569(Item):
    name = "SILS"
    title = "SIL-Supplement"
    var = Variation_460
class Item_1520(Item):
    name = "SDA"
    title = "Horizontal Position System Design Assurance Level (as Defined in Version 2)"
    var = Variation_587
class Item_926(Item):
    name = "GVA"
    title = "Geometric Altitude Accuracy"
    var = Variation_759
class Item_1290(Item):
    name = "PIC"
    title = "Position Integrity Category"
    var = Variation_127
class Variation_1206(Extended):
    items = [Item_1249, Item_1248, None, Item_1224, Item_1568, Item_1207, None, Item_1, Item_1569, Item_1520, Item_926, None, Item_1290, Item_21, None]
class Item_963(Item):
    name = "I"
    title = "Intensity Level"
    var = Variation_412
class Item_1494(Item):
    name = "S"
    title = "Shading Orientation with Respect to North"
    var = Variation_680
class Variation_1207(Extended):
    items = [Item_1264, Item_963, Item_1494, None]
class Item_1774(Item):
    name = "TST"
    title = ""
    var = Variation_724
class Item_840(Item):
    name = "ER"
    title = ""
    var = Variation_792
class Variation_1208(Extended):
    items = [Item_1264, Item_963, Item_1494, None, Item_4, Item_1774, Item_840, None]
class Item_1430(Item):
    name = "REFTR1"
    title = "Ref Trans 1 Status"
    var = Variation_113
class Item_1431(Item):
    name = "REFTR2"
    title = "Ref Trans 2 Status"
    var = Variation_678
class Item_1432(Item):
    name = "REFTR3"
    title = "Ref Trans 3 Status"
    var = Variation_113
class Item_1433(Item):
    name = "REFTR4"
    title = "Ref Trans 4 Status"
    var = Variation_678
class Variation_1209(Extended):
    items = [Item_1430, Item_12, Item_1431, Item_26, None, Item_1432, Item_12, Item_1433, Item_26, None]
class Item_1455(Item):
    name = "RP"
    title = "Report Period for Category 021 Reports"
    var = Variation_209
class Item_1513(Item):
    name = "SC"
    title = "Service Class"
    var = Variation_123
class Item_1613(Item):
    name = "SSRP"
    title = "Service Status Reporting Period"
    var = Variation_152
class Variation_1210(Extended):
    items = [Item_1455, Item_1513, Item_18, None, Item_1613, None]
class Item_1577(Item):
    name = "SN"
    title = "Step Number"
    var = Variation_141
class Variation_1211(Extended):
    items = [Item_1577, Item_26, None]
class Item_1592(Item):
    name = "SR"
    title = ""
    var = Variation_83
class Item_531(Item):
    name = "AR"
    title = ""
    var = Variation_386
class Item_839(Item):
    name = "ER"
    title = ""
    var = Variation_496
class Item_874(Item):
    name = "FR"
    title = ""
    var = Variation_575
class Item_1180(Item):
    name = "MR"
    title = ""
    var = Variation_662
class Item_1312(Item):
    name = "PR"
    title = ""
    var = Variation_755
class Item_724(Item):
    name = "CR"
    title = ""
    var = Variation_835
class Item_984(Item):
    name = "ID"
    title = ""
    var = Variation_82
class Item_1088(Item):
    name = "MA"
    title = ""
    var = Variation_387
class Item_1579(Item):
    name = "SP"
    title = ""
    var = Variation_497
class Item_944(Item):
    name = "HG"
    title = ""
    var = Variation_576
class Item_938(Item):
    name = "HD"
    title = ""
    var = Variation_663
class Variation_1212(Extended):
    items = [Item_1592, Item_531, Item_839, Item_874, Item_1180, Item_1312, Item_724, None, Item_984, Item_1088, Item_1579, Item_944, Item_938, Item_24, None]
class Item_1604(Item):
    name = "SSR"
    title = ""
    var = Variation_63
class Item_1186(Item):
    name = "MS"
    title = ""
    var = Variation_368
class Item_943(Item):
    name = "HF"
    title = ""
    var = Variation_455
class Item_1853(Item):
    name = "VDL4"
    title = ""
    var = Variation_583
class Item_1815(Item):
    name = "UAT"
    title = ""
    var = Variation_665
class Item_787(Item):
    name = "DME"
    title = ""
    var = Variation_711
class Item_1268(Item):
    name = "OT"
    title = ""
    var = Variation_824
class Item_1392(Item):
    name = "RAB"
    title = ""
    var = Variation_69
class Item_1585(Item):
    name = "SPI"
    title = ""
    var = Variation_333
class Item_634(Item):
    name = "CHN"
    title = ""
    var = Variation_433
class Item_907(Item):
    name = "GBS"
    title = ""
    var = Variation_579
class Item_727(Item):
    name = "CRT"
    title = ""
    var = Variation_645
class Item_1575(Item):
    name = "SIM"
    title = ""
    var = Variation_705
class Item_1776(Item):
    name = "TST"
    title = ""
    var = Variation_801
class Variation_1213(Extended):
    items = [Item_1604, Item_1186, Item_943, Item_1853, Item_1815, Item_787, Item_1268, None, Item_1392, Item_1585, Item_634, Item_907, Item_727, Item_1575, Item_1776, None]
class Item_1642(Item):
    name = "STP"
    title = ""
    var = Variation_8
class Item_958(Item):
    name = "HTS"
    title = ""
    var = Variation_361
class Item_959(Item):
    name = "HTT"
    title = ""
    var = Variation_456
class Item_957(Item):
    name = "HRD"
    title = ""
    var = Variation_581
class Item_922(Item):
    name = "GSS"
    title = "Ground Speed"
    var = Variation_692
class Item_945(Item):
    name = "HGT"
    title = "Heading/Ground Track Information"
    var = Variation_153
class Variation_1214(Extended):
    items = [Item_1642, Item_958, Item_959, Item_957, Item_922, None, Item_945, None]
class Item_1654(Item):
    name = "SUI"
    title = "System Unit Identification"
    var = Variation_191
class Item_1640(Item):
    name = "STN"
    title = "System Track Number"
    var = Variation_233
class Variation_1215(Extended):
    items = [Item_1654, Item_1640, None]
class Item_1453(Item):
    name = "ROT"
    title = "Rate of Turn"
    var = Variation_150
class Variation_1216(Extended):
    items = [Item_1703, Item_14, None, Item_1453, None]
class Item_1795(Item):
    name = "TYP"
    title = ""
    var = Variation_67
class Item_1573(Item):
    name = "SIM"
    title = ""
    var = Variation_334
class Item_1614(Item):
    name = "SSRPSR"
    title = "Radar Detection in Last Antenna Scan"
    var = Variation_509
class Item_520(Item):
    name = "ANT"
    title = ""
    var = Variation_660
class Item_1588(Item):
    name = "SPI"
    title = ""
    var = Variation_722
class Item_1395(Item):
    name = "RAB"
    title = ""
    var = Variation_798
class Item_1771(Item):
    name = "TST"
    title = ""
    var = Variation_34
class Item_798(Item):
    name = "DS1DS2"
    title = "Radar Detection in Last Antenna Scan"
    var = Variation_395
class Item_1132(Item):
    name = "ME"
    title = ""
    var = Variation_539
class Item_1149(Item):
    name = "MI"
    title = ""
    var = Variation_617
class Variation_1217(Extended):
    items = [Item_1795, Item_1573, Item_1614, Item_520, Item_1588, Item_1395, None, Item_1771, Item_798, Item_1132, Item_1149, Item_24, None]
class Item_1797(Item):
    name = "TYP"
    title = ""
    var = Variation_122
class Item_1413(Item):
    name = "RDP"
    title = ""
    var = Variation_655
class Item_1587(Item):
    name = "SPI"
    title = ""
    var = Variation_704
class Item_1396(Item):
    name = "RAB"
    title = ""
    var = Variation_827
class Item_1772(Item):
    name = "TST"
    title = ""
    var = Variation_68
class Item_842(Item):
    name = "ERR"
    title = ""
    var = Variation_370
class Item_1929(Item):
    name = "XPP"
    title = ""
    var = Variation_475
class Item_1134(Item):
    name = "ME"
    title = ""
    var = Variation_564
class Item_1151(Item):
    name = "MI"
    title = ""
    var = Variation_649
class Item_863(Item):
    name = "FOEFRI"
    title = ""
    var = Variation_762
class Variation_1218(Extended):
    items = [Item_1797, Item_1574, Item_1413, Item_1587, Item_1396, None, Item_1772, Item_842, Item_1929, Item_1134, Item_1151, Item_863, None]
class Item_494(Item):
    name = "ADSB"
    title = "On-Site ADS-B Information"
    var = Variation_962
class Item_1517(Item):
    name = "SCN"
    title = "Surveillance Cluster Network Information"
    var = Variation_978
class Item_1282(Item):
    name = "PAI"
    title = "Passive Acquisition Interface Information"
    var = Variation_977
class Variation_1219(Extended):
    items = [Item_1797, Item_1574, Item_1413, Item_1587, Item_1396, None, Item_1772, Item_842, Item_1929, Item_1134, Item_1151, Item_863, None, Item_494, Item_1517, Item_1282, Item_26, None]
class Item_1798(Item):
    name = "TYP"
    title = ""
    var = Variation_126
class Item_772(Item):
    name = "DCR"
    title = ""
    var = Variation_563
class Item_635(Item):
    name = "CHN"
    title = ""
    var = Variation_613
class Item_908(Item):
    name = "GBS"
    title = ""
    var = Variation_757
class Item_728(Item):
    name = "CRT"
    title = ""
    var = Variation_820
class Item_1571(Item):
    name = "SIM"
    title = ""
    var = Variation_4
class Item_1773(Item):
    name = "TST"
    title = ""
    var = Variation_350
class Item_1393(Item):
    name = "RAB"
    title = ""
    var = Variation_490
class Item_1062(Item):
    name = "LOP"
    title = ""
    var = Variation_593
class Item_1732(Item):
    name = "TOT"
    title = ""
    var = Variation_769
class Item_1584(Item):
    name = "SPI"
    title = ""
    var = Variation_2
class Variation_1220(Extended):
    items = [Item_1798, Item_772, Item_635, Item_908, Item_728, None, Item_1571, Item_1773, Item_1393, Item_1062, Item_1732, None, Item_1584, Item_9, None]
class Item_1824(Item):
    name = "UDS"
    title = "Uplink Default Status"
    var = Variation_80
class Item_775(Item):
    name = "DDS"
    title = "Downlink Default Status"
    var = Variation_385
class Item_1823(Item):
    name = "UCS"
    title = "Uplink Current Status"
    var = Variation_495
class Item_774(Item):
    name = "DCS"
    title = "Downlink Current Status"
    var = Variation_574
class Item_813(Item):
    name = "EI"
    title = "Exit Indication"
    var = Variation_834
class Item_982(Item):
    name = "IC"
    title = "Interrogator Control"
    var = Variation_81
class Variation_1221(Extended):
    items = [Item_1824, Item_775, Item_1823, Item_774, Item_20, Item_813, None, Item_982, Item_9, None]
class Variation_1222(Repetitive):
    rep = 1
    var = Variation_155
class Variation_1223(Repetitive):
    rep = 1
    var = Variation_162
class Variation_1224(Repetitive):
    rep = 1
    var = Variation_190
class Variation_1225(Repetitive):
    rep = 1
    var = Variation_234
class Variation_1226(Repetitive):
    rep = 1
    var = Variation_296
class Variation_1227(Repetitive):
    rep = 1
    var = Variation_313
class Variation_1228(Repetitive):
    rep = 1
    var = Variation_315
class Variation_1229(Repetitive):
    rep = 1
    var = Variation_323
class Variation_1230(Repetitive):
    rep = 1
    var = Variation_329
class Variation_1231(Repetitive):
    rep = 1
    var = Variation_330
class Variation_1232(Repetitive):
    rep = 1
    var = Variation_331
class Variation_1233(Repetitive):
    rep = 1
    var = Variation_898
class Variation_1234(Repetitive):
    rep = 1
    var = Variation_906
class Variation_1235(Repetitive):
    rep = 1
    var = Variation_920
class Variation_1236(Repetitive):
    rep = 1
    var = Variation_933
class Variation_1237(Repetitive):
    rep = 1
    var = Variation_934
class Variation_1238(Repetitive):
    rep = 1
    var = Variation_935
class Variation_1239(Repetitive):
    rep = 1
    var = Variation_938
class Variation_1240(Repetitive):
    rep = 1
    var = Variation_940
class Variation_1241(Repetitive):
    rep = 1
    var = Variation_943
class Variation_1242(Repetitive):
    rep = 1
    var = Variation_944
class Variation_1243(Repetitive):
    rep = 1
    var = Variation_958
class Variation_1244(Repetitive):
    rep = 1
    var = Variation_959
class Variation_1245(Repetitive):
    rep = 1
    var = Variation_1014
class Variation_1246(Repetitive):
    rep = 1
    var = Variation_1015
class Variation_1247(Repetitive):
    rep = 1
    var = Variation_1016
class Variation_1248(Repetitive):
    rep = 1
    var = Variation_1028
class Variation_1249(Repetitive):
    rep = 1
    var = Variation_1052
class Variation_1250(Repetitive):
    rep = 1
    var = Variation_1056
class Variation_1251(Repetitive):
    rep = 1
    var = Variation_1060
class Variation_1252(Repetitive):
    rep = 1
    var = Variation_1061
class Variation_1253(Repetitive):
    rep = 1
    var = Variation_1062
class Variation_1254(Repetitive):
    rep = 1
    var = Variation_1063
class Variation_1255(Repetitive):
    rep = 1
    var = Variation_1066
class Variation_1256(Repetitive):
    rep = 1
    var = Variation_1087
class Variation_1257(Repetitive):
    rep = 1
    var = Variation_1088
class Variation_1258(Repetitive):
    rep = 1
    var = Variation_1089
class Variation_1259(Repetitive):
    rep = 1
    var = Variation_1092
class Variation_1260(Repetitive):
    rep = 1
    var = Variation_1100
class Variation_1261(Repetitive):
    rep = 1
    var = Variation_1101
class Variation_1262(Repetitive):
    rep = 1
    var = Variation_1105
class Variation_1263(Repetitive):
    rep = 1
    var = Variation_1106
class Variation_1264(Repetitive):
    rep = 1
    var = Variation_1107
class Variation_1265(Repetitive):
    rep = 1
    var = Variation_1108
class Variation_1266(Repetitive):
    rep = 1
    var = Variation_1109
class Variation_1267(Repetitive):
    rep = 1
    var = Variation_1110
class Variation_1268(Repetitive):
    rep = 1
    var = Variation_1111
class Variation_1269(Repetitive):
    rep = 1
    var = Variation_1162
class Variation_1270(Repetitive):
    rep = 1
    var = Variation_1167
class Variation_1271(Repetitive):
    rep = 1
    var = Variation_1169
class Variation_1272(Repetitive):
    rep = 1
    var = Variation_1170
class Variation_1273(Repetitive):
    rep = None
    var = Variation_142
class Variation_1274(Repetitive):
    rep = None
    var = Variation_144
class Variation_1275(Repetitive):
    rep = None
    var = Variation_145
class Variation_1276(Repetitive):
    rep = None
    var = Variation_146
class Variation_1277(Repetitive):
    rep = None
    var = Variation_147
class Variation_1278(Repetitive):
    rep = None
    var = Variation_148
class Variation_1279(Explicit):
    t = None
class Variation_1280(Explicit):
    t = ReservedExpansion
class Variation_1281(Explicit):
    t = SpecialPurpose
class Item_60(Item):
    name = "010"
    title = "Data Source Identification"
    var = Variation_1061
class Item_137(Item):
    name = "040"
    title = "Target Report Descriptor"
    var = Variation_956
class Item_107(Item):
    name = "030"
    title = "Time of Day"
    var = Variation_311
class Item_274(Item):
    name = "130"
    title = "Position in WGS-84 Co-ordinates"
    var = Variation_999
class Item_214(Item):
    name = "080"
    title = "Target Address"
    var = Variation_296
class Item_286(Item):
    name = "140"
    title = "Geometric Altitude"
    var = Variation_257
class Item_222(Item):
    name = "090"
    title = "Figure of Merit"
    var = Variation_921
class Item_352(Item):
    name = "210"
    title = "Link Technology Indicator"
    var = Variation_888
class Item_366(Item):
    name = "230"
    title = "Roll Angle"
    var = Variation_246
class Item_292(Item):
    name = "145"
    title = "Flight Level"
    var = Variation_248
class Item_298(Item):
    name = "150"
    title = "Air Speed"
    var = Variation_993
class Item_301(Item):
    name = "151"
    title = "True Airspeed"
    var = Variation_264
class Item_303(Item):
    name = "152"
    title = "Magnetic Heading"
    var = Variation_293
class Item_304(Item):
    name = "155"
    title = "Barometric Vertical Rate"
    var = Variation_258
class Item_306(Item):
    name = "157"
    title = "Geometric Vertical Rate"
    var = Variation_258
class Item_309(Item):
    name = "160"
    title = "Ground Vector"
    var = Variation_987
class Item_317(Item):
    name = "165"
    title = "Rate Of Turn"
    var = Variation_1216
class Item_321(Item):
    name = "170"
    title = "Target Identification"
    var = Variation_326
class Item_238(Item):
    name = "095"
    title = "Velocity Accuracy"
    var = Variation_155
class Item_119(Item):
    name = "032"
    title = "Time of Day Accuracy"
    var = Variation_220
class Item_341(Item):
    name = "200"
    title = "Target Status"
    var = Variation_161
class Item_84(Item):
    name = "020"
    title = "Emitter Category"
    var = Variation_172
class Item_1884(Item):
    name = "WS"
    title = "Wind Speed"
    var = Variation_264
class Item_1877(Item):
    name = "WD"
    title = "Wind Direction"
    var = Variation_269
class Item_1718(Item):
    name = "TMP"
    title = "Temperature"
    var = Variation_251
class Item_1749(Item):
    name = "TRB"
    title = "Turbulence"
    var = Variation_191
class Variation_1375(Compound):
    fspec_size = None
    items = [Item_1884, Item_1877, Item_1718, Item_1749]
class Item_361(Item):
    name = "220"
    title = "Met Information"
    var = Variation_1375
class Item_294(Item):
    name = "146"
    title = "Intermediate State Selected Altitude"
    var = Variation_1069
class Item_296(Item):
    name = "148"
    title = "Final State Selected Altitude"
    var = Variation_1019
class Item_1715(Item):
    name = "TIS"
    title = "Trajectory Intent Status"
    var = Variation_1202
class Item_1710(Item):
    name = "TID"
    title = "Trajectory Intent Data"
    var = Variation_1257
class Variation_1371(Compound):
    fspec_size = None
    items = [Item_1715, Item_1710]
class Item_262(Item):
    name = "110"
    title = "Trajectory Intent"
    var = Variation_1371
class Item_1421(Item):
    name = "RE"
    title = "Reserved Expansion Field"
    var = Variation_1280
class Item_1580(Item):
    name = "SP"
    title = "Special Purpose Field"
    var = Variation_1281
class Variation_1282(Compound):
    fspec_size = None
    items = [Item_60, Item_137, Item_107, Item_274, Item_214, Item_286, Item_222, Item_352, Item_366, Item_292, Item_298, Item_301, Item_303, Item_304, Item_306, Item_309, Item_317, Item_321, Item_238, Item_119, Item_341, Item_84, Item_361, Item_294, Item_296, Item_262, None, None, None, None, None, None, None, Item_1421, Item_1580]
class Item_275(Item):
    name = "130"
    title = "Position in WGS-84 Co-ordinates"
    var = Variation_1001
class Variation_1283(Compound):
    fspec_size = None
    items = [Item_60, Item_137, Item_107, Item_275, Item_214, Item_286, Item_222, Item_352, Item_366, Item_292, Item_298, Item_301, Item_303, Item_304, Item_306, Item_309, Item_317, Item_321, Item_238, Item_119, Item_341, Item_84, Item_361, Item_294, Item_296, Item_262, None, None, None, None, None, None, None, Item_1421, Item_1580]
class Item_191(Item):
    name = "070"
    title = "Mode 3/A Code in Octal Representation"
    var = Variation_1124
class Item_281(Item):
    name = "131"
    title = "Signal Amplitude"
    var = Variation_155
class Variation_1284(Compound):
    fspec_size = None
    items = [Item_60, Item_137, Item_107, Item_275, Item_214, Item_286, Item_222, Item_352, Item_366, Item_292, Item_298, Item_301, Item_303, Item_304, Item_306, Item_309, Item_317, Item_321, Item_238, Item_119, Item_341, Item_84, Item_361, Item_294, Item_296, Item_262, Item_191, Item_281, None, None, None, None, None, Item_1421, Item_1580]
class Item_138(Item):
    name = "040"
    title = "Target Report Descriptor"
    var = Variation_1177
class Item_314(Item):
    name = "161"
    title = "Track Number"
    var = Variation_912
class Item_72(Item):
    name = "015"
    title = "Service Identification"
    var = Variation_155
class Item_200(Item):
    name = "071"
    title = "Time of Applicability for Position"
    var = Variation_311
class Item_279(Item):
    name = "131"
    title = "High-Resolution Position in WGS-84 Co-ordinates"
    var = Variation_1002
class Item_201(Item):
    name = "072"
    title = "Time of Applicability for Velocity"
    var = Variation_311
class Item_302(Item):
    name = "151"
    title = "True Airspeed"
    var = Variation_1044
class Item_202(Item):
    name = "073"
    title = "Time of Message Reception for Position"
    var = Variation_311
class Item_204(Item):
    name = "074"
    title = "Time of Message Reception of Position-High Precision"
    var = Variation_981
class Item_205(Item):
    name = "075"
    title = "Time of Message Reception for Velocity"
    var = Variation_311
class Item_207(Item):
    name = "076"
    title = "Time of Message Reception of Velocity-High Precision"
    var = Variation_982
class Item_287(Item):
    name = "140"
    title = "Geometric Height"
    var = Variation_257
class Item_228(Item):
    name = "090"
    title = "Quality Indicators"
    var = Variation_1206
class Item_353(Item):
    name = "210"
    title = "MOPS Version"
    var = Variation_878
class Item_190(Item):
    name = "070"
    title = "Mode 3/A Code in Octal Representation"
    var = Variation_905
class Item_343(Item):
    name = "200"
    title = "Target Status"
    var = Variation_992
class Item_305(Item):
    name = "155"
    title = "Barometric Vertical Rate"
    var = Variation_1041
class Item_307(Item):
    name = "157"
    title = "Geometric Vertical Rate"
    var = Variation_1043
class Item_308(Item):
    name = "160"
    title = "Airborne Ground Vector"
    var = Variation_1042
class Item_318(Item):
    name = "165"
    title = "Track Angle Rate"
    var = Variation_917
class Item_209(Item):
    name = "077"
    title = "Time of ASTERIX Report Transmission"
    var = Variation_311
class Item_83(Item):
    name = "020"
    title = "Emitter Category"
    var = Variation_160
class Item_295(Item):
    name = "146"
    title = "Selected Altitude"
    var = Variation_1068
class Item_297(Item):
    name = "148"
    title = "Final State Selected Altitude"
    var = Variation_1020
class Item_76(Item):
    name = "016"
    title = "Service Management"
    var = Variation_209
class Item_58(Item):
    name = "008"
    title = "Aircraft Operational Status"
    var = Variation_1039
class Item_385(Item):
    name = "271"
    title = "Surface Capabilities and Characteristics"
    var = Variation_1174
class Item_282(Item):
    name = "132"
    title = "Message Amplitude"
    var = Variation_193
class Item_376(Item):
    name = "250"
    title = "Mode S MB Data"
    var = Variation_1230
class Item_381(Item):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    var = Variation_1103
class Item_418(Item):
    name = "400"
    title = "Receiver ID"
    var = Variation_155
class Item_522(Item):
    name = "AOS"
    title = "Aircraft Operational Status Age"
    var = Variation_211
class Item_1752(Item):
    name = "TRD"
    title = "Target Report Descriptor Age"
    var = Variation_211
class Item_1084(Item):
    name = "M3A"
    title = "Mode 3/A Age"
    var = Variation_211
class Item_1383(Item):
    name = "QI"
    title = "Quality Indicators Age"
    var = Variation_211
class Item_1704(Item):
    name = "TI1"
    title = "Trajectory Intent Age"
    var = Variation_211
class Item_1095(Item):
    name = "MAM"
    title = "Message Amplitude Age"
    var = Variation_211
class Item_912(Item):
    name = "GH"
    title = "Geometric Height Age"
    var = Variation_211
class Item_858(Item):
    name = "FL"
    title = "Flight Level Age"
    var = Variation_211
class Item_1001(Item):
    name = "ISA"
    title = "Intermediate State Selected Altitude Age"
    var = Variation_211
class Item_882(Item):
    name = "FSA"
    title = "Final State Selected Altitude Age"
    var = Variation_211
class Item_542(Item):
    name = "AS"
    title = "Air Speed Age"
    var = Variation_211
class Item_1674(Item):
    name = "TAS"
    title = "True Air Speed Age"
    var = Variation_211
class Item_1145(Item):
    name = "MH"
    title = "Magnetic Heading Age"
    var = Variation_211
class Item_601(Item):
    name = "BVR"
    title = "Barometric Vertical Rate Age"
    var = Variation_211
class Item_929(Item):
    name = "GVR"
    title = "Geometric Vertical Rate Age"
    var = Variation_211
class Item_925(Item):
    name = "GV"
    title = "Ground Vector Age"
    var = Variation_211
class Item_1671(Item):
    name = "TAR"
    title = "Track Angle Rate Age"
    var = Variation_211
class Item_1705(Item):
    name = "TI2"
    title = "Target Identification Age"
    var = Variation_211
class Item_1764(Item):
    name = "TS"
    title = "Target Status Age"
    var = Variation_211
class Item_1138(Item):
    name = "MET"
    title = "Met Information Age"
    var = Variation_211
class Item_1452(Item):
    name = "ROA"
    title = "Roll Angle Age"
    var = Variation_211
class Item_533(Item):
    name = "ARA"
    title = "ACAS Resolution Advisory Age"
    var = Variation_211
class Item_1514(Item):
    name = "SCC"
    title = "Surface Capabilities and Characteristics Age"
    var = Variation_211
class Variation_1332(Compound):
    fspec_size = None
    items = [Item_522, Item_1752, Item_1084, Item_1383, Item_1704, Item_1095, Item_912, Item_858, Item_1001, Item_882, Item_542, Item_1674, Item_1145, Item_601, Item_929, Item_925, Item_1671, Item_1705, Item_1764, Item_1138, Item_1452, Item_533, Item_1514]
class Item_390(Item):
    name = "295"
    title = "Data Ages"
    var = Variation_1332
class Variation_1285(Compound):
    fspec_size = None
    items = [Item_60, Item_138, Item_314, Item_72, Item_200, Item_274, Item_279, Item_201, Item_298, Item_302, Item_214, Item_202, Item_204, Item_205, Item_207, Item_287, Item_228, Item_353, Item_190, Item_366, Item_292, Item_303, Item_343, Item_305, Item_307, Item_308, Item_318, Item_209, Item_321, Item_83, Item_361, Item_295, Item_297, Item_262, Item_76, Item_58, Item_385, Item_282, Item_376, Item_381, Item_418, Item_390, None, None, None, None, None, Item_1421, Item_1580]
class Item_354(Item):
    name = "210"
    title = "MOPS Version"
    var = Variation_879
class Variation_1286(Compound):
    fspec_size = None
    items = [Item_60, Item_138, Item_314, Item_72, Item_200, Item_274, Item_279, Item_201, Item_298, Item_302, Item_214, Item_202, Item_204, Item_205, Item_207, Item_287, Item_228, Item_354, Item_190, Item_366, Item_292, Item_303, Item_343, Item_305, Item_307, Item_308, Item_318, Item_209, Item_321, Item_83, Item_361, Item_295, Item_297, Item_262, Item_76, Item_58, Item_385, Item_282, Item_376, Item_381, Item_418, Item_390, None, None, None, None, None, Item_1421, Item_1580]
class Item_139(Item):
    name = "040"
    title = "Target Report Descriptor"
    var = Variation_1178
class Item_1506(Item):
    name = "SAL"
    title = "Selected Altitude Age"
    var = Variation_211
class Variation_1333(Compound):
    fspec_size = None
    items = [Item_522, Item_1752, Item_1084, Item_1383, Item_1704, Item_1095, Item_912, Item_858, Item_1506, Item_882, Item_542, Item_1674, Item_1145, Item_601, Item_929, Item_925, Item_1671, Item_1705, Item_1764, Item_1138, Item_1452, Item_533, Item_1514]
class Item_391(Item):
    name = "295"
    title = "Data Ages"
    var = Variation_1333
class Variation_1287(Compound):
    fspec_size = None
    items = [Item_60, Item_139, Item_314, Item_72, Item_200, Item_274, Item_279, Item_201, Item_298, Item_302, Item_214, Item_202, Item_204, Item_205, Item_207, Item_287, Item_228, Item_353, Item_190, Item_366, Item_292, Item_303, Item_343, Item_305, Item_307, Item_308, Item_318, Item_209, Item_321, Item_83, Item_361, Item_295, Item_297, Item_262, Item_76, Item_58, Item_385, Item_282, Item_376, Item_381, Item_418, Item_391, None, None, None, None, None, Item_1421, Item_1580]
class Item_140(Item):
    name = "040"
    title = "Target Report Descriptor"
    var = Variation_1179
class Item_342(Item):
    name = "200"
    title = "Target Status"
    var = Variation_991
class Item_384(Item):
    name = "271"
    title = "Surface Capabilities and Characteristics"
    var = Variation_1173
class Variation_1288(Compound):
    fspec_size = None
    items = [Item_60, Item_140, Item_314, Item_72, Item_200, Item_274, Item_279, Item_201, Item_298, Item_302, Item_214, Item_202, Item_204, Item_205, Item_207, Item_287, Item_228, Item_354, Item_190, Item_366, Item_292, Item_303, Item_342, Item_305, Item_307, Item_308, Item_318, Item_209, Item_321, Item_83, Item_361, Item_295, Item_297, Item_262, Item_76, Item_58, Item_384, Item_282, Item_376, Item_381, Item_418, Item_390, None, None, None, None, None, Item_1421, Item_1580]
class Variation_1289(Compound):
    fspec_size = None
    items = [Item_60, Item_140, Item_314, Item_72, Item_200, Item_274, Item_279, Item_201, Item_298, Item_302, Item_214, Item_202, Item_204, Item_205, Item_207, Item_287, Item_228, Item_354, Item_190, Item_366, Item_292, Item_303, Item_343, Item_305, Item_307, Item_308, Item_318, Item_209, Item_321, Item_83, Item_361, Item_295, Item_297, Item_262, Item_76, Item_58, Item_385, Item_282, Item_376, Item_381, Item_418, Item_390, None, None, None, None, None, Item_1421, Item_1580]
class Item_61(Item):
    name = "010"
    title = "Data Source Identifier"
    var = Variation_1061
class Item_197(Item):
    name = "070"
    title = "Time Of Track Information"
    var = Variation_311
class Item_251(Item):
    name = "105"
    title = "Calculated Position In WGS-84 Co-ordinates"
    var = Variation_1001
class Item_240(Item):
    name = "100"
    title = "Calculated Track Position (Cartesian)"
    var = Variation_1161
class Item_332(Item):
    name = "185"
    title = "Calculated Track Velocity (Cartesian)"
    var = Variation_1141
class Item_351(Item):
    name = "210"
    title = "Calculated Acceleration (Cartesian)"
    var = Variation_930
class Item_184(Item):
    name = "060"
    title = "Track Mode 3/A Code"
    var = Variation_1131
class Item_370(Item):
    name = "245"
    title = "Target Identification"
    var = Variation_1083
class Item_488(Item):
    name = "ADR"
    title = "Target Address"
    var = Variation_296
class Item_987(Item):
    name = "ID"
    title = "Target Identification"
    var = Variation_326
class Item_1146(Item):
    name = "MHG"
    title = "Magnetic Heading"
    var = Variation_293
class Item_980(Item):
    name = "IAS"
    title = "Indicated Airspeed/Mach No"
    var = Variation_994
class Item_1675(Item):
    name = "TAS"
    title = "True Airspeed"
    var = Variation_264
class Item_1505(Item):
    name = "SAL"
    title = "Selected Altitude"
    var = Variation_1067
class Item_885(Item):
    name = "FSS"
    title = "Final State Selected Altitude"
    var = Variation_1018
class Item_1716(Item):
    name = "TIS"
    title = "Trajectory Intent Status"
    var = Variation_1203
class Item_1711(Item):
    name = "TID"
    title = "Trajectory Intent Data"
    var = Variation_1258
class Item_687(Item):
    name = "COM"
    title = "Communications/ACAS Capability and Flight Status"
    var = Variation_949
class Item_1499(Item):
    name = "SAB"
    title = "Status Reported by ADS-B"
    var = Variation_922
class Item_480(Item):
    name = "ACS"
    title = "ACAS Resolution Advisory Report"
    var = Variation_327
class Item_599(Item):
    name = "BVR"
    title = "Barometric Vertical Rate"
    var = Variation_258
class Item_927(Item):
    name = "GVR"
    title = "Geometric Vertical Rate"
    var = Variation_258
class Item_1404(Item):
    name = "RAN"
    title = "Roll Angle"
    var = Variation_246
class Item_1670(Item):
    name = "TAR"
    title = "Track Angle Rate"
    var = Variation_1090
class Item_1666(Item):
    name = "TAN"
    title = "Track Angle"
    var = Variation_293
class Item_916(Item):
    name = "GS"
    title = "Ground Speed"
    var = Variation_256
class Item_1866(Item):
    name = "VUN"
    title = "Velocity Uncertainty"
    var = Variation_155
class Item_1139(Item):
    name = "MET"
    title = "Meteorological Data"
    var = Variation_1144
class Item_822(Item):
    name = "EMC"
    title = "Emitter Category"
    var = Variation_173
class Item_1308(Item):
    name = "POS"
    title = "Position"
    var = Variation_1008
class Item_900(Item):
    name = "GAL"
    title = "Geometric Altitude"
    var = Variation_257
class Item_1341(Item):
    name = "PUN"
    title = "Position Uncertainty"
    var = Variation_907
class Item_1099(Item):
    name = "MB"
    title = "MODE S MB DATA"
    var = Variation_1230
class Item_976(Item):
    name = "IAR"
    title = "Indicated Airspeed"
    var = Variation_264
class Item_1089(Item):
    name = "MAC"
    title = "Mach Number"
    var = Variation_278
class Item_597(Item):
    name = "BPS"
    title = "Barometric Pressure Setting (derived from Mode S BDS 4,0)"
    var = Variation_895
class Variation_1325(Compound):
    fspec_size = None
    items = [Item_488, Item_987, Item_1146, Item_980, Item_1675, Item_1505, Item_885, Item_1716, Item_1711, Item_687, Item_1499, Item_480, Item_599, Item_927, Item_1404, Item_1670, Item_1666, Item_916, Item_1866, Item_1139, Item_822, Item_1308, Item_900, Item_1341, Item_1099, Item_976, Item_1089, Item_597]
class Item_404(Item):
    name = "380"
    title = "Aircraft Derived Data"
    var = Variation_1325
class Item_141(Item):
    name = "040"
    title = "Track Number"
    var = Variation_234
class Item_215(Item):
    name = "080"
    title = "Track Status"
    var = Variation_1198
class Item_1757(Item):
    name = "TRK"
    title = "Track Age"
    var = Variation_217
class Item_1332(Item):
    name = "PSR"
    title = "PSR Age"
    var = Variation_217
class Item_1609(Item):
    name = "SSR"
    title = "SSR Age"
    var = Variation_217
class Item_1128(Item):
    name = "MDS"
    title = "Mode S Age"
    var = Variation_217
class Item_492(Item):
    name = "ADS"
    title = "ADS-C Age"
    var = Variation_282
class Item_846(Item):
    name = "ES"
    title = "ADS-B Extended Squitter Age"
    var = Variation_217
class Item_1850(Item):
    name = "VDL"
    title = "ADS-B VDL Mode 4 Age"
    var = Variation_217
class Item_1816(Item):
    name = "UAT"
    title = "ADS-B UAT Age"
    var = Variation_217
class Item_1064(Item):
    name = "LOP"
    title = "Loop Age"
    var = Variation_217
class Item_1160(Item):
    name = "MLT"
    title = "Multilateration Age"
    var = Variation_217
class Variation_1372(Compound):
    fspec_size = None
    items = [Item_1757, Item_1332, Item_1609, Item_1128, Item_492, Item_846, Item_1850, Item_1816, Item_1064, Item_1160]
class Item_389(Item):
    name = "290"
    title = "System Track Update Ages"
    var = Variation_1372
class Item_337(Item):
    name = "200"
    title = "Mode of Movement"
    var = Variation_1094
class Item_1143(Item):
    name = "MFL"
    title = "Measured Flight Level Age"
    var = Variation_217
class Item_1109(Item):
    name = "MD1"
    title = "Mode 1 Age"
    var = Variation_217
class Item_1112(Item):
    name = "MD2"
    title = "Mode 2 Age"
    var = Variation_217
class Item_1121(Item):
    name = "MDA"
    title = "Mode 3/A Age"
    var = Variation_217
class Item_1114(Item):
    name = "MD4"
    title = "Mode 4 Age"
    var = Variation_217
class Item_1116(Item):
    name = "MD5"
    title = "Mode 5 Age"
    var = Variation_217
class Item_1147(Item):
    name = "MHG"
    title = "Magnetic Heading Age"
    var = Variation_217
class Item_979(Item):
    name = "IAS"
    title = "Indicated Airspeed / Mach Nb Age"
    var = Variation_217
class Item_1676(Item):
    name = "TAS"
    title = "True Airspeed Age"
    var = Variation_217
class Item_1507(Item):
    name = "SAL"
    title = "Selected Altitude Age"
    var = Variation_217
class Item_886(Item):
    name = "FSS"
    title = "Final State Selected Altitude Age"
    var = Variation_217
class Item_1709(Item):
    name = "TID"
    title = "Trajectory Intent Age"
    var = Variation_217
class Item_683(Item):
    name = "COM"
    title = "Communication/ACAS Capability and Flight Status Age"
    var = Variation_217
class Item_1500(Item):
    name = "SAB"
    title = "Status Reported by ADS-B Age"
    var = Variation_217
class Item_481(Item):
    name = "ACS"
    title = "ACAS Resolution Advisory Report Age"
    var = Variation_217
class Item_602(Item):
    name = "BVR"
    title = "Barometric Vertical Rate Age"
    var = Variation_217
class Item_930(Item):
    name = "GVR"
    title = "Geometrical Vertical Rate Age"
    var = Variation_217
class Item_1405(Item):
    name = "RAN"
    title = "Roll Angle Age"
    var = Variation_217
class Item_1672(Item):
    name = "TAR"
    title = "Track Angle Rate Age"
    var = Variation_217
class Item_1667(Item):
    name = "TAN"
    title = "Track Angle Age"
    var = Variation_217
class Item_921(Item):
    name = "GSP"
    title = "Ground Speed Age"
    var = Variation_217
class Item_1867(Item):
    name = "VUN"
    title = "Velocity Uncertainty Age"
    var = Variation_217
class Item_1140(Item):
    name = "MET"
    title = "Meteorological Data Age"
    var = Variation_217
class Item_823(Item):
    name = "EMC"
    title = "Emitter Category Age"
    var = Variation_217
class Item_1309(Item):
    name = "POS"
    title = "Position Age"
    var = Variation_217
class Item_901(Item):
    name = "GAL"
    title = "Geometric Altitude Age"
    var = Variation_217
class Item_1342(Item):
    name = "PUN"
    title = "Position Uncertainty Age"
    var = Variation_217
class Item_1100(Item):
    name = "MB"
    title = "Mode S MB Data Age"
    var = Variation_217
class Item_977(Item):
    name = "IAR"
    title = "Indicated Airspeed Data Age"
    var = Variation_217
class Item_1090(Item):
    name = "MAC"
    title = "Mach Number Data Age"
    var = Variation_217
class Item_598(Item):
    name = "BPS"
    title = "Barometric Pressure Setting Data Age"
    var = Variation_217
class Variation_1354(Compound):
    fspec_size = None
    items = [Item_1143, Item_1109, Item_1112, Item_1121, Item_1114, Item_1116, Item_1147, Item_979, Item_1676, Item_1507, Item_886, Item_1709, Item_683, Item_1500, Item_481, Item_602, Item_930, Item_1405, Item_1672, Item_1667, Item_921, Item_1867, Item_1140, Item_823, Item_1309, Item_901, Item_1342, Item_1100, Item_977, Item_1090, Item_598]
class Item_392(Item):
    name = "295"
    title = "Track Data Ages"
    var = Variation_1354
class Item_284(Item):
    name = "136"
    title = "Measured Flight Level"
    var = Variation_248
class Item_272(Item):
    name = "130"
    title = "Calculated Track Geometric Altitude"
    var = Variation_257
class Item_283(Item):
    name = "135"
    title = "Calculated Track Barometric Altitude"
    var = Variation_1036
class Item_360(Item):
    name = "220"
    title = "Calculated Rate of Climb/Descent"
    var = Variation_258
class Item_1665(Item):
    name = "TAG"
    title = "FPPS Identification Tag"
    var = Variation_1061
class Item_729(Item):
    name = "CS"
    title = "Callsign"
    var = Variation_328
class Item_993(Item):
    name = "IFI"
    title = "IFPS_FLIGHT_ID"
    var = Variation_1099
class Item_854(Item):
    name = "FCT"
    title = "Flight Category"
    var = Variation_983
class Item_1664(Item):
    name = "TAC"
    title = "Type of Aircraft"
    var = Variation_314
class Item_1888(Item):
    name = "WTC"
    title = "Wake Turbulence Category"
    var = Variation_190
class Item_776(Item):
    name = "DEP"
    title = "Departure Airport"
    var = Variation_314
class Item_800(Item):
    name = "DST"
    title = "Destination Airport"
    var = Variation_314
class Item_1418(Item):
    name = "RDS"
    title = "Runway Designation"
    var = Variation_1026
class Item_628(Item):
    name = "CFL"
    title = "Current Cleared Flight Level"
    var = Variation_280
class Item_746(Item):
    name = "CTL"
    title = "Current Control Position"
    var = Variation_942
class Item_1727(Item):
    name = "TOD"
    title = "Time of Departure / Arrival"
    var = Variation_1261
class Item_545(Item):
    name = "AST"
    title = "Aircraft Stand"
    var = Variation_325
class Item_1646(Item):
    name = "STS"
    title = "Stand Status"
    var = Variation_960
class Item_1635(Item):
    name = "STD"
    title = "Standard Instrument Departure"
    var = Variation_328
class Item_1618(Item):
    name = "STA"
    title = "Standard Instrument Arrival"
    var = Variation_328
class Item_1288(Item):
    name = "PEM"
    title = "Pre-Emergency Mode 3/A"
    var = Variation_893
class Item_1287(Item):
    name = "PEC"
    title = "Pre-Emergency Callsign"
    var = Variation_328
class Variation_1369(Compound):
    fspec_size = None
    items = [Item_1665, Item_729, Item_993, Item_854, Item_1664, Item_1888, Item_776, Item_800, Item_1418, Item_628, Item_746, Item_1727, Item_545, Item_1646, Item_1635, Item_1618, Item_1288, Item_1287]
class Item_412(Item):
    name = "390"
    title = "Flight Plan Related Data"
    var = Variation_1369
class Item_383(Item):
    name = "270"
    title = "Target Size and Orientation"
    var = Variation_1192
class Item_396(Item):
    name = "300"
    title = "Vehicle Fleet Identification"
    var = Variation_167
class Item_1655(Item):
    name = "SUM"
    title = "Mode 5 Summary"
    var = Variation_1011
class Item_1295(Item):
    name = "PMN"
    title = "Mode 5 PIN/ National Origin/Mission Code"
    var = Variation_882
class Item_1305(Item):
    name = "POS"
    title = "Mode 5 Reported Position"
    var = Variation_999
class Item_899(Item):
    name = "GA"
    title = "Mode 5 GNSS-derived Altitude"
    var = Variation_877
class Item_818(Item):
    name = "EM1"
    title = "Extended Mode 1 Code in Octal Representation"
    var = Variation_897
class Item_1730(Item):
    name = "TOS"
    title = "Time Offset for POS and GA"
    var = Variation_199
class Item_1926(Item):
    name = "XP"
    title = "X Pulse Presence"
    var = Variation_894
class Variation_1365(Compound):
    fspec_size = None
    items = [Item_1655, Item_1295, Item_1305, Item_899, Item_818, Item_1730, Item_1926]
class Item_259(Item):
    name = "110"
    title = "Mode 5 Data Reports and Extended Mode 1 Code"
    var = Variation_1365
class Item_271(Item):
    name = "120"
    title = "Track Mode 2 Code"
    var = Variation_901
class Item_438(Item):
    name = "510"
    title = "Composed Track Number"
    var = Variation_1194
class Item_524(Item):
    name = "APC"
    title = "Estimated Accuracy Of Track Position (Cartesian)"
    var = Variation_1152
class Item_702(Item):
    name = "COV"
    title = "XY Covariance Component"
    var = Variation_242
class Item_529(Item):
    name = "APW"
    title = "Estimated Accuracy Of Track Position (WGS-84)"
    var = Variation_996
class Item_497(Item):
    name = "AGA"
    title = "Estimated Accuracy Of Calculated Track Geometric Altitude"
    var = Variation_222
class Item_470(Item):
    name = "ABA"
    title = "Estimated Accuracy Of Calculated Track Barometric Altitude"
    var = Variation_213
class Item_552(Item):
    name = "ATV"
    title = "Estimated Accuracy Of Track Velocity (Cartesian)"
    var = Variation_1153
class Item_466(Item):
    name = "AA"
    title = "Estimated Accuracy Of Acceleration (Cartesian)"
    var = Variation_1151
class Item_539(Item):
    name = "ARC"
    title = "Estimated Accuracy Of Rate Of Climb/Descent"
    var = Variation_223
class Variation_1334(Compound):
    fspec_size = None
    items = [Item_524, Item_702, Item_529, Item_497, Item_470, Item_552, Item_466, Item_539]
class Item_433(Item):
    name = "500"
    title = "Estimated Accuracies"
    var = Variation_1334
class Item_1560(Item):
    name = "SID"
    title = "Sensor Identification"
    var = Variation_1061
class Item_1304(Item):
    name = "POS"
    title = "Measured Position"
    var = Variation_1049
class Item_942(Item):
    name = "HEIGHT"
    title = "Measured 3-D Height"
    var = Variation_270
class Item_1122(Item):
    name = "MDC"
    title = ""
    var = Variation_1138
class Item_1118(Item):
    name = "MDA"
    title = ""
    var = Variation_1133
class Item_1802(Item):
    name = "TYP"
    title = ""
    var = Variation_1104
class Variation_1363(Compound):
    fspec_size = None
    items = [Item_1560, Item_1304, Item_942, Item_1122, Item_1118, Item_1802]
class Item_401(Item):
    name = "340"
    title = "Measured Information"
    var = Variation_1363
class Variation_1290(Compound):
    fspec_size = None
    items = [Item_61, None, Item_72, Item_197, Item_251, Item_240, Item_332, Item_351, Item_184, Item_370, Item_404, Item_141, Item_215, Item_389, Item_337, Item_392, Item_284, Item_272, Item_283, Item_360, Item_412, Item_383, Item_396, Item_259, Item_271, Item_438, Item_433, Item_401, None, None, None, None, None, Item_1421, Item_1580]
class Item_688(Item):
    name = "COM"
    title = "Communications/ACAS Capability and Flight Status"
    var = Variation_951
class Item_582(Item):
    name = "BDSDATA"
    title = "BDS Register DATA"
    var = Variation_1230
class Item_595(Item):
    name = "BPS"
    title = "Barometric Pressure Setting"
    var = Variation_895
class Variation_1326(Compound):
    fspec_size = None
    items = [Item_488, Item_987, Item_1146, Item_980, Item_1675, Item_1505, Item_885, Item_1716, Item_1711, Item_688, Item_1499, Item_480, Item_599, Item_927, Item_1404, Item_1670, Item_1666, Item_916, Item_1866, Item_1139, Item_822, Item_1308, Item_900, Item_1341, Item_582, Item_976, Item_1089, Item_595]
class Item_405(Item):
    name = "380"
    title = "Aircraft Derived Data"
    var = Variation_1326
class Item_217(Item):
    name = "080"
    title = "Track Status"
    var = Variation_1200
class Item_941(Item):
    name = "HEIGHT"
    title = "Measured 3-D Height"
    var = Variation_240
class Variation_1362(Compound):
    fspec_size = None
    items = [Item_1560, Item_1304, Item_941, Item_1122, Item_1118, Item_1802]
class Item_400(Item):
    name = "340"
    title = "Measured Information"
    var = Variation_1362
class Variation_1291(Compound):
    fspec_size = None
    items = [Item_61, None, Item_72, Item_197, Item_251, Item_240, Item_332, Item_351, Item_184, Item_370, Item_405, Item_141, Item_217, Item_389, Item_337, Item_392, Item_284, Item_272, Item_283, Item_360, Item_412, Item_383, Item_396, Item_259, Item_271, Item_438, Item_433, Item_400, None, None, None, None, None, Item_1421, Item_1580]
class Variation_1327(Compound):
    fspec_size = None
    items = [Item_488, Item_987, Item_1146, Item_980, Item_1675, Item_1505, Item_885, Item_1716, Item_1711, Item_688, Item_1499, Item_480, Item_599, Item_927, Item_1404, Item_1670, Item_1666, Item_916, Item_1866, Item_1139, Item_822, Item_1308, Item_900, Item_1341, Item_1099, Item_976, Item_1089, Item_595]
class Item_406(Item):
    name = "380"
    title = "Aircraft Derived Data"
    var = Variation_1327
class Item_216(Item):
    name = "080"
    title = "Track Status"
    var = Variation_1199
class Variation_1292(Compound):
    fspec_size = None
    items = [Item_61, None, Item_72, Item_197, Item_251, Item_240, Item_332, Item_351, Item_184, Item_370, Item_406, Item_141, Item_216, Item_389, Item_337, Item_392, Item_284, Item_272, Item_283, Item_360, Item_412, Item_383, Item_396, Item_259, Item_271, Item_438, Item_433, Item_400, None, None, None, None, None, Item_1421, Item_1580]
class Variation_1328(Compound):
    fspec_size = None
    items = [Item_488, Item_987, Item_1146, Item_980, Item_1675, Item_1505, Item_885, Item_1716, Item_1711, Item_688, Item_1499, Item_480, Item_599, Item_927, Item_1404, Item_1670, Item_1666, Item_916, Item_1866, Item_1139, Item_822, Item_1308, Item_900, Item_1341, Item_1099, Item_976, Item_1089, Item_597]
class Item_407(Item):
    name = "380"
    title = "Aircraft Derived Data"
    var = Variation_1328
class Variation_1293(Compound):
    fspec_size = None
    items = [Item_61, None, Item_72, Item_197, Item_251, Item_240, Item_332, Item_351, Item_184, Item_370, Item_407, Item_141, Item_216, Item_389, Item_337, Item_392, Item_284, Item_272, Item_283, Item_360, Item_412, Item_383, Item_396, Item_259, Item_271, Item_438, Item_433, Item_401, None, None, None, None, None, Item_1421, Item_1580]
class Item_34(Item):
    name = "000"
    title = "Message Type"
    var = Variation_169
class Item_71(Item):
    name = "015"
    title = "SDPS Identifier"
    var = Variation_1252
class Item_94(Item):
    name = "020"
    title = "Time of Message"
    var = Variation_311
class Item_130(Item):
    name = "040"
    title = "Alert Identifier"
    var = Variation_234
class Item_154(Item):
    name = "045"
    title = "Alert Status"
    var = Variation_909
class Item_178(Item):
    name = "060"
    title = "Safety Net Function and System Status"
    var = Variation_1201
class Item_109(Item):
    name = "030"
    title = "Track Number 1"
    var = Variation_234
class Item_502(Item):
    name = "AI1"
    title = "Aircraft Identifier (in 7 Characters) of Aircraft 1 Involved in the Conflict"
    var = Variation_328
class Item_1081(Item):
    name = "M31"
    title = "Mode 3/A Code Aircraft 1"
    var = Variation_903
class Item_721(Item):
    name = "CPW"
    title = "Predicted Conflict Position Target 1 in WGS-84 Coordinates"
    var = Variation_998
class Item_718(Item):
    name = "CPC"
    title = "Predicted Conflict Position for the Aircraft 1 Involved in the Conflict"
    var = Variation_1159
class Item_1782(Item):
    name = "TT1"
    title = "Time to Runway Threshold for First Approaching Aircraft in a RIMCA"
    var = Variation_311
class Item_801(Item):
    name = "DT1"
    title = "Distance to Runway Threshold for Aircraft 1 Involved in a RIMCA"
    var = Variation_271
class Item_475(Item):
    name = "AC1"
    title = "Characteristics of Aircraft 1 Involved in the Conflict"
    var = Variation_1191
class Item_1187(Item):
    name = "MS1"
    title = "Aircraft Identification Downloaded from Aircraft 1 Involved in the Conflict If Equipped with a Mode-S Transponder"
    var = Variation_325
class Item_869(Item):
    name = "FP1"
    title = "Number of the Flight Plan Correlated to Aircraft 1 Involved in the Conflict"
    var = Variation_914
class Item_626(Item):
    name = "CF1"
    title = "Cleared Flight Level for Aircraft 1 Involved in the Conflict"
    var = Variation_280
class Variation_1329(Compound):
    fspec_size = None
    items = [Item_502, Item_1081, Item_721, Item_718, Item_1782, Item_801, Item_475, Item_1187, Item_869, Item_626]
class Item_319(Item):
    name = "170"
    title = "Aircraft Identification and Characteristics 1"
    var = Variation_1329
class Item_646(Item):
    name = "CN"
    title = "Conflict Nature"
    var = Variation_1193
class Item_613(Item):
    name = "CC"
    title = "Conflict Classification"
    var = Variation_1091
class Item_715(Item):
    name = "CP"
    title = "Conflict Probability"
    var = Variation_208
class Item_615(Item):
    name = "CD"
    title = "Conflict Duration"
    var = Variation_311
class Variation_1338(Compound):
    fspec_size = None
    items = [Item_646, Item_613, Item_715, Item_615]
class Item_265(Item):
    name = "120"
    title = "Conflict Characteristics"
    var = Variation_1338
class Item_1679(Item):
    name = "TC"
    title = "Time to Conflict"
    var = Variation_311
class Item_1683(Item):
    name = "TCA"
    title = "Time to Closest Approach"
    var = Variation_311
class Item_638(Item):
    name = "CHS"
    title = "Current Horizontal Separation"
    var = Variation_307
class Item_1148(Item):
    name = "MHS"
    title = "Estimated Minimum Horizontal Separation"
    var = Variation_271
class Item_750(Item):
    name = "CVS"
    title = "Current Vertical Separation"
    var = Variation_270
class Item_1206(Item):
    name = "MVS"
    title = "Estimated Minimum Vertical Separation"
    var = Variation_270
class Variation_1370(Compound):
    fspec_size = None
    items = [Item_1679, Item_1683, Item_638, Item_1148, Item_750, Item_1206]
class Item_186(Item):
    name = "070"
    title = "Conflict Timing and Separation"
    var = Variation_1370
class Item_208(Item):
    name = "076"
    title = "Vertical Deviation"
    var = Variation_240
class Item_203(Item):
    name = "074"
    title = "Longitudinal Deviation"
    var = Variation_241
class Item_206(Item):
    name = "075"
    title = "Transversal Distance Deviation"
    var = Variation_299
class Item_519(Item):
    name = "AN"
    title = "Area Name"
    var = Variation_326
class Item_607(Item):
    name = "CAN"
    title = "Crossing Area Name"
    var = Variation_328
class Item_1480(Item):
    name = "RT1"
    title = "Runway/Taxiway Designator 1"
    var = Variation_328
class Item_1481(Item):
    name = "RT2"
    title = "Runway/Taxiway Designator 2"
    var = Variation_328
class Item_1512(Item):
    name = "SB"
    title = "Stop Bar Designator"
    var = Variation_328
class Item_895(Item):
    name = "G"
    title = "Gate Designator"
    var = Variation_328
class Variation_1331(Compound):
    fspec_size = None
    items = [Item_519, Item_607, Item_1480, Item_1481, Item_1512, Item_895]
class Item_239(Item):
    name = "100"
    title = "Area Definition"
    var = Variation_1331
class Item_124(Item):
    name = "035"
    title = "Track Number 2"
    var = Variation_234
class Item_503(Item):
    name = "AI2"
    title = "Aircraft Identifier (in 7 Characters) of Aircraft 2 Involved in the Conflict"
    var = Variation_328
class Item_1082(Item):
    name = "M32"
    title = "Mode 3/A Code Aircraft 2"
    var = Variation_904
class Item_722(Item):
    name = "CPW"
    title = "Predicted Conflict Position Target 2 in WGS-84 Coordinates"
    var = Variation_998
class Item_719(Item):
    name = "CPL"
    title = "Predicted Conflict Position for the Aircraft 2 Involved in the Conflict"
    var = Variation_1159
class Item_1783(Item):
    name = "TT2"
    title = "Time to Runway Threshold for Second Approaching Aircraft in a RIMCA"
    var = Variation_311
class Item_802(Item):
    name = "DT2"
    title = "Distance to Runway Threshold for Aircraft 2 Involved in a RIMCA"
    var = Variation_271
class Item_476(Item):
    name = "AC2"
    title = "Characteristics of Aircraft 2 Involved in the Conflict"
    var = Variation_1191
class Item_1188(Item):
    name = "MS2"
    title = "Aircraft Identification Downloaded From Aircraft 2 Involved in the Conflict If Eequipped With a Mode-S Transponder"
    var = Variation_325
class Item_870(Item):
    name = "FP2"
    title = "Number of the Flight Plan Correlated to Aircraft 2 Involved in the Conflict"
    var = Variation_914
class Item_627(Item):
    name = "CF2"
    title = "Cleared Flight Level for Aircraft 2 Involved in the Conflict"
    var = Variation_280
class Variation_1330(Compound):
    fspec_size = None
    items = [Item_503, Item_1082, Item_722, Item_719, Item_1783, Item_802, Item_476, Item_1188, Item_870, Item_627]
class Item_330(Item):
    name = "171"
    title = "Aircraft Identification and Characteristics 2"
    var = Variation_1330
class Item_256(Item):
    name = "110"
    title = "FDPS Sector Control Identification"
    var = Variation_1240
class Variation_1294(Compound):
    fspec_size = None
    items = [Item_61, Item_34, Item_71, Item_94, Item_130, Item_154, Item_178, Item_109, Item_319, Item_265, Item_186, Item_208, Item_203, Item_206, Item_239, Item_124, Item_330, Item_256, None, Item_1421, Item_1580]
class Item_35(Item):
    name = "000"
    title = "Message Type"
    var = Variation_175
class Item_85(Item):
    name = "020"
    title = "Sector Number"
    var = Variation_224
class Item_144(Item):
    name = "041"
    title = "Antenna Rotation Speed"
    var = Variation_287
class Item_682(Item):
    name = "COM"
    title = "Common Part"
    var = Variation_1023
class Item_1335(Item):
    name = "PSR"
    title = "Specific Status Information for a PSR Sensor"
    var = Variation_925
class Item_1612(Item):
    name = "SSR"
    title = "Specific Status Information for a SSR Sensor"
    var = Variation_927
class Item_1131(Item):
    name = "MDS"
    title = "Specific Status Information for a Mode S Sensor"
    var = Variation_926
class Variation_1340(Compound):
    fspec_size = None
    items = [Item_682, None, None, Item_1335, Item_1612, Item_1131]
class Item_167(Item):
    name = "050"
    title = "System Configuration and Status"
    var = Variation_1340
class Item_681(Item):
    name = "COM"
    title = "Common Part"
    var = Variation_875
class Item_1334(Item):
    name = "PSR"
    title = "Specific Processing Mode Information for a PSR Sensor"
    var = Variation_1030
class Item_1611(Item):
    name = "SSR"
    title = "Specific Processing Mode Information for a SSR Sensor"
    var = Variation_1046
class Item_1130(Item):
    name = "MDS"
    title = "Specific Processing Mode Information for a Mode S Sensor"
    var = Variation_1045
class Variation_1339(Compound):
    fspec_size = None
    items = [Item_681, None, None, Item_1334, Item_1611, Item_1130]
class Item_182(Item):
    name = "060"
    title = "System Processing Mode"
    var = Variation_1339
class Item_188(Item):
    name = "070"
    title = "Message Count Values"
    var = Variation_1263
class Item_242(Item):
    name = "100"
    title = "Generic Polar Window"
    var = Variation_1051
class Item_254(Item):
    name = "110"
    title = "Data Filter"
    var = Variation_158
class Item_263(Item):
    name = "120"
    title = "3D-Position Of Data Source"
    var = Variation_990
class Item_221(Item):
    name = "090"
    title = "Collimation Error"
    var = Variation_1053
class Variation_1295(Compound):
    fspec_size = None
    items = [Item_61, Item_35, Item_107, Item_85, Item_144, Item_167, Item_182, Item_188, Item_242, Item_254, Item_263, Item_221, Item_1421, Item_1580]
class Item_36(Item):
    name = "000"
    title = "Message Type"
    var = Variation_176
class Item_189(Item):
    name = "070"
    title = "Message Count Values"
    var = Variation_1264
class Variation_1296(Compound):
    fspec_size = None
    items = [Item_61, Item_36, Item_107, Item_85, Item_144, Item_167, Item_182, Item_189, Item_242, Item_254, Item_263, Item_221, Item_1421, Item_1580]
class Item_37(Item):
    name = "000"
    title = "Message Type"
    var = Variation_177
class Variation_1297(Compound):
    fspec_size = None
    items = [Item_61, Item_37, Item_107, Item_85, Item_144, Item_167, Item_182, Item_189, Item_242, Item_254, Item_263, Item_221, Item_1421, Item_1580]
class Item_38(Item):
    name = "000"
    title = "Message Type"
    var = Variation_178
class Item_166(Item):
    name = "050"
    title = "Station Configuration Status"
    var = Variation_1273
class Item_180(Item):
    name = "060"
    title = "Station Processing Mode"
    var = Variation_1273
class Item_196(Item):
    name = "070"
    title = "Plot Count Values"
    var = Variation_1235
class Item_241(Item):
    name = "100"
    title = "Dynamic Window Type 1"
    var = Variation_1054
class Item_220(Item):
    name = "090"
    title = "Collimation Error"
    var = Variation_1040
class Item_218(Item):
    name = "080"
    title = "Warning/Error Conditions"
    var = Variation_1273
class Variation_1298(Compound):
    fspec_size = None
    items = [Item_61, Item_38, Item_85, Item_107, Item_144, Item_166, Item_180, Item_196, Item_241, Item_220, Item_218, None, Item_1580, None]
class Item_39(Item):
    name = "000"
    title = "Message Type"
    var = Variation_179
class Item_96(Item):
    name = "020"
    title = "Vector Qualifier"
    var = Variation_1208
class Item_127(Item):
    name = "036"
    title = "Sequence of Cartesian Vectors in SPF Notation"
    var = Variation_1269
class Item_122(Item):
    name = "034"
    title = "Sequence of Polar Vectors in SPF Notation"
    var = Variation_1256
class Item_131(Item):
    name = "040"
    title = "Contour Identifier"
    var = Variation_1027
class Item_164(Item):
    name = "050"
    title = "Sequence of Contour Points in SPF Notation"
    var = Variation_1271
class Item_231(Item):
    name = "090"
    title = "Time of Day"
    var = Variation_311
class Item_246(Item):
    name = "100"
    title = "Processing Status"
    var = Variation_1189
class Item_261(Item):
    name = "110"
    title = "Station Configuration Status"
    var = Variation_1273
class Item_270(Item):
    name = "120"
    title = "Total Number of Items Constituting One Weather Picture"
    var = Variation_235
class Item_129(Item):
    name = "038"
    title = "Sequence of Weather Vectors in SPF Notation"
    var = Variation_1272
class Variation_1299(Compound):
    fspec_size = None
    items = [Item_61, Item_39, Item_96, Item_127, Item_122, Item_131, Item_164, Item_231, Item_246, Item_261, Item_270, Item_129, Item_1580, None]
class Item_40(Item):
    name = "000"
    title = "Message Type"
    var = Variation_180
class Item_108(Item):
    name = "030"
    title = "Time of Message"
    var = Variation_311
class Item_81(Item):
    name = "020"
    title = "Batch Number"
    var = Variation_191
class Item_136(Item):
    name = "040"
    title = "SDPS Configuration and Status"
    var = Variation_1022
class Item_165(Item):
    name = "050"
    title = "Service Status Report"
    var = Variation_181
class Variation_1300(Compound):
    fspec_size = None
    items = [Item_61, Item_40, Item_72, Item_108, Item_81, Item_136, Item_165, None, None, None, None, None, Item_1421, Item_1580]
class Item_41(Item):
    name = "000"
    title = "Message Type"
    var = Variation_182
class Item_289(Item):
    name = "140"
    title = "Time of Day"
    var = Variation_311
class Item_441(Item):
    name = "550"
    title = "System Status"
    var = Variation_1025
class Item_442(Item):
    name = "551"
    title = "Tracking Processor Detailed Status"
    var = Variation_1093
class Item_443(Item):
    name = "552"
    title = "Remote Sensor Detailed Status"
    var = Variation_1250
class Item_444(Item):
    name = "553"
    title = "Reference Transponder Detailed Status"
    var = Variation_1209
class Item_447(Item):
    name = "600"
    title = "Position of the MLT System Reference Point"
    var = Variation_1002
class Item_455(Item):
    name = "610"
    title = "Height of the MLT System Reference Point"
    var = Variation_249
class Item_458(Item):
    name = "620"
    title = "WGS-84 Undulation"
    var = Variation_194
class Variation_1301(Compound):
    fspec_size = None
    items = [Item_61, Item_41, Item_289, Item_441, Item_442, Item_443, Item_444, Item_447, Item_455, Item_458, None, None, Item_1421, Item_1580]
class Item_44(Item):
    name = "000"
    title = "Message Type"
    var = Variation_185
class Item_92(Item):
    name = "020"
    title = "Target Report Descriptor"
    var = Variation_1220
class Item_146(Item):
    name = "041"
    title = "Position in WGS-84 Co-ordinates"
    var = Variation_1003
class Item_134(Item):
    name = "040"
    title = "Measured Position in Polar Co-ordinates"
    var = Variation_1050
class Item_152(Item):
    name = "042"
    title = "Position in Cartesian Co-ordinates"
    var = Variation_1160
class Item_335(Item):
    name = "200"
    title = "Calculated Track Velocity in Polar Co-ordinates"
    var = Variation_989
class Item_345(Item):
    name = "202"
    title = "Calculated Track Velocity in Cartesian Co-ordinates"
    var = Variation_1143
class Item_312(Item):
    name = "161"
    title = "Track Number"
    var = Variation_910
class Item_322(Item):
    name = "170"
    title = "Track Status"
    var = Variation_1181
class Item_176(Item):
    name = "060"
    title = "Mode-3/A Code in Octal Representation"
    var = Variation_1136
class Item_362(Item):
    name = "220"
    title = "Target Address"
    var = Variation_296
class Item_371(Item):
    name = "245"
    title = "Target Identification"
    var = Variation_1084
class Item_377(Item):
    name = "250"
    title = "Mode S MB Data"
    var = Variation_1245
class Item_224(Item):
    name = "090"
    title = "Flight Level in Binary Representation"
    var = Variation_1132
class Item_232(Item):
    name = "091"
    title = "Measured Height"
    var = Variation_257
class Item_440(Item):
    name = "550"
    title = "System Status"
    var = Variation_1024
class Item_398(Item):
    name = "310"
    title = "Pre-programmed Message"
    var = Variation_1096
class Item_436(Item):
    name = "500"
    title = "Standard Deviation of Position"
    var = Variation_957
class Item_386(Item):
    name = "280"
    title = "Presence"
    var = Variation_1244
class Item_278(Item):
    name = "131"
    title = "Amplitude of Primary Plot"
    var = Variation_155
class Item_350(Item):
    name = "210"
    title = "Calculated Acceleration"
    var = Variation_932
class Variation_1302(Compound):
    fspec_size = None
    items = [Item_61, Item_44, Item_92, Item_289, Item_146, Item_134, Item_152, Item_335, Item_345, Item_312, Item_322, Item_176, Item_362, Item_371, Item_377, Item_396, Item_224, Item_232, Item_383, Item_440, Item_398, Item_436, Item_386, Item_278, Item_350, None, Item_1580, Item_1421]
class Item_46(Item):
    name = "000"
    title = "Message Type"
    var = Variation_187
class Item_97(Item):
    name = "020"
    title = "Video Record Header"
    var = Variation_315
class Item_110(Item):
    name = "030"
    title = "Video Summary"
    var = Variation_1224
class Item_143(Item):
    name = "040"
    title = "Video Header Nano"
    var = Variation_1082
class Item_148(Item):
    name = "041"
    title = "Video Header Femto"
    var = Variation_1081
class Item_156(Item):
    name = "048"
    title = "Video Cells Resolution & Data Compression Indicator"
    var = Variation_936
class Item_157(Item):
    name = "049"
    title = "Video Octets & Video Cells Counters"
    var = Variation_1021
class Item_169(Item):
    name = "050"
    title = "Video Block Low Data Volume"
    var = Variation_1227
class Item_170(Item):
    name = "051"
    title = "Video Block Medium Data Volume"
    var = Variation_1231
class Item_171(Item):
    name = "052"
    title = "Video Block High Data Volume"
    var = Variation_1232
class Variation_1303(Compound):
    fspec_size = None
    items = [Item_61, Item_46, Item_97, Item_110, Item_143, Item_148, Item_156, Item_157, Item_169, Item_170, Item_171, Item_289, Item_1421, Item_1580]
class Item_47(Item):
    name = "000"
    title = "Message Type"
    var = Variation_188
class Item_95(Item):
    name = "020"
    title = "Vector Qualifier"
    var = Variation_1207
class Item_106(Item):
    name = "030"
    title = "Sequence of Cartesian Vectors"
    var = Variation_1270
class Item_181(Item):
    name = "060"
    title = "Synchronisation/Control Signal"
    var = Variation_1211
class Item_199(Item):
    name = "070"
    title = "Time of Day"
    var = Variation_311
class Item_211(Item):
    name = "080"
    title = "Processing Status"
    var = Variation_1189
class Item_229(Item):
    name = "090"
    title = "Radar Configuration and Status"
    var = Variation_1251
class Item_249(Item):
    name = "100"
    title = "Vector Count"
    var = Variation_235
class Variation_1304(Compound):
    fspec_size = None
    items = [Item_61, Item_47, Item_95, Item_106, Item_181, Item_199, Item_211, Item_229, Item_249]
class Item_48(Item):
    name = "000"
    title = "Message Type"
    var = Variation_1017
class Item_87(Item):
    name = "020"
    title = "Target Report Descriptor"
    var = Variation_1195
class Item_111(Item):
    name = "030"
    title = "Warning/Error Conditions"
    var = Variation_1273
class Item_293(Item):
    name = "145"
    title = "Time of Applicability"
    var = Variation_311
class Item_316(Item):
    name = "161"
    title = "Track/Plot Number"
    var = Variation_235
class Item_329(Item):
    name = "170"
    title = "Track/Plot Status"
    var = Variation_1180
class Item_168(Item):
    name = "050"
    title = "Update Period"
    var = Variation_885
class Item_1038(Item):
    name = "LEN"
    title = "Target Length"
    var = Variation_274
class Item_1880(Item):
    name = "WDT"
    title = "Target Width"
    var = Variation_274
class Item_948(Item):
    name = "HGT"
    title = "Target Height"
    var = Variation_274
class Item_1267(Item):
    name = "ORT"
    title = "Target Orientation"
    var = Variation_293
class Variation_1351(Compound):
    fspec_size = None
    items = [Item_1038, Item_1880, Item_948, Item_1267]
class Item_382(Item):
    name = "270"
    title = "Target Size & Orientation"
    var = Variation_1351
class Item_393(Item):
    name = "300"
    title = "Object Classification"
    var = Variation_1242
class Item_416(Item):
    name = "400"
    title = "Measurement Identifier"
    var = Variation_1029
class Item_1280(Item):
    name = "P84"
    title = "Horizontal Position in WGS-84 Coordinates"
    var = Variation_1009
class Item_956(Item):
    name = "HPR"
    title = "Horizontal Position Resolution"
    var = Variation_1055
class Item_952(Item):
    name = "HPP"
    title = "Horizontal Position Precision"
    var = Variation_1073
class Variation_1355(Compound):
    fspec_size = None
    items = [Item_1280, Item_956, Item_952]
class Item_446(Item):
    name = "600"
    title = "Horizontal Position Information"
    var = Variation_1355
class Item_911(Item):
    name = "GH"
    title = "Geometric Height (WGS-84)"
    var = Variation_302
class Item_1469(Item):
    name = "RSGH"
    title = "Geometric Height Resolution"
    var = Variation_310
class Item_1530(Item):
    name = "SDGH"
    title = "Geometric Height Precision"
    var = Variation_310
class Item_639(Item):
    name = "CI6"
    title = "Confidence Interval for Geometric Height (67%)"
    var = Variation_1112
class Item_640(Item):
    name = "CI9"
    title = "Confidence Interval for Geometric Height (95%)"
    var = Variation_1113
class Item_663(Item):
    name = "COGHHP"
    title = "Correlation of Geometric Height and Horizontal Position"
    var = Variation_1145
class Item_664(Item):
    name = "COGHHV"
    title = "Correlation of Geometric Height and Horizontal Velocity"
    var = Variation_1145
class Item_662(Item):
    name = "COGHHA"
    title = "Correlation of Geometric Height and Horizontal Acceleration"
    var = Variation_1145
class Variation_1347(Compound):
    fspec_size = None
    items = [Item_911, Item_1469, Item_1530, Item_639, Item_640, Item_663, Item_664, Item_662]
class Item_449(Item):
    name = "601"
    title = "Geometric Height Information"
    var = Variation_1347
class Item_960(Item):
    name = "HV"
    title = "Horizontal Velocity Vector"
    var = Variation_1150
class Item_1472(Item):
    name = "RSHV"
    title = "Horizontal Velocity Resolution"
    var = Variation_1149
class Item_1535(Item):
    name = "SDHV"
    title = "Horizontal Velocity Precision"
    var = Variation_1148
class Item_675(Item):
    name = "COHVHP"
    title = "Correlation of Horizontal Velocity and Horizontal Position"
    var = Variation_947
class Variation_1349(Compound):
    fspec_size = None
    items = [Item_960, Item_1472, Item_1535, Item_675]
class Item_450(Item):
    name = "602"
    title = "Horizontal Velocity Information"
    var = Variation_1349
class Item_931(Item):
    name = "HA"
    title = "Horizontal Acceleration Vector"
    var = Variation_1146
class Item_1532(Item):
    name = "SDHA"
    title = "Horizontal Acceleration Precision"
    var = Variation_1147
class Item_665(Item):
    name = "COHAHP"
    title = "Correlation of Horizontal Acceleration and Horizontal Position"
    var = Variation_945
class Item_666(Item):
    name = "COHAHV"
    title = "Correlation of Horizontal Acceleration and Horizontal Velocity"
    var = Variation_946
class Variation_1348(Compound):
    fspec_size = None
    items = [Item_931, Item_1532, Item_665, Item_666]
class Item_451(Item):
    name = "603"
    title = "Horizontal Acceleration Information"
    var = Variation_1348
class Item_1868(Item):
    name = "VV"
    title = "Vertical Velocity"
    var = Variation_303
class Item_1479(Item):
    name = "RSVV"
    title = "Vertical Velocity Resolution"
    var = Variation_275
class Item_1545(Item):
    name = "SDVV"
    title = "Vertical Velocity Precision"
    var = Variation_1077
class Item_711(Item):
    name = "COVVHP"
    title = "Correlation of Vertical Velocity and Horizontal Position"
    var = Variation_1145
class Item_712(Item):
    name = "COVVHV"
    title = "Correlation of Vertical Velocity and Horizontal Velocity"
    var = Variation_1145
class Item_710(Item):
    name = "COVVHA"
    title = "Correlation of Vertical Velocity and Horizontal Acceleration"
    var = Variation_1145
class Variation_1374(Compound):
    fspec_size = None
    items = [Item_1868, Item_1479, Item_1545, Item_711, Item_712, Item_710]
class Item_452(Item):
    name = "604"
    title = "Vertical Velocity Information"
    var = Variation_1374
class Item_1831(Item):
    name = "VA"
    title = "Vertical Acceleration"
    var = Variation_245
class Item_1478(Item):
    name = "RSVA"
    title = "Vertical Acceleration Precision"
    var = Variation_1076
class Item_705(Item):
    name = "COVAHP"
    title = "Correlation of Vertical Acceleration and Horizontal Position"
    var = Variation_1145
class Item_706(Item):
    name = "COVAHV"
    title = "Correlation of Vertical Acceleration and Horizontal Velocity"
    var = Variation_1145
class Item_704(Item):
    name = "COVAHA"
    title = "Correlation of Vertical Acceleration and Horizontal Acceleration"
    var = Variation_1145
class Variation_1373(Compound):
    fspec_size = None
    items = [Item_1831, Item_1478, Item_705, Item_706, Item_704]
class Item_454(Item):
    name = "605"
    title = "Vertical Velocity Information"
    var = Variation_1373
class Item_430(Item):
    name = "480"
    title = "Associations"
    var = Variation_1229
class Item_1388(Item):
    name = "R"
    title = "Range"
    var = Variation_300
class Item_1475(Item):
    name = "RSR"
    title = "Range Resolution"
    var = Variation_308
class Item_1537(Item):
    name = "SDR"
    title = "Range Precision"
    var = Variation_308
class Item_1462(Item):
    name = "RR"
    title = "Range Rate"
    var = Variation_301
class Item_1476(Item):
    name = "RSRR"
    title = "Range Rate Resolution"
    var = Variation_309
class Item_1541(Item):
    name = "SDRR"
    title = "Range Rate Precision"
    var = Variation_1075
class Item_1390(Item):
    name = "RA"
    title = "Range Acceleration"
    var = Variation_254
class Item_1539(Item):
    name = "SDRA"
    title = "Range Acceleration Precision"
    var = Variation_1074
class Variation_1360(Compound):
    fspec_size = None
    items = [Item_1388, Item_1475, Item_1537, Item_1462, Item_1476, Item_1541, Item_1390, Item_1539]
class Item_459(Item):
    name = "625"
    title = "Range Information"
    var = Variation_1360
class Item_809(Item):
    name = "DV"
    title = "Doppler Velocity"
    var = Variation_303
class Item_1526(Item):
    name = "SDDV"
    title = "Precision of Doppler Velocity"
    var = Variation_283
class Item_756(Item):
    name = "DA"
    title = "Doppler Acceleration"
    var = Variation_254
class Item_1525(Item):
    name = "SDDA"
    title = "Precision of Doppler Acceleration"
    var = Variation_1071
class Item_658(Item):
    name = "CODVR"
    title = "Correlation of Doppler Velocity and Range"
    var = Variation_197
class Item_660(Item):
    name = "CODVRR"
    title = "Correlation of Doppler Velocity and Range Rate"
    var = Variation_197
class Item_659(Item):
    name = "CODVRA"
    title = "Correlation of Doppler Velocity and Range Acceleration"
    var = Variation_197
class Item_655(Item):
    name = "CODAR"
    title = "Correlation of Doppler Acceleration and Range"
    var = Variation_197
class Item_657(Item):
    name = "CODARR"
    title = "Correlation of Doppler Acceleration and Range Rate"
    var = Variation_197
class Item_656(Item):
    name = "CODARA"
    title = "Correlation of Doppler Acceleration and Range Acceleration"
    var = Variation_197
class Variation_1343(Compound):
    fspec_size = None
    items = [Item_809, Item_1526, Item_756, Item_1525, Item_658, Item_660, Item_659, Item_655, Item_657, Item_656]
class Item_460(Item):
    name = "626"
    title = "Doppler Information"
    var = Variation_1343
class Item_1467(Item):
    name = "RSAZ"
    title = "Azimuth Resolution"
    var = Variation_290
class Item_1521(Item):
    name = "SDASZ"
    title = "Standard Deviation of Azimuth"
    var = Variation_290
class Item_571(Item):
    name = "AZR"
    title = "Azimuth Rate"
    var = Variation_259
class Item_1523(Item):
    name = "SDAZR"
    title = "Standard Deviation of Azimuth Rate"
    var = Variation_1070
class Item_569(Item):
    name = "AZEX"
    title = "Azimuth Extent"
    var = Variation_1059
class Variation_1336(Compound):
    fspec_size = None
    items = [Item_567, Item_1467, Item_1521, Item_571, Item_1523, Item_569]
class Item_461(Item):
    name = "627"
    title = "Azimuth Information"
    var = Variation_1336
class Item_814(Item):
    name = "EL"
    title = "Elevation"
    var = Variation_259
class Item_1468(Item):
    name = "RSEL"
    title = "Elevation Resolution"
    var = Variation_290
class Item_1527(Item):
    name = "SDEL"
    title = "Standard Deviation of Elevation"
    var = Variation_290
class Item_841(Item):
    name = "ER"
    title = "Elevation Rate"
    var = Variation_260
class Item_1529(Item):
    name = "SDER"
    title = "Standard Deviation of Elevation Rate"
    var = Variation_1072
class Item_816(Item):
    name = "ELEX"
    title = "Elevation Extent"
    var = Variation_1058
class Variation_1344(Compound):
    fspec_size = None
    items = [Item_814, Item_1468, Item_1527, Item_841, Item_1529, Item_816]
class Item_462(Item):
    name = "628"
    title = "Elevation Information"
    var = Variation_1344
class Item_793(Item):
    name = "DPP"
    title = "Direct Path - Power"
    var = Variation_192
class Item_794(Item):
    name = "DPS"
    title = "Direct Path - Signal to Noise Ratio (SNR)"
    var = Variation_192
class Item_1460(Item):
    name = "RPP"
    title = "Reflected Path - Power"
    var = Variation_918
class Item_1461(Item):
    name = "RPS"
    title = "Reflected Path - Signal to Noise Ratio (SNR)"
    var = Variation_192
class Variation_1342(Compound):
    fspec_size = None
    items = [Item_793, Item_794, Item_1460, Item_1461]
class Item_463(Item):
    name = "630"
    title = "Path Quality"
    var = Variation_1342
class Item_464(Item):
    name = "631"
    title = "Contour (Azimuth, Elevation Angle, Range Extent)"
    var = Variation_1236
class Variation_1305(Compound):
    fspec_size = None
    items = [Item_61, Item_48, Item_72, Item_87, Item_111, Item_293, Item_316, Item_329, Item_168, Item_382, Item_393, Item_416, Item_446, Item_449, Item_450, Item_451, Item_452, Item_454, Item_430, Item_459, Item_460, Item_461, Item_462, Item_463, Item_464, Item_1580]
class Item_49(Item):
    name = "000"
    title = "Report Type"
    var = Variation_170
class Item_73(Item):
    name = "015"
    title = "Service Type and Identification"
    var = Variation_1078
class Item_243(Item):
    name = "100"
    title = "Ground Station Status"
    var = Variation_1205
class Item_250(Item):
    name = "101"
    title = "Service Configuration"
    var = Variation_1210
class Item_338(Item):
    name = "200"
    title = "Operational Range"
    var = Variation_205
class Item_260(Item):
    name = "110"
    title = "Service Status"
    var = Variation_1175
class Item_269(Item):
    name = "120"
    title = "Service Statistics"
    var = Variation_1267
class Variation_1306(Compound):
    fspec_size = None
    items = [Item_61, Item_49, Item_73, Item_199, Item_243, Item_250, Item_338, Item_260, Item_269, None, None, None, Item_1421, Item_1580]
class Item_50(Item):
    name = "000"
    title = "Report Type"
    var = Variation_1057
class Item_336(Item):
    name = "200"
    title = "Message Identification"
    var = Variation_298
class Item_86(Item):
    name = "020"
    title = "Service Designator"
    var = Variation_326
class Item_248(Item):
    name = "100"
    title = "System and Service Status"
    var = Variation_1204
class Item_253(Item):
    name = "105"
    title = "System and Service Error Codes"
    var = Variation_1223
class Item_264(Item):
    name = "120"
    title = "Component Status"
    var = Variation_1241
class Item_288(Item):
    name = "140"
    title = "Service Statistics"
    var = Variation_1268
class Item_448(Item):
    name = "600"
    title = "Position of the System Reference Point"
    var = Variation_1004
class Item_456(Item):
    name = "610"
    title = "Height of the System Reference Point"
    var = Variation_249
class Variation_1307(Compound):
    fspec_size = None
    items = [Item_61, Item_50, Item_336, Item_72, Item_86, Item_199, Item_248, Item_253, Item_264, Item_288, Item_1580, Item_448, Item_456]
class Item_67(Item):
    name = "012"
    title = "Data Destination Identifier"
    var = Variation_1061
class Item_33(Item):
    name = "000"
    title = "Message Type"
    var = Variation_159
class Item_402(Item):
    name = "350"
    title = "Cluster Station/Node List"
    var = Variation_1252
class Item_359(Item):
    name = "220"
    title = "Aircraft Address"
    var = Variation_296
class Item_363(Item):
    name = "221"
    title = "Duplicate Address Reference Number (DRN)"
    var = Variation_234
class Item_155(Item):
    name = "045"
    title = "Calculated Position in WGS-84 Coordinates"
    var = Variation_1000
class Item_192(Item):
    name = "070"
    title = "Mode 3/A Code in Octal Representation"
    var = Variation_1128
class Item_159(Item):
    name = "050"
    title = "Flight Level in Binary Representation"
    var = Variation_1129
class Item_344(Item):
    name = "200"
    title = "Track Velocity in Polar Co-ordinates"
    var = Variation_988
class Item_367(Item):
    name = "230"
    title = "Transponder Capability"
    var = Variation_937
class Item_369(Item):
    name = "240"
    title = "Track Status"
    var = Variation_954
class Item_355(Item):
    name = "210"
    title = "Mode S Address List"
    var = Variation_1226
class Item_403(Item):
    name = "360"
    title = "Cluster Controller Command State"
    var = Variation_155
class Variation_1308(Compound):
    fspec_size = None
    items = [Item_61, Item_67, Item_33, Item_402, Item_359, Item_363, Item_289, Item_155, Item_192, Item_159, Item_344, Item_367, Item_369, Item_355, Item_403, None, None, None, None, None, Item_1580]
class Item_42(Item):
    name = "000"
    title = "Message Type"
    var = Variation_183
class Item_340(Item):
    name = "200"
    title = "System Configuration Reporting Period"
    var = Variation_206
class Item_394(Item):
    name = "300"
    title = "Pair Identification"
    var = Variation_1248
class Item_417(Item):
    name = "400"
    title = "Position of the System Reference Point"
    var = Variation_1003
class Item_419(Item):
    name = "405"
    title = "Height of System Reference Point"
    var = Variation_249
class Item_421(Item):
    name = "410"
    title = "Transmitter Properties"
    var = Variation_1259
class Item_423(Item):
    name = "420"
    title = "Receiver Properties"
    var = Variation_1249
class Variation_1309(Compound):
    fspec_size = None
    items = [Item_61, Item_72, Item_42, Item_289, Item_340, Item_394, Item_417, Item_419, Item_421, Item_423, Item_1580]
class Item_43(Item):
    name = "000"
    title = "Message Type"
    var = Variation_184
class Item_135(Item):
    name = "040"
    title = "Report Number"
    var = Variation_155
class Item_230(Item):
    name = "090"
    title = "Radio Channel Name"
    var = Variation_328
class Item_162(Item):
    name = "050"
    title = "Position in WGS-84 Coordinates"
    var = Variation_1006
class Item_177(Item):
    name = "060"
    title = "Position in Cartesian Coordinates"
    var = Variation_1168
class Item_187(Item):
    name = "070"
    title = "Local Bearing"
    var = Variation_277
class Item_213(Item):
    name = "080"
    title = "System Bearing"
    var = Variation_277
class Item_247(Item):
    name = "100"
    title = "Quality of Measurement"
    var = Variation_155
class Item_255(Item):
    name = "110"
    title = "Estimated Uncertainty"
    var = Variation_207
class Item_266(Item):
    name = "120"
    title = "Contributing Sensors"
    var = Variation_1222
class Item_273(Item):
    name = "130"
    title = "Conflicting Transmitter Position in WGS-84 Coordinates"
    var = Variation_1006
class Item_285(Item):
    name = "140"
    title = "Conflicting Transmitter Position in Cartesian Coordinates"
    var = Variation_1168
class Item_299(Item):
    name = "150"
    title = "Conflicting Transmitter Estimated Uncertainty"
    var = Variation_207
class Item_310(Item):
    name = "160"
    title = "Track Number"
    var = Variation_234
class Item_320(Item):
    name = "170"
    title = "Sensor Identification"
    var = Variation_155
class Item_331(Item):
    name = "180"
    title = "Signal Level"
    var = Variation_244
class Item_333(Item):
    name = "190"
    title = "Signal Quality"
    var = Variation_155
class Item_339(Item):
    name = "200"
    title = "Signal Elevation"
    var = Variation_246
class Variation_1310(Compound):
    fspec_size = None
    items = [Item_61, Item_72, Item_43, Item_107, Item_135, Item_230, Item_162, Item_177, Item_187, Item_213, Item_247, Item_255, Item_266, Item_273, Item_285, Item_299, Item_310, Item_320, Item_331, Item_333, Item_339, Item_1580]
class Item_163(Item):
    name = "050"
    title = "Sensor Identifier"
    var = Variation_1061
class Item_179(Item):
    name = "060"
    title = "Sensor Configuration and Status"
    var = Variation_1186
class Item_198(Item):
    name = "070"
    title = "Time Stamping Bias"
    var = Variation_238
class Item_212(Item):
    name = "080"
    title = "SSR / Mode S Range Gain and Bias"
    var = Variation_1080
class Item_219(Item):
    name = "081"
    title = "SSR Mode S Azimuth Bias"
    var = Variation_262
class Item_227(Item):
    name = "090"
    title = "PSR Range Gain and Bias"
    var = Variation_1033
class Item_233(Item):
    name = "091"
    title = "PSR Azimuth Bias"
    var = Variation_262
class Item_235(Item):
    name = "092"
    title = "PSR Elevation Bias"
    var = Variation_262
class Variation_1311(Compound):
    fspec_size = None
    items = [Item_61, Item_72, Item_108, Item_163, Item_179, Item_198, Item_212, Item_219, Item_227, Item_233, Item_235, None, Item_1421, Item_1580]
class Item_439(Item):
    name = "550"
    title = "Category Version Number Report"
    var = Variation_1239
class Variation_1312(Compound):
    fspec_size = None
    items = [Item_61, Item_72, Item_289, Item_439, None, Item_1580, Item_1421]
class Item_88(Item):
    name = "020"
    title = "Target Report Descriptor"
    var = Variation_1213
class Item_145(Item):
    name = "041"
    title = "Position In WGS-84 Coordinates"
    var = Variation_1001
class Item_153(Item):
    name = "042"
    title = "Position in Cartesian Coordinates"
    var = Variation_1168
class Item_313(Item):
    name = "161"
    title = "Track Number"
    var = Variation_911
class Item_323(Item):
    name = "170"
    title = "Track Status"
    var = Variation_1182
class Item_195(Item):
    name = "070"
    title = "Mode-3/A Code in Octal Representation"
    var = Variation_1137
class Item_346(Item):
    name = "202"
    title = "Calculated Track Velocity in Cartesian Coordinates"
    var = Variation_1140
class Item_244(Item):
    name = "100"
    title = "Mode C Code"
    var = Variation_1130
class Item_373(Item):
    name = "245"
    title = "Target Identification"
    var = Variation_1086
class Item_258(Item):
    name = "110"
    title = "Measured Height (Local Cartesian Coordinates)"
    var = Variation_257
class Item_252(Item):
    name = "105"
    title = "Geometric Height (WGS-84)"
    var = Variation_257
class Item_348(Item):
    name = "210"
    title = "Calculated Acceleration"
    var = Variation_930
class Item_397(Item):
    name = "310"
    title = "Pre-programmed Message"
    var = Variation_1095
class Item_788(Item):
    name = "DOP"
    title = "DOP of Position"
    var = Variation_1154
class Item_1536(Item):
    name = "SDP"
    title = "Standard Deviation of Position"
    var = Variation_1158
class Item_1531(Item):
    name = "SDH"
    title = "Standard Deviation of Geometric Height (WGS 84)"
    var = Variation_271
class Variation_1341(Compound):
    fspec_size = None
    items = [Item_788, Item_1536, Item_1531]
class Item_435(Item):
    name = "500"
    title = "Position Accuracy"
    var = Variation_1341
class Item_415(Item):
    name = "400"
    title = "Contributing Devices"
    var = Variation_1237
class Item_374(Item):
    name = "250"
    title = "BDS Register Data"
    var = Variation_1246
class Item_365(Item):
    name = "230"
    title = "Communications/ACAS Capability and Flight Status"
    var = Variation_950
class Item_380(Item):
    name = "260"
    title = "ACAS Resolution Advisory Report"
    var = Variation_327
class Item_113(Item):
    name = "030"
    title = "Warning/Error Conditions"
    var = Variation_1278
class Item_173(Item):
    name = "055"
    title = "Mode-1 Code in Octal Representation"
    var = Variation_1134
class Item_161(Item):
    name = "050"
    title = "Mode-2 Code in Octal Representation"
    var = Variation_1135
class Variation_1313(Compound):
    fspec_size = None
    items = [Item_61, Item_88, Item_289, Item_145, Item_153, Item_313, Item_323, Item_195, Item_346, Item_224, Item_244, Item_362, Item_373, Item_258, Item_252, Item_348, Item_396, Item_397, Item_435, Item_415, Item_374, Item_365, Item_380, Item_113, Item_173, Item_161, Item_1421, Item_1580]
class Item_324(Item):
    name = "170"
    title = "Track Status"
    var = Variation_1183
class Item_378(Item):
    name = "250"
    title = "Mode S MB Data"
    var = Variation_1246
class Variation_1314(Compound):
    fspec_size = None
    items = [Item_61, Item_88, Item_289, Item_145, Item_153, Item_313, Item_324, Item_195, Item_346, Item_224, Item_244, Item_362, Item_373, Item_258, Item_252, Item_348, Item_396, Item_397, Item_435, Item_415, Item_378, Item_365, Item_380, Item_113, Item_173, Item_161, Item_1421, Item_1580]
class Item_89(Item):
    name = "020"
    title = "Target Report Descriptor"
    var = Variation_1217
class Item_132(Item):
    name = "040"
    title = "Measured Position in Polar Co-ordinates"
    var = Variation_1047
class Item_194(Item):
    name = "070"
    title = "Mode-3/A Code in Octal Representation"
    var = Variation_1127
class Item_226(Item):
    name = "090"
    title = "Mode-C Code in Binary Representation"
    var = Variation_1121
class Item_276(Item):
    name = "130"
    title = "Radar Plot Characteristics"
    var = Variation_1273
class Item_291(Item):
    name = "141"
    title = "Truncated Time of Day"
    var = Variation_287
class Item_160(Item):
    name = "050"
    title = "Mode-2 Code in Octal Representation"
    var = Variation_1123
class Item_267(Item):
    name = "120"
    title = "Measured Radial Doppler Speed"
    var = Variation_201
class Item_280(Item):
    name = "131"
    title = "Received Power"
    var = Variation_193
class Item_210(Item):
    name = "080"
    title = "Mode-3/A Code Confidence Indicator"
    var = Variation_908
class Item_245(Item):
    name = "100"
    title = "Mode-C Code and Code Confidence Indicator"
    var = Variation_1118
class Item_174(Item):
    name = "060"
    title = "Mode-2 Code Confidence Indicator"
    var = Variation_908
class Item_112(Item):
    name = "030"
    title = "Warning/Error Conditions"
    var = Variation_1274
class Item_300(Item):
    name = "150"
    title = "Presence of X-Pulse"
    var = Variation_1171
class Variation_1315(Compound):
    fspec_size = None
    items = [Item_61, Item_89, Item_132, Item_194, Item_226, Item_276, Item_291, Item_160, Item_267, Item_280, Item_210, Item_245, Item_174, Item_112, Item_300, None, None, None, None, Item_1580, None]
class Item_315(Item):
    name = "161"
    title = "Track Plot Number"
    var = Variation_234
class Item_150(Item):
    name = "042"
    title = "Calculated Position in Cartesian Co-ordinates"
    var = Variation_1164
class Item_334(Item):
    name = "200"
    title = "Calculated Track Velocity in Polar Co-ordinates"
    var = Variation_988
class Item_326(Item):
    name = "170"
    title = "Track Status"
    var = Variation_1185
class Item_357(Item):
    name = "210"
    title = "Track Quality"
    var = Variation_1273
class Variation_1316(Compound):
    fspec_size = None
    items = [Item_61, Item_89, Item_315, Item_132, Item_150, Item_334, Item_194, Item_226, Item_291, Item_276, Item_280, Item_267, Item_326, Item_357, Item_160, Item_210, Item_245, Item_174, Item_112, Item_1580, None, Item_300]
class Item_90(Item):
    name = "020"
    title = "Target Report Descriptor"
    var = Variation_1218
class Item_133(Item):
    name = "040"
    title = "Measured Position in Polar Co-ordinates"
    var = Variation_1048
class Item_193(Item):
    name = "070"
    title = "Mode-3/A Code in Octal Representation"
    var = Variation_1126
class Item_223(Item):
    name = "090"
    title = "Flight Level in Binary Representation"
    var = Variation_1119
class Item_1600(Item):
    name = "SRL"
    title = "SSR Plot Runlength"
    var = Variation_226
class Item_1601(Item):
    name = "SRR"
    title = "Number of Received Replies for (M)SSR"
    var = Variation_191
class Item_1509(Item):
    name = "SAM"
    title = "Amplitude of (M)SSR Reply"
    var = Variation_193
class Item_1325(Item):
    name = "PRL"
    title = "Primary Plot Runlength"
    var = Variation_226
class Item_1284(Item):
    name = "PAM"
    title = "Amplitude of Primary Plot"
    var = Variation_193
class Item_1457(Item):
    name = "RPD"
    title = "Difference in Range Between PSR and SSR Plot"
    var = Variation_200
class Item_526(Item):
    name = "APD"
    title = "Difference in Azimuth Between PSR and SSR Plot"
    var = Variation_203
class Variation_1364(Compound):
    fspec_size = None
    items = [Item_1600, Item_1601, Item_1509, Item_1325, Item_1284, Item_1457, Item_526]
class Item_277(Item):
    name = "130"
    title = "Radar Plot Characteristics"
    var = Variation_1364
class Item_368(Item):
    name = "240"
    title = "Aircraft Identification"
    var = Variation_326
class Item_375(Item):
    name = "250"
    title = "BDS Register Data"
    var = Variation_1247
class Item_151(Item):
    name = "042"
    title = "Calculated Position in Cartesian Co-ordinates"
    var = Variation_1165
class Item_325(Item):
    name = "170"
    title = "Track Status"
    var = Variation_1184
class Item_356(Item):
    name = "210"
    title = "Track Quality"
    var = Variation_1079
class Item_115(Item):
    name = "030"
    title = "Warning/Error Conditions and Target Classification"
    var = Variation_1276
class Item_257(Item):
    name = "110"
    title = "Height Measured by a 3D Radar"
    var = Variation_880
class Item_605(Item):
    name = "CAL"
    title = "Calculated Doppler Speed"
    var = Variation_955
class Item_1417(Item):
    name = "RDS"
    title = "Raw Doppler Speed"
    var = Variation_1243
class Variation_1337(Compound):
    fspec_size = None
    items = [Item_605, Item_1417]
class Item_268(Item):
    name = "120"
    title = "Radial Doppler Speed"
    var = Variation_1337
class Item_364(Item):
    name = "230"
    title = "Communications/ACAS Capability and Flight Status"
    var = Variation_948
class Item_172(Item):
    name = "055"
    title = "Mode-1 Code in Octal Representation"
    var = Variation_1122
class Item_185(Item):
    name = "065"
    title = "Mode-1 Code Confidence Indicator"
    var = Variation_891
class Variation_1317(Compound):
    fspec_size = None
    items = [Item_61, Item_289, Item_90, Item_133, Item_193, Item_223, Item_277, Item_359, Item_368, Item_375, Item_313, Item_151, Item_334, Item_325, Item_356, Item_115, Item_210, Item_245, Item_257, Item_268, Item_364, Item_380, Item_172, Item_160, Item_185, Item_174, Item_1580, Item_1421]
class Item_379(Item):
    name = "250"
    title = "Mode S MB Data"
    var = Variation_1247
class Item_114(Item):
    name = "030"
    title = "Warning/Error Conditions and Target Classification"
    var = Variation_1275
class Variation_1318(Compound):
    fspec_size = None
    items = [Item_61, Item_289, Item_90, Item_133, Item_193, Item_223, Item_277, Item_359, Item_368, Item_379, Item_313, Item_151, Item_334, Item_325, Item_356, Item_114, Item_210, Item_245, Item_257, Item_268, Item_364, Item_380, Item_172, Item_160, Item_185, Item_174, Item_1580, Item_1421]
class Variation_1319(Compound):
    fspec_size = None
    items = [Item_61, Item_289, Item_90, Item_133, Item_193, Item_223, Item_277, Item_359, Item_368, Item_379, Item_313, Item_151, Item_334, Item_325, Item_356, Item_115, Item_210, Item_245, Item_257, Item_268, Item_364, Item_380, Item_172, Item_160, Item_185, Item_174, Item_1580, Item_1421]
class Item_91(Item):
    name = "020"
    title = "Target Report Descriptor"
    var = Variation_1219
class Item_116(Item):
    name = "030"
    title = "Warning/Error Conditions and Target Classification"
    var = Variation_1277
class Variation_1320(Compound):
    fspec_size = None
    items = [Item_61, Item_289, Item_91, Item_133, Item_193, Item_223, Item_277, Item_359, Item_368, Item_375, Item_313, Item_151, Item_334, Item_325, Item_356, Item_116, Item_210, Item_245, Item_257, Item_268, Item_364, Item_380, Item_172, Item_160, Item_185, Item_174, Item_1580, Item_1421]
class Item_62(Item):
    name = "010"
    title = "Data Source Identifier"
    var = Variation_1065
class Item_45(Item):
    name = "000"
    title = "Message Type"
    var = Variation_186
class Item_290(Item):
    name = "140"
    title = "Time of Track Information"
    var = Variation_311
class Item_147(Item):
    name = "041"
    title = "Position in WGS-84 Coordinates"
    var = Variation_1007
class Item_149(Item):
    name = "042"
    title = "Calculated Position in Cartesian Co-ordinates"
    var = Variation_1163
class Item_347(Item):
    name = "202"
    title = "Calculated Track Velocity in Cartesian Coordinates"
    var = Variation_1142
class Item_349(Item):
    name = "210"
    title = "Calculated Acceleration"
    var = Variation_931
class Item_175(Item):
    name = "060"
    title = "Mode-3/A Code in Octal Representation"
    var = Variation_900
class Item_372(Item):
    name = "245"
    title = "Target Identification"
    var = Variation_1085
class Item_1098(Item):
    name = "MB"
    title = "BDS"
    var = Variation_1230
class Item_487(Item):
    name = "ADR"
    title = "24 Bits Aircraft Address"
    var = Variation_296
class Item_689(Item):
    name = "COMACAS"
    title = "Communications/ACAS Capability and Flight Status"
    var = Variation_952
class Item_482(Item):
    name = "ACT"
    title = "Aircraft Derived Aircraft Type"
    var = Variation_314
class Item_812(Item):
    name = "ECAT"
    title = "Emitter Category"
    var = Variation_171
class Item_560(Item):
    name = "AVTECH"
    title = "Available Technologies"
    var = Variation_1139
class Variation_1352(Compound):
    fspec_size = None
    items = [Item_1098, Item_487, None, Item_689, None, None, None, Item_482, Item_812, None, Item_560]
class Item_408(Item):
    name = "380"
    title = "Mode-S / ADS-B Related Data"
    var = Variation_1352
class Item_311(Item):
    name = "161"
    title = "Track Number"
    var = Variation_873
class Item_327(Item):
    name = "170"
    title = "Track Status"
    var = Variation_1196
class Item_1330(Item):
    name = "PSR"
    title = "Age of The Last Primary Detection Used to Update the Track"
    var = Variation_217
class Item_1607(Item):
    name = "SSR"
    title = "Age of the Last Secondary Detection Used to Update the Track"
    var = Variation_217
class Item_1119(Item):
    name = "MDA"
    title = "Age of the Last Mode A Detection Used to Update the Track"
    var = Variation_217
class Item_1141(Item):
    name = "MFL"
    title = "Age of the Last Mode C Detection Used to Update the Track"
    var = Variation_217
class Item_1125(Item):
    name = "MDS"
    title = "Age of the Last Mode S Detection Used to Update the Track"
    var = Variation_217
class Item_493(Item):
    name = "ADS"
    title = "Age of the Last ADS Report Used to Update the Track"
    var = Variation_282
class Item_483(Item):
    name = "ADB"
    title = "Age of the Last ADS-B Report Used to Update the Track"
    var = Variation_217
class Item_1108(Item):
    name = "MD1"
    title = "Age of the Last Valid Mode 1 Used to Update the Track"
    var = Variation_217
class Item_1110(Item):
    name = "MD2"
    title = "Age of the Last Mode 2 Used to Update the Track"
    var = Variation_217
class Item_1063(Item):
    name = "LOP"
    title = "Age of the Last Magentic Loop Detection"
    var = Variation_217
class Item_1756(Item):
    name = "TRK"
    title = "Actual Track Age Since First Occurrence"
    var = Variation_217
class Item_1202(Item):
    name = "MUL"
    title = "Age of the Last Multilateration Detection"
    var = Variation_217
class Variation_1357(Compound):
    fspec_size = None
    items = [Item_1330, Item_1607, Item_1119, Item_1141, Item_1125, Item_493, Item_483, Item_1108, Item_1110, Item_1063, Item_1756, Item_1202]
class Item_387(Item):
    name = "290"
    title = "System Track Update Ages"
    var = Variation_1357
class Item_424(Item):
    name = "430"
    title = "Phase of Flight"
    var = Variation_168
class Item_225(Item):
    name = "090"
    title = "Measured Flight Level"
    var = Variation_248
class Item_237(Item):
    name = "093"
    title = "Calculated Track Barometric Altitude"
    var = Variation_1038
class Item_234(Item):
    name = "092"
    title = "Calculated Track Geometric Altitude"
    var = Variation_257
class Item_358(Item):
    name = "215"
    title = "Calculated Rate Of Climb/Descent"
    var = Variation_258
class Item_873(Item):
    name = "FPPSID"
    title = "FPPS Identification Tag"
    var = Variation_1064
class Item_732(Item):
    name = "CSN"
    title = "Callsign"
    var = Variation_328
class Item_994(Item):
    name = "IFPSFLIGHTID"
    title = "IFPS_FLIGHT_ID"
    var = Variation_1102
class Item_861(Item):
    name = "FLIGHTCAT"
    title = "Flight Category"
    var = Variation_986
class Item_1724(Item):
    name = "TOA"
    title = "Type of Aircraft"
    var = Variation_314
class Item_1887(Item):
    name = "WTC"
    title = "Wake Turbulence Category"
    var = Variation_189
class Item_484(Item):
    name = "ADEP"
    title = "Departure Airport"
    var = Variation_314
class Item_485(Item):
    name = "ADES"
    title = "Destination Airport"
    var = Variation_314
class Item_1491(Item):
    name = "RWY"
    title = "Runway Designation"
    var = Variation_297
class Item_614(Item):
    name = "CCP"
    title = "Current Control Position"
    var = Variation_942
class Item_1725(Item):
    name = "TOD"
    title = "Time of Departure"
    var = Variation_1262
class Item_1647(Item):
    name = "STS"
    title = "Stand Status"
    var = Variation_961
class Variation_1346(Compound):
    fspec_size = None
    items = [Item_873, Item_732, Item_994, Item_861, Item_1724, Item_1887, Item_484, Item_485, Item_1491, Item_628, Item_614, Item_1725, Item_545, Item_1647]
class Item_411(Item):
    name = "390"
    title = "Flight Plan Related Data"
    var = Variation_1346
class Item_395(Item):
    name = "300"
    title = "Vehicle Fleet Identification"
    var = Variation_157
class Item_399(Item):
    name = "310"
    title = "Pre-programmed Message"
    var = Variation_1097
class Item_525(Item):
    name = "APC"
    title = "Estimated Accuracy Of Track Position (Cartesian)"
    var = Variation_1156
class Item_530(Item):
    name = "APW"
    title = "Estimated Accuracy Of Track Position (WGS84)"
    var = Variation_997
class Item_547(Item):
    name = "ATH"
    title = "Estimated Accuracy Of Track Height"
    var = Variation_242
class Item_554(Item):
    name = "AVC"
    title = "Estimated Accuracy Of Track Velocity (Cartesian)"
    var = Variation_1157
class Item_538(Item):
    name = "ARC"
    title = "Estimated Accuracy Of Rate Of Climb / Descent"
    var = Variation_243
class Item_469(Item):
    name = "AAC"
    title = "Estimated Accuracy Of Acceleration (Cartesian)"
    var = Variation_1155
class Variation_1335(Compound):
    fspec_size = None
    items = [Item_525, Item_530, Item_547, Item_554, Item_538, Item_469]
class Item_434(Item):
    name = "500"
    title = "Estimated Accuracies"
    var = Variation_1335
class Item_445(Item):
    name = "600"
    title = "Alert Messages"
    var = Variation_923
class Item_453(Item):
    name = "605"
    title = "Tracks in Alert"
    var = Variation_1233
class Item_457(Item):
    name = "610"
    title = "Holdbar Status"
    var = Variation_1238
class Variation_1321(Compound):
    fspec_size = None
    items = [Item_62, Item_45, Item_72, Item_290, Item_147, Item_149, Item_347, Item_349, Item_175, Item_372, Item_408, Item_311, Item_327, Item_387, Item_424, Item_225, Item_237, Item_234, Item_358, Item_383, Item_411, Item_395, Item_399, Item_434, Item_445, Item_453, Item_457, Item_1580, Item_1421]
class Item_690(Item):
    name = "COMACAS"
    title = "Communications/ACAS Capability and Flight Status"
    var = Variation_953
class Variation_1353(Compound):
    fspec_size = None
    items = [Item_1098, Item_487, None, Item_690, None, None, None, Item_482, Item_812, None, Item_560]
class Item_409(Item):
    name = "380"
    title = "Mode-S / ADS-B Related Data"
    var = Variation_1353
class Item_328(Item):
    name = "170"
    title = "Track Status"
    var = Variation_1197
class Item_1331(Item):
    name = "PSR"
    title = "Age of the Last Primary Report Used to Update the Track"
    var = Variation_217
class Item_1608(Item):
    name = "SSR"
    title = "Age of the Last Secondary Report Used to Update the Track"
    var = Variation_217
class Item_1120(Item):
    name = "MDA"
    title = "Age of the Last Valid Mode A Report Used to Update the Track"
    var = Variation_217
class Item_1142(Item):
    name = "MFL"
    title = "Age of the Last Valid and Credible Mode C Used to Update the Track"
    var = Variation_217
class Item_1126(Item):
    name = "MDS"
    title = "Age of the Last Mode S Report Used to Update the Track"
    var = Variation_217
class Item_1111(Item):
    name = "MD2"
    title = "Age of the Last Valid Mode 2 Used to Update the Track"
    var = Variation_217
class Variation_1358(Compound):
    fspec_size = None
    items = [Item_1331, Item_1608, Item_1120, Item_1142, Item_1126, Item_493, Item_483, Item_1108, Item_1111, Item_1063, Item_1756, Item_1202]
class Item_388(Item):
    name = "290"
    title = "System Track Update Ages"
    var = Variation_1358
class Item_236(Item):
    name = "093"
    title = "Calculated Track Barometric Altitude"
    var = Variation_1037
class Item_860(Item):
    name = "FLIGHTCAT"
    title = "Flight Category"
    var = Variation_985
class Variation_1345(Compound):
    fspec_size = None
    items = [Item_873, Item_732, Item_994, Item_860, Item_1724, Item_1887, Item_484, Item_485, Item_1491, Item_628, Item_614, Item_1725, Item_545, Item_1647]
class Item_410(Item):
    name = "390"
    title = "Flight Plan Related Data"
    var = Variation_1345
class Variation_1322(Compound):
    fspec_size = None
    items = [Item_62, Item_45, Item_72, Item_290, Item_147, Item_149, Item_347, Item_349, Item_175, Item_372, Item_409, Item_311, Item_328, Item_388, Item_424, Item_225, Item_236, Item_234, Item_358, Item_383, Item_410, Item_395, Item_399, Item_434, Item_445, Item_453, Item_457, Item_1580, Item_1421]
class Item_63(Item):
    name = "010"
    title = "Server Identification Tag"
    var = Variation_1061
class Item_74(Item):
    name = "015"
    title = "User Number"
    var = Variation_235
class Item_78(Item):
    name = "018"
    title = "Data Source Identification Tag"
    var = Variation_1061
class Item_125(Item):
    name = "035"
    title = "Type of Message"
    var = Variation_979
class Item_93(Item):
    name = "020"
    title = "Time of ASTERIX Report Generation"
    var = Variation_311
class Item_142(Item):
    name = "040"
    title = "Track Number"
    var = Variation_235
class Item_158(Item):
    name = "050"
    title = "Composed Track Number"
    var = Variation_1215
class Item_183(Item):
    name = "060"
    title = "Track Mode 3/A"
    var = Variation_902
class Item_414(Item):
    name = "400"
    title = "Callsign"
    var = Variation_328
class Item_420(Item):
    name = "410"
    title = "Plan Number"
    var = Variation_235
class Item_422(Item):
    name = "420"
    title = "Flight Category"
    var = Variation_984
class Item_427(Item):
    name = "440"
    title = "Departure Aerodrome"
    var = Variation_314
class Item_428(Item):
    name = "450"
    title = "Destination Aerodrome"
    var = Variation_314
class Item_431(Item):
    name = "480"
    title = "Current Cleared Flight Level"
    var = Variation_280
class Item_432(Item):
    name = "490"
    title = "Current Control Position"
    var = Variation_941
class Item_425(Item):
    name = "430"
    title = "Type of Aircraft"
    var = Variation_314
class Item_426(Item):
    name = "435"
    title = "Wake Turbulence Category"
    var = Variation_189
class Item_429(Item):
    name = "460"
    title = "Allocated SSR Codes"
    var = Variation_1234
class Item_992(Item):
    name = "IFI"
    title = "IFPS FLIGHT ID"
    var = Variation_1098
class Item_1485(Item):
    name = "RVP"
    title = "RVSM & Flight Priority"
    var = Variation_916
class Item_1726(Item):
    name = "TOD"
    title = "Time of Departure / Arrival"
    var = Variation_1260
class Item_1562(Item):
    name = "SID"
    title = "Standard Instrument Departure"
    var = Variation_328
class Item_1619(Item):
    name = "STAR"
    title = "Standard Instrument Arrival"
    var = Variation_328
class Variation_1350(Compound):
    fspec_size = None
    items = [Item_992, Item_1485, Item_1418, Item_1726, Item_545, Item_1646, Item_1562, Item_1619]
class Item_437(Item):
    name = "500"
    title = "Supplementary Flight Data"
    var = Variation_1350
class Variation_1323(Compound):
    fspec_size = None
    items = [Item_63, Item_74, Item_78, Item_125, Item_93, Item_142, Item_158, Item_183, Item_414, Item_420, Item_422, Item_427, Item_428, Item_431, Item_432, Item_425, Item_426, Item_429, Item_437, None, Item_1421]
class Item_126(Item):
    name = "036"
    title = "Data Source Identifier"
    var = Variation_1061
class Item_128(Item):
    name = "037"
    title = "Data Destination Identifier"
    var = Variation_1061
class Item_32(Item):
    name = "000"
    title = "Message Type"
    var = Variation_156
class Item_51(Item):
    name = "001"
    title = "Result"
    var = Variation_939
class Item_54(Item):
    name = "005"
    title = "Mode S Address"
    var = Variation_296
class Item_75(Item):
    name = "016"
    title = "Packet Number"
    var = Variation_315
class Item_77(Item):
    name = "017"
    title = "Packet Number List"
    var = Variation_1228
class Item_79(Item):
    name = "018"
    title = "Mode S Packet Properties"
    var = Variation_874
class Item_80(Item):
    name = "019"
    title = "Mode S Packet"
    var = Variation_1279
class Item_103(Item):
    name = "028"
    title = "GICB Extraction Periodicity"
    var = Variation_268
class Item_105(Item):
    name = "030"
    title = "GICB Properties"
    var = Variation_1034
class Item_101(Item):
    name = "025"
    title = "GICB Number"
    var = Variation_315
class Item_102(Item):
    name = "027"
    title = "BDS Code"
    var = Variation_155
class Item_104(Item):
    name = "029"
    title = "GICB Extracted"
    var = Variation_327
class Item_52(Item):
    name = "002"
    title = "Time of Day"
    var = Variation_311
class Item_55(Item):
    name = "006"
    title = "Mode S Address List"
    var = Variation_1226
class Item_56(Item):
    name = "007"
    title = "Aircraft Data Link Command"
    var = Variation_1114
class Item_57(Item):
    name = "008"
    title = "Aircraft Data Link Status"
    var = Variation_1221
class Item_59(Item):
    name = "009"
    title = "Aircraft Data Link Report Request"
    var = Variation_1212
class Item_64(Item):
    name = "010"
    title = "Transponder Communications Capability"
    var = Variation_913
class Item_65(Item):
    name = "011"
    title = "Capability Report"
    var = Variation_327
class Item_69(Item):
    name = "014"
    title = "Aircraft Position in Polar Co-ordinates"
    var = Variation_1048
class Item_70(Item):
    name = "015"
    title = "Aircraft Position in Cartesian Co-ordinates"
    var = Variation_1166
class Item_82(Item):
    name = "020"
    title = "Broadcast Number"
    var = Variation_315
class Item_98(Item):
    name = "021"
    title = "Broadcast Properties"
    var = Variation_1035
class Item_99(Item):
    name = "022"
    title = "Broadcast Prefix"
    var = Variation_915
class Item_100(Item):
    name = "023"
    title = "Uplink or Downlink Broadcast"
    var = Variation_327
class Item_53(Item):
    name = "004"
    title = "II Code"
    var = Variation_1032
class Item_117(Item):
    name = "031"
    title = "Aircraft Identity"
    var = Variation_324
class Item_118(Item):
    name = "032"
    title = "Aircraft Mode A"
    var = Variation_1125
class Item_120(Item):
    name = "033"
    title = "Aircraft Height"
    var = Variation_1120
class Item_121(Item):
    name = "034"
    title = "Aircraft Speed"
    var = Variation_289
class Item_123(Item):
    name = "035"
    title = "Aircraft Heading"
    var = Variation_293
class Item_66(Item):
    name = "012"
    title = "Aircraft Coverage Quality Factor"
    var = Variation_980
class Item_68(Item):
    name = "013"
    title = "Aircraft CQF Calculation Method"
    var = Variation_155
class Variation_1324(Compound):
    fspec_size = None
    items = [Item_126, Item_128, Item_32, Item_51, Item_54, Item_75, Item_77, Item_79, Item_80, Item_103, Item_105, Item_101, Item_102, Item_104, Item_52, Item_55, Item_56, Item_57, Item_59, Item_64, Item_65, Item_69, Item_70, Item_82, Item_98, Item_99, Item_100, Item_53, Item_117, Item_118, Item_120, Item_121, Item_123, Item_66, Item_68]
class Item_1298(Item):
    name = "PNB"
    title = "Plot Number"
    var = Variation_234
class Item_1458(Item):
    name = "RPL"
    title = "Replies/Plot Link"
    var = Variation_1266
class Item_1578(Item):
    name = "SNB"
    title = "Scan Number"
    var = Variation_191
class Item_759(Item):
    name = "DATE"
    title = "Common and Plot Characteristics Date"
    var = Variation_1172
class Variation_1356(Compound):
    fspec_size = None
    items = [Item_1298, Item_1458, Item_1578, Item_759]
class Item_1339(Item):
    name = "PTL"
    title = "Plot/Track Link"
    var = Variation_892
class Item_548(Item):
    name = "ATL"
    title = "ADS-B/Track Link"
    var = Variation_1225
class Item_1761(Item):
    name = "TRN"
    title = "Turn State"
    var = Variation_204
class Item_1243(Item):
    name = "NPP"
    title = "Next Predicted Position"
    var = Variation_1031
class Item_785(Item):
    name = "DLK"
    title = "Data Link Characteristics"
    var = Variation_1265
class Item_1036(Item):
    name = "LCK"
    title = "Lockout Characteristics"
    var = Variation_1010
class Item_1680(Item):
    name = "TC"
    title = "Transition Code"
    var = Variation_919
class Item_1717(Item):
    name = "TLC"
    title = "Track Life Cycle"
    var = Variation_924
class Item_543(Item):
    name = "ASI"
    title = "Adjacent Sensor Information"
    var = Variation_1255
class Item_1697(Item):
    name = "TES"
    title = "Track Extrapolation Source"
    var = Variation_166
class Item_1000(Item):
    name = "IR"
    title = "Identity Requested"
    var = Variation_995
class Variation_1359(Compound):
    fspec_size = None
    items = [Item_1339, Item_548, Item_1761, Item_1243, Item_785, Item_1036, Item_1680, Item_1717, Item_543, Item_1697, Item_1000]
class Item_1519(Item):
    name = "SCO"
    title = "Score"
    var = Variation_191
class Item_1596(Item):
    name = "SRC"
    title = "Signal/Clutter Ratio"
    var = Variation_272
class Item_1489(Item):
    name = "RW"
    title = "Range Width"
    var = Variation_288
class Item_532(Item):
    name = "AR"
    title = "Ambiguous Range"
    var = Variation_288
class Variation_1361(Compound):
    fspec_size = None
    items = [Item_1519, Item_1596, Item_1489, Item_532]
class Item_1656(Item):
    name = "SUM"
    title = "Mode 5 Summary"
    var = Variation_1012
class Item_1299(Item):
    name = "PNO"
    title = "Mode 5 PIN / National Origin"
    var = Variation_884
class Item_820(Item):
    name = "EM1"
    title = "Extended Mode 1 Code in Octal Representation"
    var = Variation_1116
class Item_1925(Item):
    name = "XP"
    title = "X Pulse Presence"
    var = Variation_887
class Item_868(Item):
    name = "FOM"
    title = "Figure of Merit"
    var = Variation_890
class Item_1078(Item):
    name = "M2"
    title = "Mode 2 Code in Octal Representation"
    var = Variation_1117
class Variation_1366(Compound):
    fspec_size = None
    items = [Item_1656, Item_1299, Item_820, Item_1925, Item_868, Item_1078]
class Item_1657(Item):
    name = "SUM"
    title = "Mode 5 Summary"
    var = Variation_1013
class Item_1296(Item):
    name = "PMN"
    title = "PIN/ National Origin/Mission Code"
    var = Variation_881
class Item_1306(Item):
    name = "POS"
    title = "Mode 5 Reported Position"
    var = Variation_1005
class Item_898(Item):
    name = "GA"
    title = "Mode 5 GNSS-derived Altitude"
    var = Variation_876
class Item_819(Item):
    name = "EM1"
    title = "Extended Mode 1 Code in Octal Representation"
    var = Variation_1115
class Item_1731(Item):
    name = "TOS"
    title = "Time Offset for POS and GA"
    var = Variation_219
class Item_1924(Item):
    name = "XP"
    title = "X Pulse Presence"
    var = Variation_886
class Variation_1367(Compound):
    fspec_size = None
    items = [Item_1657, Item_1296, Item_1306, Item_898, Item_819, Item_1731, Item_1924]
class Item_1297(Item):
    name = "PMN"
    title = "PIN/ National Origin/Mission Code"
    var = Variation_883
class Item_867(Item):
    name = "FOM"
    title = "Figure of Merit"
    var = Variation_889
class Variation_1368(Compound):
    fspec_size = None
    items = [Item_1657, Item_1297, Item_1306, Item_898, Item_819, Item_1731, Item_1924, Item_867]
class Item_596(Item):
    name = "BPS"
    title = "Barometric Pressure Setting"
    var = Variation_896
class Item_1553(Item):
    name = "SH"
    title = "Selected Heading"
    var = Variation_899
class Item_1211(Item):
    name = "NAV"
    title = "Navigation Mode"
    var = Variation_928
class Item_902(Item):
    name = "GAO"
    title = "GPS Antenna Offset"
    var = Variation_155
class Item_1551(Item):
    name = "SGV"
    title = "Surface Ground Vector"
    var = Variation_1214
class Item_1616(Item):
    name = "STA"
    title = "Aircraft Status"
    var = Variation_1187
class Item_1721(Item):
    name = "TNH"
    title = "True North Heading"
    var = Variation_293
class Item_1137(Item):
    name = "MES"
    title = "Military Extended Squitter"
    var = Variation_1366
class Variation_1376(Compound):
    fspec_size = 1
    items = [Item_596, Item_1553, Item_1211, Item_902, Item_1551, Item_1616, Item_1721, Item_1137]
class Item_1212(Item):
    name = "NAV"
    title = "Navigation Mode"
    var = Variation_929
class Item_1617(Item):
    name = "STA"
    title = "Aircraft Status"
    var = Variation_1188
class Variation_1377(Compound):
    fspec_size = 1
    items = [Item_596, Item_1553, Item_1212, Item_902, Item_1551, Item_1617, Item_1721, Item_1137]
class Item_740(Item):
    name = "CST"
    title = "Contributing Sensors With Local Tracknumbers"
    var = Variation_1254
class Item_734(Item):
    name = "CSN"
    title = "Contributing Sensors No Local Tracknumbers"
    var = Variation_1253
class Item_1792(Item):
    name = "TVS"
    title = "Calculated Track Velocity Relative to System Reference Point"
    var = Variation_1140
class Item_1648(Item):
    name = "STS"
    title = "Supplementary Track Status"
    var = Variation_1190
class Variation_1378(Compound):
    fspec_size = 1
    items = [Item_740, Item_734, Item_1792, Item_1648]
class Item_1117(Item):
    name = "MD5"
    title = "Mode 5 Reports"
    var = Variation_1367
class Item_1087(Item):
    name = "M5N"
    title = "Mode 5 Reports, New Format"
    var = Variation_1368
class Item_1085(Item):
    name = "M4E"
    title = "Extended Mode 4 Report"
    var = Variation_1176
class Item_1456(Item):
    name = "RPC"
    title = "Radar Plot Characteristics"
    var = Variation_1361
class Item_843(Item):
    name = "ERR"
    title = "Extended Range Report"
    var = Variation_312
class Item_1482(Item):
    name = "RTC"
    title = "Radar Track Characteristics"
    var = Variation_1359
class Item_717(Item):
    name = "CPC"
    title = "Common and Plot Characteristics"
    var = Variation_1356
class Variation_1379(Compound):
    fspec_size = 1
    items = [Item_1117, Item_1087, Item_1085, Item_1456, Item_843, Item_1482, Item_717]

# Uap set
class Uap_0(UapSingle):
    var = Variation_1282
class Uap_1(UapSingle):
    var = Variation_1283
class Uap_2(UapSingle):
    var = Variation_1284
class Uap_3(UapSingle):
    var = Variation_1285
class Uap_4(UapSingle):
    var = Variation_1286
class Uap_5(UapSingle):
    var = Variation_1287
class Uap_6(UapSingle):
    var = Variation_1288
class Uap_7(UapSingle):
    var = Variation_1289
class Uap_8(UapSingle):
    var = Variation_1290
class Uap_9(UapSingle):
    var = Variation_1291
class Uap_10(UapSingle):
    var = Variation_1292
class Uap_11(UapSingle):
    var = Variation_1293
class Uap_12(UapSingle):
    var = Variation_1294
class Uap_13(UapSingle):
    var = Variation_1295
class Uap_14(UapSingle):
    var = Variation_1296
class Uap_15(UapSingle):
    var = Variation_1297
class Uap_16(UapSingle):
    var = Variation_1298
class Uap_17(UapSingle):
    var = Variation_1299
class Uap_18(UapSingle):
    var = Variation_1300
class Uap_19(UapSingle):
    var = Variation_1301
class Uap_20(UapSingle):
    var = Variation_1302
class Uap_21(UapSingle):
    var = Variation_1303
class Uap_22(UapSingle):
    var = Variation_1304
class Uap_23(UapSingle):
    var = Variation_1305
class Uap_24(UapSingle):
    var = Variation_1306
class Uap_25(UapSingle):
    var = Variation_1307
class Uap_26(UapSingle):
    var = Variation_1308
class Uap_27(UapSingle):
    var = Variation_1309
class Uap_28(UapSingle):
    var = Variation_1310
class Uap_29(UapSingle):
    var = Variation_1311
class Uap_30(UapSingle):
    var = Variation_1312
class Uap_31(UapSingle):
    var = Variation_1313
class Uap_32(UapSingle):
    var = Variation_1314
class Uap_33(UapSingle):
    var = Variation_1317
class Uap_34(UapSingle):
    var = Variation_1318
class Uap_35(UapSingle):
    var = Variation_1319
class Uap_36(UapSingle):
    var = Variation_1320
class Uap_37(UapSingle):
    var = Variation_1321
class Uap_38(UapSingle):
    var = Variation_1322
class Uap_39(UapSingle):
    var = Variation_1323
class Uap_40(UapSingle):
    var = Variation_1324
class Uap_41(UapMultiple):
    lst = [("plot", Variation_1315), ("track", Variation_1316)]
    selector = (["020", "TYP"], {0: "plot", 1: "track"})

# Spec set
class AstSpec_0(AstCat):
    uap = Uap_0
class AstSpec_1(AstCat):
    uap = Uap_1
class AstSpec_2(AstCat):
    uap = Uap_2
class AstSpec_3(AstCat):
    uap = Uap_3
class AstSpec_4(AstCat):
    uap = Uap_4
class AstSpec_5(AstCat):
    uap = Uap_5
class AstSpec_6(AstCat):
    uap = Uap_6
class AstSpec_7(AstCat):
    uap = Uap_7
class AstSpec_8(AstCat):
    uap = Uap_8
class AstSpec_9(AstCat):
    uap = Uap_9
class AstSpec_10(AstCat):
    uap = Uap_10
class AstSpec_11(AstCat):
    uap = Uap_11
class AstSpec_12(AstCat):
    uap = Uap_12
class AstSpec_13(AstCat):
    uap = Uap_13
class AstSpec_14(AstCat):
    uap = Uap_14
class AstSpec_15(AstCat):
    uap = Uap_15
class AstSpec_16(AstCat):
    uap = Uap_16
class AstSpec_17(AstCat):
    uap = Uap_17
class AstSpec_18(AstCat):
    uap = Uap_18
class AstSpec_19(AstCat):
    uap = Uap_19
class AstSpec_20(AstCat):
    uap = Uap_20
class AstSpec_21(AstCat):
    uap = Uap_21
class AstSpec_22(AstCat):
    uap = Uap_22
class AstSpec_23(AstCat):
    uap = Uap_23
class AstSpec_24(AstCat):
    uap = Uap_24
class AstSpec_25(AstCat):
    uap = Uap_25
class AstSpec_26(AstCat):
    uap = Uap_26
class AstSpec_27(AstCat):
    uap = Uap_27
class AstSpec_28(AstCat):
    uap = Uap_28
class AstSpec_29(AstCat):
    uap = Uap_29
class AstSpec_30(AstCat):
    uap = Uap_30
class AstSpec_31(AstCat):
    uap = Uap_31
class AstSpec_32(AstCat):
    uap = Uap_32
class AstSpec_33(AstCat):
    uap = Uap_33
class AstSpec_34(AstCat):
    uap = Uap_34
class AstSpec_35(AstCat):
    uap = Uap_35
class AstSpec_36(AstCat):
    uap = Uap_36
class AstSpec_37(AstCat):
    uap = Uap_37
class AstSpec_38(AstCat):
    uap = Uap_38
class AstSpec_39(AstCat):
    uap = Uap_39
class AstSpec_40(AstCat):
    uap = Uap_40
class AstSpec_41(AstCat):
    uap = Uap_41
class AstSpec_42(AstRef):
    var = Variation_1376
class AstSpec_43(AstRef):
    var = Variation_1377
class AstSpec_44(AstRef):
    var = Variation_1378
class AstSpec_45(AstRef):
    var = Variation_1379

# Asterix set
class Asterix_0(Asterix):
    cat = 1
    edition = (1, 2)
    spec = AstSpec_41
class Asterix_1(Asterix):
    cat = 1
    edition = (1, 3)
    spec = AstSpec_41
class Asterix_2(Asterix):
    cat = 1
    edition = (1, 4)
    spec = AstSpec_41
class Asterix_3(Asterix):
    cat = 2
    edition = (1, 0)
    spec = AstSpec_16
class Asterix_4(Asterix):
    cat = 2
    edition = (1, 1)
    spec = AstSpec_16
class Asterix_5(Asterix):
    cat = 4
    edition = (1, 12)
    spec = AstSpec_12
class Asterix_6(Asterix):
    cat = 8
    edition = (1, 2)
    spec = AstSpec_17
class Asterix_7(Asterix):
    cat = 8
    edition = (1, 3)
    spec = AstSpec_17
class Asterix_8(Asterix):
    cat = 9
    edition = (2, 1)
    spec = AstSpec_22
class Asterix_9(Asterix):
    cat = 10
    edition = (1, 1)
    spec = AstSpec_20
class Asterix_10(Asterix):
    cat = 11
    edition = (1, 2)
    spec = AstSpec_37
class Asterix_11(Asterix):
    cat = 11
    edition = (1, 3)
    spec = AstSpec_38
class Asterix_12(Asterix):
    cat = 15
    edition = (1, 0)
    spec = AstSpec_23
class Asterix_13(Asterix):
    cat = 15
    edition = (1, 1)
    spec = AstSpec_23
class Asterix_14(Asterix):
    cat = 16
    edition = (1, 0)
    spec = AstSpec_27
class Asterix_15(Asterix):
    cat = 17
    edition = (1, 3)
    spec = AstSpec_26
class Asterix_16(Asterix):
    cat = 18
    edition = (1, 7)
    spec = AstSpec_40
class Asterix_17(Asterix):
    cat = 19
    edition = (1, 3)
    spec = AstSpec_19
class Asterix_18(Asterix):
    cat = 20
    edition = (1, 9)
    spec = AstSpec_32
class Asterix_19(Asterix):
    cat = 20
    edition = (1, 10)
    spec = AstSpec_31
class Asterix_20(Asterix):
    cat = 21
    edition = (0, 23)
    spec = AstSpec_0
class Asterix_21(Asterix):
    cat = 21
    edition = (0, 24)
    spec = AstSpec_1
class Asterix_22(Asterix):
    cat = 21
    edition = (0, 25)
    spec = AstSpec_1
class Asterix_23(Asterix):
    cat = 21
    edition = (0, 26)
    spec = AstSpec_2
class Asterix_24(Asterix):
    cat = 21
    edition = (1, 4)
    spec = AstSpec_42
class Asterix_25(Asterix):
    cat = 21
    edition = (1, 5)
    spec = AstSpec_43
class Asterix_26(Asterix):
    cat = 21
    edition = (2, 1)
    spec = AstSpec_6
class Asterix_27(Asterix):
    cat = 21
    edition = (2, 2)
    spec = AstSpec_7
class Asterix_28(Asterix):
    cat = 21
    edition = (2, 3)
    spec = AstSpec_4
class Asterix_29(Asterix):
    cat = 21
    edition = (2, 4)
    spec = AstSpec_4
class Asterix_30(Asterix):
    cat = 21
    edition = (2, 5)
    spec = AstSpec_3
class Asterix_31(Asterix):
    cat = 21
    edition = (2, 6)
    spec = AstSpec_5
class Asterix_32(Asterix):
    cat = 23
    edition = (1, 2)
    spec = AstSpec_24
class Asterix_33(Asterix):
    cat = 23
    edition = (1, 3)
    spec = AstSpec_24
class Asterix_34(Asterix):
    cat = 25
    edition = (1, 5)
    spec = AstSpec_25
class Asterix_35(Asterix):
    cat = 32
    edition = (1, 1)
    spec = AstSpec_39
class Asterix_36(Asterix):
    cat = 34
    edition = (1, 27)
    spec = AstSpec_13
class Asterix_37(Asterix):
    cat = 34
    edition = (1, 28)
    spec = AstSpec_14
class Asterix_38(Asterix):
    cat = 34
    edition = (1, 29)
    spec = AstSpec_15
class Asterix_39(Asterix):
    cat = 48
    edition = (1, 11)
    spec = AstSpec_45
class Asterix_40(Asterix):
    cat = 48
    edition = (1, 27)
    spec = AstSpec_34
class Asterix_41(Asterix):
    cat = 48
    edition = (1, 28)
    spec = AstSpec_35
class Asterix_42(Asterix):
    cat = 48
    edition = (1, 29)
    spec = AstSpec_33
class Asterix_43(Asterix):
    cat = 48
    edition = (1, 30)
    spec = AstSpec_33
class Asterix_44(Asterix):
    cat = 48
    edition = (1, 31)
    spec = AstSpec_36
class Asterix_45(Asterix):
    cat = 62
    edition = (1, 2)
    spec = AstSpec_44
class Asterix_46(Asterix):
    cat = 62
    edition = (1, 17)
    spec = AstSpec_8
class Asterix_47(Asterix):
    cat = 62
    edition = (1, 18)
    spec = AstSpec_11
class Asterix_48(Asterix):
    cat = 62
    edition = (1, 19)
    spec = AstSpec_10
class Asterix_49(Asterix):
    cat = 62
    edition = (1, 20)
    spec = AstSpec_9
class Asterix_50(Asterix):
    cat = 63
    edition = (1, 6)
    spec = AstSpec_29
class Asterix_51(Asterix):
    cat = 65
    edition = (1, 4)
    spec = AstSpec_18
class Asterix_52(Asterix):
    cat = 65
    edition = (1, 5)
    spec = AstSpec_18
class Asterix_53(Asterix):
    cat = 65
    edition = (1, 6)
    spec = AstSpec_18
class Asterix_54(Asterix):
    cat = 205
    edition = (1, 0)
    spec = AstSpec_28
class Asterix_55(Asterix):
    cat = 240
    edition = (1, 3)
    spec = AstSpec_21
class Asterix_56(Asterix):
    cat = 247
    edition = (1, 2)
    spec = AstSpec_30
class Asterix_57(Asterix):
    cat = 247
    edition = (1, 3)
    spec = AstSpec_30

# Aliases
Cat_001_1_2: TypeAlias = AstSpec_41
Cat_001_1_3: TypeAlias = AstSpec_41
Cat_001_1_4: TypeAlias = AstSpec_41
Cat_002_1_0: TypeAlias = AstSpec_16
Cat_002_1_1: TypeAlias = AstSpec_16
Cat_004_1_12: TypeAlias = AstSpec_12
Cat_008_1_2: TypeAlias = AstSpec_17
Cat_008_1_3: TypeAlias = AstSpec_17
Cat_009_2_1: TypeAlias = AstSpec_22
Cat_010_1_1: TypeAlias = AstSpec_20
Cat_011_1_2: TypeAlias = AstSpec_37
Cat_011_1_3: TypeAlias = AstSpec_38
Cat_015_1_0: TypeAlias = AstSpec_23
Cat_015_1_1: TypeAlias = AstSpec_23
Cat_016_1_0: TypeAlias = AstSpec_27
Cat_017_1_3: TypeAlias = AstSpec_26
Cat_018_1_7: TypeAlias = AstSpec_40
Cat_019_1_3: TypeAlias = AstSpec_19
Cat_020_1_9: TypeAlias = AstSpec_32
Cat_020_1_10: TypeAlias = AstSpec_31
Cat_021_0_23: TypeAlias = AstSpec_0
Cat_021_0_24: TypeAlias = AstSpec_1
Cat_021_0_25: TypeAlias = AstSpec_1
Cat_021_0_26: TypeAlias = AstSpec_2
Ref_021_1_4: TypeAlias = AstSpec_42
Ref_021_1_5: TypeAlias = AstSpec_43
Cat_021_2_1: TypeAlias = AstSpec_6
Cat_021_2_2: TypeAlias = AstSpec_7
Cat_021_2_3: TypeAlias = AstSpec_4
Cat_021_2_4: TypeAlias = AstSpec_4
Cat_021_2_5: TypeAlias = AstSpec_3
Cat_021_2_6: TypeAlias = AstSpec_5
Cat_023_1_2: TypeAlias = AstSpec_24
Cat_023_1_3: TypeAlias = AstSpec_24
Cat_025_1_5: TypeAlias = AstSpec_25
Cat_032_1_1: TypeAlias = AstSpec_39
Cat_034_1_27: TypeAlias = AstSpec_13
Cat_034_1_28: TypeAlias = AstSpec_14
Cat_034_1_29: TypeAlias = AstSpec_15
Ref_048_1_11: TypeAlias = AstSpec_45
Cat_048_1_27: TypeAlias = AstSpec_34
Cat_048_1_28: TypeAlias = AstSpec_35
Cat_048_1_29: TypeAlias = AstSpec_33
Cat_048_1_30: TypeAlias = AstSpec_33
Cat_048_1_31: TypeAlias = AstSpec_36
Ref_062_1_2: TypeAlias = AstSpec_44
Cat_062_1_17: TypeAlias = AstSpec_8
Cat_062_1_18: TypeAlias = AstSpec_11
Cat_062_1_19: TypeAlias = AstSpec_10
Cat_062_1_20: TypeAlias = AstSpec_9
Cat_063_1_6: TypeAlias = AstSpec_29
Cat_065_1_4: TypeAlias = AstSpec_18
Cat_065_1_5: TypeAlias = AstSpec_18
Cat_065_1_6: TypeAlias = AstSpec_18
Cat_205_1_0: TypeAlias = AstSpec_28
Cat_240_1_3: TypeAlias = AstSpec_21
Cat_247_1_2: TypeAlias = AstSpec_30
Cat_247_1_3: TypeAlias = AstSpec_30

# Manifest
manifest = [Cat_001_1_2, Cat_001_1_3, Cat_001_1_4, Cat_002_1_0, Cat_002_1_1, Cat_004_1_12, Cat_008_1_2, Cat_008_1_3, Cat_009_2_1, Cat_010_1_1, Cat_011_1_2, Cat_011_1_3, Cat_015_1_0, Cat_015_1_1, Cat_016_1_0, Cat_017_1_3, Cat_018_1_7, Cat_019_1_3, Cat_020_1_9, Cat_020_1_10, Cat_021_0_23, Cat_021_0_24, Cat_021_0_25, Cat_021_0_26, Ref_021_1_4, Ref_021_1_5, Cat_021_2_1, Cat_021_2_2, Cat_021_2_3, Cat_021_2_4, Cat_021_2_5, Cat_021_2_6, Cat_023_1_2, Cat_023_1_3, Cat_025_1_5, Cat_032_1_1, Cat_034_1_27, Cat_034_1_28, Cat_034_1_29, Ref_048_1_11, Cat_048_1_27, Cat_048_1_28, Cat_048_1_29, Cat_048_1_30, Cat_048_1_31, Ref_062_1_2, Cat_062_1_17, Cat_062_1_18, Cat_062_1_19, Cat_062_1_20, Cat_063_1_6, Cat_065_1_4, Cat_065_1_5, Cat_065_1_6, Cat_205_1_0, Cat_240_1_3, Cat_247_1_2, Cat_247_1_3]
