# Example script with errors
#
# Misspelled item name, SA instead of SAC (2x)
# get_item('010') result is not checked
# if the item is actually present, which might result in runtime error

from asterix.generated import *

Spec = Cat_008_1_3
rec = Spec.cv_record.create({'010': (('SA', 1), ('SIC', 2))})
i010 = rec.get_item('010')
print(i010.variation.get_item('SA').as_uint())
