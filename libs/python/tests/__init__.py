# asterix test support functions

from typing import *

from asterix.base import *

def populate_record(val: bool, Rec: Type[Record]) -> Record:
    """Create record with all items set to zero/one."""

    def mk_var(Var: Any) -> Any:
        if issubclass(Var, Element):
            return Var.create(-1 if val else 0)
        elif issubclass(Var, Group):
            items = [mk_item(i) for (i, _o) in Var.cv_items_list]
            return Var.create(tuple(items))
        elif issubclass(Var, Extended):
            Groups = Var.cv_items_list
            def mk_ext_item(Arg: Any) -> Any:
                if Arg is None:
                    return None
                (I, _O) = Arg
                return mk_item(I)
            def mk_ext_group(G: Any) -> Any:
                return tuple([mk_ext_item(Arg) for Arg in G])
            groups = tuple([mk_ext_group(G) for G in Groups])
            return Var.create(groups)
        elif issubclass(Var, Repetitive):
            rep_var = mk_var(Var.cv_variation)
            return Var.create([rep_var] * 10)
        elif issubclass(Var, Explicit):
            return Var.create(b'')
        elif issubclass(Var, Compound):
            d = {key: mk_nonspare(Nsp) for (key, Nsp) in Var.cv_items_dict.items()}
            return Var.create(d)
        else:
            raise Exception('Unexpected', Var)

    def mk_rulevar(Rv: Any) -> Any:
        if issubclass(Rv, RuleVariationContextFree):
            return Rv.create(mk_var(Rv.cv_variation))
        elif issubclass(Rv, RuleVariationDependent):
            return Rv.create(mk_var(Rv.cv_default_variation))
        else:
            raise Exception('Unexpected', Rv)

    def mk_nonspare(Nsp: Any) -> Any:
        return Nsp.create(mk_rulevar(Nsp.cv_rule))

    def mk_item(I: Any) -> Any:
        if issubclass(I, Item):
            return I.create(mk_nonspare(I.cv_non_spare))
        elif issubclass(I, Spare):
            return I.create(0)
        else:
            raise Exception('Unexpected', I)

    d = {key: mk_nonspare(Nsp) for (key, Nsp) in Rec.cv_items_dict.items()}
    return Rec.create(d) # type: ignore

def sample_records(Spec: AstCat) -> List[Tuple[Optional[str], Record, Record]]:
    """Generate sample records from the given spec.
    A result is a list (in case of multiple UAPs) of element, where each element
    is a tuple (Optional[uap name], record with zeros, record with ones in each item).
    """

    Uap = Spec.cv_uap

    result: List[Tuple[Optional[str], Record, Record]] = []

    def add_sample(name: Optional[str], Rec: Any) -> None:
        r1 = populate_record(False, Rec)
        r2 = populate_record(True, Rec)
        result.append((name, r1, r2))

    if issubclass(Uap, UapSingle):
        add_sample(None, Uap.cv_record)

    elif issubclass(Uap, UapMultiple):
        for name, Rec in Uap.cv_uaps.items():
            add_sample(name, Rec)
    else:
        raise Exception('Unexpected', Uap)

    return result

