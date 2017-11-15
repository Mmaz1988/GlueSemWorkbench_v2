package gluePaP.semantics;

import Prover.SemEquality;

import java.util.LinkedHashSet;

import static gluePaP.semantics.SemAtom.SemSort.CONST;
import static gluePaP.semantics.SemAtom.SemSort.VAR;

public class SemAtom extends SemRepresentation {
    private String name;
    //private String value;
    //private AtomicType atomicType;
    private SemSort sort;



    public SemAtom(SemSort sort, String name, SemType.AtomicType type)
    {
        this.name = name;
        this.sort = sort;
        this.setType(type);
    }

    public SemAtom(SemSort sort, String name, SemType type)
    {
        this.name = name;
        this.sort = sort;
        this.setType(type);
    }


    public enum SemSort {
        VAR, CONST
    }

/*    public SemType getType() {
        return new SemType(this.atomicType);
    }*/


    // If this method is called, something must have gone wrong, return false.
    @Override
    public boolean applyTo(SemAtom var, SemRepresentation arg) {
        return false;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public SemSort getSort() {
        return sort;
    }

    public void setSort(SemSort sort) {
        this.sort = sort;
    }

    @Override
    public String toString() {
        return name + "_" + getType();
    }

    public LinkedHashSet<SemEquality> checkCompatibility(SemRepresentation term) {
        if (term instanceof SemAtom) {
            if (this.getType().equals(VAR)) {
                {
                    if (((SemAtom) term).getSort().equals(VAR)) {
                        // List<Equality> emptyList = Collections.emptyList();
                        // return emptyList;
                        // Not possible to unify two variables?
                        return null;
                    } else if ( ((SemAtom) term).getSort().equals(CONST) &&
                            this.getSort().equals(((SemAtom) term).getSort())) {
                        {
                            LinkedHashSet<SemEquality> newEq = new LinkedHashSet<>();
                            newEq.add(new SemEquality(this, (SemAtom) term));
                            return newEq;
                        }
                    }
                }
            } else if (this.getSort().equals(CONST)) {
                if (((SemAtom) term).getSort().equals(CONST)) {
                    if (this.getName().equals(((SemAtom) term).getName()) &&
                            this.getType().equals(((SemAtom) term).getSort())) {
                        LinkedHashSet<SemEquality> emptyList = new LinkedHashSet();
                        return emptyList;
                    } else
                    {
                        return null;
                    }
                } else if (((SemAtom) term).getSort().equals(VAR) &&
                        this.getType().equals(((SemAtom) term).getSort()))
                {
                    LinkedHashSet<SemEquality> newEq = new LinkedHashSet<>();
                    newEq.add(new SemEquality((SemAtom) term,  this));
                    return newEq;
                }


            }
        }
        return null;
    }
}
