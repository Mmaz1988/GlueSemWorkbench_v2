package gluePaP.linearLogic;


import java.util.List;

public class Sequent {
    private List<Atom> lhs;
    private Atom rhs;

    public Sequent(List<Atom> lhs) {
        this.lhs = lhs;
    }
}
