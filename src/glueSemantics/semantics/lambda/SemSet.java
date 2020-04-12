package glueSemantics.semantics.lambda;

import glueSemantics.semantics.SemanticRepresentation;
import prover.ProverException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class SemSet extends SemanticExpression {

    private List<SemanticRepresentation> members;

    public SemSet(List<SemanticRepresentation> members,SemType type)
    {

        this.members = members;
        setType(type);

        if (!checkMembers())
        {
            System.out.println("Warning: An invalid set has been created!");
        }

    }

    public Boolean checkMembers()
    {
        for (SemanticRepresentation m : members)
        {
            if (!m.getType().equals(getType()))
            {
                return false;
            }
        }
        return true;
    }
    
    public void addMember(SemanticRepresentation member)
    {
        if (member.getType().equals(getType()))
        {
            members.add(member);
        }
        else
        {
            System.out.println("Element " + member.toString() + " cannot be added to set due to incompatible types");
        }
    }

    @Override
    public SemanticRepresentation betaReduce() {
        List<SemanticRepresentation> out = new ArrayList<>();
        for (SemanticRepresentation m : members)
        {
            try {
                SemanticRepresentation n = m.betaReduce();
                out.add(n);
            } catch (ProverException e) {
                e.printStackTrace();
            }
        }
        return new SemSet(out,out.get(0).getType());
    }

    @Override
    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg) {

    return null;
    }

    @Override
    public SemanticExpression clone() {
        return null;
    }

    @Override
    public Set<SemAtom> findBoundVariables() {
        Set<SemAtom> out = new HashSet<>();
        for (SemanticRepresentation sr : members)
        {
            out.addAll(sr.findBoundVariables());
        }
        return out;
    }

    @Override
    public String toString() {
        return "{" + members.stream().map(Object::toString).collect(Collectors.joining(", ")) + '}';
    }

    public List<SemanticRepresentation> getMembers() {
        return members;
    }

    public void setMembers(List<SemanticRepresentation> members) {
        this.members = members;
    }
}
