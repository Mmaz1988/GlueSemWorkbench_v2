package glueSemantics.linearLogic;

import java.util.HashSet;
import java.util.Set;

public class Category {

    public Category left;
    public Category right;
    public String category;
    public Boolean atomic;
    public Set<Integer> discharges;

    public Category(String category, Set<Integer> discharges)
    {
        this.category = category;
        this.discharges = discharges;
        this.atomic = true;

    }

    public Category(Category left, Category right, Set<Integer> discharges)
    {
        this.left = left;
        this.right = right;
        this.discharges = discharges;
        this.atomic = false;
        this.category = this.toString();
    }

    @Override
    public String toString() {

        if (atomic)
        {
            return category;
        } else
        {
            return left.toString() + " -o " + right.toString();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        Category c = (Category) obj;
        return category.equals(c.category);
    }

    @Override
    public int hashCode() {
        return category.hashCode();
    }



    public Set<Integer> dischargeRequirements() {

        if (atomic)
        {
            return new HashSet<>(discharges);
        } else
        {
            Set<Integer> requirements = new HashSet<>();
            requirements.addAll(left.dischargeRequirements());
            requirements.addAll(right.dischargeRequirements());
            return requirements;
        }
    }



}
