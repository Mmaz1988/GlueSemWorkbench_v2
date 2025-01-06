package glueSemantics.linearLogic;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class Category {

    // "e","t","v","i","s","d","p","x","temp","a"
    // map semTypes to subscript versions
    public static HashMap<String, String> semTypesToUTF8 = new HashMap<String, String>() {{
        put("e", "ₑ"); // subscript e
        put("t", "ₜ"); // subscript t
        put("v", "ᵥ"); // subscript v
        put("i", "ᵢ"); // subscript i
        put("s", "ₛ"); // subscript s
        put("d", "ₐ"); // subscript d (note: Unicode uses a for this)
        put("p", "ₚ"); // subscript p
        put("x", "ₓ"); // subscript x
        put("temp", "ₜₑₘₚ"); // subscript temp (composed of individual subscripts)
        put("a", "ₐ"); // subscript a
    }};




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

    public String toUTF8() {

        if (atomic)
        {
            //Split into constant and type (at _)
            String[] split = category.split("_");
            if (split.length == 2)
            {
                return split[0] + semTypesToUTF8.get(split[1]);
            } else
            {
                return category;
            }
        } else
        {
            String left = this.left.toUTF8();

            if (!this.left.atomic)
            {
                left = "(" + left + ")";
            }

            return left + " ⊸ " + this.right.toUTF8();
        }
    }

    public String toLateX() {

            if (atomic)
            {
                    return category;
            } else
            {
                String left = this.left.toLateX();

                if (!this.left.atomic)
                {
                    left = "(" + left + ")";
                }

                return left + " \\multimap " + this.right.toLateX();
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


    public int getSize() {
        if (atomic)
        {
            return 1;
        } else
        {
            return left.getSize() + right.getSize();
        }
    }


    public String isModifier()
    {
        if (this.left == null)
        {
            return null;
        } else {
            if (this.left.toString().equals(this.right.toString()))
            {
                return left.toString();
            } else {
                return this.right.isModifier();
            }
        }

    }
}
