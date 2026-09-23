package glueSemantics.linearLogic;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
            return atomToUTF8(category);
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


    /** An atom of a category written as text: anything but blanks and brackets. */
    private static final Pattern ATOM = Pattern.compile("[^\\s()]+");

    /**
     * Pretty-prints a category given only as text ("(g_e -o f_t) -o h_t"), as {@link #toUTF8()}
     * does a parsed one: "-o" becomes "⊸" and each atom is printed by {@link #atomToUTF8}. For
     * graph nodes that carry only their category's string.
     */
    public static String toUTF8(String category) {
        Matcher m = ATOM.matcher(category);
        StringBuilder sb = new StringBuilder();
        while (m.find()) {
            String atom = m.group();
            m.appendReplacement(sb, Matcher.quoteReplacement(atom.equals("-o") ? "⊸" : atomToUTF8(atom)));
        }
        m.appendTail(sb);
        return sb.toString();
    }

    /** An atom with a known semantic type after its one "_" gets it as a subscript (g_e → gₑ);
     *  anything else stays as written (an unknown type used to print as "null"). */
    private static String atomToUTF8(String atom) {
        String[] split = atom.split("_");
        String subscript = split.length == 2 ? semTypesToUTF8.get(split[1]) : null;
        return subscript == null ? atom : split[0] + subscript;
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
