package glueSemantics.linearLogic;

public class Category {

    public Category left;
    public Category right;
    public String category;
    public Boolean atomic;

    public Category(String category)
    {
        this.category = category;
        this.atomic = true;
    }

    public Category(Category left, Category right)
    {
        this.left = left;
        this.right = right;
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
}
