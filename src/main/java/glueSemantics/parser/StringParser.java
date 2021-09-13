package glueSemantics.parser;

import glueSemantics.semantics.lambda.SemType;

public class StringParser {


    private int pos = 0;

    public StringParser()
    {}

    public SemType callParser(String input)
    {
        pos = 0;
        return typeParser(input);
    }

    public SemType typeParser(String input)
    {
        {
            while(pos < input.length())
            {
                char c = input.charAt(pos);
                if (c == 'e')
                {
                    return  new SemType(SemType.AtomicType.E);
                }
                if (c == 't')
                {

                    return new SemType(SemType.AtomicType.T);
                }

                if (c == 'i')
                {

                    return new SemType(SemType.AtomicType.I);
                }

                if (c == 's')
                {
                    return new SemType(SemType.AtomicType.S);
                }

                if (c == 'v')
                {
                    return new SemType(SemType.AtomicType.V);
                }


                if (c == '<')
                {
                    pos++;
                    SemType left = typeParser(input);
                    pos++;
                    SemType right = typeParser(input);
                    return new SemType(left,right);
                }
                pos++;
            }
        }

        return null;

    }

}
