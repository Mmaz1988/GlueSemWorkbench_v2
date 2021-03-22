package glueSemantics.parser;

import glueSemantics.semantics.lambda.SemType;

public class StringParser {


    public StringParser()
    {}

    public SemType typeParser(String input, int i)
    {
        {
            while(i < input.length())
            {
                char c = input.charAt(i);
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
                    i++;
                    SemType left = typeParser(input,i);
                    i++;
                    SemType right = typeParser(input,i);
                    return new SemType(left,right);
                }
                i++;
            }
        }

        return null;

    }

}
