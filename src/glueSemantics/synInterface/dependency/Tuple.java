/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package glueSemantics.synInterface.dependency;

import edu.stanford.nlp.ling.IndexedWord;

public class Tuple {

    final IndexedWord right;
    final String left;

    public Tuple(String left,IndexedWord right){
        this.left = left;
        this.right = right;


    }

    @Override
    public String toString() {
        return "(" + left + ": " + right.toString() + "--" + right.index() + ")";
    }
}
