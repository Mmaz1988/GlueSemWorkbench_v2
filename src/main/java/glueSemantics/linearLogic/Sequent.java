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

package glueSemantics.linearLogic;


import glueSemantics.semantics.MeaningConstructor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class Sequent {
    private List<Premise> lhs;
    private LLAtom rhs;
    private int idCounter;


    public List<Premise> getLhs() {
        return lhs;
    }



    public Sequent(List<LLTerm> parsedTerms,String msg) {
        lhs = new ArrayList<>();
        for (idCounter = 0; idCounter < parsedTerms.size(); idCounter++) {
            HashSet<Integer> idSet = new HashSet<>();
            idSet.add(idCounter);
            lhs.add(new Premise(idSet, parsedTerms.get(idCounter)));
        }
    }


     public Sequent(List<MeaningConstructor> lexEn) {
         lhs = new ArrayList<>();
         for (idCounter = 0; idCounter < lexEn.size(); idCounter++) {
             HashSet<Integer> idSet = new HashSet<>();
             idSet.add(idCounter);
             lhs.add(new Premise(idSet, lexEn.get(idCounter)));
         }
     }

    // Returns the set containing all index sets (usually singletons) of the sequent's premises
    public HashSet<Integer> getMaxIDSet() {
        HashSet<Integer> maxIDSet = new HashSet<>();
        for (Premise premise : lhs) {
            maxIDSet.addAll(premise.getPremiseIDs());
        }
        return maxIDSet;
    }

    public HashSet<Integer> getNewID() {
        HashSet<Integer> newID = new HashSet<>();
        newID.add(idCounter++);
        return newID;
    }

    @Override
    public String toString() {
        return lhs + " => " +  "null";//rhs.toString();
    }


}