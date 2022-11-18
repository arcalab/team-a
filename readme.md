# Featured Extended Team Automata (FETA)

FETA is a formalism to reason about communication safety in families (or software product lines) of teams,
which are systems of reactive components that communicate according 
to specified synchronisation types.

This repository provides a prototype tool to specify and analysis FETA. 
Currently, it is possible to try the tool online at

 - [http://arcatools.org/feta](http://arcatools.org/feta).

To compile and run locally these analysis using a web frontend you need to compile and run the code from a specific branch of the ReoLive repository, available at https://github.com/ReoLanguage/ReoLive/tree/feta-only.

When running the tool you will be able to:
 
 - specify Extended Team Automata (ETA) and Feature Extended Team Automata (FETA);
 - Infer how many products of the Featured family exist, how many states, how many transitions, and other properties;
 - Visualise the global automaton, both before and after applying the synchronisation policies;
 - Visualise the automata of the individual components;
 - Generate logical formulas that characterise receptiveness and responsiveness, and their weak counterparts of a single ETA;
 - Use [mCRL2 model checker](https://mcrl2.org) to infer whether the ETA is receptive and responsive, producing counter-examples when not.


## Publications
- (2021) [Featured Team Automata](https://jose.proenca.org/publication/terbeek-featured-2021/). _Maurice H. ter Beek, Guillermina Cledou, Rolf Hennicker, José Proença_.  Formal Methods - 24th international symposium, FM 2019, Beijing, China, November 20-26, 2021, Proceedings.

- (2023) __Can we Communicate? Using Dynamic Logic to Verify Team Automata.__ _Maurice H. ter Beek, Guillermina Cledou, Rolf Hennicker, José Proença_.  Formal Methods - 26th international symposium, FM 2023, Lübeck, Germany, March 6-10, 2023, Proceedings - to appear.


## Developers
- [Guillermina Cledou](https://www.linkedin.com/in/guillerminacledou/)
- [Maurice H. ter Beek](http://fmt.isti.cnr.it/~mtbeek/)
- [Rolf Hennicker](https://www.sosy-lab.org/people/hennicker/)
- [José Proença](https://jose.proenca.org)