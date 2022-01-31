Exercise 2.90

* The polynomial package has 3 separate data entities that are abstracted independent of one another (in order of highest to lowest level):
	1. poly
	2. termlist
	3. terms
* The terms have type-tag 'term, termlist 'sparse or 'dense, and poly 'polynomial.
* When manipulating termlists, the generic procedures on termlists will be able to tell the type of termlist, whether it is sparse or dense.
* Polynomial procedures will still work regardless of the type of termlist used.

	