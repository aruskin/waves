# waves
Text-mining Virginia Woolf

Currently have code to download text from Project Gutenberg (Australia) and convert into dataframe such that each row corresponds to a paragraph (with the speaker identified) and the columns are the frequency with which each word occurs in that paragraph.

Next steps:

* Generate word clouds for each speaker

* PCA--visualize any major distinctions in speakers' styles

* Can we come up with a ML algorithm to distinguish between speakers? And should it be a GLM? GBM? SVM? 

* Let's say we find some of E.M. Forster, Lytton Strachey, T.S. Eliot, Mary Hutchinson, and Vanessa Bell's letters to Virginia Woolf (we def have some Forster and Strachey easily on hand, although ideally should find some that are already digitized). How similar are they (if at all) to the style of the characters that they supposedly serve as a basis for?