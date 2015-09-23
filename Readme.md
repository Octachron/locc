Locc is a helper tool to extract comments from ocaml source code and classify
them in function of their primary language. The classification is done using
aspell dictionaries coupled to a max-likehood estimator based on utterly 
simplistic statistic model.

##Model

The statistical model used assume that

  * There is no correlation between words, i.e. sentences are i.i.d sequences
of words

  * For a given primary language, every word existing in a language has the same probability

For instance, the default model is 


```
     \    french   english  unknown      secondary

  french   0.8      0.1        0.1

  english  0.1      0.8        0.1

  primary


````

In this model, we assume that within a text primary in french, there is a 
10% probability that an English word or a word of unknown origin appears.
And reciprocally for a text primary in English, the model considers that
there is a 10% probability that a french or unknown word appears.



##Usage

```sh
locc -m model -o logs target
```
With this invocation, locc will analyze all the Ocaml source files 
(i.e ".ml{,i,y,l}") presents in `target`. If target is a directory, all the 
files and sub-directories contained in `target` will be analyzed.

Locc will then output on std a report listing the number of comments detected 
under each subclasses of the `model`. The detailed log of the analysis will be 
written in the `logs` directory.

If the option `model` is not provided, the default model is

```
 fr 0.8 0.1 0.1
%en 0.1 0.8 0.1

```
The models themselve are a '%'-separated list of "primary language name" + 
list of language probabilities within a text of primary language. Note that
the primary language name must be an aspell dictionary name.


