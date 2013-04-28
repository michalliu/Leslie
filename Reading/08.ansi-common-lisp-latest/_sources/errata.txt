ANSI Common Lisp 勘誤表
----------------------------

Here's a list of problems people have reported in ANSI Common Lisp. I haven't checked them all yet. The ones that say caught I have checked, the ones that say reported I haven't.

p. 34. I should have said that our-equal works for "lists of symbols," rather than just "lists". Caught by Bill Stratford.

p. 46. Richard Fateman points out that the best way to check for palindromes is simply (equal x (reverse x)).

p. 57. To solve problem 3.6(b) you need to use a rest parameter, which is not introduced until p. 102. Caught by Ricardo Ferreira de Oliveira.

p. 57. Problem 3.9 should say the longest path with no duplicates.

p. 60. The version of bin-search in figure 4.1 blows up if you give it an obj which is smaller than the smallest element in vec. Reported by Richard Green.

p. 71. The definition of bst-remove is broken. Sorry about that! It and bst-delete have been corrected in the source code. Caught by Chris Stover.

p. 80. In exercise 4.5, the definition of bst-adjoin is identical to the functionality of the already written bst-insert. Reported by Richard Green.

p. 97. In exercise 5.4, num-month should be month-num. Caught by David Sletten.

p. 120. "there should be single line" should have an "a" in it. Caught by Marcel Molina Jr.

p. 165. In the definition of quicksort, > should be >= on lines 12 and 13. Reported by Jed Crosby.

p. 177. Pierpaolo Bernardi reports that bubble sort should be selection sort.

p. 203. The definition of bst-delete is broken. See note on p. 71.

p. 204. In the last full paragraph, "rest field" should be "next field". Caught by Adam Langley.

p. 317. The template for cond has an extra set of parentheses. It should be (cond (test expression*)*). Caught by Adam Jacobs.

p. 338 The :read-only-p defstruct slot option should be :read-only. Caught by Laurent Peron.