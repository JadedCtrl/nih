================================================================================
NIH : `Not Invented Here`	A redundant misc. library
================================================================================

I've found that, generally, I end up using the same custom functions over and
over again across all of my projects-- sometimes I'll copy-and-paste, other
times I'll just re-implement.

... which, predictably, leads to inconsistensies. While some versions of `x`
function will flourish, gain new features, etc. etc., the others will
inevitably lag behind.

Well, no longer!

I've decided to stuff (somewhat) refactored versions of these generic functions
here, in this library!

Most of them have to deal with string manipulations, but there's a bit more 
than that-- and everything has tests! :)



----------------------------------------
USAGE
----------------------------------------

I mean... it's a library. Use a function you like. Or not. =w=
But here's my logic with the string manipulation functions:

I want to treat strings like lists. Getting the `car` of "hi there, dad", for
example, should return "hi". Getting the `cdr` should be "there, dad".
But, then again, that uses WORDS as the unit.
What if I wanted to get the `car` or some such thing as say, a character? Or
maybe even have the unit be a line?

Thus, there are three different `units`:
	* line-
	* char-
	* word-

Each of these have the following functions:
	* get-all
	* get
	* remove-all
	* remove
	* position
	* positions
	* nth
	* car
	* cdr
	* caar
	* cddr
	* (all combos of cdr and car)

... which behave pretty predictably.

The `get` functions use regex (as do the remove functions), position(s) use
the verbatim item value, etc.


Examples
--------------------

(word-car "hiya, mate-o. how're you doing? <3")
> "hiya,"

(word-nth 2 "hiya, dad. what's up?")
> "dad."

(line-get-all ".*e.*"
"hiya, what's up?
don't eat anyone else, OK?
i wouldn't like that.
would you?")
> "don't eat anyone else, OK?
would you?"

(char-remove #\h "hi there")
> "i there"

(char-remove-all #\h "hi there")
> "i tere"

... so on and so forth. 


Oh, and those are just for generic string manips. There are still some

Honourable Mentions
--------------------
	#'get-colon-values
	#'remove-colon-values
	#'positions
	#'nil-blank
	#'before
	#'after
	#'preceding
	... etc



----------------------------------------
BORING STUFF
----------------------------------------

License is in COPYING.txt (GNU LGPLv3~! <3)
Author is Jaidyn Ann <jadedctrl@teknik.io>
Sauce is at https://git.eunichx.us/nih
